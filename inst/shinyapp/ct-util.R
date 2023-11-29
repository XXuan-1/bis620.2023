library(dplyr)
library(tidyr)
library(purrr)
library(DBI)
library(DT)
library(ggplot2)
library(tm)
library(wordcloud)
library(memoise)
library(NLP)



#' @title Query keywords from a database table.
#' @description filter rows with specific query keywords
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).

query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}

#' @title Column Selection
#' @description Select the column in a specific new data set.
#' @param base_data the `studies` database table.
#' @param new_data the new dataset used for feature selection.
#' @param colnms the name of selected columns.
#' @return A tibble of new dataset with selected feature.
select_colomns = function(base_data, new_data, colnms) {
  feature = enquo(colnms)

  new_data |>
    select(nct_id, !!feature) |>
    collect() |>
    filter(nct_id %in% base_data$nct_id) |>
    distinct() |>
    group_by(nct_id) |>
    summarize(column_name = list(!!feature))
}



#' @title Phase Histogram
#' @description Create a histogram of the study phases returned by a brief title keyword search
#' @param d the database table.
#' @param way the filtering choices of allocation ways
#' @return A histogram for numerical phases of clinical trials
create_phase_histogram_plot = function(d, way) {

  designs_data = select_colomns(d, designs, intervention_model)
  colnames(designs_data)[2] = "design"
  d = left_join(d, designs_data, by = "nct_id")

  alct_data = select_colomns(d, designs, allocation)
  colnames(alct_data)[2] = "allocation"
  d = left_join(d, alct_data, by = "nct_id")

  if (!is.null(way)) {
    d = d |>
      filter(allocation %in% !!way)
  }

  d$phase[is.na(d$phase)] = "NA"
  d = d |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  phase_name =  c("NA", "Not Applicable", "Early Phase 1", "Phase 1", "Phase 1/Phase 2",
                                           "Phase 2", "Phase 2/Phase 3", "Phase 3", "Phase 4")

  for (name in phase_name) {
    if (!name %in% d$phase){
      d = d |>
        add_row(phase = name, n = 0)
    }
  }

  ggplot(d, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#' @title Condition Histogram
#' @description  Create a histogram of the conditions.
#' @param x the database table.
#' @param num_conditions Number of most frequent conditions.
#' @return A histogram of the most frequent conditions in a determined amount.
create_condition_histogram = function(x, num_conditions) {

  conditions_data = select_colomns(x, conditions, name)
  colnames(conditions_data)[2] = "condition"
  x = left_join(x, conditions_data, by = "nct_id")

  x = x |>
    select(condition) |>
    unnest(condition) |>
    group_by(condition) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    head(num_conditions)

  ggplot(x, aes(x = condition, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Condition") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#' @title Design Histogram
#' @description Create a histogram of the intervention study models
#' @param x the database table.
#' @param way the filtering choices of allocation ways
#' @return A histogram for the strategy for assigning interventions to participants.
create_design_histogram = function(x, way) {

  designs_data = select_colomns(x, designs, intervention_model)
  colnames(designs_data)[2] = "design"
  x = left_join(x, designs_data, by = "nct_id")

  alct_data = select_colomns(x, designs, allocation)
  colnames(alct_data)[2] = "allocation"
  x = left_join(x, alct_data, by = "nct_id")

  if (!is.null(way)) {
    x = x |>
      filter(allocation %in% !!way)
  }

  x$design[is.na(x$design)] = "NA"
  x$allocation[is.na(x$allocation)] = "NA"

  x = x |>
    select(design) |>
    group_by(design) |>
    summarize(n = n())

  x$design<- as.character(trimws(x$design))

  ggplot(x, aes(x = design, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Intervention Study Model") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#' @title Word Frequency for Word Cloud
#' @description  Get the frequency of words from the text.
#' @param x the text.
#' @return Frequency of words with descending order.
getTermMatrix <- memoise(function(x) {

  conditions_data = select_colomns(x, conditions, name)
  colnames(conditions_data)[2] = "condition"
  x = left_join(x, conditions_data, by = "nct_id")

  designs_data = select_colomns(x, designs, intervention_model)
  colnames(designs_data)[2] = "design"
  x = left_join(x, designs_data, by = "nct_id")

  alct_data = select_colomns(x, designs, allocation)
  colnames(alct_data)[2] = "allocation"
  x = left_join(x, alct_data, by = "nct_id")

  x = as.character(x)
  if (is.character(x)){
    myCorpus = Corpus(VectorSource(x))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    sort(rowSums(m), decreasing = TRUE)
  }
})