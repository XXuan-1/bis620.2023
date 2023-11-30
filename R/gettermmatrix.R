#' @title Word Frequency for Word Cloud
#' @description  Get the frequency of words from the text.
#' @param x the text.
#' @return Frequency of words with descending order.
#' @importFrom memoise memoise
#' @export
gettermmatrix <- memoise(function(x) {

  conditions_data <- select_colomns(x, conditions, name)
  colnames(conditions_data)[2] <- "condition"
  x <- left_join(x, conditions_data, by = "nct_id")

  designs_data <- select_colomns(x, designs, intervention_model)
  colnames(designs_data)[2] <- "design"
  x <- left_join(x, designs_data, by = "nct_id")

  alct_data <- select_colomns(x, designs, allocation)
  colnames(alct_data)[2] <- "allocation"
  x <- left_join(x, alct_data, by = "nct_id")

  x <- as.character(x)
  if (is.character(x)) {
    mycorpus <- Corpus(VectorSource(x))
    mycorpus <- tm_map(mycorpus, content_transformer(tolower))
    mycorpus <- tm_map(mycorpus, removePunctuation)
    mycorpus <- tm_map(mycorpus, removeNumbers)
    mycorpus <- tm_map(mycorpus, removeWords,
                       c(stopwords("SMART"), "thy", "thou", "thee", "the",
                         "and", "but"))
    mydtm <- TermDocumentMatrix(mycorpus,
                                control = list(minWordLength = 1))
    m <- as.matrix(mydtm)
    sort(rowSums(m), decreasing = TRUE)
  }
})
