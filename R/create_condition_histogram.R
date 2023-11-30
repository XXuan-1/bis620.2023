#' @title Condition Histogram
#' @description  Create a histogram of the conditions.
#' @param x the database table.
#' @param num_conditions Number of most frequent conditions.
#' @return A histogram of the most frequent conditions in a determined amount.
#' @importFrom utils head
#' @importFrom dplyr arrange desc
#' @importFrom tidyr unnest
#' @importFrom ggplot2 geom_col theme_bw xlab ylab theme element_text
#' @export
create_condition_histogram <- function(x, num_conditions) {

  conditions_data <- select_colomns(x, conditions, name)
  colnames(conditions_data)[2] <- "condition"
  x <- left_join(x, conditions_data, by = "nct_id")

  x <- x |>
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
