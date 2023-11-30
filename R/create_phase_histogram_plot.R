#' @title Phase Histogram
#' @description Create a histogram of the study phases returned by
#' Phase Histogram
#'
#' Create a histogram of the phases.
#'
#' @param d the database table.
#' @param way the filtering choices of allocation ways
#' @return A histogram for numerical phases of clinical trials
#' @importFrom dplyr left_join select filter group_by summarize
#' @importFrom ggplot2 geom_col theme_bw xlab ylab theme element_text
#' @importFrom tibble add_row
#' @export
create_phase_histogram_plot <- function(d, way) {

  designs_data <- select_colomns(d, designs, intervention_model)
  colnames(designs_data)[2] <- "design"
  d <- left_join(d, designs_data, by = "nct_id")

  alct_data <- select_colomns(d, designs, allocation)
  colnames(alct_data)[2] <- "allocation"
  d <- left_join(d, alct_data, by = "nct_id")

  if (!is.null(way)) {
    d <- d |>
      filter(allocation %in% !!way)
  }

  d$phase[is.na(d$phase)] <- "NA"
  d <- d |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  phase_name <-  c("NA", "Not Applicable", "Early Phase 1",
                   "Phase 1", "Phase 1/Phase 2",
                   "Phase 2", "Phase 2/Phase 3", "Phase 3", "Phase 4")

  for (name in phase_name) {
    if (!name %in% d$phase) {
      d <- d |>
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
