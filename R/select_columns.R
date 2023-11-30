#' @title Column Selection
#' @description Select the column in a specific new data set.
#' @param base_data the `studies` database table.
#' @param new_data the new dataset used for feature selection.
#' @param colnms the name of selected columns.
#' @return A tibble of new dataset with selected feature.
#' @importFrom dplyr select collect filter distinct group_by summarize
#' @importFrom rlang enquo
#' @export
select_colomns <- function(base_data, new_data, colnms) {
  feature <- enquo(colnms)

  new_data |>
    select(nct_id, !!feature) |>
    collect() |>
    filter(nct_id %in% base_data$nct_id) |>
    distinct() |>
    group_by(nct_id) |>
    summarize(column_name = list(!!feature))
}
