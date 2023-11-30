#' Accelerometry Data Resampled from UK Biobank
#'
#' Toy accelerometry data for BIS620
#'
#' @format ## `accel`
#' A data frame with 1,080,000 rows and 4 columns:
#' \describe{
#'   \item{time}{the time of the measurement}
#'   \item{X, Y, Z}{Accelerometry measurement (in milligravities).}
#' }
"accel"

#' Information about studies for Query Data from ClinicalTrials.gov
#'
#'
#' @format ## `studies`
#' A tibble with 474,199 rows and 70 columns, including nct_id,
#' studies' submitted date, etc..
#' \describe{
#'   \item{nct_id}{clinical trials id number}
#'   \item{brief_title}{brief title keywords}
#'   \item{source_class}{sponsor type}
#' }
"studies"

#' Information about conditions for Query Data from ClinicalTrials.gov
#'
#'
#' @format ## `conditions`
#' A tibble with 821,676 rows and 4 columns, including nct_id, id,
#' conditions' name and corresponding lowercase.
#' \describe{
#'   \item{nct_id}{clinical trials id number}
#'   \item{id}{patient id number}
#'   \item{name}{conditions name}
#'   \item{downcase_name}{conditions name}
#' }
"conditions"

#' Information about designs for Query Data from ClinicalTrials.gov
#'
#'
#' @format ## `designs`
#' A tibble with 469,656 rows and 14 columns, including nct_id, allocation
#' ways, intervention model, etc..
#' \describe{
#'   \item{nct_id}{clinical trials id number}
#'   \item{allocation}{allocation ways, whether or not randomized}
#'   \item{intervention_model}{intervention model}
#' }
"designs"
