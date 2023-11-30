## code to prepare `DATASET` dataset goes here

accel <- readRDS("accel.rds")
usethis::use_data(accel, overwrite = TRUE)

conditions <- readRDS("conditions.rds")
usethis::use_data(conditions, overwrite = TRUE)

studies <- readRDS("studies.rds")
usethis::use_data(studies, overwrite = TRUE)

designs <- readRDS("designs.rds")
usethis::use_data(designs, overwrite = TRUE)
