## tar_load(birth_cohort)

## saveRDS(birth_cohort, "data/processed/birth-cohort.rds")

## d <- readRDS("data/processed/birth-cohort.rds")

writeData <- function(data, dir = "data/processed") {
  target_name <- as.character(substitute(data))

  rds_path <- file.path(dir, paste0(target_name, ".rds"))
  csv_path <- file.path(dir, paste0(target_name, ".csv"))

  saveRDS(data, rds_path)
  write_csv(data, csv_path)

  c(rds_path, csv_path)
}
