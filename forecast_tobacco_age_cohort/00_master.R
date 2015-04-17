# run all scripts
dir <- "~/prog/work_projects/forecast_smoking_age_cohort/"

files <- c(
  "01_curated_data_prep.R",
  "02_raw_data_prep.R",
  "03_models.R"
)

for (fname in files) { source(file.path(dir, fname)) }