#
# Age-cohort smoking forecasts - raw data prep
# Reed Sorensen, March 2015
#

require(dplyr)
require(data.table)

j_drive <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/snfs1/")
data_dir <- "~/prog/data/forecast_smoking_age_cohort/"

# READ IN DATA
require(readstata13)
raw.in <- read.dta13(file.path(j_drive, "WORK/01_covariates/02_inputs/smoking_prevalence/visuals/VizToolDatasets/data_sourced.dta"))
saveRDS(raw.in, file.path(data_dir, "infiles/smoking_prev_raw_original.RDS"))


# GET REGION FOR EACH COUNTRY
codes188 <- readRDS(file.path(data_dir, "infiles/codes188.RDS"))
raw2.in <- raw.in %>%
  left_join(., codes188[, c("iso3", "gbd_analytical_region_id", "gbd_analytical_region_name")],
    by = "iso3")


# -- subset to GBD countries (188) and create cohort variable
agecat_old <- readRDS(file.path(data_dir, "infiles/age_categories_over1.RDS"))
raw <- raw2.in %>%
  filter(iso3 %in% unique(codes188$iso3)) %>%
  left_join(., as.data.table(agecat_old), by = c("age_start" = "age_start_year"))

raw$cohort <- raw$year - raw$age_mid


saveRDS(raw, file.path(data_dir, "infiles/smoking_prev_raw.RDS"))



