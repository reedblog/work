#
# Age-cohort smoking forecasts - curated data prep
# Reed Sorensen, March 2015
#

require(dplyr)
require(data.table)


## CREATE DIRECTORY FOR INPUT AND OUTPUT DATA FILES

data_dir <- "~/prog/data/forecast_smoking_age_cohort/"
dir.create(file.path(data_dir), showWarnings = FALSE)
dir.create(file.path(data_dir, "infiles"), showWarnings = FALSE)
dir.create(file.path(data_dir, "outfiles"), showWarnings = FALSE)



## GET LOCATION IDENTIFIERS

require(foreign)
codes.in <- read.dta("/snfs1/DATA/IHME_COUNTRY_CODES/IHME_COUNTRY_CODES_Y2013M07D26.DTA")
saveRDS(codes.in, file.path(data_dir, "infiles/codes_in.RDS"))
loc_vars <- c(
  "location_name", "location_id", "iso3",
  "gbd_analytical_region_id", "gbd_analytical_region_name" )
codes <- arrange(codes.in[, loc_vars], location_id)



## SUBSET LOCATIONS TO GBD COUNTRIES (this gets 188, not 192)

pop.in <- fread(input = "/snfs1/temp/fbd/data/reed_pop_1980.csv") %>% as.data.frame(.)
saveRDS(pop.in, file.path(data_dir, "infiles/reed_pop_1980.RDS"))
codes188 <- codes[codes$location_id %in% unique(pop.in$location_id), ]
saveRDS(codes188, file.path(data_dir, "infiles/codes188.RDS"))



## DEFINE AGE CATEGORIES

agecat_old <- data.frame(
  age_group_id = 5:21,
  age_start_year = c(1, seq(5, 80, by = 5)),
  n = c(4, rep(5, 15), 31)
)
agecat_old$age_mid <- agecat_old$age_start_year + (agecat_old$n / 2)
agecat_old$age_mid[agecat_old$age_mid == 95.5] <- 82.5
saveRDS(agecat_old, file.path(data_dir, "infiles/age_categories_over1.RDS"))



## SET CONSTANTS

iso3.const <- codes188$iso3
countries.const <- codes188[codes188$iso3 %in% iso3.const, "location_id"]
years.const <- 1980:2013
sex.const <- c(1, 2)



## DATA PREP

prev.in <- fread(input = "/snfs1/temp/fbd/data/smoking_prev_all.csv")
saveRDS(prev.in, file.path(data_dir, "infiles/smoking_prev_all.RDS"))

prev <- prev.in %>%
  filter(
    location_id %in% countries.const &
      year %in% years.const &
      sex %in% sex.const &
      age_group_id %in% 7:21) %>%
  left_join(., as.data.table(agecat_old), by = "age_group_id") %>%
  mutate(
    smoking_prev = ifelse(smoking_prev == 0, 0.0000001, smoking_prev),
    cohort = year - age_mid ) %>%
  as.data.frame(.)

dat <- pop.in %>%
  select(year, location_id, sex, age_group_id, pop, envelope) %>%
  right_join(., prev, by = c("location_id", "year", "sex", "age_group_id"))

dat$n_smokers <- dat$pop * dat$smoking_prev



## SAVE FINAL ANALYTIC DATA SET

saveRDS(dat, file.path(data_dir, "infiles/dat.RDS"))
write.csv(dat, file.path(data_dir, "infiles/dat.csv"), row.names = FALSE)


