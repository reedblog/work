# INLA smoking forecasts - data prep
# Reed Sorensen, February 2015
#
# -- input data for INLA is sent to "~/prog/data"
# 

require(dplyr)
require(data.table)
require(INLA)
require(stringr)
require(boot)
require(reshape2)


# get location IDs

require(foreign)
country_codes.dat <- read.dta("J:/DATA/IHME_COUNTRY_CODES/IHME_COUNTRY_CODES_Y2013M07D26.DTA")
loc_vars <- c(
  "location_name", "location_id", "iso3", 
  "gbd_analytical_region_id", "gbd_analytical_region_name" )
cds <- arrange(country_codes.dat[, loc_vars], location_id)

# saveRDS(df_pop, "infiles/df_pop.RDS")
# saveRDS(country_codes.dat, "infiles/country_codes.RDS")


# -- subset locations to those with available data (this gets 188, not 192)
df_poptmp <- fread(input = "J:/temp/fbd/data/reed_pop_1980.csv") %>% as.data.frame(.)
locs <- cds[cds$location_id %in% unique(df_poptmp$location_id), ]

# saveRDS(locs, "infiles/locs.RDS")


## SETTING CONSTANTS

iso3.const <- locs$iso3
countries.const <- locs[locs$iso3 %in% iso3.const, "location_id"]
years.const <- 1980:2013
sex.const <- c(1, 2)


## DATA PREP

prev.in <- fread(input = "J:/temp/fbd/data/smoking_prev_all.csv")

prev <- prev.in %>%
  filter(
    location_id %in% countries.const & 
      year %in% years.const & 
      sex %in% sex.const & 
      age_group_id %in% 7:21) %>%
  mutate(smoking_prev = ifelse(smoking_prev == 0, 0.0000001, smoking_prev)) %>%
  as.data.frame(.)

pop.in <- df_poptmp

dat <- pop.in %>%
  select(year, location_id, sex, age_group_id, pop, envelope) %>%
  right_join(., prev, by = c("location_id", "year", "sex", "age_group_id"))

dat$n_smokers <- dat$pop * dat$smoking_prev


dir.create("~/prog/data/inla_smoking_forecast_v2")

nPred <- 27 # number of years to predict

nSmoke <- do.call("rbind", lapply(split(dat, paste0(dat$iso3, "_", dat$sex)), function(df) {
  
#   df <- subset(dat, iso3 == "CAN" & sex == 1) # just for debugging
  df2 <- df[, c("iso3", "location_id", "sex", "age_group_id", "year", "n_smokers")]
  df3 <- dcast(df2, formula = iso3 + location_id + age_group_id + sex ~ year, value.var = "n_smokers")
  for (yr in (as.numeric(names(df3)[ncol(df3)]) + 1):(as.numeric(names(df3)[ncol(df3)]) + nPred)) {
    df3[, as.character(yr)] <- as.numeric(NA)
  }
  
  write.table(df3[5:ncol(df3)], 
    file = paste0("~/prog/data/inla_smoking_forecast_v2/nSmokers_", unique(df2$iso3), "_", unique(df2$sex), ".csv"),
    sep = ",",
    na = "",
    row.names = FALSE,
    col.names = FALSE
  )
  
  return(df3)
  
}))


nPop <- do.call("rbind", lapply(split(dat, paste0(dat$iso3, "_", dat$sex)), function(df) {
  
  #   df <- subset(dat, iso3 == "CAN" & sex == 1) # just for debugging
  df2 <- df[, c("iso3", "location_id", "sex", "age_group_id", "year", "pop")]
  df3 <- dcast(df2, formula = iso3 + location_id + age_group_id + sex ~ year, value.var = "pop")
  for (yr in (as.numeric(names(df3)[ncol(df3)]) + 1):(as.numeric(names(df3)[ncol(df3)]) + nPred)) {
    df3[, as.character(yr)] <- as.numeric(NA)
  }
  
  write.table(df3[5:ncol(df3)], 
    file = paste0("~/prog/data/inla_smoking_forecast_v2/pop_", unique(df2$iso3), "_", unique(df2$sex), ".csv"),
    sep = ",",
    na = "",
    row.names = FALSE,
    col.names = FALSE
  )
  
  return(df3)
  
}))


## read in and prepping raw data

# raw.in <- read.dta("J:/WORK/01_covariates/02_inputs/smoking_prevalence/visuals/VizToolDatasets/data_sourced.dta")
raw.in <- read.csv("~/prog/data/_old_data/inla_smoking_forecast_v2/infiles/smoking_prev_raw.csv")

raw <- subset(raw.in, iso3 %in% c("USA", "CAN"))



