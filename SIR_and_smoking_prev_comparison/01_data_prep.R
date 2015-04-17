#
# 01_data_prep.R
# Reed Sorensen, April 2015
#
# Prepare smoking prevalence and SIR data
# -- Smoking prevalence from a STGPR model defined below
# -- SIR data from 'J:\WORK\05_risk\02_models\02_results\smoking_direct_sir\exp\50'
#

require(dplyr)
require(data.table)
require(foreign)
require(boot)

project_name <- "SIR_and_smoking_prev_comparsion"
model_name <- "logit_rescaled_l2_o05_z99_1333_bothsexes" # for getting smoking prev data
adjustment <- 1.333333


#-- set OS-specific paths
jpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")
on_windows <- ifelse(Sys.info()[["sysname"]] == "Windows", TRUE, FALSE)


#-- create folders for saving data inputs/outputs
data_sav <- paste0("C:/Users/rsoren/Documents/prog/data/", project_name, "/")
data_dir <- paste0(hpath, "prog/data/", project_name, "/")
dir.create(file.path(data_sav), showWarnings = FALSE)
dir.create(file.path(data_dir), showWarnings = FALSE)


#-- read in SIR data
path_to_sir <- paste0(jpath, "WORK/05_risk/02_models/02_results/smoking_direct_sir/exp/50/")

sir.in <- rbindlist(lapply(list.files(path_to_sir), function(x) {
  tmp <- fread(paste0(path_to_sir, x))
  tmp$iso3 <- substr(x, 5, nchar(x) - 4)
  return(tmp)
}))

sir <- sir.in %>%
  mutate(
    sex = ifelse(sex == 1, "male", ifelse(sex == 2, "female", NA))) %>%
  filter(
    sex == "male",
    parameter == "cat1") %>%
  select(iso3, year, sex, age = gbd_age_start, exp_mean, exp_lower, exp_upper)


saveRDS(sir, paste0(data_dir, "sir_only.RDS"))
sir_USA <- subset(sir, iso3 == "USA")


#-- read in smoking prevalence data

path_to_prev <- paste0(hpath, "prog/data/forecast_tobacco_gpr/", model_name, "/")
prev.in <- fread(paste0(path_to_prev, "gpr_output.csv"))

prev <- prev.in %>%
  mutate(
    observed_data2 = inv.logit(observed_data) * 1/adjustment,
    stage1_prediction2 = inv.logit(stage1_prediction) * 1/adjustment,
    st_prediction2 = inv.logit(st_prediction) * 1/adjustment,
    gpr_mean2 = inv.logit(gpr_mean) * 1/adjustment,
    gpr_lower2 = inv.logit(gpr_lower) * 1/adjustment,
    gpr_upper2 = inv.logit(gpr_upper) * 1/adjustment,
    cohort = as.factor(year - age )) %>%
  filter(
    year %in% 1970:2013) %>%
  select(iso3, year, sex, age, gpr_mean2, gpr_lower, gpr_upper2)

prev2 <- prev[!duplicated(subset(prev, select = c(iso3, year, age, sex)))]
prev_USA <- subset(prev2, iso3 == "USA")


#-- check data before merge
str(subset(prev_USA, select = c(iso3, year, age, sex)))
str(subset(sir_USA, select = c(iso3, year, age, sex)))


df <- merge(prev2, sir, by = c("iso3", "year", "age", "sex"), all.x = TRUE) %>%
  arrange(iso3, year, age, sex)

saveRDS(df, paste0(data_sav, "smoking_prev_sir_merged.RDS"))
saveRDS(df, paste0(data_dir, "smoking_prev_sir_merged.RDS"))


## lung cancer mortality

lcm <- read.csv(paste0(data_dir, "lung_cancer_mortality.csv"))
saveRDS(lcm, paste0(data_sav, "lung_cancer_mortality.RDS"))




