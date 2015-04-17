#
# Age-cohort smoking forecasts - models
# Reed Sorensen, March 2015
#

require(lme4)
require(dplyr)
require(data.table)

# LOAD DATA
data_dir <- "~/prog/data/forecast_smoking_age_cohort/"
dat <- readRDS(file.path(data_dir, "infiles/smoking_prev_raw.RDS"))

dat2 <- dat[1:3, ]
write.csv(dat2, file.path(data_dir, "outfiles/cluster_test5.csv"))

fit1 <- lmer(prevalence ~ as.factor(age_mid) + as.factor(cohort)
  + (1 | iso3) + (as.factor(age_mid) | gbd_analytical_region_id)
  + (as.factor(cohort) | gbd_analytical_region_id),
  data = dat)

saveRDS(fit1, file.path(data_dir, "outfiles/lmer_fit1.RDS"))

