# 01_linear_model.R
# Reed Sorensen, March 2015
#
# Run a linear model on raw smoking prevalnce data
# -- First stage in spacetime GPR
#

require(dplyr)
require(data.table)
require(foreign)
require(boot)

project_name <- "forecast_tobacco_gpr"

#-- set OS-specific paths
jpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")
on_windows <- ifelse(Sys.info()[["sysname"]] == "Windows", TRUE, FALSE)


#-- create folders for saving data inputs/outputs
data_sav <- paste0("C:/Users/rsoren/Documents/prog/data/", project_name, "/")
data_dir <- paste0(hpath, "prog/data/", project_name, "/")
dir.create(file.path(data_sav), showWarnings = FALSE)
dir.create(file.path(data_dir), showWarnings = FALSE)


#-- get location identifiers
codes.in <- read.dta(file.path(jpath, "/DATA/IHME_COUNTRY_CODES/IHME_COUNTRY_CODES_Y2013M07D26.DTA"))
if (on_windows) saveRDS(codes.in, file.path(data_sav, "codes_in.RDS"))
# codes.in <- readRDS(file.path(data_sav, "codes_in.RDS"))
loc_vars <- c(
  "location_name", "location_id", "iso3",
  "gbd_analytical_region_id", "gbd_analytical_region_name",
  "gbd_superregion_id", "gbd_superregion_name")
codes <- arrange(codes.in[, loc_vars], location_id)


#-- define age groups
agecat_old <- data.frame(
  age_group_id = 5:21,
  age_start = c(1, seq(5, 80, by = 5)),
  n = c(4, rep(5, 15), 31) )
agecat_old$age_mid <- agecat_old$age_start + (agecat_old$n / 2)
agecat_old$age_mid[agecat_old$age_mid == 95.5] <- 82.5
if (on_windows) saveRDS(agecat_old, file.path(data_sav, "age_categories_over1.RDS"))


#-- read in population data and get IDs for 188 countries
# pop.in <- fread(input = file.path(jpath, "/temp/fbd/data/reed_pop_1980.csv")) %>% as.data.frame(.)
# if (on_windows) saveRDS(pop.in, file.path(data_sav, "reed_pop_1980.RDS"))
# pop.in <- readRDS(file.path(data_sav, "reed_pop_1980.RDS"))
# codes188 <- codes[codes$location_id %in% unique(pop.in$location_id), ]
# if (on_windows) saveRDS(codes188, file.path(data_sav, "codes188.RDS"))
# pop <- left_join(pop.in, agecat_old, by = "age_group_id")
# saveRDS(codes188, paste0(data_dir, "codes188.RDS"))
codes188 <- readRDS(paste0(data_dir, "codes188.RDS"))



#-- read in raw smoking prevalence data and subset to 188 countries
#
#-- ## the following lines are for the old data without sample size
# require(readstata13)
# raw.in <- read.dta13(file.path(jpath,
#   "WORK/01_covariates/02_inputs/smoking_prevalence/visuals/VizToolDatasets/data_sourced.dta"))
# if (on_windows) saveRDS(raw.in, file.path(data_sav, "smoking_prev_raw_original.RDS"))
# raw.in <- readRDS(file.path(data_sav, "smoking_prev_raw_original.RDS"))
#
# raw <- raw.in %>%
#   left_join(., codes188, by = "iso3") %>%
#   mutate(year = year_start) %>%
#   filter(location_id %in% unique(codes188$location_id)) %>%
#   left_join(., pop[, c("location_id", "sex", "year", "age_start", "pop")],
#     by = c("location_id", "sex", "year", "age_start")) %>%
#   mutate(age = age_start)


#-- raw data with sample size
# raw2.in <- read.dta(file.path(jpath,
#   "WORK/01_covariates/02_inputs/smoking_prevalence/data/03_age_split/",
#   "prepped_plusnew/prepped_nosn.dta"))
# if (on_windows) saveRDS(raw2.in, file.path(data_sav, "smoking_prev_raw_with_samplesize.RDS"))
raw2.in <- readRDS(file.path(data_sav, "smoking_prev_raw_with_samplesize.RDS"))

n_by_superregion <- raw2.in %>%
  select(iso3, sample_size) %>%
  mutate(sample_size2 = ifelse(sample_size == 0, NA, sample_size)) %>%
  left_join(., codes188[, c("iso3", "gbd_superregion_name")], by = "iso3") %>%
  group_by(gbd_superregion_name) %>%
  summarize(mean_sample = mean(sample_size, na.rm = TRUE))

quantile(n_by_superregion$mean_sample, probs = 0.025) # n = 318, for replacing all n=0 and NA

raw <- raw2.in %>%
  left_join(., codes188, by = "iso3") %>%
  mutate(year = year_start) %>%
  filter(iso3 %in% unique(codes188$iso3)) %>%
  select(iso3, year, sex, age_start, sample_size, prevalence)

raw$sample_size[raw$sample_size %in% c(0, NA)] <- 318 # based on superregion 2.5 percentile above




#-- re-assign outliers and re-scale prevalence to fill [0,1]

quantile(raw$prevalence, probs = 0.99) # 99th pctl= 0.6669453
max_prev <- 0.7
table(raw$prevalence > max_prev) # 227 surveys with prev > 0.7
table(raw$iso3, raw$prevalence > max_prev) # check survey prev > 0.7 by country
raw$prevalence2 <- raw$prevalence # where prev > 0.7, replace with 0.7
raw$prevalence2[raw$prevalence2 > max_prev] <- max_prev
adjustment <- 1 / (max_prev + 0.05) # re-scaling prev to range [0,1] with some margin on upper end

# #-- get standard errors for prevalence estimates
# raw$prevalence_se <- mapply(
#   function(p, n) { sqrt(1/n*p*(1-p)) },
#   p = raw$prevalence,
#   n = raw$sample_size
# )

#-- alternative to calculating variance from sample size, assume SE = 0.03
raw$prevalence_se <- 0.03


#-- get 1000 draws from distribution around rnorm(prevalence, SE)
#
# draws <- data.frame(
#   mean = raw$prevalence2,
#   sd = raw$prevalence_se
# )
# draws[, paste0("draw", 1:1000)] <- as.numeric(NA) # initialize draw variables
#
# for (i in 1:1000) {
#   draws[, paste0("draw", i)] <- mapply(rnorm, 1, draws$mean, draws$sd)
#   draws[, paste0("draw", i)] <- ifelse( # if less than 0, replace with 0.0001
#     draws[, paste0("draw", i)] <= 0, 0.0001, draws[, paste0("draw", i)])
#   print(i)
# }
# saveRDS(draws, paste0(data_sav, "raw_savdraws.RDS")) # raw data with n, but assume SE = 0.03
# saveRDS(draws, paste0(data_sav, "raw_savdraws_withn.RDS")) # raw data with n converted to SE
draws <- readRDS(paste0(data_sav, "raw_savdraws.RDS"))

draws_matrix <- as.matrix(draws[, names(draws)[grepl("draw", names(draws))]])
draws_matrix[draws_matrix > max_prev + 0.05] <- max_prev + 0.04
draws_matrix <- logit(draws_matrix * adjustment)


raw$observed_data <- apply(draws_matrix, 1, median)
raw$observed_data_se <- apply(draws_matrix, 1, sd)
raw$obs_data_variance <- raw$observed_data_se^2

#-- artificially increase standard error, if applicable
# raw$observed_data_se <- raw$observed_data_se * 2.0
# raw$obs_data_variance <- raw$observed_data_se^2


# raw$test_logit <- logit(raw$prevalence * adjustment)
# with(raw, plot(observed_data, observed_data_se))




#-- prepare variables for the linear regression
template <- expand.grid(stringsAsFactors = FALSE,
  sex = c(1,2),
  iso3 = unique(codes188$iso3),
  age_start = unique(raw$age_start),
  year = 1970:2040) %>%
  left_join(., raw, by = c("iso3", "year", "sex", "age_start"))

codes_region <- codes188[, c("iso3", "gbd_analytical_region_id")]
names(codes_region)[names(codes_region) == "gbd_analytical_region_id"] <- "gbd_region_name"

template2 <- template %>%
  mutate(
    observed_data_outlier = NA,
    obs_data_variance_outlier = NA,
    sex = as.factor(ifelse(sex == 1, "male", "female")),
    age_start = as.character(age_start),
    iso3 = as.factor(iso3)) %>%
  left_join(., codes_region, by = "iso3") %>%
  mutate(gbd_region_name = as.factor(gbd_region_name))

#-- set reference age category as 42.5
age_order <- sort(unique(template2$age))
age_order_factor <- c("40", age_order[!age_order == "40"])
template2$age <- factor(template2$age_start, levels = age_order_factor)


template3 <- template2[, c(
  "iso3", "gbd_region_name", "sex", "age", "year",
  "observed_data", "observed_data_se", "obs_data_variance",
  "observed_data_outlier", "obs_data_variance_outlier")] %>%
  arrange(gbd_region_name, iso3, year, sex, age)

template3_2 <- template3 # for out of sample testing

fit <- lm(observed_data ~ year + sex + age + gbd_region_name, data = template3)
template3$stage1_prediction <- predict(fit, newdata = template3)

# write.csv(template3, paste0(data_dir, "linear_output.csv"))
# if (on_windows) write.csv(template3, paste0(data_sav, "linear_output.csv"))

output_name <- "logit_rescaled_l2_o05_z99_1333_bothsexes"

dir.create(paste0(data_dir, output_name), showWarnings = FALSE)
write.csv(template3, paste0(data_dir, output_name, "/linear_output.csv"))




## out of sample

in_sample <- c(1980, 2003)
out_sample <- c(2004, 2013)

obs_vars <- c(
  "observed_data",
  "observed_data_se",
  "obs_data_variance",
  "observed_data_outlier",
  "obs_data_variance_outlier" )


df_train1 <- subset(template3_2, year %in% in_sample[1]:in_sample[2])
df_train2 <- subset(template3_2, year %in% out_sample[1]:out_sample[2])
df_train2[, obs_vars] <- lapply(df_train2[obs_vars], function(x) x <- NA)
df_train <- rbind(df_train1, df_train2) %>%
  arrange(iso3, sex, age, year)

fit <- lm(observed_data ~ year + sex + age + gbd_region_name, data = df_train)
df_train$stage1_prediction <- predict(fit, newdata = df_train)

write.csv(df_train, paste0(data_dir, output_name, "/linear_output_outofsample_test.csv"))




