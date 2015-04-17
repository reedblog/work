#
# INLA smoking forecasts - data prep
# Reed Sorensen, February 2015
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


# -- subset locations to those with available data (this gets 188, not 192)
df_poptmp <- fread(input = "J:/temp/fbd/data/reed_pop_1980.csv") %>% as.data.frame(.)
locs <- cds[cds$location_id %in% unique(df_poptmp$location_id), ]


## SETTING CONSTANTS

iso3.const <- locs$iso3
countries.const <- locs$location_id
years.const <- 1980:2013
sex.const <- c(1, 2)



## DATA PREP

# prev.in <- fread(input = "J:/temp/fbd/data/smoking_prev_all.csv")
# saveRDS(prev.in, file = paste0(dir, "infiles/smoking_prev_all.RDS"))
prev.in <- readRDS(file = paste0(dir, "infiles/smoking_prev_all.RDS"))

prev <- prev.in %>%
  filter(
    location_id %in% countries.const & 
    year %in% years.const & 
    sex %in% sex.const & 
    age_group_id %in% 7:21) %>%
  mutate(smoking_prev = ifelse(smoking_prev == 0, 0.0000001, smoking_prev)) %>%
  as.data.frame(.)

# prev$age_start <- as.numeric(NA)
# prev$age_mid <- as.numeric(NA)
# 
# for (i in 1:nrow(prev)) { 
#   prev[i, "age_start"] <- agecat[agecat$age_group_id == prev[i, "age_group_id"], "age_start_year"]
#   prev[i, "age_mid"] <- agecat[agecat$age_group_id == prev[i, "age_group_id"], "age_mid"]
# }
# 
# prev$cohort <- prev$year - prev$age_mid
# prev$smoking_prev_logit <- boot::logit(prev$smoking_prev)


pop.in <- as.data.frame(fread(input= "J:/temp/fbd/data/reed_pop_1980.csv"))

dat <- pop.in %>%
  select(year, location_id, sex, age_group_id, pop, envelope) %>%
  right_join(., prev, by = c("location_id", "year", "sex", "age_group_id"))

dat$n_smokers <- dat$pop * dat$smoking_prev



nPred <- 27 # number of years to predict

nSmoke <- do.call("rbind", lapply(split(dat, paste0(dat$iso3, "_", dat$sex)), function(df) {
  
#   df <- subset(dat, iso3 == "CAN" & sex == 1) # just for debugging
  df2 <- df[, c("iso3", "location_id", "sex", "age_group_id", "year", "n_smokers")]
  df3 <- dcast(df2, formula = iso3 + location_id + age_group_id + sex ~ year, value.var = "n_smokers")
  for (yr in (as.numeric(names(df3)[ncol(df3)]) + 1):(as.numeric(names(df3)[ncol(df3)]) + nPred)) {
    df3[, as.character(yr)] <- as.numeric(NA)
  }
  
  write.table(df3[5:ncol(df3)], 
    file = paste0("data2/nSmokers_", unique(df2$iso3), "_", unique(df2$sex), ".csv"),
    sep = ",",
    na = "",
    row.names = FALSE,
    col.names = FALSE
  )
  
  return(df3)
  
}))


prevSmoke <- do.call("rbind", lapply(split(dat, paste0(dat$iso3, "_", dat$sex)), function(df) {
  
#   df <- subset(dat, iso3 == "CAN" & sex == 1) # just for debugging
  df2 <- df[, c("iso3", "location_id", "sex", "age_group_id", "year", "smoking_prev")]
  df3 <- dcast(df2, formula = iso3 + location_id + age_group_id + sex ~ year, value.var = "smoking_prev")
  for (yr in (as.numeric(names(df3)[ncol(df3)]) + 1):(as.numeric(names(df3)[ncol(df3)]) + nPred)) {
    df3[, as.character(yr)] <- as.numeric(NA)
  }
  
  write.table(df3[5:ncol(df3)], 
    file = paste0("data2/prev_", unique(df2$iso3), "_", unique(df2$sex), ".csv"),
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
    file = paste0("data2/pop_", unique(df2$iso3), "_", unique(df2$sex), ".csv"),
    sep = ",",
    na = "",
    row.names = FALSE,
    col.names = FALSE
  )
  
  return(df3)
  
}))






















# 
# ## INLA REGRESSION -- simple age-cohort model
# 
# # fit1 <- inla(
# #   formula = smoking_prev_logit ~ as.factor(age_mid) + as.factor(cohort),
# #   data = prev
# # )
# 
# 
# 
# # n = 100
# # n.pred = 10
# # y = arima.sim(n=n, model=list(ar=0.9))
# # N = n + n.pred
# # yy = c(y, rep(NA, n.pred))
# # i = 1:N
# # formula = yy ~ f(i, model="rw2")
# # 
# # r = inla(formula, data = data.frame(i,yy),
# #   control.family = list(initial = 10, fixed=TRUE)) ## no observational noise
# 
# 
# prev2 <- prev[, c("age_mid", "cohort", "smoking_prev_logit")]
# 
# prev2.tmp <- prev2 %>%
#   group_by(age_mid) %>%
#   summarize(
#     cohort_maxyear = max(cohort)) %>%
#   as.data.frame(.)
# 
# nPred <- 10
# 
# prev3.tmp <- data.frame(
#   age_mid = NULL,
#   cohort_maxyear = NULL
# )
# 
# for (i in 1:nrow(prev2.tmp)) {
#   
#   x1 <- prev2.tmp[i, ]
#   for (j in 1:nPred) {
#     x2 <- x1
#     x2$cohort_maxyear <- x2$cohort_maxyear + j 
#     prev3.tmp <- rbind(prev3.tmp, x2)
#   }
# }
# 
# prev3.tmp$smoking_prev_logit <- NA
# names(prev3.tmp)[names(prev3.tmp) == "cohort_maxyear"] <- "cohort"
# prev3 <- rbind(prev2, prev3.tmp)
# 
# 
# fit3 <- inla(
#   formula = smoking_prev_logit ~ as.factor(age_mid) + as.factor(cohort),
#   data = prev3
# )
# 
# fit3$summary.fixed$i[(n+1):N, c("mean", "sd") ]
# 
# # r$summary.random$i[(n+1):N, c("mean", "sd") ] # example from INLA website
# # this was my attempt to do a prediction with INLA, currently doesn't work
# # -- might need to add random effects to use the example data
# 
# 
# # -- prediction
# 
# 
# 
# 
# 
# 
# 
# 
# ## GRAPHING
# 
# fit <- fit1
# x <- as.matrix(summary(fit)$fixed)[, 1]
# 
# names_cohort <- names(x)[grepl("cohort", names(x))]
# names_age <- names(x)[grepl("age_mid", names(x))]
# 
# x_age <- as.numeric(substr(names_age, str_locate(names_age, "\\)")[, "start"] + 1, nchar(names_age)))
# x_cohort <- as.numeric(substr(names_cohort, str_locate(names_cohort, "\\)")[, "start"] + 1, nchar(names_cohort)))
# y_age <- as.numeric(x[names_age])
# y_cohort <- as.numeric(x[names_cohort])
# 
# plot(x_age, y_age, main = "Age effects (logit smoking prevalence, age 10+ only)", xlab = "Age", ylab = "Coefficient")
# lines(spline(x_age, y_age))
# plot(x_cohort, y_cohort, main = "Cohort effects (logit smoking prevalence, age 10+ only)", xlab = "Birth year", ylab = "Coefficient")






