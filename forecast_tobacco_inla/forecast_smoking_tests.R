require(dplyr)
require(data.table)
require(reshape2)
require(stringr)
require(boot)

## prep data

dir <- "C:/Users/rsoren/Dropbox/projects/forecast_smoking/"

locations <- readRDS("C:/Users/rsoren/Dropbox/projects/cohort_component_model_v5/infiles/locations.RDS")

agecat <- readRDS(file = paste0("C:/Users/rsoren/Dropbox/projects/", 
  "cohort_component_model_v5/infiles/older_age_categories.RDS"))
agecat$n <- c(4, rep(5, 15), 31)
agecat$age_mid <- agecat$age_start_year + (agecat$n / 2)


iso3 <- "USA"
country <- locations[locations$local_id == iso3, "location_id"]

prev.in <- fread(input = paste0(dir, "infiles/smoking_prev.csv"))

prev <- prev.in %>%
  filter(location_id == country & year %in% 1995:2013 & sex == 1 & age_group_id %in% 7:21) %>%
  mutate(smoking_prev = ifelse(smoking_prev == 0, 0.0000001, smoking_prev)) %>%
  as.data.frame(.)

prev$age_start <- as.numeric(NA)
prev$age_mid <- as.numeric(NA)

for (i in 1:nrow(prev)) { 
  prev[i, "age_start"] <- agecat[agecat$age_group_id == prev[i, "age_group_id"], "age_start_year"]
  prev[i, "age_mid"] <- agecat[agecat$age_group_id == prev[i, "age_group_id"], "age_mid"]
}

prev$cohort <- prev$year - prev$age_mid
prev$smoking_prev_logit <- logit(prev$smoking_prev)



############# INLA approach
# install.packages("INLA", repos="http://www.math.ntnu.no/inla/R/stable")
library("INLA")

fit <- inla(
  formula = smoking_prev_logit ~ as.factor(age_mid) + as.factor(cohort),
  data = prev
)

x <- as.matrix(summary(fit)$fixed)[, 1]

names_cohort <- names(x)[grepl("cohort", names(x))]
names_age <- names(x)[grepl("age_mid", names(x))]

x_age <- as.numeric(substr(names_age, str_locate(names_age, "\\)")[, "start"] + 1, nchar(names_age)))
x_cohort <- as.numeric(substr(names_cohort, str_locate(names_cohort, "\\)")[, "start"] + 1, nchar(names_cohort)))
y_age <- as.numeric(x[names_age])
y_cohort <- as.numeric(x[names_cohort])

plot(x_age, y_age, main = "Age effects (logit smoking prevalence, age 10+ only)", xlab = "Age", ylab = "Coefficient")
lines(spline(x_age, y_age))
plot(x_cohort, y_cohort, main = "Cohort effects (logit smoking prevalence, age 10+ only)", xlab = "Birth year", ylab = "Coefficient")



##################### lm() regression on age and cohort

fit2 <- lm(prev$smoking_prev_logit ~ as.factor(prev$age_mid) + as.factor(prev$cohort))

x2 <- fit2$coefficients

names_cohort <- names(x2)[grepl("cohort", names(x2))]
names_age <- names(x2)[grepl("age_mid", names(x2))]

x2_age <- as.numeric(substr(names_age, str_locate(names_age, "\\)")[, "start"] + 1, nchar(names_age)))
x2_cohort <- as.numeric(substr(names_cohort, str_locate(names_cohort, "\\)")[, "start"] + 1, nchar(names_cohort)))
y2_age <- as.numeric(x2[names_age])
y2_cohort <- as.numeric(x2[names_cohort])

plot(x2_age, y2_age, main = "Age effects (logit smoking prevalence, age 10+ only)", xlab = "Age", ylab = "Coefficient")
lines(spline(x2_age, y2_age))
plot(x2_cohort, y2_cohort, main = "Cohort effects (logit smoking prevalence, age 10+ only)", xlab = "Birth year", ylab = "Coefficient")

plot(NULL, NULL, xlim = c(1995, 2013), ylim = c(0, max(prev$smoking_prev)),
  xlab = "Year", ylab = "Smoking prevalence", main = "Age-specific smoking prevalence over time")

for (ag in unique(prev$age_group_id)) {
  tmp <- subset(prev, age_group_id == ag)
  lines(tmp$year, tmp$smoking_prev)
}

# 
# prev2 <- prev %>%
#   dcast(location_id + sex + age_group_id ~ year, value.var = "smoking_prev") %>%
#   subset(., select = 4:ncol(.))
# 
# tmp <- fts(x = unique(prev$age_mid), y = prev2, xname = "Age", yname = "Smoking prevalence")
# tmp2 <- ftsm(tmp, order = 2)
# plot(forecast(tmp2, h = 20, method = "rwdrift"))
# plot(forecast(tmp2, h = 20, method = "arima"))
# plot(forecast(tmp2, h = 20, method = "ets"))



# income.in <- fread(input = paste0(dir, "infiles/income.csv"))
# 
# income <- income.in %>%
#   filter(iso3 == iso3 & year %in% 1995:2013 & sex == 1 & age_group_id %in% 7:21)


########################## Functional Data Analysis approach using 'demography' and 'ftsa'

require(demography)
require(ftsa)

# usa.mort <- hmd.mx("USA", "sorensen.reed@gmail.com", "Phaedo98", "United States")

usa.sm <- smooth.demogdata(usa.mort, obs.var = "theoretical")
usa.fit <- fdm(usa.sm, order = 2, series = "male")
usa.fcast <- forecast(usa.fit, 50)
plot(usa.fcast)
models(usa.fcast)

# Simulation
usa.sim <- simulate(usa.fcast, nsim = 100)

# Plot.fmforecast
plot(usa.fcast)
