# glmer, negative binomial
# cause-specific and general flow charts, need to incorporate impairments into both
# set up meeting with Joe to talk about INLA
# random effects by country, not region


require(stringr)
require(dplyr)

x <- as.matrix(summary(fit2)$coefficients)

names_cohort <- row.names(x)[grepl("cohort", row.names(x))]
names_age <- row.names(x)[grepl("age_mid", row.names(x))]

x_age <- as.numeric(substr(names_age, str_locate(names_age, "\\)")[, "start"] + 1, nchar(names_age)))
x_cohort <- as.numeric(substr(names_cohort, str_locate(names_cohort, "\\)")[, "start"] + 1, nchar(names_cohort)))
y_age <- as.numeric(x[names_age, "Estimate"])
y_cohort <- as.numeric(x[names_cohort, "Estimate"])

plot(x_age, y_age, main = "Age effects (smoking prevalence, raw data)", xlab = "Age", ylab = "Coefficient")
lines(spline(x_age, y_age))
plot(x_cohort, y_cohort, main = "Cohort effects (smoking prevalence, raw data)", xlab = "Birth year", ylab = "Coefficient")


#####
fit <- readRDS(file = file.path("~/prog/temp/lmer_fit1.RDS"))

x <- as.matrix(summary(fit)$coefficients)

names_cohort <- row.names(x)[grepl("cohort", row.names(x))]
names_age <- row.names(x)[grepl("age_mid", row.names(x))]

x_age <- as.numeric(substr(names_age, str_locate(names_age, "age_mid")[, "end"] + 1, nchar(names_age)))
x_cohort <- as.numeric(substr(names_cohort, str_locate(names_cohort, "cohort")[, "end"] + 1, nchar(names_cohort)))
y_age_fixed <- as.numeric(x[names_age, "Estimate"])
y_cohort_fixed <- as.numeric(x[names_cohort, "Estimate"])

y_age_random <- ranef(fit)$age_nest
y_age_random$colon_loc <- gregexpr("\\:", row.names(y_age_random))
y_age_random$region <- substr(row.names(y_age_random), 1, as.numeric(y_age_random$colon_loc) - 1)
y_age_random$age <- substr(row.names(y_age_random), as.numeric(y_age_random$colon_loc) + 1, nchar(row.names(y_age_random)))

regions <- unique(dat$gbd_analytical_region_id)
y_age_fixed2 <- rep(y_age_fixed, length(regions))

df_age <- data.frame(x_age, y_age_fixed)
df <- merge(subset(y_age_random, select = -c(colon_loc)) , df_age, by.x = "age", by.y = "x_age")

df$age_effect_both <- df$intercept + df$y_age_fixed

max_y <- max(abs(df$age_effect_both))
plot(NULL, NULL, ylim = c(-1 * max_y, max_y), xlim = c(0, 85),
  ylab = "Coefficient", xlab = "Age", main = "Age effects by region")

for (reg in regions) {
  tmp <- subset(df, region == reg)
  lines(spline(tmp$age, tmp$age_effect_both))
}



