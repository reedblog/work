#
# 02_prev_sir_correlation.R
#
# Check correlation between SIR and various metrics of smoking prevalence
# Reed Sorensen, March 2015
#

require(dplyr)
require(data.table)
require(ggplot2)

project_name <- "SIR_and_smoking_prev_comparsion"
model_name <- "logit_rescaled_l2_o05_z99_1333_bothsexes"
adjustment <- 1.333333


#-- set OS-specific paths
jpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")
on_windows <- ifelse(Sys.info()[["sysname"]] == "Windows", TRUE, FALSE)


#-- set folders for saving data inputs/outputs, created in 01_data_prep.R
data_sav <- paste0("C:/Users/rsoren/Documents/prog/data/", project_name, "/")
data_dir <- paste0(hpath, "prog/data/", project_name, "/")

df.in <- readRDS(paste0(data_dir, "smoking_prev_sir_merged.RDS"))

df <- df.in %>%
  mutate(
    cohort = year - age,
    age_factor = as.factor(age),
    year_factor = as.factor(year) )

# df <- subset(df, year %in% unique(sir$year)) # subset data to GBD years



df <- subset(df, !(iso3 %in% c("CHN", "GBR", "MEX"))) %>%
  mutate(
    prev_L5 = as.numeric(NA),
    prev_L10 = as.numeric(NA)) %>%
  as.data.frame(.)

prev <- df[, c("iso3", "year", "sex", "age", "cohort", "gpr_mean2")] %>%
  mutate(age_mid = age + 2.5)

#-- get 20-year smoking prevalence lags for each cohort
# 1. interpolate age groups into 1-year ages
# 2. for each age in a given GBD year, get all cohort data
#    -- reshape such that the previous 20 years are represented in 20 variables
# 3. re-aggregate individual ages into age groups

grid_male <- expand.grid(seq(-10, 82.5, by = 0.5), 1970:2013, "male", unique(prev$iso3))
names(grid_male) <- c("age_mid", "year", "sex", "iso3")

require(zoo)
p2 <- left_join(grid_male, subset(prev, sex == "male"))
p2$gpr_mean2[p2$age_mid == 10] <- 0
p2$gpr_mean2[p2$age_mid == -10] <- 0
p2$p <- na.approx(p2$gpr_mean2)
p3 <- subset(p2, age_mid %% 1 == 0)
p3$cohort <- p3$year - p3$age_mid


gbd_years <- c(1990, 1995, 2000, 2005, 2010, 2013)

p4 <- rbindlist(lapply(split(p3, paste0(p3$iso3, p3$sex, p3$cohort)), function(x) {

  x %>% mutate(prev_sumL20 =
    lag(p,n=1) + lag(p,n=2) + lag(p,n=3) + lag(p,n=4) + lag(p,n=5) +
    lag(p,n=6) + lag(p,n=7) + lag(p,n=8) + lag(p,n=9) + lag(p,n=10) +
    lag(p,n=11) + lag(p,n=12) + lag(p,n=13) + lag(p,n=14) + lag(p,n=15) +
    lag(p,n=16) + lag(p,n=17) + lag(p,n=18) + lag(p,n=19) + lag(p,n=20))

}))

p5 <- arrange(p4, iso3, year, sex, age_mid)
p5_USA <- subset(p5, iso3 == "USA" & year %in% 1990:2013)


p6 <- subset(p5, age_mid %in% 10:82 & year %in% gbd_years)
p6$age_group_id <- as.numeric(NA)

for (age_start in seq(10, 80, by = 5)) {

  p6$age_group_id[p6$age_mid %in% age_start:(age_start + 4)] <- age_start

}

p7 <- p6 %>%
  group_by(iso3, year, sex, age_group_id) %>%
  summarize(prev_L20 = mean(prev_sumL20))
setnames(p7, "age_group_id", "age")

sir <- subset(df.in, !(is.na(exp_mean)), select = c(iso3, year, sex, age, exp_mean))

p8 <- left_join(sir, p7, by = c("iso3", "year", "sex", "age")) %>%
  mutate(age_factor = factor(age))

# for groups with SIR data, get 5-year and 10-year cohort lags of smoking prev

gbd_years <- c(1990, 1995, 2000, 2005, 2010, 2013)
sir_ages <- seq(30, 80, by = 5)




df2 <- as.data.frame(rbindlist(lapply(split(df, paste0(df$iso3, df$sex)), function(tmp) {

#   tmp <- subset(df, iso3 == "ARG" & sex == "male") # just for debugging

  for (yr in gbd_years) {
    for (ag in sir_ages) {
      tmp[tmp$year == yr & tmp$age == ag, "prev_L5"] <- tmp[tmp$year == (yr - 5) & tmp$age == (ag - 5), "gpr_mean2"]
      tmp[tmp$year == yr & tmp$age == ag, "prev_L10"] <- tmp[tmp$year == (yr - 10) & tmp$age == (ag - 10), "gpr_mean2"]
    }
  }

  return(tmp)


})))



df2$cumulative_cohort_10yr <- df2$gpr_mean2 * 5 + df2$prev_L5 * 5 + df2$prev_L10 * 5






#-- x = cumulative cohort smoking prev (20 yr), y = SIR, color = age, scatterplot
#-- -- country-specific


pdf(file = paste0(data_dir, "smoking_xcumprev20yr_ysir_by_age.pdf"))

lapply(split(p8, p8$iso3), function(df_tmp) {

#       df_tmp <- subset(p8, iso3 == "DEU") # for debugging only
  ggplot(df_tmp, aes(x = prev_L20, y = exp_mean,
    group = age_factor, color = age_factor)) +
    xlab("Cumulative smoking prevalence, by cohort, 20yr") +
    ylab("Smoking impact ratio  ( SIR )") +
    ggtitle(unique(df_tmp$iso3)) +
    geom_point(cex = 5)

})

dev.off()


#-- -- all countries
pdf(file = paste0(data_dir, "smoking_xcumprev20_ysir_byage_allcountries.pdf"))

ggplot(p8, aes(x = prev_L20, y = exp_mean,
  group = age_factor, color = age_factor, 0.01)) +
  xlab("Cumulative smoking prevalence, by cohort, 20yr") +
  ylab("Smoking impact ratio  ( SIR )") +
  ggtitle("All countries") +
  geom_point(cex = 1)

dev.off()

#-- -- excluding ages 30 to 40

pdf(file = paste0(data_dir, "smoking_xcumprev20_ysir_byage_allcountries_agesubset.pdf"))

p8_2 <- subset(p8, !(age_factor %in% c("30", "35")))
ggplot(p8_2, aes(x = prev_L20, y = exp_mean,
  group = age_factor, color = age_factor, 0.01)) +
  xlab("Cumulative smoking prevalence, by cohort, 20yr") +
  ylab("Smoking impact ratio  ( SIR )") +
  ggtitle("All countries") +
  geom_point(cex = 1)

dev.off()




#-- x = cumulative cohort smoking prev (10 yr), y = SIR, color = age, scatterplot

pdf(file = paste0(data_dir, "smoking_xcumprev_ysir_by_age.pdf"))

lapply(split(df2, df2$iso3), function(df_tmp) {

#     df_tmp <- subset(df2, iso3 == "ARG") # for debugging only
  ggplot(df_tmp, aes(x = cumulative_cohort_10yr, y = exp_mean,
    group = age_factor, color = age_factor)) +
    xlab("Cumulative smoking prev, cohort, 10yr") +
    ylab("Smoking impact ratio  ( SIR )") +
    ggtitle(unique(df_tmp$iso3)) +
    geom_point(cex = 5)

})

dev.off()



#-- all countries together
#-- same-year smoking prev
pdf(file = paste0(data_dir, "smoking_xprev_ysir_byage_allcountries.pdf"))

ggplot(df2, aes(x = gpr_mean2, y = exp_mean,
  group = age_factor, color = age_factor, 0.01)) +
  xlab("Smoking prevalence") +
  ylab("Smoking impact ratio  ( SIR )") +
  ggtitle("All countries") +
  geom_point(cex = 1)

dev.off()


#-- 10-year cumulative cohort smoking prev
pdf(file = paste0(data_dir, "smoking_xcumprev_ysir_byage_allcountries.pdf"))

ggplot(df2, aes(x = cumulative_cohort_10yr, y = exp_mean,
  group = age_factor, color = age_factor, 0.01)) +
  xlab("Cumulative smoking prev, cohort, 10yr") +
  ylab("Smoking impact ratio  ( SIR )") +
  ggtitle("All countries") +
  geom_point(cex = 1)

dev.off()


#-- x = smoking prevalence, y = SIR, color = age, scatterplot

pdf(file = paste0(data_dir, "smoking_xprev_ysir_by_age.pdf"))

lapply(split(df, df$iso3), function(df_tmp) {

#   df_tmp <- df[df$iso3 == "USA"] # for debugging only
  ggplot(df_tmp, aes(x = gpr_mean2, y = exp_mean,
    group = age_factor, color = age_factor)) +
    xlab("Smoking prevalence") +
    ylab("Smoking impact ratio  ( SIR )") +
    ggtitle(unique(df_tmp$iso3)) +
    geom_point(cex = 5)

})

dev.off()



#-- x = age, y = SIR, color = GBD year, scatterplot


df_sir <- readRDS(paste0(data_dir, "sir_only.RDS")) %>%
  mutate(year_factor = factor(year))

pdf(file = paste0(data_dir, "xage_ysir_bycountry.pdf"))

lapply(split(df_sir, df_sir$iso3), function(df_tmp) {

#   df_tmp <- subset(df_sir, iso3 == "MEX_4672") # for debugging only
  df_tmp <- subset(df_tmp, year %in% gbd_years)
  ggplot(df_tmp, aes(x = age, y = exp_mean,
    group = year_factor, color = year_factor)) +
    xlab("Age") +
    ylab("Smoking impact ratio  ( SIR )") +
    ggtitle(unique(df_tmp$iso3)) +
    geom_point(cex = 5)

})

dev.off()








