# 06_test_out_of_sample.R
# Reed Sorensen, March 2015
#
# Compare GPR predictions to real data
# -- Training period: 1980 - 2003
# -- Prediction period: 2004 - 2013
#

require(dplyr)
require(data.table)
require(boot)
require(ggplot2)

project_name <- "forecast_tobacco_gpr"
output_name <- "logit_rescaled_l2_o2_z99_1333"
adjustment <- 1.333333

#-- set OS-specific paths
jpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")
on_windows <- ifelse(Sys.info()[["sysname"]] == "Windows", TRUE, FALSE)

codes188 <- readRDS(paste0(hpath, "prog/data/forecast_tobacco_gpr/codes188.RDS"))


#-- create folders for saving data inputs/outputs
data_sav <- paste0("C:/Users/rsoren/Documents/prog/data/", project_name, "/")
data_dir <- paste0(hpath, "prog/data/", project_name, "/", output_name, "/")

df_pred.in <- fread(paste0(data_dir, "gpr_output_outofsample_test.csv"))
df_pred <- df_pred.in %>%
  mutate(
    obs_pred = inv.logit(observed_data) / adjustment,
    gpr_mean2 = inv.logit(gpr_mean) / adjustment,
    gpr_lower2 = inv.logit(gpr_lower) / adjustment,
    gpr_upper2 = inv.logit(gpr_upper) / adjustment )

df_pred2 <- df_pred[!duplicated(subset(df_pred, select = c(iso3, year, age, sex)))]


df_real.in <- fread(paste0(hpath, "prog/data/forecast_tobacco_gpr/", output_name, "/linear_output.csv"))
df_real <- df_real.in %>%
  mutate(
    age = as.integer(age),
    obs_real = inv.logit(observed_data) / adjustment) %>%
  filter(
    sex %in% unique(df_pred$sex),
    !is.na(observed_data))


df <- left_join(df_real, df_pred2, by = c("iso3", "year", "age", "sex")) %>%
  select(iso3, sex, age, year, obs_real, gpr_mean2, gpr_lower2, gpr_upper2)

df_out <- filter(df, year %in% 2004:2013)

isOverlapping <- function(x1,x2,y1,y2) {
  return(max(x1,y1) <= min(x2,y2))
}

df2 <- df_out %>%
  mutate(
    error = gpr_mean2 - obs_real,
    error2 = sqrt((gpr_mean2 - obs_real)^2),
    SE_hi = obs_real + 0.06,
    SE_lo = obs_real - 0.06,
    inRange = mapply(isOverlapping, SE_lo, SE_hi, gpr_lower2, gpr_upper2)) %>%
  group_by(iso3) %>%
  summarize(
    n = n(),
    mean_error = round(mean(error), digits = 3),
    rmse = round(sum(error2) / n(), digits = 3),
    coverage = sum(inRange) / n()) %>%
  as.data.frame(.)

#
# all_dat.in <- fread(paste0(data_dir, "linear_output.csv"))
#
# all_dat <- all_dat.in %>%
#   mutate(
#     obs2 = inv.logit(observed_data),
#     age_mid = as.numeric(age) + 2.5)


modeled_dat <- fread(paste0(hpath, "prog/data/forecast_smoking_age_cohort/infiles/dat.csv"))


# pdf(paste0(
#   hpath, "prog/work_projects/forecast_tobacco_gpr/fit_logit_byage_outofsample.pdf"))


pdf(paste0(data_dir, "outofsample_", output_name, ".pdf"))

lapply(split(df_pred, df_pred$iso3), function(df_tmp) {

#   df_tmp <- subset(df_pred, iso3 == "JPN")
  df_tmp$age <- as.factor(df_tmp$age)
  y.lim <- max(df_tmp$gpr_upper2)

  rmse <- ifelse(unique(df_tmp$iso3 %in% unique(df2$iso3)),
    as.character(df2[df2$iso3 == unique(df_tmp$iso3), "rmse"]), "N/A")

  ggplot(df_tmp, aes(x = year, y = gpr_mean2,
    group = age, color = age)) +
  xlab("Year") +
  ylab("Smoking prevalence") +
  ggtitle(paste0(
    codes188[codes188$iso3 == unique(df_tmp$iso3), "location_name"],
    " (OOS: RMSE = ", rmse, ")")) +
  geom_line(cex = 1.5) +
  geom_vline(xintercept = 2003) +
  facet_wrap(~ age) +
#     geom_point(
#       data = all_dat[all_dat$iso3 == unique(df_tmp$iso3), ],
#       aes(x = year, y = obs2, group = age, fill = age),
#         color = "black", cex = 1) +
    geom_point(
      data = df[df$iso3 == unique(df_tmp$iso3), ],
      aes(x = year, y = obs_real, group = age, fill = age),
      color = "black", cex = 1) +
  theme(legend.position="none")

})

dev.off()



### cohort plots -- needs updating


# pdf(paste0(data_dir, "outofsample_cohort_", output_name, ".pdf"))
#
# lapply(split(df, df$iso3), function(df_tmp) {
#
#   df_tmp <- subset(df, iso3 == "CHN")
#   y.lim <- max(df_tmp$prev_gpr_upper)
#
#   insample_only <- TRUE
#   if (insample_only) { df_tmp <- subset(df_tmp, year %in% 1980:2013) }
#
#   ggplot(df_tmp, aes(x = age_mid, y = prev_gpr,
#     group = cohort, color = cohort)) +
#   xlab("Age") +
#   ylab("Smoking prevalence") +
#   ggtitle(codes188[codes188$iso3 == unique(df_tmp$iso3), "location_name"]) +
#     geom_line(cex = 0.75) +
#     geom_point(
#       data = df_tmp,
#       aes(x = age_mid, y = prev_observed, group = cohort, fill = cohort),
#         color = "black", pch = 21, cex = 4.5) +
#     theme(legend.position = "none")
#
# #     scale_color_gradientn("Age",colours=rainbow(7))
# #   +
# #     theme(legend.margin=unit(-0.02,"npc"),legend.text=element_text(size=8))
#
# })
#
# dev.off()
#
#


