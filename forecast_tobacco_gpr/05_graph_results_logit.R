#
# 05_graph_results.R
# Graph the output of spacetime GPR
#
# Reed Sorensen, March 2015
#

require(dplyr)
require(data.table)
require(ggplot2)
require(boot)

project_name <- "forecast_tobacco_gpr"
output_name <- "logit_rescaled_l2_o05_z99_1333_bothsexes"
adjustment <- 1.333333

out_of_sample <- FALSE
oos <- ifelse(out_of_sample, "_outofsample_test", "")

jpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")
data_dir <- paste0(hpath, "prog/data/", project_name, "/")
codes188 <- readRDS(paste0(data_dir, "codes188.RDS"))

df.in <- fread(paste0(
  hpath, "prog/data/", project_name ,"/",
  output_name, "/gpr_output", oos, ".csv"))

df <- df.in %>%
  mutate(
    observed_data2 = inv.logit(observed_data) * 1/adjustment,
    stage1_prediction2 = inv.logit(stage1_prediction) * 1/adjustment,
    st_prediction2 = inv.logit(st_prediction) * 1/adjustment,
    gpr_mean2 = inv.logit(gpr_mean) * 1/adjustment,
    gpr_lower2 = inv.logit(gpr_lower) * 1/adjustment,
    gpr_upper2 = inv.logit(gpr_upper) * 1/adjustment,
    cohort = as.factor(year - age ) )

df2 <- df %>%
  filter(year %in% 1980:2040) %>%
  select(iso3, year, sex, age, smoking_prev = gpr_mean2) %>%
  arrange(iso3, year, sex, age, smoking_prev)
df2 <- df2[!duplicated(subset(df2, select = c(iso3, year, sex, age)))]

add_ages <- expand.grid(
  unique(df2$iso3),
  unique(df2$year),
  unique(df2$sex),
  c(0, 0.01, 0.1, 1, 5)
)
names(add_ages) <- c("iso3", "year", "sex", "age")
add_ages$smoking_prev <- 0
add_ages <- add_ages[names(df2)]
df3 <- rbind(df2, add_ages) %>%
  arrange(iso3, year, sex, age)

write.csv(df3, paste0(data_dir, "smoking_prev_1980_2040_logit_rescaled_l2_o05_z99_1333.csv"))

x <- rbindlist(lapply(split(df2, paste0(df2$iso3, df2$sex, df2$year)) function(x) {
  tmp <- subset(df, iso3 == "USA" & sex == "male" & year == 2008)

# df <- subset(df, sex == "female")


# pdf(paste0(hpath, "prog/tmp/gpr_tobacco/fit_logit_bycohort_1980_to_2013.pdf"))
pdf(paste0(data_dir, output_name, "/cohort_graphs_", output_name, ".pdf"))

lapply(split(df, df$iso3), function(df_tmp) {

  df_tmp <- subset(df, iso3 == "USA")
  y.lim <- max(df_tmp$gpr_upper2)

  insample_only <- TRUE
  if (insample_only) { df_tmp <- subset(df_tmp, year %in% 1980:2013) }

  ggplot(df_tmp, aes(x = age, y = gpr_mean2,
    group = cohort, color = cohort)) +
  xlab("Age") +
  ylab("Smoking prevalence") +
  ggtitle(codes188[codes188$iso3 == unique(df_tmp$iso3), "location_name"]) +
    geom_line(cex = 0.75) +
    geom_point(
      data = df_tmp,
      aes(x = age, y = observed_data2, group = cohort, fill = cohort),
        color = "black", pch = 21, cex = 4.5) +
    theme(legend.position = "none")

})

dev.off()


###################

pdf(paste0(data_dir, output_name, "/forecast_", output_name, ".pdf"))

lapply(split(df, df$iso3), function(df_tmp) {

#   df_tmp <- subset(df, iso3 == "DEU")
  df_tmp$age <- as.factor(df_tmp$age)
  y.lim <- max(df_tmp$gpr_upper2)

  ggplot(df_tmp, aes(x = year, y = gpr_mean2,
    group = age, color = age)) +
  xlab("Year") +
  ylab("Smoking prevalence") +
  ggtitle(codes188[codes188$iso3 == unique(df_tmp$iso3), "location_name"]) +
  geom_line(cex = 1.5) +
  geom_vline(xintercept = 2012) +
  facet_wrap(~ age) +
    geom_point(
      aes(x = year, y = observed_data2, group = age, fill = age),
      color = "black", cex = 1) +
    geom_line(
      data = subset(df_tmp, year %in% 2012:2040),
      aes(x = year, y = gpr_lower2, group = age, color = age)) +
    geom_line(
      data = subset(df_tmp, year %in% 2012:2040),
      aes(x = year, y = gpr_upper2, group = age, color = age)) +
    geom_line(
      aes(x = year, y = stage1_prediction2, group = age, fill = age),
      color = "black", cex = 0.5, lty = 2) +
    geom_line(
      aes(x = year, y = st_prediction2, group = age, fill = age),
      color = "black", cex = 0.5) +
    theme(legend.position = "none")

})

dev.off()




