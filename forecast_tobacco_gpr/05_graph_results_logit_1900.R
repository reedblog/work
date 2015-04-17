
#
# 05_graph_results.R
# Graph the output of spacetime GPR, including cohort 1900+
#
# Reed Sorensen, March 2015
#

require(dplyr)
require(data.table)
require(ggplot2)
require(boot)

project_name <- "smoking_prev_GPR"
data_dir <- paste0("~/prog/data/", project_name, "/")
codes188 <- readRDS(paste0(data_dir, "infiles/codes188.RDS"))

jpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")

df.in <- fread(paste0(hpath, "prog/tmp/gpr_tobacco/gpr_output_sav3_logit_1900.csv"))

df <- df.in %>%
  mutate(
    prev_observed = inv.logit(observed_data),
    prev_gpr = inv.logit(gpr_mean),
    prev_gpr_lower = inv.logit(gpr_lower),
    prev_gpr_upper = inv.logit(gpr_upper),
    age_mid = age + 2.5,
    cohort = as.factor(year - age_mid ))


##

pdf(paste0(hpath, "prog/tmp/gpr_tobacco/fit_1900_to_2013.pdf"))

lapply(split(df, df$iso3), function(df_tmp) {

#   df_tmp <- subset(df, iso3 == "CHN")
  y.lim <- max(df_tmp$prev_gpr_upper)

  insample_only <- TRUE
  if (insample_only) { df_tmp <- subset(df_tmp, cohort %in% 1900.5:1998.5) }

  ggplot(df_tmp, aes(x = age_mid, y = prev_gpr,
    group = cohort, color = cohort)) +
  xlab("Age") +
  ylab("Smoking prevalence") +
  ggtitle(codes188[codes188$iso3 == unique(df_tmp$iso3), "location_name"]) +
    geom_line(cex = 0.75) +
    geom_point(
      data = df_tmp,
      aes(x = age_mid, y = prev_observed, group = cohort, fill = cohort),
        color = "black", pch = 21, cex = 4.5) +
  theme(legend.position = "none")

})

dev.off()
