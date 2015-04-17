#
# 03_prep_gpr.R
# An R implementation of 03_prep_gpr.do
#
# Reed Sorensen, March 2015
#

require(data.table)
require(dplyr)

output_name <- "logit_rescaled_l2_o05_z99_1333_bothsexes"
out_of_sample <- FALSE


both_sexes <- grepl("bothsexes", output_name)
oos <- ifelse(out_of_sample, "_outofsample_test", "")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")


if (both_sexes == FALSE) {

  df <- fread(paste0(
    hpath, "prog/data/forecast_tobacco_gpr/",
    output_name, "/spacetime_output", oos, ".csv")) %>%
    arrange(iso3, year)
  setnames(df, "mad_regional", "spacetime_amplitude_1")


} else if (both_sexes == TRUE) {

  df_male <- fread(paste0(
    hpath, "prog/data/forecast_tobacco_gpr/",
    output_name, "/spacetime_output", oos, "_male.csv")) %>%
    arrange(iso3, year)
  setnames(df_male, "mad_regional", "spacetime_amplitude_1")

  df_female <- fread(paste0(
    hpath, "prog/data/forecast_tobacco_gpr/",
    output_name, "/spacetime_output", oos, "_female.csv")) %>%
    arrange(iso3, year)
  setnames(df_female, "mad_regional", "spacetime_amplitude_1")

  df <- rbind(df_male, df_female)


}


# GPR breaks with variance = 0
df$obs_data_variance[df$obs_data_variance == 0] <- 0.00001

write.csv(df, paste0(hpath, "prog/data/forecast_tobacco_gpr/", output_name,
  "/spacetime_output", oos, "_prepped.csv"))