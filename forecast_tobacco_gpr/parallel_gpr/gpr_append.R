#
# gpr_append
# Reed Sorensen, April 2015
#
# Append GPR results after parallelization
#

require(dplyr)
require(data.table)
require(foreign)
require(boot)

project_name <- "forecast_tobacco_gpr"
model_name <- "logit_rescaled_l2_o05_z99_1333_bothsexes" # for getting smoking prev data
adjustment <- 1.333333


#-- set OS-specific paths
jpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[["sysname"]] == "Windows", "H:/", "/homes/rsoren/")
on_windows <- ifelse(Sys.info()[["sysname"]] == "Windows", TRUE, FALSE)


#-- create folders for saving data inputs/outputs
data_sav <- paste0("C:/Users/rsoren/Documents/prog/data/", project_name, "/")
data_dir <- paste0(hpath, "prog/data/", project_name, "/")
dir.create(file.path(data_sav), showWarnings = FALSE)
dir.create(file.path(data_dir), showWarnings = FALSE)


#-- read in GPR data after parallelization
path_to_dat <- paste0(hpath, "prog/data/", project_name, "/", model_name, "/parallel/")

system.time(

  dat.in <- rbindlist(lapply(list.files(path_to_dat), function(x) {

    tmp <- fread(paste0(path_to_dat, x)) %>% as.data.frame(.)
    tmp <- tmp[!duplicated(subset(tmp, select = c(iso3, year, sex, age)))]
    for (j in paste0("draw", 0:999)) { tmp[, j] <- inv.logit(tmp[, j]) * 1/adjustment }
    return(tmp)

  }))

)

# data for neal

df2 <- dat.in %>%
  filter(year %in% 1980:2040) %>%
  select_(.dots = c("iso3", "year", "sex", "age", paste0("draw", 0:999)))

df2 <- df2[!duplicated(subset(df2, select = c(iso3, year, sex, age)))]

add_ages <- expand.grid(
  unique(df2$iso3),
  unique(df2$year),
  unique(df2$sex),
  c(0, 0.01, 0.1, 1, 5)
)
names(add_ages) <- c("iso3", "year", "sex", "age")

for (j in paste0("draw", 0:999)) {
  add_ages[, j] <- 0
}

add_ages <- add_ages[names(df2)]
df3 <- rbind(df2, add_ages) %>%
  arrange(iso3, year, sex, age)

# write.csv(df3, paste0(data_dir, "smoking_prev_draws_1980_2040_logit_rescaled_l2_o05_z99_1333.csv"))


