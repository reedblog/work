#
# Calculate child mortality using the Brass method
# Reed Sorensen, 2-24-15
#
# 1. Specify working directory (change 'dir' below)
# 2. Specify the survey year from which the data came (change 'survey_year')
# 3. Create a folder called 'data' in the working directory
# 4. Put input files in the 'data' folder
#    -- Multiple input files are okay, and "RESULT" output files will be ignored
#    -- Variable order: momAgeGroup, nWomen, nChildrenBorn, nChildrenDead
#    -- Note that the variable names are arbitrary, only variable order matters
# 5. Run the script
#    -- Results for all model families will be output in 'data' folder
#


## USER-DEFINED VARIABLES
dir <- "C:/Users/rsoren/Documents/prog/work_projects/gh590a_final_project/"
survey_year <- 2019



## CALCULATE BRASS METHOD

files.in <- list.files(paste0(dir, "data"))
files <- files.in[!(grepl("RESULT", files.in))]


for (filename in files) {
  for (model_family in c("north", "south", "east", "west")) {

#     filename <- "scenario1_hiv_changing_fert.csv" # just for debugging
#     model_family <- "west" # just for debugging

    # read in source data
    df <- read.csv(file = paste0(dir, "data/", filename))
    names(df) <- c("maternal_age_group", "number_of_women",
      "children_ever_born", "children_dead")
    df[] <- lapply(df, as.character)
    df[2:4] <- lapply(df[2:4], function(x) as.numeric(gsub(",", "", x)))

    # read in brass method tables from GH590A assignment 2
    t47 <- readRDS(file = paste0(dir, "brass_table47.RDS"))
    t48 <- readRDS(file = paste0(dir, "brass_table48.RDS"))

    # calculate parity ratios p1/p2 and p2/p3
    df$parity <- df$children_ever_born / df$number_of_women
    parity_ratio_p1p2 <- df[1, "parity"] / df[2, "parity"]
    parity_ratio_p2p3 <- df[2, "parity"] / df[3, "parity"]

    # calculate proportion of children dead in each maternal age group
    df$Di <- df$children_dead / df$children_ever_born

    # calculate k(i) for each maternal age group, based on table 47
    # Equation: k(i) = a(i) + b(i)(P(1)/P(2)) + c(i)(P(2)/P(3))
    df <- cbind(df, t47[t47$family == model_family, c("index", "coef_a", "coef_b", "coef_c")])
    df$ki <- df$coef_a + df$coef_b*parity_ratio_p1p2 + df$coef_c*parity_ratio_p2p3

    # calculate q(x) for each child age, k(i) * D(i)
    df$qx <- df$ki * df$Di

    # use a model life table to calculate q5

    qx_table.in <- read.csv(paste0(dir, "qx_model_families.csv"))
    qx_table.in$family <- tolower(qx_table.in$family)
    qx_table <- subset(qx_table.in, family == model_family, select = -c(family))

    # qx_table <- read.csv(
    #   file = paste0(dir, "qx for ", tolower(model_family), " tables.csv"))
    df$q5 <- as.numeric(NA)

    for (i in 1:nrow(df)) {
      qx_est <- df[i, "qx"]
      colnum_hi <- table(qx_est < qx_table[i, 2:ncol(qx_table)])[["TRUE"]] + 1
      qx_hi <- qx_table[i, colnum_hi]
      qx_lo <- qx_table[i, colnum_hi + 1]
      q5_hi <- qx_table[qx_table$age == 5, colnum_hi]
      q5_lo <- qx_table[qx_table$age == 5, colnum_hi + 1]
      df[i, "q5"] <- q5_lo + (qx_est - qx_lo) / (qx_hi - qx_lo) * (q5_hi - q5_lo)
    }


    # calculate t(x) time adjustment for each maternal age group, based on table 48
    # Equation: t(x) = a(i) + b(i)(P(1)/P(2)) + c(i)(P(2)/P(3))
    df <- merge(df, t48[t48$family == model_family, c("index", "coef_a2", "coef_b2", "coef_c2")])
    df$tx <- df$coef_a2 + df$coef_b2*parity_ratio_p1p2 + df$coef_c2*parity_ratio_p2p3
    df$time <- survey_year - df$tx


    # save the output file (into same directory as input file)
    write.csv(df, row.names = FALSE,
      file = paste0(dir, "data/RESULT_", toupper(model_family), "_", filename))

  }
}








