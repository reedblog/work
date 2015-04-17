# source("inla-apc-complete.R")

# To do:
# include negative binomial, or quasi-poisson
# run with 'raw' data

## load the R-INLA library
library(INLA)
require(dplyr)
require(data.table)
require(foreign)


# get location IDs (this gets 188, not 192)
df_pop <- fread(input = "J:/temp/fbd/data/reed_pop_1980.csv") %>% as.data.frame(.)
# locations <- readRDS("C:/Users/rsoren/Dropbox/projects/cohort_component_model_v5/infiles/locations.RDS")
# locs <- locations[locations$location_id %in% unique(df$location_id), ]
# names(locs)[1] <- "iso3"
# locs$iso3 <- as.character(locs$iso3)

country_codes.dat <- read.dta("J:/DATA/IHME_COUNTRY_CODES/IHME_COUNTRY_CODES_Y2013M07D26.DTA")
cds <- subset(country_codes.dat, location_id %in% unique(df_pop$location_id),
  select = c(iso3, location_id, location_name, 
    gbd_analytical_region_id, gbd_analytical_region_name,
    wb_income_group_long, wb_income_group_short)
)

# saveRDS(df_pop, "infiles/df_pop.RDS")
# saveRDS(country_codes.dat, "infiles/country_codes.RDS")


for (reg in unique(cds$gbd_analytical_region_id)) {  # this loops through regions
  
#   reg <- 5
#   reg <- 96
#   reg <- 100 # need to re-run this, CAN is all males and USA is all females
# reg <- 9 # this didn't finish; need to run it again
  
iso <- cds[cds$gbd_analytical_region_id == reg, "iso3"]
  
sex <- c(1)


# # load data
# death_data <- list()
# pop_data <- list()
# for (i in iso) {
#     for (s in sex) {
#         death_data[[paste(i,s)]] <- round(as.matrix(read.csv(file.path('data', paste0('deaths_', i, '_', s, '.csv')), header=FALSE)))
#         pop_data[[paste(i,s)]] <- round(as.matrix(read.csv(file.path('data', paste0('pop_', i, '_', s, '.csv')), header=FALSE)))
#     }
# }


# # load data ##edit-reed
death_data <- list()
pop_data <- list()
for (i in iso) {
    for (s in sex) {  
        death_data[[paste(i,s)]] <- round(as.matrix(read.csv(file.path('data2', paste0('nSmokers_', i, '_', s, '.csv')), header=FALSE)))
        pop_data[[paste(i,s)]] <- round(as.matrix(read.csv(file.path('data2', paste0('pop_', i, '_', s, '.csv')), header=FALSE)))
    }
}

death_data <- lapply(death_data, FUN = function(x) apply(x, 2, function(y) round(y / 100)))
pop_data <- lapply(pop_data, FUN = function(x) apply(x, 2, function(y) round(y / 100)))

# # load data ##edit-reed
# death_data <- list()
# pop_data <- list()
# for (i in iso) {
#     for (s in sex) {  ##edit-reed, I removed round() for 'death_data' because it's a proportion
#         death_data[[paste(i,s)]] <- as.matrix(read.csv(file.path('data2', paste0('prev_', i, '_', s, '.csv')), header=FALSE))
#         pop_data[[paste(i,s)]] <- round(as.matrix(read.csv(file.path('data2', paste0('pop_', i, '_', s, '.csv')), header=FALSE)))
#     }
# }

## number of agegroups
I <- dim(death_data[[paste(iso[1], sex[1])]])[1]
## total number of periods
J <- dim(death_data[[paste(iso[1], sex[1])]])[2]

G <- 5
## number of cohorts
K <- G*(I-1) + J

## function to get the cohort index following Heuer (1997): k=G*(I-i)+j
get_cohort_index <- function(I, J, G){
  k <- c()
  for(j in 1:J){
   for(i in 1:I){
      k <- c(k, G*(I - i) + j)
    }
  }
  return(k)
}

## age index
i <- rep(1:I, J)
# ## period index
j <- rep(1:J, each=I)
## cohort index
k <- get_cohort_index(I,J,G)


## Prior parameter specifications
# log-gamma prior for log precision
prior.rw2 <- c(1,0.00005)
prior.iid <- c(1,0.005)
# normal prior for Fisher's z-transformed corrrelation
prior.cor <- c(0,0.2)

## Combine data
deaths <- c(sapply(death_data, function(x) { return(c(x)) }))
pop <- c(sapply(pop_data, function(x) { return(c(x)) }))

## find number of units to predict (e.g. countries/sexes)
num <- length(death_data)

## overdispersion (uncorrelated)
overdis_uncor <- 1:(num*I*J)
## overdispersion (correlated)
overdis_cor <- rep(1:(I*J),num)

## generate the data.frame used to apply inla
data <- data.frame(y=deaths, pop=pop,
              i=rep(i,num),
              j=rep(j,num),
              k=rep(k,num),
              z=overdis_uncor,zcor=overdis_cor)

## grouping index (for each country/sex)
g <- rep(seq(num), each=length(i))

## add intercept for each country/sex
for (n in 1:num) {
    data[paste0('mu',n)] <- as.numeric(g == n)
}


# ## apc model with correlated time effects and overdispersion (possible as projections are identifiable)
# formula_corapc_z <- 'y ~ f(i, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=1.27)),
#     control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=4.66))),
#     group=g, constr=TRUE, rankdef=2) +
#   f(j, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=6.46)),
#     control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=4.66))),
#     group=g, constr=TRUE, rankdef=2) +
#   f(k, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=7.29)),
#     control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=2.27))),
#     group=g, constr=TRUE, rankdef=2) +
#   f(zcor, model="iid", hyper=list(prec = list(param=prior.iid, initial=3.62)),
#     control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=1.96))), group=g) - 1'

#edit-reed, taking out the period term 
formula_corapc_z <- 'y ~ f(i, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=1.27)), 
    control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=4.66))),
    group=g, constr=TRUE, rankdef=2) +
  f(k, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=7.29)),
    control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=2.27))),
    group=g, constr=TRUE, rankdef=2) +
  f(zcor, model="iid", hyper=list(prec = list(param=prior.iid, initial=3.62)),
    control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=1.96))), group=g) - 1'

for (n in 1:num) {
    formula_corapc_z <- paste(formula_corapc_z, '+', paste0('mu', n))
}
formula_corapc_z <- as.formula(formula_corapc_z)

# with so many hyperparameters we have to increase the number of maximum function evaluation in
# the derivation of the posterior marginals for the hyperparameters
inla_fit = inla(formula_corapc_z, family="poisson", E=pop, data=data,
        quantile=c(0.025,0.1,0.5,0.9,0.975), verbose=TRUE,
        control.compute=list(dic=TRUE, cpo=TRUE, hyperpar =TRUE),
        control.predictor=list(compute=TRUE),
        control.inla=list(numint.maxfeval=80000000))

# inla_fit = inla(formula_corapc_z, family="binomial", E=pop, data=data, ##edit-reed, changed family from poisson to binomial
#         quantile=c(0.025,0.1,0.5,0.9,0.975), verbose=TRUE,
#         control.compute=list(dic=TRUE, cpo=TRUE, hyperpar =TRUE),
#         control.predictor=list(compute=TRUE),
#         control.inla=list(numint.maxfeval=80000000))

## store the analyses
# save(inla_fit, file=file.path('results2', 'inla_fit_APC_prev_nSmokers.RData'))
# save(inla_fit, file=file.path('results2', 'inla_fit_AC_nSmokers_yr27_n188.RData'))
# save(inla_fit, file=file.path('results_by_region', paste0('inla_fit_AC_nSmokers_yr27_region_males', reg, '.RData')))
save(inla_fit, file=file.path('results_by_region2', paste0('inla_fit_AC_nSmokers_yr27_region', reg, 'sex', sex, '.RData')))

## extract predictions
# number of observations
N <- length(inla_fit$.args$data$y)

## vector to store the means
mu_pred <- rep(NA, N)

## vector to store the sd
lower_pred <- rep(NA, N)
upper_pred <- rep(NA, N)

## loop through observations
for(idx in 1:N) {
    # store mean
  # RS 2-20-15: might need to run 'tmarginal()' to convert to difference space
    mu_pred[idx] <- inla.mmarginal(inla_fit$marginals.linear.predictor[[idx]])

    # find confidence intervals
    tmp <- inla.hpdmarginal(.95, inla_fit$marginals.linear.predictor[[idx]])
    lower_pred[idx] <- tmp[1]
    upper_pred[idx] <- tmp[2]
}

## observed
observed <- log(inla_fit$.args$data$y / inla_fit$.args$data$pop)

## other attributes
age <- (inla_fit$.args$data$i - 1) * 5
year <- (inla_fit$.args$data$j - 1) + 1980
iso3 <- rep(iso, each=I*J)
sex_v <- rep(sex, each=I*J/length(sex), length.out = length(iso3))


## store results
results <- data.frame(iso3=iso3, sex=sex_v, age=age, year=year, mx=observed, mean=mu_pred, lower=lower_pred, upper=upper_pred)

## save results
# save(results, file=file.path('results2', 'predictions_AC_nSmokers_yr27_n188.RData'))
# write.csv(results, file=file.path('results2', 'predictions_AC_nSmokers_yr27_n188.csv'), row.names=FALSE)
save(results, file=file.path('results_by_region2', paste0('predictions_AC_nSmokers_yr27_region', reg, 'sex', sex, '.RData')))
write.csv(results, file=file.path('results_by_region2', paste0('predictions_AC_nSmokers_yr27_region', reg, 'sex', sex, '.csv')), row.names=FALSE)

} # end of for-loop that loops through countries

time_end <- Sys.time()
