inla_apc2 <- function(iso3, sex, predict=TRUE, effects=TRUE, stub='', num.threads=NULL) {

    # load the R-INLA library
    library(INLA)

    # place to store results
    outs <- list()

    # make sure iso3/sex are vectors
    if (!is.vector(sex)) sex <- c(sex)
    if (!is.vector(iso3)) iso3 <- c(iso3)

    # load data
    death_data <- list()
    pop_data <- list()
    for (i in iso3) {
        for (s in sex) {
            death_data[[paste(i,s)]] <- round(as.matrix(read.csv(file.path('data', paste0('deaths_', i, '_', s, '.csv')), header=FALSE)))
            pop_data[[paste(i,s)]] <- round(as.matrix(read.csv(file.path('data', paste0('pop_', i, '_', s, '.csv')), header=FALSE)))
        }
    }

    # number of agegroups
    I <- dim(death_data[[paste(iso3[1], sex[1])]])[1]
    # total number of periods
    J <- dim(death_data[[paste(iso3[1], sex[1])]])[2]
    # grid factor (because age group and period are both given in 5-year intervals)
    G <- 5
    # number of cohorts
    K <- G*(I-1) + J

    # function to get the cohort index following Heuer (1997): k=G*(I-i)+j
    get_cohort_index <- function(I, J, G){
      k <- c()
      for(j in 1:J){
       for(i in 1:I){
          k <- c(k, G*(I - i) + j)
        }
      }
      return(k)
    }

    # age index
    i <- rep(1:I, J)
    # period index
    j <- rep(1:J, each=I)
    # cohort index
    k <- get_cohort_index(I,J,G)


    # Prior parameter specifications
    # log-gamma prior for log precision
    prior.rw2 <- c(1,0.00005)
    prior.iid <- c(1,0.005)
    # normal prior for Fisher's z-transformed corrrelation
    prior.cor <- c(0,0.2)

    # Combine data
    deaths <- c(sapply(death_data, function(x) { return(c(x)) }))
    pop <- c(sapply(pop_data, function(x) { return(c(x)) }))

    # find number of units to predict (e.g. countries/sexes)
    num <- length(death_data)

    # overdispersion (correlated)
    overdis_cor <- rep(1:(I*J),num)

    # generate the data.frame used to apply inla
    data <- data.frame(y=deaths, pop=pop,
                  i=rep(i,num),
                  j=rep(j,num),
                  k=rep(k,num),
                  zcor=overdis_cor)

    # grouping index (for each country/sex)
    g <- rep(seq(num), each=length(i))

    # add intercept for each country/sex
    for (n in 1:num) {
        data[paste0('mu',n)] <- as.numeric(g == n)
    }

    # apc model with correlated time effects and overdispersion (possible as projections are identifiable)
    formula_apc <- 'y ~ f(i, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=1.27)),
        control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=4.66))),
        group=g, constr=TRUE, rankdef=2) +
      f(j, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=6.46)),
        control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=4.66))),
        group=g, constr=TRUE, rankdef=2) +
      f(k, model="rw2", hyper=list(prec = list(param=prior.rw2, initial=7.29)),
        control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=2.27))),
        group=g, constr=TRUE, rankdef=2) +
      f(zcor, model="iid", hyper=list(prec = list(param=prior.iid, initial=3.62)),
        control.group=list(model="exchangeable", hyper=list(rho = list(prior="normal", param=prior.cor, initial=1.96))), group=g) - 1'
    for (n in 1:num) {
        formula_apc <- paste(formula_apc, '+', paste0('mu', n))
    }
    formula_apc <- as.formula(formula_apc)

    # with so many hyperparameters we have to increase the number of maximum function evaluation in
    # the derivation of the posterior marginals for the hyperparameters
    inla_fit <- inla(formula_apc, family="poisson", E=pop, data=data,
            quantile=c(0.025,0.1,0.5,0.9,0.975), verbose=TRUE,
            control.compute=list(dic=TRUE, cpo=TRUE, hyperpar =TRUE),
            control.predictor=list(compute=TRUE),
            control.inla=list(numint.maxfeval=80000000), num.threads=num.threads)

    # store the analyses
    outs[['inla_fit']] <- inla_fit

    # save cohort/period/age effects
    if (effects) {
        # placeholders
        age_mean <- rep(NA, I*num)
        age_upper <- rep(NA, I*num)
        age_lower <- rep(NA, I*num)
        period_mean <- rep(NA, J*num)
        period_upper <- rep(NA, J*num)
        period_lower <- rep(NA, J*num)
        cohort_mean <- rep(NA, K*num)
        cohort_upper <- rep(NA, K*num)
        cohort_lower <- rep(NA, K*num)

        # loop through ages
        for (idx in 1:I*num) {
            age_mean[idx] <- inla.mmarginal(inla_fit$marginals.random$i[[idx]])
            tmp <- inla.hpdmarginal(.95, inla_fit$marginals.random$i[[idx]])
            age_lower <- tmp[1]
            age_upper <- tmp[2]
        }
        age <- rep(seq(0, (I-1)*5, 5), num)
        iso3_v <- rep(iso3, each=I*num)
        sex_v <- rep(sex, each=I*num)
        age_effects <- data.frame(age=age, iso3=iso3_v, sex=sex_v, mean=age_mean, lower=age_lower, upper=age_upper)
        write.csv(age_effects, file=file.path('results', paste0('age_effects',stub,'.csv')))

        # loop through periods
        for (idx in 1:J*num) {
            period_mean[idx] <- inla.mmarginal(inla_fit$marginals.random$j[[idx]])
            tmp <- inla.hpdmarginal(.95, inla_fit$marginals.random$j[[idx]])
            period_lower <- tmp[1]
            period_upper <- tmp[2]
        }
        period <- rep(seq(1980, 1980+J-1, 1), num)
        iso3_v <- rep(iso3, each=J*num)
        sex_v <- rep(sex, each=J*num)
        period_effects <- data.frame(period=period, iso3=iso3_v, sex=sex_v, mean=period_mean, lower=period_lower, upper=period_upper)
        write.csv(period_effects, file=file.path('results', paste0('period_effects',stub,'.csv')))

        # loop through cohorts
        for (idx in 1:K*num) {
            cohort_mean[idx] <- inla.mmarginal(inla_fit$marginals.random$k[[idx]])
            tmp <- inla.hpdmarginal(.95, inla_fit$marginals.random$k[[idx]])
            cohort_lower <- tmp[1]
            cohort_upper <- tmp[2]
        }
        cohort <- rep(seq(1900, 1900+K-1, 1), num)
        iso3_v <- rep(iso3, each=K*num)
        sex_v <- rep(sex, each=K*num)
        cohort_effects <- data.frame(cohort=cohort, iso3=iso3_v, sex=sex_v, mean=cohort_mean, lower=cohort_lower, upper=cohort_upper)
        write.csv(cohort_effects, file=file.path('results', paste0('cohort_effects',stub,'.csv')))

        # return results
        outs[['age_effects']] <- age_effects
        outs[['period_effects']] <- period_effects
        outs[['cohort_effects']] <- cohort_effects
    }

    # extract predictions
    if (predict) {
        # number of observations
        N <- length(inla_fit$.args$data$y)

        # vector to store the means
        mu_pred <- rep(NA, N)

        # vector to store the sd
        lower_pred <- rep(NA, N)
        upper_pred <- rep(NA, N)

        # loop through observations
        for(idx in 1:N) {
            # store mean
            mu_pred[idx] <- inla.mmarginal(inla_fit$marginals.linear.predictor[[idx]])

            # find confidence intervals
            tmp <- inla.hpdmarginal(.95, inla_fit$marginals.linear.predictor[[idx]])
            lower_pred[idx] <- tmp[1]
            upper_pred[idx] <- tmp[2]
        }

        # observed
        observed <- log(inla_fit$.args$data$y / inla_fit$.args$data$pop)

        # other attributes
        age <- (inla_fit$.args$data$i - 1) * 5
        year <- (inla_fit$.args$data$j - 1) + 1980
        iso3_v <- rep(iso3, each=I*J)
        sex_v <- rep(sex, each=I*J)

        # store results
        results <- data.frame(iso3=iso3_v, sex=sex_v, age=age, year=year, mx=observed, mean=mu_pred, lower=lower_pred, upper=upper_pred)

        # save results
        save(results, file=file.path('results', paste0('predictions',stub,'.RData')))
        write.csv(results, file=file.path('results', paste0('predictions',stub,'.csv')), row.names=FALSE)
        outs[['results']] <- results
    }

    # save outputs
    inla_model <- outs
    save(inla_model, file=file.path('results', paste0('inla_model',stub,'.RData')))

    return(outs)
}
