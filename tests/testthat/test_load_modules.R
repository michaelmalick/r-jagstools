library(jagstools)
library(rjags)
context("Load modules")



## Setup data and JAGS model -------------------------------
sim.data <- function(N = 100) {

    # True parameter values
    alpha <- 3
    beta  <- 0.5
    sigma <- 5.0

    # Simulate data
    x       <- runif(N, 5, 95)
    epsilon <- rnorm(N, 0, sigma)
    y       <- alpha + beta*x + epsilon

    # Return data in a list
    dat <- list(N = N, x = x, y = y)
    return(dat)
}


## Simulate and plot data
set.seed(129)
dat <- sim.data()


model.string <-
    'model {
        for (i in 1:N){
            y[i] ~ dnorm(mu.y[i], tau)
            mu.y[i] <- alpha + beta * x[i]
        }
        # Priors
        alpha ~ dnorm(0, .0001)
        beta  ~ dnorm(0, .0001)
        tau   <- pow(sigma, -2)
        sigma ~ dunif(0, 100)
    }'
writeLines(model.string, con = "example_jags.bug")



## Test load.modules ---------------------------------------
inits <- function() list(alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))

params <- c("alpha", "beta", "sigma", "deviance")


test_that("jags_sample load.modules", {

    # Need this line for R CMD check to run properly
    # https://github.com/hadley/testthat/issues/129
    Sys.setenv("R_TESTS" = "")


    fit.s <- jags_sample(data = dat,
                         inits = inits,
                         file = "example_jags.bug",
                         variable.names = params,
                         method = "serial",
                         progress.bar = "none",
                         load.modules = c("dic", "lecuyer"))

    fit.p <- jags_sample(data = dat,
                         inits = inits,
                         file = "example_jags.bug",
                         variable.names = params,
                         method = "parallel",
                         progress.bar = "none",
                         load.modules = c("dic", "lecuyer"))

    expect_equal(class(fit.s), "mcmc.list")
    expect_equal(class(fit.p), "mcmc.list")
    expect_equal("dic" %in% list.modules(), TRUE)
    expect_equal("lecuyer" %in% list.modules(), TRUE)
    expect_equal("deviance" %in% varnames(fit.s), TRUE)
    expect_equal("deviance" %in% varnames(fit.p), TRUE)
})


## clean-up
unlink("example_jags.bug")
