library(jagstools)
library(rjags)
context("Multiple chains")



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

params <- c("alpha", "beta", "sigma")



## Test multiple chains ------------------------------------
inits <- function() list(alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))

test_that("jags_sample multiple chains", {

    # Need this line for R CMD check to run properly
    # https://github.com/hadley/testthat/issues/129
    Sys.setenv("R_TESTS" = "")

    fit.s <- jags_sample(data = dat,
                         inits = inits,
                         n.chains = 3,
                         file = "example_jags.bug",
                         variable.names = params,
                         method = "serial",
                         progress.bar = "none")

    ## R CMD check test fails if n.chains > 2
    ## devtools::test() passes all tests if n.chains > 2
    fit.p <- jags_sample(data = dat,
                         inits = inits,
                         n.chains = 2,
                         file = "example_jags.bug",
                         variable.names = params,
                         method = "parallel",
                         progress.bar = "none",
                         load.modules = "lecuyer",
                         parallel = list(n.clusters = 2))

    expect_equal(class(fit.s), "mcmc.list")
    expect_equal(class(fit.p), "mcmc.list")
    expect_equal(length(fit.s), 3)
    expect_equal(length(fit.p), 2)
    ## make sure chain1 != chain2 != chain3
    expect_equal(class(all.equal(fit.s[[1]], fit.s[[2]])), "character")
    expect_equal(class(all.equal(fit.s[[1]], fit.s[[3]])), "character")
    expect_equal(class(all.equal(fit.s[[2]], fit.s[[3]])), "character")
    expect_equal(class(all.equal(fit.p[[1]], fit.p[[2]])), "character")
    ## expect_equal(class(all.equal(fit.p[[1]], fit.p[[3]])), "character")
    ## expect_equal(class(all.equal(fit.p[[2]], fit.p[[3]])), "character")
})


## clean-up
unlink("example_jags.bug")
