library(jagstools)
library(rjags)
library(foreach)
context("Initial values")



# ----------------------------
# Setup data and JAGS model
# ----------------------------
# {{{
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

# }}}



# ----------------------------
# Test inititial values
# ----------------------------

inits.1 <- NULL
inits.2 <- list(alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
inits.3 <- function() list(alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))

inits.4 <- function() {
    list(
        .RNG.seed = runif(1, 0, 2^31),
        .RNG.name = "lecuyer::RngStream",
        alpha = rnorm(1, sd = 5),
        beta  = rnorm(1, sd = 5),
        sigma = runif(1, min = 0.01, max = 15))
}
# inits.4()

inits.fun <- function(chain) {
    out <- vector("list", chain)
    for(i in 1:chain)
        out[[i]] <-  list(
            .RNG.name  = "lecuyer::RngStream",
            .RNG.seed  = runif(1, 0, 2^31),
            alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
    out
}
inits.5 <- inits.fun(2)

inits.6 <- function(chain) {
    if(chain == 1) {
        set.seed(123)
        l1 <- list(
            .RNG.name  = "lecuyer::RngStream",
            .RNG.seed  = 123,
            alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
    }
    if(chain == 2) {
        set.seed(613971936)
        l1 <- list(
            .RNG.name  = "lecuyer::RngStream",
            .RNG.seed  = 123,
            alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
    }
    return(l1)
}



for(i in 1:6) {

    l.inits <- paste("inits.", i, sep = "")
    inits <- get(l.inits)

    txt <- paste("jags_sample", l.inits)
    test_that(txt, {

        # Need this line for R CMD check to run properly
        # https://github.com/hadley/testthat/issues/129
        Sys.setenv("R_TESTS" = "")

        fit.s <- jags_sample(
            data = dat,
            inits = inits,
            n.chains = 2,
            file = "example_jags.bug",
            variable.names = params,
            method = "serial",
            load.modules = "lecuyer",
            progress.bar = "none")

        fit.p <- jags_sample(
            data = dat,
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
    })


}










##
unlink("example_jags.bug")
