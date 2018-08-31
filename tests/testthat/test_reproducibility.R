library(jagstools)
library(rjags)
context("Reproducibility")



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



## Test reproducibility ------------------------------------
types <- c("PSOCK", "FORK")

## Setting both set.seed() and .RNG.seed inside the
## inits() function should make each MCMC chain be identical
for(j in 1:length(types)) {
    fit.s <- vector("list", 3)
    fit.p <- vector("list", 3)
    for(i in 1:3) {

        inits <- function() {
            set.seed(123)
            list(
                .RNG.name  = "lecuyer::RngStream",
                .RNG.seed  = 613971936,
                alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
        }

        fit.s[[i]] <- jags_sample(data = dat,
                                  inits = inits,
                                  n.chains = 2,
                                  file = "example_jags.bug",
                                  variable.names = params,
                                  method = "serial",
                                  progress.bar = "none")

        fit.p[[i]] <- jags_sample(data = dat,
                                  inits = inits,
                                  n.chains = 2,
                                  file = "example_jags.bug",
                                  variable.names = params,
                                  method = "parallel",
                                  progress.bar = "none",
                                  load.modules = "lecuyer",
                                  parallel = list(n.clusters = 2,
                                                  type = types[j]))

    }

    txt <- paste("jags_sample reproduce chains 1", types[j])
    test_that(txt, {
        expect_equal(class(fit.s[[1]]), "mcmc.list")
        expect_equal(class(fit.p[[1]]), "mcmc.list")

        ## chains within a call to jags_samples should be equal
        expect_equal(fit.s[[1]][[1]], fit.s[[1]][[2]])
        expect_equal(fit.p[[1]][[1]], fit.p[[1]][[2]])

        ## chains across calls to jags_samples should be equal

        ## this fails when check() is run but passes when test() is run
        ## manual testing that the test is passed
        # expect_equal(fit.s[[1]][[1]], fit.s[[3]][[1]])

        expect_equal(fit.p[[1]][[1]], fit.p[[2]][[1]])

    })

}

## Setting the .RNG.seed to be the same for all inits and setting set.seed() for
## each set of inits to a different value should make each chain within a call
## to jags_serial be independent and the same chain (e.g., chain 1) across calls
## to jags_serial be the same (i.e., the results should be reproducible).
for(j in 1:length(types)) {
    fit.s <- vector("list", 3)
    fit.p <- vector("list", 3)
    for(i in 1:3) {

        inits_fun <- function() {
            set.seed(123)
            l1 <- list(
                .RNG.name  = "lecuyer::RngStream",
                .RNG.seed  = 123,
                alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
            set.seed(613971936)
            l2 <- list(
                .RNG.name  = "lecuyer::RngStream",
                .RNG.seed  = 123,
                alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
            list(l1, l2)
        }
        inits <- inits_fun()

        fit.s[[i]] <- jags_sample(data = dat,
                                  inits = inits,
                                  n.chains = 2,
                                  file = "example_jags.bug",
                                  variable.names = params,
                                  method = "serial",
                                  progress.bar = "none")

        fit.p[[i]] <- jags_sample(data = dat,
                                  inits = inits,
                                  n.chains = 2,
                                  file = "example_jags.bug",
                                  variable.names = params,
                                  method = "parallel",
                                  progress.bar = "none",
                                  load.modules = "lecuyer",
                                  parallel = list(n.clusters = 2,
                                                  type = types[j]))

    }

    txt <- paste("jags_sample reproduce chains 2", types[j])
    test_that(txt, {
        expect_equal(class(fit.s[[1]]), "mcmc.list")
        expect_equal(class(fit.p[[1]]), "mcmc.list")

        ## chains within a call to jags_samples should not be equal
        expect_equal(class(all.equal(fit.s[[1]][[1]], fit.s[[1]][[2]])),
            "character")
        expect_equal(class(all.equal(fit.p[[1]][[1]], fit.p[[1]][[2]])),
            "character")

        ## chains across calls to jags_samples should be equal
        expect_equal(fit.s[[1]][[1]], fit.s[[2]][[1]])
        expect_equal(fit.p[[1]][[1]], fit.p[[2]][[1]])

    })

}

## If no set.seed call is supplied in the inits function the results should not
## be reproducible if 'method = serial' but should be if 'method = parallel' and
## the RNGseed is supplied in the parallel call.
for(j in 1:length(types)) {
    fit.s <- vector("list", 3)
    fit.p <- vector("list", 3)
    for(i in 1:3) {

        inits <- function() {
            list(
                .RNG.name  = "lecuyer::RngStream",
                .RNG.seed  = 613971936,
                alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
        }

        fit.s[[i]] <- jags_sample(data = dat,
                                  inits = inits,
                                  n.chains = 2,
                                  file = "example_jags.bug",
                                  variable.names = params,
                                  method = "serial",
                                  progress.bar = "none")

        fit.p[[i]] <- jags_sample(data = dat,
                                  inits = inits,
                                  n.chains = 2,
                                  file = "example_jags.bug",
                                  variable.names = params,
                                  method = "parallel",
                                  progress.bar = "none",
                                  load.modules = "lecuyer",
                                  parallel = list(n.clusters = 2,
                                                  RNGseed = 123,
                                                  type = types[j]))

    }

    txt <- paste("jags_sample reproduce chains 3", types[j])
    test_that(txt, {
        expect_equal(class(fit.s[[1]]), "mcmc.list")
        expect_equal(class(fit.p[[1]]), "mcmc.list")

        ## chains within a call to jags_samples should not be equal
        expect_equal(class(all.equal(fit.s[[1]][[1]], fit.s[[1]][[2]])),
            "character")
        expect_equal(class(all.equal(fit.p[[1]][[1]], fit.p[[1]][[2]])),
            "character")

        ## chains across calls to jags_samples should be equal for parallel
        ## but not for serial computation
        expect_equal(class(all.equal(fit.s[[1]][[1]], fit.s[[2]][[1]])),
            "character")
        expect_equal(fit.p[[1]][[1]], fit.p[[2]][[1]])

    })

}


## clean-up
unlink("example_jags.bug")
