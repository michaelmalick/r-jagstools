# jagstools
*An R package to conveniently run JAGS in parallel from R*


# Overview

`jagstools` consists of one main function, `jags_sample` that is a wrapper for
functions in the `rjags` package that makes it easier to sample MCMC chains
using parallel processing. Parallel processing is conducted by the `parLapply`
function in the `parallel` package. Unlike other packages that have similar
functionality (e.g., `R2jags` and `runjags`) `jags_sample` returns an
`mcmc.list` object that is easily manipulated using the `coda` package, rather
than defining a new class. Because of this, in its current state, models fit
using `jags_sample` cannot be updated later by running the sampler longer.


# Installation
The `jagstools` package is not on CRAN, but can be installed from R using:

    install.packages("devtools")
    library(devtools)
    install_github(repo = "michaelmalick/r-jagstools")
    library(jagstools)


# License
MIT
