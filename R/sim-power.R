library(survival)
library(tidyverse)
library(parallel)

simSurvTimes <- function(n, x, hr, tmax, shape, scale, dist = "exponential") {
  ## Generate survival times
  ## TODO: Change hazard function for different distribution
  ## From Bender et al. 2005
  u <- runif(n)
  if (dist == "exponential") {
    h0 <- scale
    hazard <- h0 * exp(log(hr) * x)
    times <- -log(u) / hazard
  }
  ## Create censoring times (unrelated to survival)
  cens <- tmax * runif(n)
  events <- ifelse(times <= cens, 1, 0)
  times <- pmin(times, cens)
  ## events <- sample(c(0, 1), n, prob = c(pcens, 1 - pcens), replace = TRUE)

  tibble(time = times, event = events, x)
}

## Check whether the model results in a significant effect
isSignif <- function(model, alpha = 0.05) {
  mod_summ <- summary(model)
  p_value <- mod_summ$waldtest["pvalue"]
  unname(p_value < alpha)
}

## Estimate the power as the mean number of times that a model gives a
## significant effect out of the nsims simulations.
simPower <- function(n, x, hr, ..., nsims = 1000) {
  datasets <- lapply(
    rep(n, nsims),
    function(N) simSurvTimes(N, x, hr, tmax = 30, scale = 1/5)
  )
  models <- lapply(
    datasets,
    function(df) coxph(Surv(time, event) ~ x, data = df)
  )
  signif <- sapply(models, isSignif)

  mean(signif)
}
## Parallelized simPower
simPowerPar <- function(n, x, hr, ..., nsims = 1000, ncores = 4) {
  datasets <- mclapply(
    rep(n, nsims),
    function(N) simSurvTimes(N, x, hr, tmax = 30, scale = 1/5),
    mc.cores = ncores
  )
  models <- mclapply(
    datasets,
    function(df) coxph(Surv(time, event) ~ x, data = df),
    mc.cores = ncores)
  signif <- sapply(models, isSignif)

  mean(signif)
}

par_grid <- expand.grid(
  n = seq(1, 1000, 100),
  hr = seq(1, 1.2, 0.05)
)

par_grid
par_grid
n <- 1000
x <- rnorm(n)
hr <- 1.02
system.time(simPower(n, x, hr))
system.time(simPowerPar(n, x, hr, ncores = 10))


n <- 100000
simPowerPar(n = n, x = rnorm(n), hr = 1.03, ncores = 12)
