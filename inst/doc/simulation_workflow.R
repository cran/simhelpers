## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE----------------------------------------
library(simhelpers)
library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(knitr)
library(kableExtra)
library(broom)
library(ggplot2)

## ---- eval = FALSE------------------------------------------------------------
#  generate_dat <- function(model_params) {
#  
#    return(dat)
#  }

## -----------------------------------------------------------------------------
generate_dat <- function(n1, n2, mean_diff){

  dat <- tibble(
    y = c(rnorm(n = n1, mean_diff, 1), # mean diff as mean, sd 1
          rnorm(n = n2, 0, 2)), # mean 0, sd 2
    group = c(rep("Group 1", n1), rep("Group 2", n2))
  )

  return(dat)

}

## -----------------------------------------------------------------------------
set.seed(2020143)
example_dat <- generate_dat(n1 = 10000, n2= 10000, mean_diff = 1) 

example_dat %>%
  head()

## -----------------------------------------------------------------------------
example_dat %>%
  group_by(group) %>%
  summarize(n = n(),
            M = mean(y),
            SD = sd(y)) %>%
  kable(digits = 3)

## ---- fig.width = 7, fig.height = 3-------------------------------------------
ggplot(example_dat, aes(x = y, fill = group)) + 
  geom_density(alpha = .5) + 
  labs(x = "Outcome Scores", y = "Density", fill = "Group") + 
  theme_bw() +
  theme(legend.position = c(0.9, 0.8))

## ---- eval = FALSE------------------------------------------------------------
#  estimate <- function(dat, design_params) {
#  
#    return(results)
#  }

## -----------------------------------------------------------------------------
# t and p value
calc_t <- function(est, vd, df, method){

  se <- sqrt(vd)  # standard error 
  t <- est / se # t-test 
  p_val <-  2 * pt(-abs(t), df = df) # p value
  ci <- est + c(-1, 1) * qt(.975, df = df) * se # confidence interval
  
  res <- tibble(method = method, est = est, p_val = p_val, 
                lower_bound = ci[1], upper_bound = ci[2])

  return(res)
}


estimate <- function(dat, n1, n2){

  # calculate summary stats
  means <- tapply(dat$y, dat$group, mean)
  vars <- tapply(dat$y, dat$group, var)

  # calculate summary stats
  est <- means[1] - means[2] # mean diff
  var_1 <- vars[1] # var for group 1
  var_2 <- vars[2] # var for group 2

  # conventional t-test
  dft <- n1 + n2 - 2  # degrees of freedom
  sp_sq <- ((n1 - 1) * var_1 + (n2 - 1) * var_2) / dft  # pooled var
  vdt <- sp_sq * (1 / n1 + 1 / n2) # variance of estimate

  # welch t-test
  dfw <- (var_1 / n1 + var_2 / n2)^2 / (((1 / (n1 - 1)) * (var_1 / n1)^2) + ((1 / (n2 - 1)) * (var_2 / n2)^2))  # degrees of freedom 
  vdw <- var_1 / n1 + var_2 / n2 # variance of estimate

  results <- bind_rows(calc_t(est = est, vd = vdt, df = dft, method = "t-test"),
                   calc_t(est = est, vd = vdw, df = dfw, method = "Welch t-test"))


  return(results)

}


## -----------------------------------------------------------------------------
est_res <- 
  estimate(example_dat, n1 = 10000, n2 = 10000) %>%
  mutate_if(is.numeric, round, 5)

est_res

## -----------------------------------------------------------------------------
t_res <- 
  bind_rows(
    tidy(t.test(y ~ group, data = example_dat, var.equal = TRUE)), 
    tidy(t.test(y ~ group, data = example_dat))
  ) %>%
  mutate(
    estimate = estimate1 - estimate2, 
    method = c("t-test", "Welch t-test")
  ) %>%
  select(method, est = estimate, p_val = p.value, lower_bound = conf.low, upper_bound = conf.high) %>%
  mutate_if(is.numeric, round, 5)

t_res

## ---- eval = FALSE------------------------------------------------------------
#  estimate <- function(dat, design_parameters){
#  
#    # write estimation models here
#    # e.g., fit_mimic <- lavaan::cfa(...)
#  
#  
#    # convergence
#    if(fit_mimic@optim$converged == FALSE){  # this syntax will depend on how the specific model stores convergence
#      res <- tibble(method = method, est = NA, p_val = NA,
#                    lower_bound = NA, upper_bound = NA)
#    } else{
#      res <- tibble(method = method, est = est, p_val = p_val,
#                    lower_bound = ci[1], upper_bound = ci[2])
#    }
#  
#    return(res)
#  
#  }

## ---- eval = FALSE------------------------------------------------------------
#  calc_performance <- function(results, model_params) {
#  
#    return(performance_measures)
#  }

## -----------------------------------------------------------------------------
calc_performance <- function(results) {
  
  performance_measures <- results %>%
    group_by(method) %>%
    group_modify(~ calc_rejection(.x, p_values = p_val))

  return(performance_measures)
}


## ---- eval = FALSE------------------------------------------------------------
#  run_sim <- function(iterations, model_params, design_params, seed = NULL) {
#    if (!is.null(seed)) set.seed(seed)
#  
#    results <-
#      rerun(iterations, {
#        dat <- generate_dat(model_params)
#        estimate(dat, design_params)
#      }) %>%
#      bind_rows()
#  
#    calc_performance(results, model_params)
#  }

## -----------------------------------------------------------------------------
run_sim <- function(iterations, n1, n2, mean_diff, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  results <-
    rerun(iterations, {
      dat <- generate_dat(n1 = n1, n2 = n2, mean_diff = mean_diff)
      estimate(dat = dat, n1 = n1, n2 = n2)
    }) %>%
    bind_rows()
  
  calc_performance(results)

}


## ---- eval = FALSE------------------------------------------------------------
#  set.seed(20150316) # change this seed value!
#  
#  # now express the simulation parameters as vectors/lists
#  
#  design_factors <- list(factor1 = , factor2 = , ...) # combine into a design set
#  
#  params <-
#    cross_df(design_factors) %>%
#    mutate(
#      iterations = 1000,  # change this to how many ever iterations
#      seed = round(runif(1) * 2^30) + 1:n()
#    )
#  
#  # All look right?
#  lengths(design_factors)
#  nrow(params)
#  head(params)

## -----------------------------------------------------------------------------
set.seed(20200110)

# now express the simulation parameters as vectors/lists

design_factors <- list(
  n1 = 50,
  n2 = c(50, 70),
  mean_diff = c(0, .5, 1, 2)
)
params <-
  cross_df(design_factors) %>%
  mutate(
    iterations = 1000,
    seed = round(runif(1) * 2^30) + 1:n()
  )


# All look right?
lengths(design_factors)
nrow(params)
head(params)


## ----serial-------------------------------------------------------------------
system.time(
  results <- 
    params %>%
    mutate(
      res = pmap(., .f = run_sim)
    ) %>%
    unnest(cols = res)
)

results %>%
  kable()

## ---- eval = FALSE------------------------------------------------------------
#  plan(multisession)

## ----furrr, warning = F, message = F, eval = FALSE----------------------------
#  library(future)
#  library(furrr)
#  
#  plan(multisession) # choose an appropriate plan from the future package
#  
#  system.time(
#    results <-
#      params %>%
#      mutate(res = future_pmap(., .f = run_sim)) %>%
#      unnest(cols = res)
#  )
#  

## ----evaluate-by-row, warning = F, message = F, eval = FALSE------------------
#  plan(multisession)
#  results <- evaluate_by_row(params, run_sim)

## ----setup, eval = FALSE------------------------------------------------------
#  create_skeleton()

