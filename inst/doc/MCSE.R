## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, warning = FALSE, message = FALSE---------------------------
library(dplyr)
library(tibble)
library(kableExtra)

abs_dat <- tribble(
  ~ Criterion,       ~ Measure,                        ~ Definition,                                               ~ Estimate,                                                            ~ MCSE,
  "Bias",            "Difference from true parameter", "$\\text{E}(T) - \\theta$",                                 "$\\bar{T} - \\theta$",                                                "$\\sqrt{S_T^2/ K}$",
  "Variance",        "Precision",                      "$\\text{E}\\left[(T - \\text{E}(T))^2\\right]$",           "$S_T^2$",                                                             "$S_T^2 \\sqrt{\\frac{k_T - 1}{K}}$", 
  "Standard Error",  "Precision",                      "$\\sqrt{\\text{E}\\left[(T - \\text{E}(T))^2\\right]}$",   "$S_T$",                                                               "$\\sqrt{\\frac{K - 1}{K} \\sum_{j=1}^K (\\sqrt{S_{T(j)}^2} - S_T)^2 }$",
  "MSE",             "Accuracy",                       "$\\text{E}\\left[(T - \\theta)^2\\right]$",                "$\\frac{1}{K}\\sum_{k=1}^{K}\\left(T_k - \\theta\\right)^2$",         "$\\sqrt{\\frac{1}{K}\\left[S_T^4 (k_T - 1) + 4 S_T^3 g_T(\\bar{T} - \\theta) + 4 S_T^2 (\\bar{T} - \\theta)^2\\right]}$ ",
  "RMSE",            "Accuracy",                       "$\\sqrt{\\text{E}\\left[(T - \\theta)^2\\right]}$",        "$\\sqrt{\\frac{1}{K}\\sum_{k=1}^{K}\\left(T_k - \\theta\\right)^2}$", "$\\sqrt{\\frac{K - 1}{K} \\sum_{j=1}^K \\left(RMSE_{(j)} - RMSE\\right)^2}$"
)


knitr::kable(abs_dat, escape = FALSE, caption = "Table 1. Absolute Performance Criteria") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
  

## ----message = FALSE, warning = FALSE-----------------------------------------
library(simhelpers)
library(dplyr)
library(tibble)
library(knitr)
library(dplyr)
library(kableExtra)

welch_res %>%
  glimpse()

## -----------------------------------------------------------------------------
# using do()
welch_res %>%
  filter(method == "t-test") %>% # filter just conventional t-test res
  group_by(n1, n2, mean_diff) %>% # grouping 
  do(calc_absolute(., estimates = est, true_param = mean_diff)) %>% # run the function
  kable(digits = 5) # create a kable table 

## -----------------------------------------------------------------------------
# using group_modify()
welch_res %>%
  filter(method == "t-test") %>% # filter just conventional t-test res
  mutate(params = mean_diff) %>% # group_modify cannot take in a group column as an argument
  group_by(n1, n2, mean_diff) %>% # grouping 
  group_modify(~ calc_absolute(.x, estimates = est, true_param = params)) %>%
  kable(digits = 5)

## ----echo = FALSE, warning = FALSE, message = FALSE---------------------------
rel_dat <- tibble(Criterion = c("Relative Bias","Relative MSE", "Relative RMSE"),
                  Measure = c("Relative difference from true parameter", "Accuracy", "Accuracy"),
              Definition = c("$\\text{E}(T) / \\theta$", "$\\text{E}\\left[(T - \\theta)^2\\right]/ \\theta^2$", "$\\sqrt{\\text{E}\\left[(T - \\theta)^2\\right]/ \\theta^2}$"),
              Estimate = c("$\\bar{T} / \\theta$", "$\\frac{(\\bar{T} - \\theta)^2 + S_T^2}{\\theta^2}$", "$\\sqrt{\\frac{(\\bar{T} - \\theta)^2 + S_T^2}{\\theta^2}}$"),
              MCSE = c("$\\sqrt{S_T^2 / (K\\theta^2)}$", 
                       "$\\sqrt{\\frac{1}{K\\theta^2}\\left[S_T^4 (k_T - 1) + 4 S_T^3 g_T(\\bar{T} - \\theta) + 4 S_T^2 (\\bar{T} - \\theta)^2\\right]}$", "$\\sqrt{\\frac{K - 1}{K} \\sum_{j=1}^K \\left(rRMSE_{(j)} - rRMSE)^2\\right)}$"))

knitr::kable(rel_dat, escape = FALSE, caption = "Table 2. Relative Performance Criteria") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
  

## -----------------------------------------------------------------------------
# using group_modify()
welch_res %>%
  filter(method == "t-test") %>%
  mutate(params = mean_diff) %>%
  group_by(n1, n2, mean_diff) %>%
  group_modify(~ calc_relative(.x, estimates = est, true_param = params)) %>%
  kable(digits = 5)

## ----echo = FALSE, warning = FALSE, message = FALSE---------------------------
rel_dat_var <- tibble(Criterion = c("Relative Bias","Relative MSE", "Relative RMSE"),
                      Measure = c("Relative difference from true parameter", "Accuracy", "Accuracy"),
              Definition = c("$\\text{E}(V) / \\lambda$", "$\\text{E}\\left[(V - \\lambda)^2\\right]/ \\lambda^2$", "$\\sqrt{\\text{E}\\left[(V - \\lambda)^2\\right]/ \\lambda^2}$"),
              Estimate = c("$\\bar{V} / S_T^2$", "$\\frac{(\\bar{V} - S_T^2)^2 + S_V^2}{S_T^4}$", "$\\sqrt{\\frac{(\\bar{V} - S_T^2)^2 + S_V^2}{S_T^4}}$"),
              MCSE = c("$\\sqrt{\\frac{K - 1}{K} \\sum_{j=1}^K \\left(rB_{(j)} - rB\\right)^2}$", "$\\sqrt{\\frac{K - 1}{K} \\sum_{j=1}^K \\left(rMSE_{(j)} - rMSE\\right)^2}$",
                      "$\\sqrt{\\frac{K - 1}{K} \\sum_{j=1}^K \\left(rRMSE_{(j)} - rRMSE\\right)^2}$" ))


knitr::kable(rel_dat_var, escape = FALSE, caption = "Table 3. Relative Performance Criteria for Variance Estimators") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
  

## -----------------------------------------------------------------------------
welch_res %>%
  group_by(n1, n2, mean_diff, method) %>%
  group_modify(~ calc_relative_var(.x, estimates = est, var_estimates = var)) %>%
  kable(digits = 5)

## ----echo = FALSE, warning = FALSE, message = FALSE---------------------------
hyp_dat <- tibble(Criterion = c("Rejection Rate","Coverage","Width"),
              Measure = c("Type 1 error or power", "Proportion of intervals containing true parameter", "Precision"),
              Definition = c("$\\rho_\\alpha = Pr(P_k) < \\alpha$", "$\\omega_\\beta = Pr(A \\leq \\theta \\leq B)$", "$\\text{E}(W) = \\text{E}(B - A)$"),
              Estimate = c("$r_\\alpha  = \\frac{1}{K} \\sum_{k=1}^K I(P_k < \\alpha)$", "$c_\\beta = \\frac{1}{K}\\sum_{k=1}^K I(A_k \\leq \\theta \\leq B_k)$", "$\\bar{W} = \\bar{B} - \\bar{A}$"),
              MCSE = c("$\\sqrt{r_\\alpha(1 - r_\\alpha) / K}$",  
                       "$\\sqrt{c_\\beta (1 - c_\\beta) / K}$", "$\\sqrt{S_W^2 / K}$"))

knitr::kable(hyp_dat, escape = FALSE, caption = "Table 4. Hypothesis Testing and Confidence Intervals Performance Criteria") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

## -----------------------------------------------------------------------------
# using group_modify()
welch_res %>%
  group_by(n1, n2, mean_diff, method) %>%
  group_modify(~ calc_rejection(.x, p_values = p_val)) %>%
  kable(digits = 5)

## -----------------------------------------------------------------------------
# using group_modify()
welch_res %>%
  mutate(params = mean_diff) %>%
  group_by(n1, n2, mean_diff, method) %>%
  group_modify(~ calc_coverage(.x, lower_bound = lower_bound, upper_bound = upper_bound, true_param = params)) %>%
  kable(digits = 5)

