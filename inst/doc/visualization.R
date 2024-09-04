## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=F, warning=F----------------------------------------------
library(simhelpers)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

## -----------------------------------------------------------------------------
glimpse(Tipton_Pusto)

## -----------------------------------------------------------------------------
Tipton_Pusto <- Tipton_Pusto %>%
  mutate(q_graph = paste("q = ", q),
         m = paste("m = ", num_studies))

## ----fig.width = 8, fig.height = 5.5------------------------------------------
Tipton_Pusto %>%
  ggplot(aes(x = test, y = rej_rate, fill = test)) + 
  geom_hline(yintercept = .05, linetype = "dashed") + 
  geom_boxplot(alpha = .5) + 
  facet_grid(q_graph ~ m, scales = "free") + 
  labs(x = "Method", y = "Type 1 Error Rate", caption = "FIGURE 2. Type I error rate for alpha of .05 of five alternative tests. Dashed lines indicate the nominal alpha level.") + 
  theme_bw() +
  theme(legend.position = "none",
        plot.caption=element_text(hjust = 0, size = 10))

## -----------------------------------------------------------------------------
Tipton_Pusto %>%
  group_by(test) %>%
  summarize(mcse = max(mcse)) %>%
  kable(digits = 4)

