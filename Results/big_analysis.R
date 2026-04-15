# This script is responsible for analyzing the results from the sui_soc_eco models.
# Authored by Brigg Trendler
# Updated by Brianne Weaver

# This sets the working directory to where the source file is.
 setwd("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/models/7_22/MHA/female/Death_DepRate")

library(tidyverse)
library(gridExtra)
library(grid)
library(jpeg)

beta_summary_list <- list()

for (i in 1:18) {
  model_name <- paste0("modF", i, "lincovall.Rdata")
  load(paste0(model_name))
  model <- get("modFLincov")

  # Transpose to make it a single-row data frame with proper column names
  beta_row <- as.data.frame(model$summary.results) |>
    mutate(covariate = rownames(model$summary.results),
    AgeGroup = i) |>
    select(covariate, everything())

  rownames(beta_row) <- NULL  # Remove row names

  beta_summary_list[[i]] <- beta_row
  rm(list = c("modFLincov"))
}

beta_summary <- dplyr::bind_rows(beta_summary_list)

beta_sum_F <- beta_summary
beta_sum_F <- beta_sum_F |>
  rename(Q2.5 = `2.5%`, Q97.5 = `97.5%`)

names(beta_sum_F)
beta_sum_F3 <- beta_sum_F |> filter(AgeGroup > 2 & covariate %in% c("dep_rate", "(Intercept)", "alpha", "tau2.int", "tau2.slo", "rho.int", "rho.slo")) 
max(beta_sum_F3$Q97.5)
# [1] 0.3451, 1.9756
min(beta_sum_F3$Q2.5)
# [1] -0.6902, -11.4547

sum_tab <- function(var){
jpeg(paste0("_summary_table_", var, ".jpg"), width = 600, height = 400, quality = 100)
grid.table(beta_summary |> filter(covariate == var) |> select(-covariate) |> select(AgeGroup, everything()), rows = NULL)
dev.off()
}

sum_tab("si_rate")
sum_tab("(Intercept)")
sum_tab("alpha")
sum_tab("tau2.int")
sum_tab("tau2.slo") 
sum_tab("rho.int")
sum_tab("rho.slo")


beta_summary <- beta_sum_F

cred_int_plot <- function(var, y_min = -0.5, y_max = 2) {
  ggplot(beta_summary |> filter(covariate == var & AgeGroup > 2), aes(x = AgeGroup, y = Mean)) +
    geom_line(color = "darkred") +
    geom_point(color = "darkred") +
    geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.3, color = "darkred") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("Effect of", var, "on total_deaths for females by Age Group"),
      subtitle = "Posterior Mean and 95% Credible Intervals",
      x = "Age Group",
      y = "Posterior Mean"
    ) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    theme_minimal()
}

jpeg("_cred_int_si_rate.jpg", width = 600, height = 600, quality = 100)
cred_int_plot("si_rate")
dev.off() 

jpeg("_cred_int_Intercept.jpg", width = 600, height = 600, quality = 100)
cred_int_plot("(Intercept)")
dev.off()

jpeg("_cred_int_alpha.jpg", width = 600, height = 600, quality = 100)
cred_int_plot("alpha")
dev.off()

jpeg("_cred_int_tau2.int.jpg", width = 600, height = 600, quality = 100)
cred_int_plot("tau2.int")
dev.off() 

jpeg("_cred_int_tau2.slo.jpg", width = 600, height = 600, quality = 100)
cred_int_plot("tau2.slo")
dev.off()

jpeg("_cred_int_rho.int.jpg", width = 600, height = 600, quality = 100)
cred_int_plot("rho.int")
dev.off()

jpeg("_cred_int_rho.slo.jpg", width = 600, height = 600, quality = 100)
cred_int_plot("rho.slo")
dev.off()
