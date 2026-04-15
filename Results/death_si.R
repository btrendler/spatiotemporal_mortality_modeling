library(tidyverse)
library(gridExtra)
library(grid)
library(jpeg)

load_model_summaries <- function(sex, base_path) {
  beta_summary_list <- list()
  
  if (sex == "male") {
    for (i in 1:18) {
        model_path <- file.path(base_path, sex, "Death_SIRate", paste0("modM", i, "lincovall.Rdata"))
        load(model_path)
        model <- get("modMLincov")

        beta_row <- as.data.frame(model$summary.results) |>
        mutate(covariate = rownames(model$summary.results),
                AgeGroup = i,
                Sex = sex) |>  # Add Sex column
        select(Sex, covariate, everything())

        rownames(beta_row) <- NULL
        beta_summary_list[[i]] <- beta_row

        rm(list = c("modMLincov"))
    }
  } else if (sex == "female") {
    for (i in 1:18) {
        model_path <- file.path(base_path, sex, "Death_SIRate", paste0("modF", i, "lincovall.Rdata"))
        load(model_path)
        model <- get("modFLincov")

        beta_row <- as.data.frame(model$summary.results) |>
        mutate(covariate = rownames(model$summary.results),
                AgeGroup = i,
                Sex = sex) |>  # Add Sex column
        select(Sex, covariate, everything())

        rownames(beta_row) <- NULL
        beta_summary_list[[i]] <- beta_row

        rm(list = c("modFLincov"))
    }
  }
  dplyr::bind_rows(beta_summary_list)
}

# Load both male and female model summaries
base_path <- "/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/models/7_22/MHA"
beta_summary_female <- load_model_summaries("female", base_path)
beta_summary_male   <- load_model_summaries("male", base_path)

# Combine them
beta_summary_all <- bind_rows(beta_summary_female, beta_summary_male) |>
  rename(Q2.5 = `2.5%`, Q97.5 = `97.5%`) |>
  mutate(Age = (AgeGroup - 1) * 5 + 2.5)

# Filter to covariates of interest (optional)
covariates_to_plot <- c("si_rate", "(Intercept)", "alpha", "tau2.int", "tau2.slo", "rho.int", "rho.slo")
pretty_names <- c(
  "si_rate" = "Coefficient for Suicidal Ideation Rate",
  "(Intercept)" = "Intercept",
  "alpha" = "Country-Wide Time Trend",
  "tau2.int" = "Spatial Variance Parameter",
  "tau2.slo" = "Temporal Variance Parameter",
  "rho.int" = "Spatial Dependence Parameter",
  "rho.slo" = "Temporal Dependence Parameter"
)
beta_filtered <- beta_summary_all |> filter(covariate %in% covariates_to_plot & AgeGroup > 2)

# Function to get global min and max for each covariate
get_y_limits <- function(var) {
  subset <- beta_filtered |> filter(covariate == var)
  y_min <- min(subset$Q2.5)
  y_max <- max(subset$Q97.5)
  c(y_min, y_max)
}

# Map from variable -> y-axis label (as expressions)
y_label_for <- function(var) {
  switch(var,
    "si_rate"  = expression(beta[1]),
    "(Intercept)" = expression(beta[0]),
    "alpha"     = expression(alpha),
    "tau2.int"  = expression(tau[phi]^2),
    "tau2.slo"  = expression(tau[delta]^2),
    "rho.int"   = expression(rho[phi]),
    "rho.slo"   = expression(rho[delta]),
    expression("")   # fallback
  )
}

cred_int_plot_dual <- function(var) {
  ggplot(beta_filtered |> filter(covariate == var),
         aes(x = Age, y = Mean, color = Sex, fill = Sex, linetype = Sex)) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.2, color = NA) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "black") +
    labs(
      title = pretty_names[var],
      x = "Age",
      y = y_label_for(var)
    ) +
    scale_x_continuous(breaks = seq(10, 90, by = 10)) +
    scale_color_manual(values = c("female" = "red", "male" = "deepskyblue4")) +
    scale_fill_manual(values = c("female" = "red", "male" = "deepskyblue4")) +
    scale_linetype_manual(values = c("female" = "dashed", "male" = "dashed")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5,
                                margin = margin(t = 6, b = 6)),  # internal title margin
      plot.margin = margin(t = 12, r = 12, b = 12, l = 12),      # outer plot margin
      axis.title = element_text(size = 14),
      axis.text  = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 12),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank()
    )
}

for (var in covariates_to_plot) {
  p <- cred_int_plot_dual(var)
  out <- paste0("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/models/7_22/Results/Death_SIRate/_cred_int_",
                gsub("[()]", "", var), "_male_female.png")
  ggsave(out, plot = p, width = 8, height = 6, units = "in", dpi = 300, bg = "white")
}


