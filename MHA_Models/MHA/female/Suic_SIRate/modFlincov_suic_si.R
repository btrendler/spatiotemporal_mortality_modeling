# This is a binomial model with 30,000 burn-in and 30,000 samples

library(CARBayesST)
library(tidyverse)
library(sf)
library(tigris)

# Load the data
load("/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/data_for_models/f_data.Rdata")

# For creating diagnostics table
beta1_summary_list <- list()

beta1_cred_int <- list()

all_deltas <- data.frame(
  fips = numeric(),
  value = numeric(),
  ageGroup = numeric()
)

all_phis <- data.frame(
  fips = numeric(),
  value = numeric(),
  ageGroup = numeric()
)

keep = c("keep",
         "forMod_F",
         "W",
         "beta1_summary_list",
         "beta1_cred_int",
         "all_deltas",
         "all_phis")

for(i in 1:18) {
  cat("Running model", i, "\n")

  # Select age group i
  forMod <- forMod_F |> 
    dplyr::filter(bounded_age == i)|>
    dplyr::arrange(year, fips)

  offset <- forMod$pop
  form <- Y_suicide ~ si_rate

  modFLincov <- ST.CARlinear(formula = form,
                             data = forMod,
                             W = W,
                             family = "binomial",
                             trials = offset,
                             burnin = 100000,
                             n.sample = 150000,
                             verbose = TRUE)

  # set fitted values to null to save memory
  modFLincov$samples$fitted <- NULL

  # Save the model
  save(modFLincov, file=paste("modF",i,"lincovall.Rdata", sep=""))

  # Transpose to make it a single-row data frame with proper column names
  beta_row <- as.data.frame(t(modFLincov$summary.results["si_rate", ]))
  beta_row$AgeGroup <- i
  beta1_summary_list[[i]] <- beta_row

  beta_samples <- modFLincov$samples$beta[, 2]  # column 2 = MentalHealthDays
  beta_row <- data.frame(
    Mean = mean(beta_samples),
    Q2.5 = quantile(beta_samples, 0.025),
    Q97.5 = quantile(beta_samples, 0.975),
    AgeGroup = i
  )
  beta1_cred_int[[i]] <- beta_row

  # Create a histogram of alphas
  jpeg(paste("Diagnostics/alpha_hist_F",i,".jpeg",sep=""))
  hist(modFLincov$samples$alpha, xlab="alpha")
  dev.off()

  # Save the deltas
  mean.deltas <- base::apply(modFLincov$samples$delta, 2, mean)
  jpeg(paste("Diagnostics/delta_hist_F",i,".jpeg", sep=""))
  hist(mean.deltas, xlab="Mean of Deltas")
  dev.off()

  regions <- unique(forMod$fips)
  deltas.df <- data.frame(fips=regions, value=mean.deltas, ageGroup = i)
  all_deltas <- dplyr::bind_rows(all_deltas, deltas.df)

  mean.phis <- base::apply(modFLincov$samples$phi,2,mean)
  phis.df <- data.frame(fips=regions, value=mean.phis, ageGroup=i)
  all_phis <- dplyr::bind_rows(all_phis, phis.df)

  pdf(paste("Diagnostics/modF",i,"covtrace.pdf", sep=""))
  plot(modFLincov$samples$beta[,1], type="l", main="Intercept")
  plot(modFLincov$samples$beta[,2], type="l", main="si_rate")
  plot(modFLincov$samples$alpha[], type="l", main="alpha")
  plot(modFLincov$samples$tau2[,1], type="l", main="tau2.int")
  plot(modFLincov$samples$tau2[,2], type="l", main="tau2.slo")
  plot(modFLincov$samples$rho[,1], type="l", main="rho.int")
  plot(modFLincov$samples$rho[,2], type="l", main="rho.slo")
  dev.off()

  saveRDS(all_deltas, file="Diagnostics/all_deltas.rds")
  saveRDS(all_phis, file="Diagnostics/all_phis.rds")
  saveRDS(beta1_cred_int, file="Diagnostics/beta_cred_int.rds")
  saveRDS(beta1_summary_list, file="Diagnostics/beta_summary_list.rds")

  rm(list=setdiff(ls(), keep))
  gc()
}
