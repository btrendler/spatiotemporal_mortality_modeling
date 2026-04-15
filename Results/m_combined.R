library(tidyverse)

# Initialize a list to store each model's summary
summary_list <- list()

# Base path to the model files
base_path <- "brigg_trendler/MentalHealth/rscripts/modeling/models/7_22/MHA/male/Death_DepRate"

age_labels <- c("0–4", "5–9", "10–14", "15–19", "20–24", "25–29", "30–34", "35–39",
                "40–44", "45–49", "50–54", "55–59", "60–64", "65–69", "70–74",
                "75–79", "80–84", "85+")

# Loop over age groups (1 to 18)
for (i in 1:18) {
  # Build the file path
  file_path <- file.path(base_path, paste0("modM", i, "lincovall.Rdata"))
  
  # Load the Rdata file (modMLincov is the object inside)
  load(file_path)  # This loads modMLincov into the environment

  # Extract and tidy the summary
  summary_df <- as.data.frame(modMLincov$summary.results)
  summary_df$AgeGroup <- i
  summary_df$Parameter <- rownames(summary_df)
  summary_df <- summary_df[, c("AgeGroup", "Parameter", "Mean", "2.5%", "97.5%",
                               "n.sample", "% accept", "n.effective", "Geweke.diag")]
  summary_df$AgeLabel <- age_labels[i]

  
  # Store it
  summary_list[[i]] <- summary_df
  rm(list = c("modMLincov"))  # Clean up to save memory
}

# Combine all into one data frame
all_summaries <- do.call(rbind, summary_list)
rownames(all_summaries) <- NULL

write.csv(all_summaries, "/yunity/bt327/spatmort/brigg_trendler/MentalHealth/rscripts/modeling/models/7_22/Results/m_combined_model_summaries.csv", row.names = FALSE)


