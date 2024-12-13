library(openxlsx) # For exporting tables
library(AER)      # For IV regressions

# Load the dataset
data <- read.csv("./twea.csv", row.names = FALSE)

# Generate time dummies for the year variable
generate_time_dummies <- function(data, years, column) {
  for (x in years) {
    col_name <- paste0("td_", x)
    data[[col_name]] <- as.integer(data[[column]] == x)
  }
  return(data)
}
data <- generate_time_dummies(data, unique(data$grntyr), "grntyr")

# Table 1: Summary Statistics
summary_statistics <- data.frame(mean_countcl = mean(data$count_cl, na.rm = TRUE),
                                 mean_countusa = mean(data$count_usa, na.rm = TRUE),
                                 mean_anypatent = mean(data$anypatent, na.rm = TRUE))
write.csv(summary_statistics, file = "summary_table1.csv", row.names = FALSE)

# Table 2: OLS Regressions
results_list <- list()
results_list[["Model_1"]] <- lm(count_usa ~ coun_tcl + anypatent + twea, data = data)
results_list[["Model_2"]] <- lm(count_usa ~ count_cl * twea + anypatent, data = data)

# Save OLS regression summaries
sink("ols_table2.txt")
for (model in results_list) {
  print(summary(model))
}
sink()

# Table 3: ITT Analysis
results_list <- list()
results_list[["Model_ITT1"]] <- lm(countusa ~ countcl + counterclitt + twea, data = data)
results_list[["Model_ITT2"]] <- lm(countusa ~ counterclitt + twea, data = data)

# Save ITT regression summaries
sink("itt_table3.txt")
for (model in results_list) {
  print(summary(model))
}
sink()

# Table 4: Instrumental Variables Regression
results_list <- list()
results_list[["IV_Model_1"]] <- ivreg(count_usa ~ count_cl + twea | count_cl_itt + twea, data = data)
results_list[["IV_Model_2"]] <- ivreg(count_usa ~ count_cl * twea | count_cl_itt * twea, data = data)

# Save IV regression summaries
sink("iv_table4.txt")
for (model in results_list) {
  print(summary(model))
}
sink()

# Table 5: Interaction Models
results_list <- list()
results_list[["Interaction_Model"]] <- lm(count_usa ~ count_cl * twea + count_cl_itt, data = data)

# Save Interaction Model summaries
sink("interaction_table5.txt")
for (model in results_list) {
  print(summary(model))
}
sink()

# Table 6: Subclass Analysis
subclass_analysis <- aggregate(count_usa ~ mainclass_id + subcl, data = data, FUN = sum, na.rm = TRUE)
subclass_analysis <- subclass_analysis[order(-subclass_analysis$count_usa), ]
write.csv(subclass_analysis, file = "subclass_table6.csv", row.names = FALSE)
