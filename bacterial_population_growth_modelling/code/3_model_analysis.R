##Script to analyse statistcial metrics from model fitting

#load libraries
library(dplyr) 
library(gridExtra)
library(ggplot2)
library(grid)
library(gtable)
library(readr)
library(purrr)
library(xtable)
library(tidyr)


# Load model results
model_fits <- read.csv("../results/model_fits.csv")

## group each model & ignore the stats of models w R2 less than 0.5

# Quadratic Model (R² > 0.5)
quadratic_filtered <- model_fits %>%
    filter(R2_quadratic > 0.5) %>%
    select(ID, starts_with("AIC_quadratic"), starts_with("BIC_quadratic"), starts_with("R2_quadratic"), starts_with("AICc_quadratic"))

# Log-Quadratic Model (R² > 0.5)
log_quadratic_filtered <- model_fits %>%
    filter(R2_log_quadratic > 0.5) %>%
    select(ID, starts_with("AIC_log_quadratic"), starts_with("BIC_log_quadratic"), starts_with("R2_log_quadratic"), starts_with("AICc_log_quadratic"))

# Logistic Model (R² > 0.5)
logistic_filtered <- model_fits %>%
    filter(R2_logistic > 0.5) %>%
    select(ID, starts_with("AIC_logistic"), starts_with("BIC_logistic"), starts_with("R2_logistic"), starts_with("AICc_logistic"))

# Log-Logistic Model (R² > 0.5)
log_logistic_filtered <- model_fits %>%
    filter(R2_log_logistic > 0.5) %>%
    select(ID, starts_with("AIC_log_logistic"), starts_with("BIC_log_logistic"), starts_with("R2_log_logistic"), starts_with("AICc_log_logistic"))

# Gompertz Model (R² > 0.5)
gompertz_filtered <- model_fits %>%
    filter(R2_gompertz > 0.5) %>%
    select(ID, starts_with("AIC_gompertz"), starts_with("BIC_gompertz"), starts_with("R2_gompertz"), starts_with("AICc_gompertz"))

# Log-Gompertz Model (R² > 0.5)
log_gompertz_filtered <- model_fits %>%
    filter(R2_log_gompertz > 0.5) %>%
    select(ID, starts_with("AIC_log_gompertz"), starts_with("BIC_log_gompertz"), starts_with("R2_log_gompertz"), starts_with("AICc_log_gompertz"))


#rejoin filtered data
joined_data <- quadratic_filtered %>%
    full_join(log_quadratic_filtered, by = "ID") %>%
    full_join(logistic_filtered, by = "ID") %>%
    full_join(log_logistic_filtered, by = "ID") %>%
    full_join(gompertz_filtered, by = "ID") %>%
    full_join(log_gompertz_filtered, by = "ID")



# ---- Make a table w "wining" models per each metric ----

r2_columns <- joined_data%>%
    select(ID, R2_quadratic, R2_log_quadratic, R2_logistic, R2_log_logistic, R2_gompertz, R2_log_gompertz)

# Find highest R² for each ID
model_wins_r2 <- r2_columns %>%
    pivot_longer(cols = -ID, names_to = "Model", values_to = "R2") %>%
    group_by(ID) %>%
    filter(R2 == max(R2, na.rm = TRUE)) %>%
    ungroup()

win_counts_r2 <- model_wins_r2 %>%
    count(Model) %>%
    rename(Wins = n)  # Rename column

#table
win_table_r2 <- win_counts_r2 %>%
    mutate(Model = gsub("R2_", "", Model)) %>%  # Clean model names
    arrange(desc(Wins))


# Select relevant columns 
AIC_columns <- joined_data%>%
    select(ID, AIC_quadratic, AIC_log_quadratic, AIC_logistic, AIC_log_logistic, AIC_gompertz, AIC_log_gompertz)

# Find the highest R² for each ID
model_wins_AIC <- AIC_columns %>%
    pivot_longer(cols = -ID, names_to = "Model", values_to = "AIC") %>%
    group_by(ID) %>%
    filter(AIC == min(AIC, na.rm = TRUE)) %>%
    ungroup()

# Count 
win_counts_AIC <- model_wins_AIC %>%
    count(Model) %>%
    rename(Wins = n)  # Rename column

win_table_AIC <- win_counts_AIC %>%
    mutate(Model = gsub("AIC_", "", Model)) %>%  # Clean model names
    arrange(desc(Wins))


# Select relevant columns 
AICc_columns <- joined_data%>%
    select(ID, AICc_quadratic, AICc_log_quadratic, AICc_logistic, AICc_log_logistic, AICc_gompertz, AICc_log_gompertz)

# Find the model with the highest R² for each ID
model_wins_AICc <- AICc_columns %>%
    pivot_longer(cols = -ID, names_to = "Model", values_to = "AICc") %>%
    group_by(ID) %>%
    filter(AICc == min(AICc, na.rm = TRUE)) %>%
    ungroup()

# Count total wins per model
win_counts_AICc <- model_wins_AICc %>%
    count(Model) %>%
    rename(Wins = n)  # Rename column


win_table_AICc <- win_counts_AICc %>%
    mutate(Model = gsub("AICc_", "", Model)) %>%  # Clean model names
    arrange(desc(Wins))


# Select relevant columns (
BIC_columns <- joined_data%>%
    select(ID, BIC_quadratic, BIC_log_quadratic, BIC_logistic, BIC_log_logistic, BIC_gompertz, BIC_log_gompertz)

# Find highest R² for each ID
model_wins_BIC <- BIC_columns %>%
    pivot_longer(cols = -ID, names_to = "Model", values_to = "BIC") %>%
    group_by(ID) %>%
    filter(BIC == min(BIC, na.rm = TRUE)) %>%
    ungroup()

# Count total per model
win_counts_BIC <- model_wins_BIC %>%
    count(Model) %>%
    rename(Wins = n)  # Rename column

# Create table
win_table_BIC <- win_counts_BIC %>%
    mutate(Model = gsub("BIC_", "", Model)) %>%  # Clean model names
    arrange(desc(Wins))

# table with all of the tallied wins per model per id
win_summary <- list(win_table_r2, win_table_AIC, win_table_AICc, win_table_BIC) %>%
    reduce(full_join, by = "Model") %>%
    arrange(desc(win_table_r2))

# Rename columns by appending the appropriate suffix
colnames(win_summary)[-1] <- paste0(colnames(win_summary)[-1], "_", rep(c("R2", "AIC", "AICc", "BIC"), each = ncol(win_summary) / 4))

# Print the summary 
print(win_summary)