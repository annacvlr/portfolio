# Source the existing script
source("2_model_fitting_plot.R")

# Load required libraries
library(ggplot2)

# Define a function to compute residuals
compute_residuals <- function(model, data, response_var) {
  if (is.null(model)) {
    return(NULL)  # Return NULL if model did not converge
  }
  
  y_obs <- data[[response_var]]
  y_pred <- predict(model, newdata = data)
  residuals <- y_obs - y_pred
  
  return(data.frame(Time = data$Time, Residuals = residuals))
}

# Create a list to store residuals
residuals_list <- list()

# Loop through each unique ID
for (id in unique_ids) {
  current_subset <- filter(data, num_ID == id)
  
  residuals_list[[id]] <- list(
    quadratic = compute_residuals(quadratic_fit, current_subset, "PopBio"),
    log_quadratic = compute_residuals(log_quadratic_fit, current_subset, "logPopBio"),
    logistic = compute_residuals(logistic_fit, current_subset, "PopBio"),
    log_logistic = compute_residuals(log_logistic_fit, current_subset, "logPopBio"),
    gompertz = compute_residuals(gompertz_fit, current_subset, "PopBio"),
    log_gompertz = compute_residuals(log_gompertz_fit, current_subset, "logPopBio")
  )
}

residuals_list <- lapply(residuals_list, function(id_list) {
  Filter(Negate(is.null), id_list)  # Removes NULLs
})

# Convert residuals list into a long dataframe for plotting
residuals_df <- do.call(rbind, lapply(names(residuals_list), function(id) {
  do.call(rbind, lapply(names(residuals_list[[id]]), function(model) {
    if (!is.null(residuals_list[[id]][[model]])) {
      cbind(ID = id, Model = model, residuals_list[[id]][[model]])
    }
  }))
})) %>% as.data.frame()  # Ensure it's a dataframe

# Filter out any remaining NULLs

# Plot residuals against time
# Plot residuals against time

# Select a specific ID for plotting
chosen_id <- unique(residuals_df$ID_num)[1]  # Select the first available ID (change as needed)
filtered_residuals <- filter(residuals_df, ID_num == chosen_id)

# Plot residuals against time only for the chosen ID
p <- ggplot(filtered_residuals, aes(x = Time, y = Residuals, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Model, scales = "free_y", drop = TRUE) +
  labs(title = paste("Residuals vs Time for ID:", chosen_id),
       x = "Time", y = "Residuals") +
  theme_minimal()
# Save the plot
ggsave("../results/residuals_plot.png", plot = p, width = 12, height = 8)

# Display the plot
print(p)


# Save the plot
#ggsave("../results/residuals_plot.png", plot = p, width = 12, height = 8)

# Display the plot
