#########
# Load required libraries
library(ggplot2)
library(minpack.lm)
library(dplyr)
library(data.table)
library(tidyr)



#load and clean datasets
data <- read.csv("../results/subset_data.csv") %>%
  filter(Time >= 0, PopBio > 0, !is.na(PopBio)) %>%
  mutate(logPopBio = log(PopBio)) %>%
  group_by(num_ID) %>%
  filter(n() >= 5) %>%
  ungroup()

#define unique IDs
unique_ids <- unique(data$num_ID)
n_ids <- length(unique_ids)

#define Models
logistic1 <- function(t, r, K, N0) {
  N0 * K * exp(r * t) / (K + N0 * (exp(r * t) - 1))
}

gompertz_model <- function(t, r, K, N0, t_lag) {
  N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t) / ((K - N0) * log(10)) + 1))
}



#Stat metrics= 
  #R2
calculate_R2 <- function(model, data, response_var) {
  if (is.null(model)) {
    return(NA)  # If model didn't converge, return NA
  }
  
  # Extract observed values
  y_obs <- data[[response_var]]
  y_pred <- predict(model, newdata = data)
  
  # Compute R²
  SS_res <- sum((y_obs - y_pred)^2, na.rm = TRUE)
  SS_tot <- sum((y_obs - mean(y_obs, na.rm = TRUE))^2, na.rm = TRUE)
  
  R2 <- 1 - (SS_res / SS_tot)
  
  return(R2)
}
  #AIC, BIC, R2 safe -- only if model is not null
safe_AIC <- function(model) if (!is.null(model)) tryCatch(AIC(model), error = function(e) NA) else NA
safe_BIC <- function(model) if (!is.null(model)) tryCatch(BIC(model), error = function(e) NA) else NA
safe_R2 <- function(model, data, response_var) if (!is.null(model)) tryCatch(calculate_R2(model, data, response_var), error = function(e) NA) else NA


  #AICc
calculate_AICc <- function(aic, k, n) {
  if (n - k - 1 <= 0) return(Inf)
  return(aic + (2 * k * (k + 1)) / (n - k - 1))
}


#control set for nls
control <- nls.control(maxiter = 50, tol = 1e-5)

#Initial parameters function
get_starting_parameters <- function(data, log_transform = FALSE) {
  if (nrow(data) < 2) {
    return(list(N0 = 1, Nmax = 10, rmax = 0.01, t_lag = 0))  # Default values
  }

  #pop uses logPopBio if log_transformed= T, and Popbio if log_transformed = F
  pop <- ifelse(log_transform, "logPopBio", "PopBio")
  if (!pop %in% names(data) || all(is.na(data[[pop]]))) {
    return(list(N0 = 1, Nmax = 10, rmax = 0.01, t_lag = 0))
  }

  #N0
  N0 <- min(data[[pop]], na.rm = TRUE)
  Nmax <- max(data[[pop]], na.rm = TRUE)
  if (Nmax <= 0) Nmax <- N0 + 1  #ensure Nmax > N0

  #r
  growth_rates <- diff(data[[pop]]) / diff(data$Time)
  rmax <- ifelse(length(growth_rates) > 0, max(growth_rates, na.rm = TRUE), 0.01)

  #t_lag
  t_lag <- if (!is.na(rmax) && rmax > 0) {
    inflection_index <- which.max(growth_rates)
    if (!is.na(inflection_index) && inflection_index > 0 && inflection_index <= length(data$Time)) {
      data$Time[inflection_index] - (N0 / rmax)
    } else { NA }
  } else { NA }

    #if values are NA or Inf - set some realistic parameters for comparison
  if (is.na(N0) || is.infinite(N0)) N0 <- 1
  if (is.na(Nmax) || is.infinite(Nmax)) Nmax <- 10
  if (is.na(rmax) || is.infinite(rmax)) rmax <- 0.01
  if (is.na(t_lag) || is.infinite(t_lag)) t_lag <- 0

  return(list(N0 = N0, Nmax = Nmax, rmax = rmax, t_lag = t_lag))
}

results_list <- list()


#for loop - for each unique id 
for (id in unique_ids) {
  current_subset <- filter(data, num_ID == id)
  time_values <- data.frame(Time = seq(min(current_subset$Time), max(current_subset$Time), length.out = 100))
  print(paste("Processing ID:", id, "at", Sys.time()))

  #setting log / non log initial parameters
  start_params_normal <- get_starting_parameters(current_subset, log_transform = FALSE)
  start_params_log <- get_starting_parameters(current_subset, log_transform = TRUE)

  # ---- Quadratic Models ----
  quadratic_fit <- tryCatch(lm(PopBio ~ poly(Time, 2), data = current_subset), error = function(e) NULL)
  log_quadratic_fit <- tryCatch(lm(logPopBio ~ poly(Time, 2), data = current_subset), error = function(e) NULL)
      
  # ---- Logistic Models ----
  logistic_fit <- tryCatch(
    {
      nlsLM(PopBio ~ logistic1(Time, r, K, N0),
        data = current_subset,
        start = list(r = start_params_normal$rmax, K = start_params_normal$Nmax, N0 = start_params_normal$N0),
        control = control
      )
    },
    error = function(e) NULL
  )
      
  log_logistic_fit <- tryCatch(
    {
      nlsLM(logPopBio ~ logistic1(Time, r, K, N0),
        data = current_subset,
        start = list(r = start_params_log$rmax, K = start_params_log$Nmax, N0 = start_params_log$N0),
        control = control
      )
    },
    error = function(e) NULL
  )
  
  # ---- Gompertz Models ----
  gompertz_fit <- tryCatch(
    {
      nlsLM(PopBio ~ gompertz_model(Time, r, K, N0, t_lag),
        data = current_subset,
        start = list(
          r = start_params_normal$rmax, K = start_params_normal$Nmax,
          N0 = start_params_normal$N0, t_lag = start_params_normal$t_lag
        ),
        control = control
      )
    },
    error = function(e) NULL
  )
        
  log_gompertz_fit <- tryCatch(
    {
      nlsLM(logPopBio ~ gompertz_model(Time, r, K, N0, t_lag),
        data = current_subset,
        start = list(
          r = start_params_log$rmax, K = start_params_log$Nmax,
          N0 = start_params_log$N0, t_lag = start_params_log$t_lag
        ),
        control = control
      )
    },
    error = function(e) NULL
  )


  # ---- Predictions for model plots -----  
 time_values <- time_values %>%
    mutate(
      Quadratic_Pred = if (!is.null(quadratic_fit)) predict(quadratic_fit, newdata = time_values) else NA,
      Log_Quadratic_Pred = if (!is.null(log_quadratic_fit)) predict(log_quadratic_fit, newdata = time_values) else NA,
      Logistic_Pred = if (!is.null(logistic_fit)) predict(logistic_fit, newdata = time_values) else NA,
      Log_Logistic_Pred = if (!is.null(log_logistic_fit)) predict(log_logistic_fit, newdata = time_values) else NA,
      Gompertz_Pred = if (!is.null(gompertz_fit)) predict(gompertz_fit, newdata = time_values) else NA,
      Log_Gompertz_Pred = if (!is.null(log_gompertz_fit)) predict(log_gompertz_fit, newdata = time_values) else NA
    )

  # ---- Filter for good fits ----
  time_values_long <- time_values %>%
    pivot_longer(cols = c(Quadratic_Pred, Log_Quadratic_Pred, 
                          Logistic_Pred, Log_Logistic_Pred, 
                          Gompertz_Pred, Log_Gompertz_Pred),
                 names_to = "Model", values_to = "Prediction") %>%
    filter(!is.na(Prediction)) %>%
    mutate(Scale = case_when(
      Model %in% c("Quadratic_Pred", "Logistic_Pred", "Gompertz_Pred") ~ "PopBio",
      Model %in% c("Log_Quadratic_Pred", "Log_Logistic_Pred", "Log_Gompertz_Pred") ~ "Log(PopBio)"
    ))


current_subset_long <- current_subset %>%
  pivot_longer(cols = c(PopBio, logPopBio), names_to = "Scale", values_to = "Value") %>%
  mutate(Scale = recode(Scale, PopBio = "PopBio", logPopBio = "Log(PopBio)"))

# ---- Plotting ----
plot <- ggplot() +
    geom_point(data = current_subset, aes(x = Time, y = PopBio), size = 2, alpha = 0.7) +
    geom_line(data = time_values_long, aes(x = Time, y = Prediction, color = Model), size = 1) +
    facet_wrap(~Scale, scales = "free_y", 
               labeller = labeller(Scale = c("Log(PopBio)" = "Log(PopBio)", "PopBio" = "PopBio"))) +
    labs(title = paste("Population Growth for ID:", id),
         x = "Time", y = "Population Size") +
    scale_color_manual(values = c("Quadratic_Pred" = "red", "Log_Quadratic_Pred" = "darkred",
                                  "Logistic_Pred" = "blue", "Log_Logistic_Pred" = "darkblue",
                                  "Gompertz_Pred" = "green", "Log_Gompertz_Pred" = "darkgreen"), 
                                  labels = c("Quadratic_Pred" = "Quadratic", 
                                  "Log_Quadratic_Pred" = "Log Quadratic",
                                  "Logistic_Pred" = "Logistic", 
                                  "Log_Logistic_Pred" = "Log Logistic",
                                  "Gompertz_Pred" = "Gompertz", 
                                  "Log_Gompertz_Pred" = "Log Gompertz")) +          
    theme_minimal() +
    theme_bw() +
    theme(plot.title = element_text(size = 16))

    #save plots into Results/modelplots/ subdirectory
  ggsave(filename = paste0("../results/modelplots/Models_Comparison_plot_ID_", id, ".png"), 
       plot = plot, width = 10, height = 6)

  # ---- Statistical Metrics - dataframe ----
  result <- data.frame(
    ID = id,
    quadratic_converged = !is.null(quadratic_fit),
    log_quadratic_converged = !is.null(log_quadratic_fit),
    logistic_converged = !is.null(logistic_fit),
    log_logistic_converged = !is.null(log_logistic_fit),
    gompertz_converged = !is.null(gompertz_fit),
    log_gompertz_converged = !is.null(log_gompertz_fit),

    AIC_quadratic = safe_AIC(quadratic_fit),
    AIC_log_quadratic = safe_AIC(log_quadratic_fit),
    AIC_logistic = safe_AIC(logistic_fit),
    AIC_log_logistic = safe_AIC(log_logistic_fit),
    AIC_gompertz = safe_AIC(gompertz_fit),
    AIC_log_gompertz = safe_AIC(log_gompertz_fit),

    BIC_quadratic = safe_BIC(quadratic_fit),
    BIC_log_quadratic = safe_BIC(log_quadratic_fit),
    BIC_logistic = safe_BIC(logistic_fit),
    BIC_log_logistic = safe_BIC(log_logistic_fit),
    BIC_gompertz = safe_BIC(gompertz_fit),
    BIC_log_gompertz = safe_BIC(log_gompertz_fit),

    R2_quadratic = if (!is.null(quadratic_fit)) summary(quadratic_fit)$r.squared else NA,
    R2_log_quadratic = if (!is.null(log_quadratic_fit)) summary(log_quadratic_fit)$r.squared else NA,
    R2_logistic = safe_R2(logistic_fit, current_subset, "PopBio"),
    R2_log_logistic = safe_R2(log_logistic_fit, current_subset, "logPopBio"),
    R2_gompertz = safe_R2(gompertz_fit, current_subset, "PopBio"),
    R2_log_gompertz = safe_R2(log_gompertz_fit, current_subset, "logPopBio"),

    AICc_quadratic = if (!is.null(quadratic_fit)) calculate_AICc(safe_AIC(quadratic_fit), 3, nrow(current_subset)) else NA,
    AICc_log_quadratic = if (!is.null(log_quadratic_fit)) calculate_AICc(safe_AIC(log_quadratic_fit), 3, nrow(current_subset)) else NA,
    AICc_logistic = if (!is.null(logistic_fit)) calculate_AICc(safe_AIC(logistic_fit), 3, nrow(current_subset)) else NA,
    AICc_log_logistic = if (!is.null(log_logistic_fit)) calculate_AICc(safe_AIC(log_logistic_fit), 3, nrow(current_subset)) else NA,
    AICc_gompertz = if (!is.null(gompertz_fit)) calculate_AICc(safe_AIC(gompertz_fit), 4, nrow(current_subset)) else NA,
    AICc_log_gompertz = if (!is.null(log_gompertz_fit)) calculate_AICc(safe_AIC(log_gompertz_fit), 4, nrow(current_subset)) else NA
  )
  results_list <- append(results_list, list(result))

}

#bind the df
results <- do.call(rbind, results_list)

#save statistical dataframe
write.csv(results, "../results/model_fits.csv", row.names = FALSE)


