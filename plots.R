library(ggplot2)
library(kableExtra)
#| results: hide
#| warning: false
#| message: false
#| error: false
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("MiguelRodo/DataTidyRodoSTA2005S")
data("data_tidy_air_quality", package = "DataTidyRodoSTA2005S")
head(data_tidy_air_quality)


# Density plots
mean_particle <- mean(data_tidy_air_quality$particulate_matter)
sd_particle <- sd(data_tidy_air_quality$particulate_matter)

ggplot(data_tidy_air_quality, aes(x=particulate_matter))+
  geom_histogram(fill="deepskyblue3", color="black", binwidth = 3, aes(y=..density..))+
  stat_function(fun=dnorm, args=list(mean=mean_particle, sd=sd_particle), color="firebrick4", size=1.2)+
  labs(title="Density Plot of Particulate Matter", y="Density", x="Particulate Matter")


# Question4

data_tidy_air_quality$industrial_activity <- relevel(factor(data_tidy_air_quality$industrial_activity), ref="None") 
multi_model <- lm(particulate_matter ~ . + temperature:humidity, data=data_tidy_air_quality)

estimates <- as.data.frame(summary(multi_model)$coefficients)
confint_df <- as.data.frame(confint(multi_model))
confint_df$Estimate <- as.numeric(estimates$Estimate)
confint_rows <-row.names(confint_df)
row_orders = c(confint_rows[1:8], "temperature:humidity", confint_rows[10:length(confint_rows) - 1])
confint_df_reordered <- confint_df[row_orders,c("2.5 %", "Estimate", "97.5 %")]

confint_table <- kable(confint_df_reordered, digits = 4, align="c") |>
  kable_styling(font_size = 12) |>
  pack_rows(index= c("Intercept"=1, "Traffic Density"=1, "Industrial Activity"=3, "Natural Factors" = 4, "Day of Week"=6, "Holiday"=1, "Urban Greenery"=1))


summary_df <- as.data.frame(summary(multi_model)$coefficients)
summary_categorical_df <- summary_df[c(3:5, 9:14),]
summary_table <- kable(summary_categorical_df, digits=4)|>
  kable_styling(font_size = 12)

# For reproducibility
set.seed(123)

# Number of observations in temperature data
n <- length(temperature)

# Number of simulations
n_simulations <- 100

# Function to run a single simulation
run_simulation <- function() {
  
  # Simulate heteroscedastic errors with mean variance 100 and variance of the variances = 50
  error_variances <- rnorm(n, mean = 100, sd = sqrt(50)) # Variance of each error term
  # Generate e:
  e <- c()
  for (i in 1:n){
    e[i] <- rnorm(1, mean = 0, sd = sqrt(error_variances))
  }
  #e <- rnorm(n, mean = 0, sd = sqrt(error_variances))  # Simulated errors with heteroscedasticity
  # Generate Y values under null hypothesis (beta_1 = 0)
  Y <- 30 + e
  
  # Fit the model Y ~ temperature
  model <- lm(Y ~ temperature)
  
  # Perform hypothesis test for beta_1 (test if beta_1 = 0)
  p_value <- summary(model)$coefficients[2, 4]  # Extract p-value for temperature coefficient
  
  # Return whether the null hypothesis is rejected (p-value < 0.05)
  return(as.numeric(p_value < 0.05))
}

# Run all simulations using replicate
reject_null <- replicate(n_simulations, run_simulation())

# Calculate the Type I error rate (proportion of rejected null hypotheses)
type_1_error_rate <- mean(reject_null)

# Print the Type I error rate
type_1_error_rate





