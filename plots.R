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

set.seed(123)

# Number of simulations
n_simulations <- 1000

temperature <- data_tidy_air_quality$temperature

# Initialize a vector to store whether the null hypothesis was rejected in each simulation
reject_null <- numeric(n_simulations)


# Variance of the uniform distribution needs to be 100, so we calculated b = 17.32
a <- -17.32
b <- 17.32

# Run simulations
type_1_error_counter <- 0
for (i in 1:n_simulations) {
  
  # Generate error term e ~ Uniform(a, b)
  e <- runif(length(temperature), min = a, max = b)
  e_pois <- rpois(length(temperature), var(temperature))
  e_norm <- rnorm(length(temperature),0, var(temperature))
  # Generate Y = 30 + e (since b1 = 0)
  Y <- 30 + e_pois
  
  
  # Fit the linear model Y = b0 + b1 * temperature
  model <- lm(Y ~ temperature)
  
  # Perform hypothesis test on b1 (null hypothesis: b1 = 0) and extract from lm
  p_value <- summary(model)$coefficients[2, 4]
  p_value
  
  if (p_value < 0.05) {
    type_1_error_counter <- 1 + type_1_error_counter
  }
  
}

# Calculate Type I error rate (proportion of times the null was incorrectly rejected)
type_1_error_rate <-(type_1_error_counter)/1000

# Output the Type I error rate

type_1_error_rate





