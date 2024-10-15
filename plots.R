library(tidyverse)
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
  
