library(tidyverse)

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

mean_particle <- mean(data_tidy_air_quality$particulate_matter)
sd_particle <- sd(data_tidy_air_quality$particulate_matter)

ggplot(data_tidy_air_quality, aes(x=particulate_matter))+
  geom_histogram(fill="deepskyblue", color="black", binwidth = 3, aes(y=..density..))+
  stat_function(fun=dnorm, args=list(mean=mean_particle, sd=sd_particle), color="firebrick4", size=1.2)+
  labs(title="Density Plot of Particulate Matter", y="Density", x="Particulate Matter")