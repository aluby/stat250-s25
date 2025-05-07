library(tidyverse)
library(nycflights23)

set.seed(0501)

flights_sample <- flights |>
  #drop_na(dep_delay) |>
  slice_sample(n=40)

ggplot(flights_sample, aes(x = dep_delay)) + 
  geom_histogram(bins = 15)

N_bootstraps = 100
N = 10^4

ci_results <- tibble(
  t_lower = numeric(N_bootstraps),
  t_upper = numeric(N_bootstraps),
  percentile_lower = numeric(N_bootstraps),
  percentile_upper = numeric(N_bootstraps),
  boott_lower = numeric(N_bootstraps),
  boott_upper = numeric(N_bootstraps)
)

for(j in 1:N_bootstraps){
  flights_sample <- flights |>
    drop_na(dep_delay) |>
    slice_sample(n=40)
  
  data_sample <- flights_sample$dep_delay
  xbar <- mean(data_sample, na.rm = TRUE)
  for(i in 1:N){
    x <- sample(data_sample, size = n, replace = TRUE)
    boot_means[i] <- mean(x, na.rm = TRUE)
    boot_sds[i] <- sd(x, na.rm = TRUE)
  }
  
  t_star <- (boot_means - xbar)/(boot_sds/sqrt(n))
  
  ci_results$t_lower[j] = t.test(data_sample, conf.level = .95)$conf.int[1]
  ci_results$t_upper[j] = t.test(data_sample, conf.level = .95)$conf.int[2]
  
  ci_results$percentile_lower[j] = quantile(boot_means, probs = c(.025))
  ci_results$percentile_upper[j] = quantile(boot_means, probs = c(.975))
  
  ci_results$boott_lower[j] = xbar - quantile(t_star, probs = c(.975))*(sd(data_sample)/sqrt(n))
  ci_results$boott_upper[j] = xbar - quantile(t_star, probs = c(.025))*(sd(data_sample)/sqrt(n))
}
  
ci_results %>%
  mutate(sim_id = 1:100) %>%
  pivot_longer(-sim_id) %>%
  separate(name, into = c("type", "bound")) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(
    missed = (lower > mean(flights$dep_delay, na.rm = TRUE)) | (upper < mean(flights$dep_delay, na.rm = TRUE))
  ) |>
  ggplot(aes(xmin = lower, xmax = upper, y = sim_id, col = missed)) +
  geom_errorbar(size = 2) + 
  facet_wrap(vars(type)) +
  geom_vline(xintercept = mean(flights$dep_delay, na.rm = TRUE)) +
  scale_color_viridis_d(end = .76, option = "plasma", direction = -1) +
  theme(text = element_text(size = 36))

ggsave("flights-coverage-plot.png", width = 24, height = 16)

