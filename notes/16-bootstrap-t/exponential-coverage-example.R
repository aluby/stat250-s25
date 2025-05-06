set.seed(050625)

n = 25
theta = 4


ggplot(mapping = aes(x = data_sample)) + 
  geom_histogram(bins = 10, col = "white")

N_bootstraps = 100
N = 10^4
boot_means = numeric(N)

## addl things for bootstrap t
boot_sds <- numeric(N)

## addl things for CI simulation

ci_results <- tibble(
  t_lower = numeric(N_bootstraps),
  t_upper = numeric(N_bootstraps),
  percentile_lower = numeric(N_bootstraps),
  percentile_upper = numeric(N_bootstraps),
  boott_lower = numeric(N_bootstraps),
  boott_upper = numeric(N_bootstraps)
)

for(j in 1:N_bootstraps){
data_sample <- rexp(n, theta)
xbar <- mean(data_sample, na.rm = TRUE)

for(i in 1:N){
  x <- sample(data_sample, size = n, replace = TRUE)
  boot_means[i] <- mean(x, na.rm = TRUE)
  boot_sds[i] <- sd(x, na.rm = TRUE)
}

t_star <- (boot_means - xbar)/(boot_sds/sqrt(n))

ci_results$t_lower[j] = t.test(data_sample, conf.level = .95)$conf.int[1]
ci_results$t_upper[j] = t.test(data_sample, conf.level = .95)$conf.int[2]

ci_results$percentile_lower[j] = quantile(boot_means, probs = c(.05))
ci_results$percentile_upper[j] = quantile(boot_means, probs = c(.95))

ci_results$boott_lower[j] = xbar - quantile(t_star, probs = c(.95))*(sd(data_sample)/sqrt(n))
ci_results$boott_upper[j] = xbar - quantile(t_star, probs = c(.05))*(sd(data_sample)/sqrt(n))
}

ci_results %>%
  mutate(sim_id = 1:100) %>%
  pivot_longer(-sim_id) %>%
  separate(name, into = c("type", "bound")) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  ggplot(aes(xmin = lower, xmax = upper, y = sim_id, col = type)) +
  geom_errorbar() + 
  facet_wrap(vars(type)) +
  geom_vline(xintercept = 1/4) + 
  gghighlight::gghighlight(lower > 1/4 | upper < 1/4)

ggplot() + 
  geom_qq(aes(sample = t_star), distribution = qt, dparams = 168) + 
  geom_qq_line(aes(sample = t_star), distribution = qt, dparams = 168)
