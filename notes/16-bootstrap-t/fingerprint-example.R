lp_results <- read_delim("https://www.fbi.gov/file-repository/laboratory/testresponses.txt/@@download/file/TestResponses.txt", delim = "\t")

set.seed(0501)
examiner_error_rates <- lp_results |>
  mutate(
    error = case_when(
      Compare_Value == "Exclusion" & Mating == "Mates" ~ 1,
      Compare_Value == "Individualization" & Mating == "Non-mates" ~ 1,
      is.na(Latent_Value) ~ NA,
      TRUE ~ 0
    )
  ) |>
  group_by(Examiner_ID) |>
  summarize(
    error_rate = mean(error)
  ) |>
  slice_sample(n=30)

ggplot(examiner_error_rates, aes(x = error_rate)) + 
  geom_histogram(bins = 20)

error_rates <- examiner_error_rates$error_rate
n = length(error_rates)
N = 10^4
boot_means = numeric(N)

## addl things for bootstrap t
xbar <- mean(error_rates, na.rm = TRUE)
boot_sds <- numeric(N)

for(i in 1:N){
  x <- sample(error_rates, size = n, replace = TRUE)
  boot_means[i] <- mean(x, na.rm = TRUE)
  boot_sds[i] <- sd(x, na.rm = TRUE)
}

t_star <- (boot_means - xbar)/(boot_sds/sqrt(n))

t.test(examiner_error_rates$error_rate, conf.level = .9)$conf.int
quantile(boot_means, probs = c(.05, .95))
xbar - quantile(t_star, probs = c(.05, .95))*(sd(error_rates)/sqrt(n))


## what if we only care about a one-sided confidence interval? 

t.test(examiner_error_rates$error_rate, alternative = "less")$conf.int
quantile(boot_means, probs = c(.95))
xbar - quantile(t_star, probs = c(.10, 1))*(sd(error_rates)/sqrt(n))
