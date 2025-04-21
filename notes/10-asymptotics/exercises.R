# recreating "running mean" from textbook

n <- 10000
running_mean <- numeric(n)
sample <- rcauchy(n)

for(i in 1:n){
  running_mean[i] <- mean(sample[1:i])
}

ggplot() + 
  geom_line(aes(x = 1:n, y = running_mean)) 

# recreating cauchy medians

n <- 1000
sample_var_norm <- numeric(n)
sample_var_cauchy <- numeric(n)

for(i in 1:n){
  samp1 <- rnorm(i)
  samp2 <- rcauchy(i)
  sample_var_norm[i] = var(samp1)
  sample_var_cauchy[i] = var(samp2)
}

ggplot() + 
  geom_line(aes(x = 1:n, y = sample_var_norm)) +
  ylim(c(0,3))

ggplot() + 
  geom_line(aes(x = 1:n, y = sample_var_cauchy)) 

