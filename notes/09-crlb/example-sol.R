# **Example:** The median $m$ of an Exponential($\lambda$) distribution satisfies 
# P(X ≤ m) = 0.5. Solving $1 - e^{-\lambda m} = 0.5$ gives $m = ln(2) / \lambda$. 
# This suggests an estimator for $\lambda$ based on the median: 
#   $\widehat{\lambda} = \frac{ln(2)}{m}$. Finding the analytical variance of 
# $\widehat \lambda$ is complicated. Finding the sampling distribution of $m$ is 
# complicated, and finding the non-linear transformation is also complicated.
# 
# Use simulation to see whether $\frac{ln(2)}{m}$ (a) is unbiased and (b) achieves the CRLB


lambda <- 2
n <- 50
n_sims <- 10000
hat_lambda <- numeric(n_sims)

for(i in 1:n_sims){
  x <- rexp(n, rate = 2)
  hat_lambda[i] = log(2)/median(x)
}

mean(hat_lambda)

# CRLB tells us that: 
var(hat_lambda)
