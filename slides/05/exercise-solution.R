set.seed(0408)
x <- rbeta(5, shape1 = 3, shape2 = 1)













x <- c(.83, .49, .72, .57, .66)

theta_hat <- (5 + sum(log(x)))/-sum(log(x))

lik_func <- function(theta){(theta+1)^5 *prod(x^theta)}
optimize(lik_func, interval = c(-20,20), maximum = TRUE)

p3 <- tibble(
  theta = seq(0, 10, by = .01),
  log_lik = 5*log(theta+1) + theta*sum(log(x))
) |>
  ggplot(aes(x = theta, y = log_lik)) + 
  geom_line() 

ggplotly(p3)

