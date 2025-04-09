set.seed(0408)
x <- rbeta(5, shape1 = 3, shape2 = 1)

p3 <- tibble(
  theta = seq(0, 10, by = .01),
  log_lik = 5*log(theta+1) + theta*sum(log(x))
) |>
  ggplot(aes(x = theta, y = log_lik)) + 
  geom_line() 

ggplotly(p3)