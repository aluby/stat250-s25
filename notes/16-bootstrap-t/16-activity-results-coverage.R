library(googlesheets4)
library(tidyverse)

sim_results <- read_sheet("https://docs.google.com/spreadsheets/d/1K3v67icpu-Q9vpYMcbCUsGJDJST2YPFGKtvgtmyIaQI/edit?usp=sharing")

table(sim_results$`interval type`, sim_results$`Outcome?`)

sim_results |>
  mutate(
      missed = (`lower bound (3 decimals)` > mean(flights$dep_delay, na.rm = TRUE)) | (`upper bound (3 decimals)` < mean(flights$dep_delay, na.rm = TRUE))
    ) |>
  ggplot(aes(xmin = `lower bound (3 decimals)`, 
             xmax = `upper bound (3 decimals)`, 
             y = `Your name/initials`,
             col = missed)) + 
  geom_errorbarh() + 
  facet_wrap(~`interval type`)
