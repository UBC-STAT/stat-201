library(tidyverse)

set.seed(111)
ubc_sample <- tibble(full_years = rexp(n = 40, rate = 0.25) - 1) %>% 
  ceiling() %>% 
  filter(full_years >= 0, full_years <= 10)

sample_dist <- ubc_sample %>% 
  ggplot(aes(x = full_years)) + 
  geom_histogram(binwidth = 1, colour = "white") +
  ggtitle("Sample Distribution") +
  xlab("# Full Years At UBC") +
  scale_x_continuous(breaks = seq(0, 10, 1))