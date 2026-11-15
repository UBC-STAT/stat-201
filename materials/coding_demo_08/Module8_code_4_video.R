#### Code to create population values (commute time from UBC students)
# MAKE POPULATION SMALLER!!! MAKES IT IMPOSSIBLE TO RUN THE POWER FUNCTION ON SERVER

library(tidyverse)

set.seed(92025)
n_pop <- 100000 
commute_t <- data.frame(time_m = rpois(n_pop, 55), # min
                        position = sample(c("student", "staff", "faculty"),
                                          n_pop, prob = c(70, 20, 10), replace = TRUE)) 

commute_t_student <- commute_t %>% 
  filter(position == "student") %>%
  mutate(time_h = time_m/60) %>%
  select(time_h)
head(commute_t_student)

write.csv(commute_t_student, "commute_time_student.csv", row.names = FALSE)


#### Code to explain the worksheet activities
# Null hypothesis testing

library(tidyverse)
library(infer)

# Getting student commute time in hours for student

commute_t_student <- read.csv("commute_time_student.csv")
head(commute_t_student)

## True population parameters
pop_mu <- commute_t_student %>% pull(time_h) %>% mean
pop_sd <- commute_t_student %>% pull(time_h) %>% sd
pop_mu
pop_sd


### Type I error
# Reject H_0 when H_0 is true


n <- 100 # Sample size
set.seed(123)


type_I <- commute_t_student %>%
  rep_sample_n(size = n, reps = 5000) %>%
  summarize(sample_mean = mean(time_h)) %>%
  # Assume we know pop sd
  mutate(test_statistic = sqrt(n) * (sample_mean - pop_mu) / pop_sd)  %>%
  crossing(tibble(alpha = c(0.01, 0.05, 0.1))) %>% 
  mutate(reject_H0 = abs(test_statistic) >= qnorm(1-alpha/2)) %>%
  group_by(alpha) %>%
  summarise(prop_rej = mean(reject_H0))

head(type_I)

# Change sample size, no big change (just a little closer to alpha as sample size increase thanks to CLT)

### Type II error
# Failing to reject H0 when HA is true
# Power is 1-type_II_rate (probability of rejecting H0 when it is false)
# H_0: mu = pop_mu + effect size (false since simulate with pop_mu)
# H_A: mu != pop_mu + effect size (true since simulate with pop_mu + effect size)

n <- 100 # Sample size
alpha_level <- 0.05 # Significance level
set.seed(123)


effect_sizes <- tibble(effect_size = c(0.01, 0.02, 0.03))
effect_sizes
type_II <- commute_t_student %>%
  rep_sample_n(size = n, reps = 5000) %>%
  summarise(sample_mean = mean(time_h)) %>%
  mutate(pop_mu = pop_mu,
         pop_sd = pop_sd) %>%
  crossing(effect_sizes) %>%
  mutate(mu0 = pop_mu + effect_size) %>%
  mutate(test_statistic = sqrt(n) * (sample_mean - mu0) / pop_sd) %>%
  mutate(reject_H0 = abs(test_statistic) >= qnorm(1-alpha_level/2)) %>%
  group_by(pop_mu, effect_size, mu0) %>%
  summarize(prop_rej = mean(reject_H0)) %>%
  mutate(type_II_rate = 1-prop_rej)
head(type_II)

# Mention increase in power with increase effect size
# Change sample size, see how power increase with increase sample size
# Change alpha level, see how power decrease with increase alpha

# SMALLER POPULATION, CANNOT RUN FOR BIG POPULATION
set.seed(123)
com_t_student <- commute_t_student %>% rep_sample_n(size = 10000) %>% ungroup %>% 
  select(time_h) 
#head(com_t_student)


### Power function
n <- 100 # Sample size
mu0 <- 1 # Hypothesized mean
alpha_level <- 0.05 # significance level
set.seed(123)


possible_mu <- tibble(mu = seq(0.95, 1.05, 0.01))
possible_mu

power_fx <- possible_mu %>%
  mutate(new_pop = list(commute_t_student)) %>% # Create on pop for each value of mu
  mutate(new_pop = map2(.x = mu, .y = new_pop, # Change the pop so that it's mean is mu
                        ~ .y %>% # ~ is short hand for function(.x, .y)
                          mutate(new_var = time_h  - pop_mu + .x))) %>%
  mutate(samples = map(.x = new_pop,
                       ~ .x %>% # ~ is short hand for function(.x) 
                         rep_sample_n(size = n, reps = 5000) %>%
                         summarize(sample_mean = mean(new_var)))) %>%
  select(-new_pop) %>%
  unnest(samples) %>%
  mutate(pop_sd = pop_sd) %>%
  mutate(p_value = 2 * pnorm(abs(sqrt(n) * (sample_mean - mu0)/pop_sd), lower.tail = FALSE)) %>%
  group_by(mu) %>%
  summarise(prop_rej = mean(p_value <= alpha_level))

head(power_fx)
#head(power_fx$new_pop[[1]])
#head(power_fx$samples[[1]])

power_fx_plot <- power_fx %>%
  ggplot() +
  geom_line(aes(x = mu, y = prop_rej)) +
  xlab(expression(mu)) +
  ylab(expression(paste("Probabilty of rejecting ", H[0]))) +
  ggtitle("Power function") +
  geom_vline(xintercept = mu0, color = "hotpink")

power_fx_plot 






