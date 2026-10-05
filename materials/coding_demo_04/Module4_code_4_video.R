


#### Code to create population values (commute time from UBC students)
library(tidyverse)

set.seed(92025)
n_pop <- 100000 
commute_t <- data.frame(time_m = rpois(n_pop, 55), # min
                        position = sample(c("student", "staff", "faculty"),
                                          n_pop, prob = c(70, 20, 10), replace = TRUE)) 

head(commute_t)


commute_t_student <- commute_t %>% 
  filter(position == "student") %>%
  mutate(time_h = time_m/60) %>%
  select(time_h)
head(commute_t_student)

write.csv(commute_t_student, "commute_time_student.csv", row.names = FALSE)


#### Code to explain the worksheet activities
# Quantile and confidence intervals

library(tidyverse)
library(infer)

# Getting student commute time in hours for student

commute_t_student <- read.csv("commute_time_student.csv")
head(commute_t_student)

pop_dist <- commute_t_student %>%
  ggplot() +
  geom_histogram(aes(x=time_h), binwidth = 0.05) +
  xlab("Commute time (hour)") +
  ylab("Count") +
  ggtitle("Population distribution")
pop_dist

pop_mu <- commute_t_student %>% pull(time_h) %>% mean
pop_mu

## Bootstrap distribution

set.seed(1234)

# Sample the population
sample_1 <-  commute_t_student %>%
  rep_sample_n(size = 20, reps = 1, replace = TRUE)  %>%
  ungroup() %>%
  select(time_h) 

# Re-sample the sample to create bootstrap distribution
set.seed(5555)
resampled_means <- sample_1 %>%
  rep_sample_n(size = 20, reps = 5000, replace = TRUE) %>%
  group_by(replicate) %>%
  summarize(bootstrap_mean = mean(time_h))

# Plot bootstrap distribution
bootstrap_dist <- resampled_means %>%
  ggplot() +
    geom_histogram(aes(x = bootstrap_mean)) +
    xlab("Mean of student commute time (hr)") +
    ylab("Count") +
    ggtitle("Bootstrap distribution of the sample mean") +
    geom_vline(xintercept = pop_mu, colour = "hotpink", lwd = 2)

bootstrap_dist

# Show with rerunning just bootstrap vs sample

## Quantile
# The p^th quantile is the value of the observation data set where a proportion of p fall below it, and (1-p) fall above it.
# Median is the 0.5 quantile (sometime called the 50th percentile)

quantile_0.5 <- resampled_means %>%
  pull(bootstrap_mean) %>%
  quantile(0.5)
quantile_0.5

# Check: How many resampled_means are below
p_below <- resampled_means %>%
  summarise(prop = mean(bootstrap_mean < quantile_0.5)) %>%
  pull(prop)
p_below

bootstrap_dist + 
  geom_vline(xintercept = quantile_0.5, colour = "orange", lwd = 2)


## CI
# think of a confidence interval as a range of plausible values for the population parameter
# 80% CI

ci <- resampled_means %>%
  summarise(ci_lower = quantile(bootstrap_mean, 0.1),
            ci_upper = quantile(bootstrap_mean, 0.9))
head(ci)

bootstrap_dist + 
  annotate("rect", xmin = ci$ci_lower, xmax = ci$ci_upper, ymin = 0, ymax = Inf,
           fill = "cyan",
           alpha = 0.3)


p_in_ci<- resampled_means %>%
  summarise(prop = mean(bootstrap_mean >= ci$ci_lower &
                          bootstrap_mean <= ci$ci_upper )) %>%
  pull(prop)
p_in_ci

## CI via infer
# Re-sample the sample to create bootstrap distribution
set.seed(5555)
resampled_means_infer <- sample_1 %>%
  specify(response = time_h) %>% 
  generate(type = "bootstrap", reps = 5000) %>% 
  calculate(stat = "mean")


ci_infer <- resampled_means_infer %>%
  get_ci(level = 0.8, type = "percentile")
ci
ci_infer

## Meaning of CI

set.seed(6655)
many_samples <- commute_t_student %>%
  rep_sample_n(size = 20, reps = 100, replace = FALSE) %>%
  nest()


bootstrap_means <- many_samples %>%
  ungroup() %>% 
  rename(sample = data) %>%
  rename(sample_id = replicate) %>% 
  mutate(bootstrap_samples = map(sample, 
                                   function(sample) {
                                     sample %>% 
                                       rep_sample_n(reps = 1000, size = nrow(sample), replace = TRUE) %>% 
                                       ungroup()
                                   })) %>%
  mutate(bootstrap_means = map(bootstrap_samples, 
                               function(bootstrap_sample) {
                                 bootstrap_sample %>%
                                   group_by(replicate) %>%
                                   summarize(bootstrap_mean = mean(time_h), .groups = "drop")
                               })) %>% 
  select(sample_id, bootstrap_means) %>% 
  unnest(bootstrap_means)

head(bootstrap_means)
tail(bootstrap_means)

# 80% CI interval for each samples
intervals <-
  bootstrap_means %>%
  group_by(sample_id) %>%
  summarise(ci_lower = quantile(bootstrap_mean, 0.1),
            ci_upper = quantile(bootstrap_mean, 0.9))
head(intervals)

intervals_captured <- intervals %>%
  mutate(captured = (ci_lower <= pop_mu & pop_mu <= ci_upper))
head(intervals_captured)
pop_mu

# CI plot
many_ci_plot <- 
  intervals_captured %>%
  ggplot() +
  scale_colour_manual(breaks = c("TRUE", "FALSE"), # Change colour scale for better visibility.
                      values = c("blue", "black")) +
  geom_segment(aes(x = ci_lower,
                   xend = ci_upper,
                   y = sample_id,
                   yend = sample_id,
                   colour = captured)) +
  geom_vline(xintercept = pop_mu, colour = "hotpink", lwd = 2) +
  labs(title = "100 80% Confidence Intervals",
       y = "Sample ID",
       x = "Commute time (hour)",
       colour = "Captured?") 
many_ci_plot



