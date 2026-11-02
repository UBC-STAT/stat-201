


#### Code to create population values (commute time from UBC students)
library(tidyverse)

set.seed(92025)
n_pop <- 100000 
commute_t <- data.frame(time_m = rpois(n_pop, 55), # min
                        position = sample(c("student", "staff", "faculty"),
                                          n_pop, prob = c(70, 20, 10), replace = TRUE)) 

head(commute_t)

write.csv(commute_t, "commute_time.csv", row.names = FALSE)

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
library(broom)

# Getting student commute time in hours for student

commute_t_student <- read.csv("commute_time_student.csv")
head(commute_t_student)

# The mean student time is less than 1 hr
# Null mean student time is 1 hr
# Alternative, less than 1 hr

## Mean commute time
pop_mu <- commute_t_student %>% pull(time_h) %>% mean
pop_mu
# Yes, the mean student time is less than 1 hr

# Take a sample
set.seed(101)
sample_1 <- commute_t_student %>% rep_sample_n(30) %>%
  ungroup() %>%
  select(time_h)
head(sample_1)



sample_1_dist <- 
  sample_1 %>%
  ggplot() +
    geom_histogram(aes(x = time_h), binwidth = 0.1) +
    xlab("Commute time (hours)") +
    ylab("Count") +
    ggtitle("Sample distribution")
sample_1_dist


sample_1_mean <- sample_1 %>% pull(time_h) %>% mean
sample_1_mean

sample_1_sd <- sample_1 %>% pull(time_h) %>% sd
n_1 <- nrow(sample_1)
mean_se <- sample_1_sd/sqrt(n_1)

# T-approx of sampling distribution
t_sampling_dist <- ggplot() +
  geom_line(data = tibble(
    x = seq(-3, 5.5, 0.01),
    density = dt(x, n_1 -1)),
    aes(x = sample_1_mean + x*mean_se, y = density), 
    color = "hotpink") +
  geom_vline(xintercept = sample_1_mean, 
             color = "hotpink") +
  xlab("Mean student commute time (hr)") +
  ylab("Density") +
  ggtitle("Approx. sampling distribution")
t_sampling_dist

# Null model 
t_sampling_dist_null <- t_sampling_dist +
  geom_line(data = tibble(
    x = seq(-5.5, 3, 0.01),
    density = dt(x, n_1 -1)),
    aes(x = 1 + x*mean_se, y = density), 
    color = "purple") +
  geom_vline(xintercept = 1, 
             color = "purple") +
  xlab("Mean student commute time (hr)") +
  ylab("Density") +
  ggtitle("Approx. sampling distribution")
t_sampling_dist_null


# Test statistic
test_stat_1 <- (sample_1_mean - 1)/(sample_1_sd/sqrt(n_1))
test_stat_1

# Critical value alpha 0.05
crit_val_mean <- qt(0.05, n_1-1)
crit_val_mean

# p-value
p_value_1 <- pt(test_stat_1, n_1 - 1)
p_value_1

# Null model t-test


t_null <- tibble(t = seq(-4, 4, 0.01),
                 density = dt(t, n_1 -1)) %>%
  ggplot() +
  geom_line(aes(x = t, y = density), 
    color = "purple") +
  geom_vline(xintercept = crit_val_mean, 
             color = "red") +
  geom_ribbon(. %>% filter(t <= crit_val_mean),
              mapping = aes(x = t, ymax = density, ymin = 0), 
              alpha = 0.5, fill = "red") +
  geom_vline(xintercept = test_stat_1, 
             color = "blue") +
  geom_ribbon(. %>% filter(t <= test_stat_1),
              mapping = aes(x = t, ymax = density, ymin = 0), 
              alpha = 0.5, fill = "blue") +

  xlab("Test statistic value (t)") +
  ylab("Density") +
  ggtitle("Null model, critical value, p-value")
t_null


# t-test function 

?t.test
t_test_1 <- tidy(t.test(sample_1$time_h, mu = 1, 
                        alternative = "less"))
t_test_1


# Mention two sample tests

# Proportion, proportion of student with commute 1hr or longer
# more than 20%  of student take 1 or more hour
?prop.test
prop_test_1 <- tidy(
  prop.test(x = sum(sample_1$time_h >= 1),
            n = nrow(sample_1),
            p = 0.2,
            alternative = "greater", 
            conf.level = 0.95,
            correct = FALSE)
)
prop_test_1
