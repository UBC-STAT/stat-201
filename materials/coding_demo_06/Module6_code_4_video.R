


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

## In future should show the idea behind the permute

#### Code to explain the worksheet activities
# Null hypothesis testing

library(tidyverse)
library(infer)

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

sample_1_mean <- sample_1 %>% pull(time_h) %>% mean
sample_1_mean


sample_1_dist <- 
  sample_1 %>%
  ggplot() +
    geom_histogram(aes(x = time_h), binwidth = 0.05, 
                   fill = "hotpink", alpha = 0.3) +
    xlab("Commute time (hours)") +
    ylab("Count") +
    ggtitle("Sample distribution")
sample_1_dist



# Bootstrap dist
set.seed(7777)
boot_mean_dist <- sample_1 %>%
  specify(response = time_h) %>%
  generate(type = "bootstrap", reps = 5000) %>%
  calculate(stat = "mean")
head(boot_mean_dist)



boot_mean_plot <- boot_mean_dist %>%
  ggplot() +
  geom_histogram(aes(x = stat), binwidth = 0.01, fill = "hotpink", alpha = 0.3) +
  geom_vline(xintercept = sample_1_mean, color = "hotpink", lwd = 2) +
  xlab("Mean student commute time (hr)") +
  ylab("Count") +
  ggtitle("Bootstrap distribution")
  
boot_mean_plot

# Recentering the sample to hypothesize mean

sample_1 <- sample_1 %>%
  mutate(time_h_null = time_h - sample_1_mean + 1)
head(sample_1)

sample_1_dist_null <- 
  sample_1_dist + 
  geom_histogram(data = sample_1, aes(x = time_h_null), binwidth = 0.05, 
                 fill = "purple", alpha = 0.5) + 
  geom_vline(xintercept = sample_1_mean, color = "hotpink", alpha = 0.3, lwd = 2) +
  geom_vline(xintercept = 1, color = "purple", alpha = 0.3, lwd = 2)
sample_1_dist_null

# Bootstrapping the recentered sample
set.seed(7777)
null_model_dist <- sample_1 %>%
  specify(response = time_h_null) %>%
  generate(type = "bootstrap", reps = 5000) %>%
  calculate(stat = "mean")
head(null_model_dist)

boot_mean_null_plot <- boot_mean_plot +
  geom_histogram(data = null_model_dist, aes(x = stat), binwidth = 0.01, fill = "purple", alpha = 0.3) +
  geom_vline(xintercept = 1, color = "purple",lwd = 2)
boot_mean_null_plot

p_value <- mean(null_model_dist$stat <= sample_1_mean)
p_value


# Infer
set.seed(7777)
null_model_infer <- sample_1 %>%
  specify(response = time_h) %>%
  hypothesise(null = "point", mu = 1) %>%
  generate(reps = 5000, type = "bootstrap") %>%
  calculate(stat = "mean")
cbind(head(null_model_infer), head(null_model_dist))

null_model_vis_infer <- 
  null_model_infer %>% 
  visualize() + 
  shade_p_value(obs_stat = sample_1_mean, direction = "less") +
  xlab("Mean commute time (hours)")
null_model_vis_infer

p_value_infer <- 
  null_model_infer %>% 
  get_p_value(obs_stat = sample_1_mean, direction = "less")
p_value_infer

# Two samples - permutation test

commute_t <- read.csv("commute_time.csv")
head(commute_t)

# staff vs faculty
# Null have same commute time
# Alternative, faculty have longer commute time

set.seed(5656)
sample_2 <- commute_t %>%
  filter(position %in% c("staff", "faculty")) %>% 
  rep_sample_n(60)
head(sample_2)

obs_diff_mean <- sample_2 %>%
  group_by(position) %>% 
  summarise(mean = mean(time_m)) %>%
  pivot_wider(names_from = position, values_from = mean) %>%
  mutate(diff = faculty - staff, .keep = "none") %>%
  pull(diff)
obs_diff_mean

null_model_staff_faculty <- 
  sample_2 %>% 
  specify(formula = time_m ~ position) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 5000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("faculty", "staff"))    
head(null_model_staff_faculty)

staff_faculty_result_plot <- 
  null_model_staff_faculty %>%
  visualize() + 
  shade_p_value(obs_stat = obs_diff_mean, direction = "greater") +
  xlab("Difference in means")
staff_faculty_result_plot

p_value_diff <- 
  null_model_staff_faculty %>% 
  get_p_value(obs_stat = obs_diff_mean, direction = "greater")
p_value_diff

