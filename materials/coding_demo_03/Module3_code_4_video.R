


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
# Sampling with and without replacement, bootstrap distribution as an approximation of the sampling distribution

library(tidyverse)
library(infer)

# Getting student commute time in hours for student

commute_t_student <- read.csv("commute_time_student.csv")
head(commute_t_student)

pop_dist <- commute_t_student %>%
  ggplot() +
  geom_histogram(aes(x=time_h, y = after_stat(density)), binwidth = 0.05) +
  xlab("Commute time (hour)") +
  ylab("Density") +
  ggtitle("Population distribution")
pop_dist

# Do with and without after_stat(density)
# density of points in bin, scaled to integrate to 1.


# Normal distribution
pop_mu <- commute_t_student %>% pull(time_h) %>% mean
pop_sd <- commute_t_student %>% pull(time_h) %>% sd
pop_mu
pop_sd

data_normal_pop <- tibble(time_h = seq(min(commute_t_student$time_h), 
                                       max(commute_t_student$time_h), 0.01), 
                      density = dnorm(time_h, pop_mu, pop_sd))
head(data_normal_pop)

pop_dist_norm <- pop_dist +
  geom_line(data = data_normal_pop, 
            aes(x = time_h, y = density), color="hotpink", lwd = 2)
pop_dist_norm

# Different normals

normals <- ggplot() +
  geom_line(data = tibble(x = seq(-4, 4, by= 0.01),
                          density = dnorm(x, 0, 1)), 
            aes(x = x, y = density), color="hotpink", lwd=2) +
  geom_line(data = tibble(x = seq(-4, 4, by= 0.01),
                          density = dnorm(x, 2, 1)), 
            aes(x = x, y = density), color="blue", lwd=2) +
  geom_line(data = tibble(x = seq(-4, 4, by= 0.01),
                          density = dnorm(x, 0, 0.5)), 
            aes(x = x, y = density), color="purple", lwd=2)
normals


# Sampling distribution

set.seed(6756)

samples <- commute_t_student %>%
  rep_sample_n(size = 20, reps = 5000) %>%
  group_by(replicate) %>%
  summarize(sample_mean = mean(time_h))
head(samples)


sampling_dist <-  samples %>%
  ggplot() +
  geom_histogram(aes(x = sample_mean, y = after_stat(density))) +
  xlab("Sample mean of student commute time (hr)") +
  ylab("Density") +
  ggtitle("Sampling distribution")
sampling_dist

# Approximate the sampling distribution with a normal distribution

se <- pop_sd/sqrt(20)


sampling_dist_norm <- sampling_dist +
  geom_line(data = tibble(sample_mean = seq(min(samples$sample_mean), 
                                            max(samples$sample_mean), 0.001),
                          density = dnorm(sample_mean, pop_mu, se)),
            aes(x = sample_mean, y = density), color="hotpink", lwd=2)
sampling_dist_norm


