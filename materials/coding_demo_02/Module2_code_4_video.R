# MAKE POPULATION SMALLER, RUN IN PROBLEM ON SERVER LATER ON (MODULE 08)


#### Code to create population values (commute time from UBC students)
library(tidyverse)

set.seed(92025)
n_pop <- 100000 
commute_t <- data.frame(time_m = rpois(n_pop, 55), # min
                        position = sample(c("student", "staff", "faculty"),
                                          n_pop, prob = c(70, 20, 10), replace = TRUE)) 

write.csv(commute_t, "commute_time.csv", row.names = FALSE)


#### Code to explain the worksheet activities
# Sampling with and without replacement, bootstrap distribution as an approximation of the sampling distribution

library(tidyverse)
library(infer)

# Getting student commute time in hours

commute_t <- read.csv("commute_time.csv")

head(commute_t)


commute_t_student <- commute_t %>% 
    filter(position == "student") %>%
    mutate(time_h = time_m/60) %>%
    select(time_h)
head(commute_t_student)


# Sampling distribution

set.seed(6756)

sampling_dist <- commute_t_student %>%
  rep_sample_n(size = 20, reps = 5000) %>%
  group_by(replicate) %>%
  summarize(sample_mean = mean(time_h)) %>%
  ggplot() +
  geom_histogram(aes(x = sample_mean)) +
  xlab("Sample mean of student commute time (hr)") +
  ylab("Count") +
  ggtitle("Sampling distribution")
sampling_dist






# Sampling
set.seed(5555)
sample_1 <- commute_t_student %>% 
    rep_sample_n(20) %>%
    ungroup() %>%
    select(time_h)
head(sample_1)



# Distribution of a bootstrap sample

set.seed(7877)
bootstrap_sample_1 <- sample_1 %>%
  rep_sample_n(size = 20, reps = 1, replace = TRUE) 
# Show effect of replace, by commenting of set.seed() and replacing TRUE
bootstrap_sample_dist <- bootstrap_sample_1 %>%
  ggplot() +
    geom_histogram(aes(x=time_h)) +
    xlab("Student commute time (hr)") +
    ylab("Count") +
    ggtitle("Distribution of bootstrap sample 1")
bootstrap_sample_dist





# Bootstrap distribution
resampled_means <- sample_1 %>%
  rep_sample_n(size = 20, reps = 5000, replace = TRUE) %>%
  group_by(replicate) %>%
  summarize(bootstrap_mean = mean(time_h))
head(resampled_means)

bootstrap_dist <- resampled_means %>%
  ggplot() +
  geom_histogram(aes(x = bootstrap_mean)) +
  xlab("Mean of student commute time (hr)") +
  ylab("Count") +
  ggtitle("Bootstrap distribution of the sample mean")

bootstrap_dist
sampling_dist



# Size of resample

bootstrap_dist_200 <- sample_1 %>%
  rep_sample_n(size = 200, reps = 5000, replace = TRUE) %>%
  summarize(bootstrap_mean = mean(time_h)) %>%
  ggplot() +
  geom_histogram(aes(x = bootstrap_mean)) +
  xlab("Mean of student commute time (hr)") +
  ylab("Count") +
  ggtitle("Bootstrap distribution of the sample mean (bigger resample size)")
bootstrap_dist_200 + scale_x_continuous(limits = c(0.8, 1.02))
bootstrap_dist + scale_x_continuous(limits = c(0.8, 1.02))
sampling_dist + scale_x_continuous(limits = c(0.8, 1.02))