# MAKE POPULATION SMALLER, RUN IN PROBLEM ON SERVER LATER ON (MODULE 08)
# SEE MODULES 08 & 09 for new simulation to use


#### Code to create population values (commute time from UBC students)
set.seed(92025)
n_pop <- 100000 
commute_t <- data.frame(time_m = rpois(n_pop, 55), # min
                        position = sample(c("student", "staff", "faculty"),
                                          n_pop, prob = c(70, 20, 10), replace = TRUE)) 

write.csv(commute_t, "commute_time.csv", row.names = FALSE)

head(commute_t)
commute_t %>% ggplot() +
  geom_histogram(aes(time_m))


#### Code to explain the worksheet activities
# Sampling, sampling distribution, ggplot, subset

library(tidyverse)
library(infer)

# Getting student commute time in hours

commute_t <- read.csv("commute_time.csv")

head(commute_t)
head(commute_t, 20)

commute_t_student <- commute_t %>% filter(position == "student")
commute_t_student %>% pull(position) %>% unique()
head(commute_t_student)

commute_t_student <- commute_t_student %>% 
  mutate(time_h = time_m/60) %>%
  select(time_h)
head(commute_t_student)
nrow(commute_t_student)

# Population mean

pop_dist <- commute_t_student %>%
  ggplot() +
    geom_histogram(aes(x= time_h)) +
    xlab("Student communte time (hr)") +
    ylab("Count") +
    ggtitle("Population distribution")

pop_dist

pop_mean <- commute_t_student %>% pull() %>% mean()
pop_mean

# Sampling

n_sample <- 200
set.seed(5555)
sample_1 <- commute_t_student %>% rep_sample_n(n_sample)
head(sample_1)
sample_1_dist <- sample_1 %>% 
  ggplot() +
    geom_histogram(aes(x=time_h)) +
    xlab("Student communte time (hr)") +
    ylab("Count") +
    ggtitle("Distribution of sample 1")
sample_1_dist

sample_1_mean <- sample_1 %>% pull(time_h) %>% mean
sample_1_mean

# Sampling distribution
n_rep <- 5000
set.seed(6756)
samples <- commute_t_student %>%
  rep_sample_n(size = n_sample, reps = n_rep)
head(samples, 20)
tail(samples, 20)

samples_means <- samples %>%
  group_by(replicate) %>%
  summarize(sample_mean = mean(time_h))
head(samples_means)

sampling_dist <- samples_means %>%
  ggplot() +
    geom_histogram(aes(x=sample_mean)) +
    xlab("Sample mean of student commute time (hr)") +
    ylab("Count") +
    ggtitle("Sampling distribution")
sampling_dist
pop_dist

