#### Code to create population values (commute time from UBC students)
# SMALLER POPULATION COMPARE TO OTHER MODULE - UPDATE OTHER MODULES
# ALSO DIFFERENT POPULATION

library(tidyverse)
library(infer)

set.seed(92025)
n_student <- 5000*0.7
n_staff <- 5000*0.2
n_faculty <- 5000*0.1
commute_t <- data.frame(time_m = c(rpois(n_student, 55),
                                   rpois(n_staff, 63),
                                   rpois(n_faculty, 55.5)), # min
                        position = c(rep("student", n_student),
                                     rep("staff", n_staff),
                                     rep("faculty", n_faculty))) %>%
  # shuffle, so can see staff, etc in head
  rep_sample_n(5000, replace = FALSE) %>%
  ungroup() %>%
  select(-replicate)

head(commute_t)
write.csv(commute_t, "commute_time.csv", row.names = FALSE)

commute_t_student <- commute_t %>% 
  filter(position == "student") %>%
  mutate(time_h = time_m/60) %>%
  select(time_h)
head(commute_t_student)

write.csv(commute_t_student, "commute_time_student.csv", row.names = FALSE)


#### Code to explain the worksheet activities
# ANOVA + p-value correction + predictions

library(tidyverse)
library(infer)
library(broom)

# Getting commute time in min for all

commute_t <- read.csv("commute_time.csv")
head(commute_t)


# Side-by-side boxplots
pop_boxplot <- commute_t %>% 
  ggplot(aes(x = position, y = time_m)) +
    geom_boxplot(fill = "purple") +
    geom_jitter(width = 0.2, alpha = 0.1) +
    xlab("Position") +
    ylab("Time (min)")
pop_boxplot

# Population means
pop_means <- commute_t %>%
  group_by(position) %>%
  summarize(mean = mean(time_m))
pop_means

# Take a sample
set.seed(123)
sample_1 <- commute_t %>%
      rep_sample_n(100) %>%
      ungroup() %>%
      select(-replicate)
head(sample_1)

# Sample boxplots
sample_boxplot <- sample_1 %>% 
  ggplot(aes(x = position, y = time_m)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.1) +
  xlab("Position") +
  ylab("Time (min)")
sample_boxplot

# Look at means and variance
sample_means_var <- sample_1 %>%
  group_by(position) %>%
  summarise(mean = mean(time_m), 
            var = var(time_m))
sample_means_var

# Is the biggest multiple times larger than the smallest? No, so we can use ANOVA

# ANOVA
# H0: mu_1 = mu_2 = mu_3
# HA: at least one mean is different from the others

anova_res <-
  aov(time_m ~ position, data = sample_1) %>%
  tidy()
anova_res

# multiple tests
comp_fac_sta <- 
  t.test(time_m ~ position,
         data = sample_1 %>% filter(position %in% c("faculty", "staff"))) %>%
  tidy()
comp_fac_sta
comp_fac_stu <- 
  t.test(time_m ~ position,
         data = sample_1 %>% filter(position %in% c("faculty", "student"))) %>%
  tidy()
comp_fac_stu
comp_sta_stu <- 
  t.test(time_m ~ position,
         data = sample_1 %>% filter(position %in% c("staff", "student"))) %>%
  tidy()
comp_sta_stu

comp_all <- tibble(
  comparison = c("faculty_staff",
                 "faculty_student",
                 "staff_student"),
  p_value = c(comp_fac_sta$p.value,
             comp_fac_stu$p.value,
             comp_sta_stu$p.value)
)
comp_all

# Bonferonni
comp_all <- comp_all %>%
  mutate(p_val_bonf = p.adjust(p_value, method = "bonferroni"))
comp_all

# Benjamin-Hochberg
comp_all <- comp_all %>%
  mutate(p_val_BH = p.adjust(p_value, method = "BH"))
comp_all

## Prediction and prediction intervals

commute_t_student <- read.csv("commute_time_student.csv")
head(commute_t_student)

# Prediction - using population
# Could be mean or median
pred <- commute_t_student %>% pull() %>% mean
pred

# Distribution
pop_dist <- commute_t_student %>%
  ggplot() +
  geom_histogram(aes(x = time_h), binwidth = 0.05) +
  xlab("Commute time (hour)") +
  ylab("Count")
pop_dist


# pi using population

student_pop_pi <- commute_t_student %>%
  reframe(pi = quantile(time_h, c(0.025, 0.975))) %>%
  pull(pi)
student_pop_pi

# sample
set.seed(44)
sample_2 <- commute_t_student %>%
  rep_sample_n(100) %>%
  ungroup() %>%
  select(-replicate)
sample_2




# 95% Prediction interval (using sampl*e* distribution not sampl*ing* distribution)
# reframe is like summarize but allow for the function to return multiple rows
student_samp_pi <- sample_2 %>%
  reframe(pi = quantile(time_h, c(0.025, 0.975))) %>%
  pull(pi)
student_samp_pi


# Coverage
# Here, the probability that the prediction interval based on sample covers the true prediction interval
set.seed(189)
sample_pi <- 
  commute_t_student %>% 
  rep_sample_n(100, replace = FALSE, reps = 1000) %>% 
  summarise(lower = quantile(time_h, 0.025),
            upper = quantile(time_h, 0.975))
head(sample_pi)

time_h_vec <- commute_t_student$time_h
head(time_h_vec)

coverage <- 
  sample_pi %>% 
  group_by(replicate) %>% 
  summarise(coverage = mean(
    between(time_h_vec, lower, upper)
  )) %>% 
  pull(coverage)
head(coverage)

ggplot(data.frame(coverage)) +
  geom_histogram(aes(x = coverage), bins = 30) +
  xlab("Coverage probability") +
  ylab("Count") +
  geom_vline(xintercept = 0.95)
