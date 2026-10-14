


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

## Also need to add section specific on zscore?

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


## Proportion of student that have 1 hr or more commuting
pop_p <- commute_t_student %>% mutate(long_com = time_h >=  1) %>% pull(long_com) %>% mean
pop_p

set.seed(10101)
sample_1 <- commute_t_student %>% rep_sample_n(50) %>%
  ungroup() %>%
  mutate(long_com = time_h >=  1) %>%
  select(time_h, long_com)
sample_1

# Bootstrap dist
set.seed(7654)
boot_p_dist <- sample_1 %>%
  rep_sample_n(50, 2000, replace = TRUE) %>%
  summarize(p = mean(long_com)) %>%
  select(p)
boot_p_dist
boot_p_ci <- boot_p_dist  %>%
  summarize(ci_lower = quantile(p, 0.025),
            ci_upper = quantile(p, 0.975))
boot_p_ci

boot_p_ci_plot <- boot_p_dist %>%
  ggplot() +
  geom_histogram(aes(x = p, y = after_stat(density)), binwidth = 0.02) +
  annotate("rect",
           xmin = boot_p_ci$ci_lower, xmax = boot_p_ci$ci_upper,
           ymin = 0, ymax = Inf, fill = "hotpink", alpha = 0.3) +
  xlab("Proportion of students with commute 1hr or greater") +
  ylab("Density") +
  ggtitle("Bootstrap 95% CI") +
  geom_vline(aes(xintercept = pop_p), color = "purple", lwd=1)
boot_p_ci_plot

# Normal approximation
phat <- sample_1 %>% summarize(p = mean(long_com)) %>% pull(p)
p_se <- sqrt(phat * (1-phat)/nrow(sample_1))

boot_p_ci_norm_plot <- boot_p_ci_plot + 
  geom_line(data = tibble(x = seq(0.07, 0.5, 0.005), 
                          density = dnorm(x, phat, p_se)), 
            aes(x = x, y = density), color = "skyblue", lwd = 2)
boot_p_ci_norm_plot

norm_p_ci <- tibble(
  ci_lower = qnorm(0.025, phat, p_se),
  ci_upper = qnorm(0.975, phat, p_se))
norm_p_ci
boot_p_ci

p_cis_plot <- boot_p_ci_norm_plot +
  annotate("rect",
           xmin = norm_p_ci$ci_lower, xmax = norm_p_ci$ci_upper,
           ymin = 0, ymax = Inf, fill = "skyblue", alpha = 0.3) +
  ggtitle("Bootstrap & Normal approx. 95% CIs")
p_cis_plot

## Mean commute time
pop_mu <- commute_t_student %>% pull(time_h) %>% mean
pop_sd <- commute_t_student %>% pull(time_h) %>% sd
pop_mu
pop_sd

# sample of size 10
n <- 10
set.seed(8888)

sample_2 <- commute_t_student %>% rep_sample_n(n) %>%
  ungroup() %>%
  select(time_h)
sample_2


# Bootstrap dist
set.seed(7777)
boot_mean_dist <- sample_2 %>%
  rep_sample_n(n, 2000, replace = TRUE) %>%
  summarize(boot_mean = mean(time_h)) %>%
  select(boot_mean)
boot_mean_dist
boot_mean_ci <- boot_mean_dist  %>%
  summarize(ci_lower = quantile(boot_mean, 0.025),
            ci_upper = quantile(boot_mean, 0.975))
boot_mean_ci

boot_mean_ci_plot <- boot_mean_dist %>%
  ggplot() +
  geom_histogram(aes(x = boot_mean, y = after_stat(density)), binwidth = 0.01) +
  annotate("rect",
           xmin = boot_mean_ci$ci_lower, xmax = boot_mean_ci$ci_upper,
           ymin = 0, ymax = Inf, fill = "hotpink", alpha = 0.3) +
  xlab("Mean student commute time (hr)") +
  ylab("Density") +
  ggtitle("Bootstrap 95% CI") +
  geom_vline(aes(xintercept = pop_mu), color = "purple", lwd=1)
boot_mean_ci_plot


xbar <- sample_2 %>% pull(time_h) %>% mean
s <- sample_2 %>% pull(time_h) %>% sd
s
pop_sd

# t-dist vs N(0,1)

ggplot() +
  geom_line(data = tibble(
    x = seq(-3,3,0.01),
    density = dnorm(x)),
    aes(x = x, y = density)) +
  geom_line(data = tibble(
    x = seq(-3,3,0.01),
    density = dt(x, df = 10-1)),
    aes(x = x, y = density), color = "hotpink")


# standardizing
mean_se <- s/sqrt(n)

boot_mean_dist <- boot_mean_dist %>%
  mutate(mean_std = (boot_mean - xbar)/mean_se)
boot_mean_dist

boot_mean_stand_ci_plot <- boot_mean_dist %>%
  ggplot() +
  geom_histogram(aes(x = mean_std, y = after_stat(density)), binwidth = 0.01/mean_se) +
  annotate("rect",
           xmin = (boot_mean_ci$ci_lower - xbar)/mean_se, xmax = (boot_mean_ci$ci_upper - xbar)/mean_se,
           ymin = 0, ymax = Inf, fill = "hotpink", alpha = 0.3) +
  xlab("Mean student commute time - standardized") +
  ylab("Density") +
  ggtitle("Bootstrap 95% CI") +
  geom_vline(aes(xintercept = (pop_mu - xbar)/mean_se), color = "purple", lwd=1)
boot_mean_stand_ci_plot

boot_mean_stand_ci_plot + 
  geom_line(data = tibble(
    x = seq(min(boot_mean_dist$mean_std), max(boot_mean_dist$mean_std),0.01),
    density = dnorm(x)),
    aes(x = x, y = density)) +
  geom_line(data = tibble(
    x = seq(min(boot_mean_dist$mean_std), max(boot_mean_dist$mean_std),0.01),
    density = dt(x, df = n-1)),
    aes(x = x, y = density), color = "hotpink") 
  
norm_mean_ci <- c(ci_lower = xbar + qnorm(0.025)*s/sqrt(n),
                  ci_upper = xbar + qnorm(0.975)*s/sqrt(n)) 
norm_mean_ci
qnorm(0.025)
-qnorm(0.975)
norm_mean_ci <- c(ci_lower = xbar - qnorm(0.975)*s/sqrt(n),
                  ci_upper = xbar + qnorm(0.975)*s/sqrt(n)) 
norm_mean_ci
norm_mean_ci <- c(ci_lower = xbar + qnorm(0.025)*s/sqrt(n),
                  ci_upper = xbar - qnorm(0.025)*s/sqrt(n)) 
norm_mean_ci


t_mean_ci <- c(ci_lower = xbar + qt(0.025, n-1)*s/sqrt(n),
               ci_upper = xbar + qt(0.975, n-1)*s/sqrt(n)) 
boot_mean_ci
t_mean_ci
norm_mean_ci

##### Extra

## With infer
set.seed(7654)
boot_p_dist <- sample_1 %>%
  specify(response = long_com, success = "TRUE") %>%
  generate(reps = 2000, type = "bootstrap") %>%
  calculate("prop")
boot_p_dist
boot_p_ci <- boot_p_dist  %>%
  get_ci(0.95)
boot_p_ci

boot_p_ci_plot <- boot_p_dist %>%
  ggplot() +
    geom_histogram(aes(x = stat, y = after_stat(density)), binwidth = 0.02) +
    shade_confidence_interval(boot_p_ci) +
    xlab("Proportion of students with commute 1hr or greater") +
    ylab("Density") +
    ggtitle("Bootstrap 95% CI")
boot_p_ci_plot



