#### Code to explain the worksheet activities
# A/A and A/B testing

library(tidyverse)
library(infer)
library(gsDesign)

# Looking at two-sample t-test with simulation
# H0: mu_new = mu_baseline
# HA: mu_new > mu_baseline

n <- 100 # sample for each group
mean_current <- 50 # Population mean of baseline group
d_0 <- 5 # Effect size
sd_current <- 10 # Population SD of baseline group
sd_new <- 10 # Population SD of new group

set.seed(123)
sample_current <- rnorm(n, mean = mean_current, sd = sd_current)
sample_new <- rnorm(n, mean = mean_current + d_0, sd = sd_new)
t_test_results <- t.test(sample_new, sample_current,
                         alternative = "greater")                      



# Ways to decide whether to reject H_0
t_test_results$p.value
t_test_results$p.value <= 0.05
t_test_results$statistic
qt(1-0.05, 98)
t_test_results$statistic >= qt(1-0.05, 98)

# A/A 

# Two-sample t-test with tracking sequential statistic and p-values 
# by incremental sample sizes until getting to n in each group.

# @param n (numeric): Initially planned sample size for each group (for simplicity,
#                     n needs to be a multiple of sample_increase_step).
# @param d_0 (numeric): effect size.
# @param mean_current (numeric): Population mean for control variation.
# @param sd_current (numeric): Population standard deviation for current variation.
# @param sd_new (numeric): Population standard deviation for new variation.
# @param sample_increase_step (numeric): Sample size increment.

# @return p.value.df: A tibble that has 3 columns:
# inc_sample_size, statistic, and p_value 


incremental_t_test <- function(n, d_0, mean_current, sd_current, sd_new, sample_increase_step) {
  sample_current <- rnorm(n, mean = mean_current, sd = sd_current)
  sample_new <- rnorm(n, mean = mean_current + d_0, sd = sd_new)
  
  p.value.df <- tibble(
    inc_sample_size = rep(0, n / sample_increase_step),
    statistic = rep(0, n / sample_increase_step),
    p_value = rep(0, n / sample_increase_step)
  )
  
  current_sample_size <- sample_increase_step
  
  for (i in 1:nrow(p.value.df))
  {
    t_test_results <- t.test(sample_new[1:current_sample_size], sample_current[1:current_sample_size],
                             var.equal = TRUE,
                             alternative = "greater"                      
    )
    p.value.df[i, "statistic"] <- as_tibble(t_test_results$statistic)
    p.value.df[i, "p_value"] <- as_tibble(t_test_results$p.value)
    p.value.df[i, "inc_sample_size"] <- current_sample_size
    current_sample_size <- current_sample_size + sample_increase_step
  }
  
  return(p.value.df)
}

# Generate AA
set.seed(987)
AA <- 
  incremental_t_test(n = 100, 
                     d_0 = 0, 
                     sample_increase_step = 10, 
                     mean_current = 100, sd_current = 50, sd_new = 50)
AA

# Plot p-value as sample size increases
alpha_o <- 0.05
sequential_pvalue_plot <- 
  AA %>%
  ggplot() +
  geom_line(aes(x = inc_sample_size, y = p_value)) +
  geom_hline(yintercept = alpha_o, colour = "red") +  
  geom_text(x = 50, y = alpha_o + 0.01, label = "Unadjusted", color = "red") +
  ggtitle("Evolution of p-values in AA testing") +
  ylab("p-value") +
  xlab("Sample Size")

sequential_pvalue_plot

# Bonferroni
alpha_bonf <- alpha_o/10 

sequential_pvalue_plot + 
  geom_hline(yintercept = alpha_bonf, colour = "hotpink") +  
  geom_text(x = 50, y = alpha_bonf + 0.01, label = "Bonferroni", color = "hotpink")


# Critical value - Pocock & O'Brien-Fleming
# Use Norm,
design_pocock <- gsDesign(k = 10, #number of interim analysis planned
                          test.type = 1, # for one-sided tests
                          delta = 0, # default effect size
                          alpha = alpha_o, #type I error rate
                          beta = 0.2, # type II error rate
                          sfu = 'Pocock')
crit_pocock <- design_pocock$upper$bound
head(crit_pocock)
design_OF <- gsDesign(k = 10, #number of interim analysis planned
                          test.type = 1, # for one-sided tests
                          delta = 0, # default effect size
                          alpha = alpha_o, #type I error rate
                          beta = 0.2, # type II error rate
                          sfu = 'OF')
crit_OF <- design_OF$upper$bound
head(crit_OF)
crit_o <- qnorm(1 - alpha_o) # In worksheet use qnorm, but should be qt
crit_bonf <- qnorm(1 - alpha_bonf)  

### 
sequential_crit_plot <- 
  AA %>%
  ggplot() +
  geom_line(aes(x = inc_sample_size, y = statistic)) +
  geom_hline(yintercept = crit_o, colour = "red") +  
  geom_text(x = 20, y = crit_o + 0.25, label = "Unadjusted", color = "red") +
  geom_hline(yintercept = crit_bonf, colour = "hotpink") +  
  geom_text(x = 20, y = crit_bonf + 0.25, label = "Bonferroni", color = "hotpink") +
  geom_hline(aes(yintercept = crit_pocock[1]), colour = "blue") +  
  geom_text(x = 20, y = crit_pocock[1] + 0.25, label = "Pocock", color = "blue") +
  geom_line(aes(x = inc_sample_size, y = crit_OF), colour = "purple") +  
  geom_text(x = 20, y = crit_OF[2] + 0.25, label = "O'Brien-Fleming", color = "purple") +
  ggtitle("Evolution of critical value in AA testing") +
  ylab("Statistic") +
  xlab("Sample Size")
sequential_crit_plot

# Multiple sequential tests
set.seed(123)
mult_seq_test <- 
  tibble(experiment = 1:100) %>%
  mutate(seq_test = map(.x = experiment,
                        .f = function(x) incremental_t_test(n = 100, 
                                                            d_0 = 0, 
                                                            sample_increase_step = 10, 
                                                            mean_current = 100, 
                                                            sd_current = 50, sd_new = 50)))
mult_seq_test$seq_test[[1]]
mult_seq_test$seq_test[[2]]

type_I <- mult_seq_test %>% 
  mutate(reject_o = map_dbl(.x = seq_test, .f = function(x) sum(x$p_value <= 0.05) > 0)) %>% 
  summarise(n_rejections_o = sum(reject_o),
            expected_n_rejections = 5) 

type_I
# Modify the above to add Bonf, etc
type_I <- mult_seq_test %>% 
  mutate(reject_o = map_dbl(.x = seq_test, .f = function(x) sum(x$p_value <= 0.05) > 0),
         reject_bonf = map_dbl(.x = seq_test, .f = function(x) sum(x$p_value <= 0.05/10) > 0),
         reject_pocock = map_dbl(.x = seq_test, .f = function(x) sum(x$statistic >= crit_pocock) > 0),
         reject_OF = map_dbl(.x = seq_test, .f = function(x) sum(x$statistic >= crit_OF) > 0)) %>% 
  summarise(n_rejections_o = sum(reject_o),
            n_rejections_bonf = sum(reject_bonf),
            n_rejections_pocock = sum(reject_pocock),
            n_rejections_OF = sum(reject_OF),
            expected_n_rejections = 5) 
type_I


