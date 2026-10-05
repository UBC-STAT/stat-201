library(datateachr)
library(infer)
library(repr)
library(tidyverse)

set.seed(0756)

# This script is used to generate the data frames used in section 3.

sunset_pop <- vancouver_trees %>% 
  filter(neighbourhood_name == "SUNSET") %>% 
  select(diameter) %>% 
  mutate(diameter = diameter * 2.54)

generate_bootstrap_samples <- function(sample) {
  sample %>% 
    rep_sample_n(reps = 1000, size = nrow(sample), replace = TRUE) %>% 
    ungroup()
}

compute_bootstrap_means <- function(bootstrap_sample) {
  bootstrap_sample %>%
    group_by(replicate) %>%
    summarize(bootstrap_mean = mean(diameter), .groups = "drop")
}

sunset_bootstrap_means <- sunset_pop %>%
  rep_sample_n(size = 30, reps = 100, replace = FALSE) %>%
  nest() %>%
  ungroup() %>% 
  rename(sample = data) %>%
  rename(sample_id = replicate) %>% 
  mutate(bootstrap_samples = map(sample, generate_bootstrap_samples)) %>%
  mutate(bootstrap_means = map(bootstrap_samples, compute_bootstrap_means))

bootstrap_samples <- sunset_bootstrap_means %>% 
  select(sample_id, bootstrap_samples) %>% 
  unnest(bootstrap_samples) %>% 
  filter(sample_id == 1)
write.csv(bootstrap_samples, "bootstrap_samples.csv", row.names = FALSE)

sampling_dist_estimates <- sunset_bootstrap_means %>% 
  select(sample_id, bootstrap_means) %>% 
  unnest(bootstrap_means)
write.csv(sampling_dist_estimates, "sampling_dist_estimates.csv", row.names = FALSE)