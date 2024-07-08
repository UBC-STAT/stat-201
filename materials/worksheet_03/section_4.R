library(cowplot)
library(datateachr)
library(digest)
library(gridExtra)
library(infer)
library(repr)
library(taxyvr)
library(tidyverse)

options(warn=-1)

# =========================== SECTION 3 =========================== #
set.seed(111)
ubc_sample <- tibble(full_years = rexp(n = 40, rate = 0.25) - 1) %>% 
  ceiling() %>% 
  filter(full_years >= 0, full_years <= 10)

sample_dist <- ubc_sample %>% 
  ggplot(aes(x = full_years)) + 
  geom_histogram(binwidth = 1, colour = "white") +
  ggtitle("Sample Distribution") +
  xlab("# Full Years At UBC") +
  scale_x_continuous(breaks = seq(0, 10, 1))

# =========================== SECTION 4 =========================== #
set.seed(9869)
sampling_dist_10 <- multi_family_strata %>% 
  rep_sample_n(size = 10, reps = 2000) %>% 
  group_by(replicate) %>% 
  summarise(mean_land_value = mean(current_land_value)) %>% 
  ggplot(aes(x = mean_land_value)) +
  geom_histogram(binwidth = 15000) +
  xlab("Mean Land Value (CAD)") +
  ggtitle("n = 10")

set.seed(7032)
sampling_dist_30 <- multi_family_strata %>% 
  rep_sample_n(size = 30, reps = 2000) %>% 
  group_by(replicate) %>% 
  summarise(mean_land_value = mean(current_land_value)) %>% 
  ggplot(aes(x = mean_land_value)) +
  geom_histogram(binwidth = 15000) +
  xlab("Mean Land Value (CAD)") +
  ggtitle("n = 30")

set.seed(8408)
sampling_dist_100 <- multi_family_strata %>% 
  rep_sample_n(size = 100, reps = 2000) %>% 
  group_by(replicate) %>% 
  summarise(mean_land_value = mean(current_land_value)) %>% 
  ggplot(aes(x = mean_land_value)) +
  geom_histogram(binwidth = 15000) +
  xlab("Mean Land Value (CAD)") +
  ggtitle("n = 100")

bootstrap_10_mean <- round(mean(bootstrap_dist_10$data$mean_land_value), 2)
bootstrap_30_mean <- round(mean(bootstrap_dist_30$data$mean_land_value), 2)
bootstrap_100_mean <- round(mean(bootstrap_dist_100$data$mean_land_value), 2)
sampling_10_mean <- round(mean(sampling_dist_10$data$mean_land_value), 2)
sampling_30_mean <- round(mean(sampling_dist_30$data$mean_land_value), 2)
sampling_100_mean <- round(mean(sampling_dist_100$data$mean_land_value), 2)

bootstrap_dist_row <- plot_grid(bootstrap_dist_10 +
                                  theme(axis.text.x = element_text(angle = 90)) +
                                  scale_x_continuous(breaks = seq(400000, 1200000, 200000),
                                                     limits = c(400000, 1200000)) +
                                  geom_vline(xintercept = bootstrap_10_mean, colour = "red"),
                                bootstrap_dist_30 +
                                  theme(axis.text.x = element_text(angle = 90)) +
                                  scale_x_continuous(breaks = seq(400000, 1200000, 200000),
                                                     limits = c(400000, 1200000)) +
                                  geom_vline(xintercept = bootstrap_30_mean, colour = "red"),
                                bootstrap_dist_100 +
                                  theme(axis.text.x = element_text(angle = 90)) +
                                  scale_x_continuous(breaks = seq(400000, 1200000, 200000),
                                                     limits = c(400000, 1200000)) +
                                  geom_vline(xintercept = bootstrap_100_mean, colour = "red"),
                                ncol = 3)
title <- ggdraw() + 
  draw_label("Bootstrap Sampling Distributions (of Sample Means)",
             fontface = 'bold',
             x = 0,
             hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))
bootstrap_dist_grid <- plot_grid(title,
                                 bootstrap_dist_row,
                                 ncol = 1,
                                 rel_heights = c(0.1, 1))

sampling_dist_row <- plot_grid(sampling_dist_10 +
                                 theme(axis.text.x = element_text(angle = 90)) +
                                 scale_x_continuous(breaks = seq(400000, 1200000, 200000),
                                                    limits = c(400000, 1200000)) +
                                 geom_vline(xintercept = sampling_10_mean, colour = "red"),
                               sampling_dist_30 +
                                 theme(axis.text.x = element_text(angle = 90)) +
                                 scale_x_continuous(breaks = seq(400000, 1200000, 200000),
                                                    limits = c(400000, 1200000)) +
                                 geom_vline(xintercept = sampling_30_mean, colour = "red"),
                               sampling_dist_100 +
                                 theme(axis.text.x = element_text(angle = 90)) +
                                 scale_x_continuous(breaks = seq(400000, 1200000, 200000),
                                                    limits = c(400000, 1200000)) +
                                 geom_vline(xintercept = sampling_100_mean, colour = "red"),
                               ncol = 3)
title <- ggdraw() + 
  draw_label("Sampling Distributions (of Sample Means)",
             fontface = 'bold',
             x = 0,
             hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

sampling_dist_grid <- plot_grid(title,
                                sampling_dist_row,
                                ncol = 1,
                                rel_heights = c(0.1, 1))

all_dist_grid <- plot_grid(bootstrap_dist_grid, sampling_dist_grid, ncol = 1)


