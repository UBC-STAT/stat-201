options(warn = -1)

generate_bootstrap_df <- function(seed) {
  set.seed(seed)
  sunset_pop %>%
    rep_sample_n(size = 24, reps = 1, replace = FALSE) %>%
    ungroup() %>%
    select(diameter) %>%
    rep_sample_n(size = 24, reps = 1500, replace = TRUE) %>%
    group_by(replicate) %>%
    summarize(diameter_mean = mean(diameter), .groups = "drop") %>%
    select(diameter_mean)
}
seeds <- c(5457, 4457, 5192, 4808, 0017, 4492, 8499, 0730, 8704, 2070)
bootstrap_dfs <- lapply(seeds, generate_bootstrap_df)
all_means <- unlist(bootstrap_dfs)
max_mean <- max(all_means)
min_mean <- min(all_means)

generate_bootstrap_plots <- function(df) {
  df %>%
    ggplot(aes(x = diameter_mean)) +
    geom_histogram(binwidth = 0.5) +
    geom_vline(xintercept = pop_mean, colour = "red") +
    labs(
      title = "n = 24",
      x = "Mean Diameter (cm)"
    ) +
    scale_x_continuous(limits = c(min_mean, max_mean))
}
plot_row <- plot_grid(plotlist = lapply(bootstrap_dfs, generate_bootstrap_plots), ncol = 5)

title <- ggdraw() +
  draw_label("Bootstrap Sampling Distributions (of Sample Means)",
    fontface = "bold",
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

bootstrap_dists <- plot_grid(title, plot_row, ncol = 1, rel_heights = c(0.1, 1))
