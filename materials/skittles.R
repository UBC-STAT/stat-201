library(tidyverse)
library(ggplot2)
require(gridExtra)

skittles_a <- tibble(red_skittles = c(5, 1, 4, 4, 4, 3, 3, 2, 2, 3))
skittles_b <- tibble(red_skittles = c(5, 1, 4, 2, 4, 3, 3, 2, 2, 3, 5, 2, 3, 4, 1))
skittles_c <- tibble(red_skittles = c(4, 0, 3, 1, 3, 2, 2, 1, 1, 2))
skittles_d <- tibble(red_skittles = c(5, 1, 4, 2, 4, 3, 3, 2, 2, 3))

plot_a <- skittles_a %>% 
  ggplot(aes(x = red_skittles)) +
  geom_histogram(binwidth = 1, colour = "white") +
  scale_x_continuous(limits = c(-0.5, 5.5),
                     breaks = seq(0, 5, 1)) +
  scale_y_continuous(limits = c(0, 4),
                     breaks = seq(0, 4, 1)) +
  xlab("# Red Skittles") +
  ylab("Frequency") +
  ggtitle("A") +
  theme(plot.title = element_text(face = "bold", size=22))

plot_b <- skittles_b %>% 
  ggplot(aes(x = red_skittles)) +
  geom_histogram(binwidth = 1, colour = "white") +
  scale_x_continuous(limits = c(-0.5, 5.5),
                     breaks = seq(0, 5, 1)) +
  scale_y_continuous(limits = c(0, 4),
                     breaks = seq(0, 4, 1)) +
  xlab("# Red Skittles") +
  ylab("Frequency") +
  ggtitle("B") +
  theme(plot.title = element_text(face = "bold", size=22))

plot_c <- skittles_c %>% 
  ggplot(aes(x = red_skittles)) +
  geom_histogram(binwidth = 1, colour = "white") +
  scale_x_continuous(limits = c(-0.5, 5.5),
                     breaks = seq(0, 5, 1)) +
  scale_y_continuous(limits = c(0, 4),
                     breaks = seq(0, 4, 1)) +
  xlab("# Red Skittles") +
  ylab("Frequency") +
  ggtitle("C") +
  theme(plot.title = element_text(face = "bold", size=22))

plot_d <- skittles_d %>% 
  ggplot(aes(x = red_skittles)) +
  geom_histogram(binwidth = 1, colour = "white") + 
  scale_x_continuous(limits = c(-0.5, 5.5),
                     breaks = seq(0, 5, 1)) +
  scale_y_continuous(limits = c(0, 4),
                     breaks = seq(0, 4, 1)) +
  xlab("# Red Skittles") +
  ylab("Frequency") + 
  ggtitle("D") + 
  theme(plot.title = element_text(face = "bold", size=22))

gridExtra::grid.arrange(plot_a, plot_b, plot_c, plot_d)

