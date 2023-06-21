# Packages

library(tidyverse)
library(metafor)
library(cowplot)
library(here)

# Big Picture

## Big Picture - Cor

x <- runif(10, 0, 0.5)
y <- 0 + x*0.8 + rnorm(10, sd = 0.2)

dat <- data.frame(x, y)

big_picture_cor1 <- dat |>
  filter(x > 0.15) |>
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 5, color = "firebrick3") +
  ggthemes::theme_pander(30) +
  theme(axis.text = element_blank()) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "#2980B9",
              linewidth = 1)

big_picture_cor2 <- dat |>
  filter(x > 0.15) |>
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 5, color = "blue") +
  ggthemes::theme_pander(30) +
  theme(axis.text = element_blank()) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "#2980B9",
              linewidth = 1)

## Big Picture - Forest

vi <- c(0.1, 0.15, 0.2)
yi <- c(-0.5, 1, 0)

dat <- data.frame(id = 1:length(yi), yi, vi)
fit <- rma(yi, vi)

big_picture_forest <- dat |>
  ggplot(aes(x = yi, y = factor(id))) +
  geom_point(size = 1/vi, shape = 15) +
  geom_segment(aes(x = yi - sqrt(vi)*2,
                   xend = yi + sqrt(vi)*2,
                   y = factor(id),
                   yend = factor(id))) +
  ggthemes::theme_par(15) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  xlim(c(-2,2)) +
  geom_vline(xintercept = 0, linetype = "dashed")

# Saving ------------------------------------------------------------------

ggsave(here("conferences", "sips2023", "poster", "img", "big_picture_cor1.svg"), big_picture_cor1, width = 3, height = 3)
ggsave(here("conferences", "sips2023", "poster", "img", "big_picture_cor2.svg"), big_picture_cor2, width = 3, height = 3)
ggsave(here("conferences", "sips2023", "poster", "img", "big_picture_forest.svg"), big_picture_forest, width = 5, height = 5)
