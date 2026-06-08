library(tidyverse)

set.seed(123)

# simulate data for different weights
dat <- data.frame(
  weighting = seq(0, 1, length.out = 30)
)
# generate quadratic relationship
dat$corr <- 0.15 +
  0.5 * dat$weighting -
  0.35 * dat$weighting^2 +
  rnorm(30, 0, 0.03)

# visualize graph
ggplot(data=dat, mapping = aes(x = weighting, y = corr)) +
  geom_point() +
  theme_minimal()

# fit quadtatic model
model <- lm(
  corr ~ weighting + I(weighting^2),
  data = dat
)

# look at results
summary(model)
coef(model)[c("weighting", "I(weighting^2)")]
unname(coef(model)[c("weighting", "I(weighting^2)")])

# prediction dataframe
pred_dat <- tibble(
  weighting = seq(0, 1, length.out = 200)
) |>
  mutate(
    pred = predict(model, newdata = pick(everything()))
  )

# plot
ggplot(dat, aes(weighting, corr)) +
  geom_point(size = 2) +
  geom_line(
    data = pred_dat,
    aes(y = pred),
    linewidth = 1
  ) +
  labs(
    x = "RT Weighting",
    y = "Correlation"
  ) +
  theme_minimal()