library(tidyverse)
library(boot)

# --- your existing bootstrap ---
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  cor(d$mpg, d$wt, use = "complete.obs")
}

set.seed(123)
boot_cor_out <- boot(
  data = mtcars,
  statistic = boot_cor,
  R = 500
)

# --- extract bootstrap values ---
boot_vals <- data.frame(cor = boot_cor_out$t)

# --- extract BCa CI ---
bca_ci <- boot.ci(boot_cor_out, type = "bca")$bca

lower_bca <- bca_ci[4]
upper_bca <- bca_ci[5]

# original estimate
orig <- boot_cor_out$t0

# --- plot ---
ggplot(boot_vals, aes(x = cor)) +
  geom_histogram(bins = 30, color = "black", fill = "grey80") +
  
  # original estimate
  geom_vline(xintercept = orig, linewidth = 1) +
  
  # BCa CI lines
  geom_vline(xintercept = lower_bca, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = upper_bca, linetype = "dashed", linewidth = 1) +
  
  labs(
    title = "Bootstrap Distribution of Correlation (mpg ~ wt)",
    x = "Bootstrap correlation",
    y = "Frequency"
  ) +
  theme_minimal()
