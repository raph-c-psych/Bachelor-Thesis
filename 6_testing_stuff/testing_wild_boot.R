# install.packages(c("lme4", "lmeresampler", "dplyr"))

library(lme4)
library(lmeresampler)
library(dplyr)

set.seed(123)

# Simulate fake correlation-level data
n_boot <- 100
task_pairs <- paste0("pair_", 1:6)
weights <- seq(0, 1, by = 0.05)

corr_data <- expand.grid(
  bootstrap_sample = paste0("boot_", 1:n_boot),
  task_pair = task_pairs,
  weighting = weights
)

# Random effects
boot_effects <- rnorm(n_boot, mean = 0, sd = 0.08)
names(boot_effects) <- paste0("boot_", 1:n_boot)

task_effects <- rnorm(length(task_pairs), mean = 0, sd = 0.10)
names(task_effects) <- task_pairs

# Generate Fisher-z correlations
corr_data <- corr_data %>%
  mutate(
    fisher_z_r =
      0.25 +                         # intercept
      0.15 * weighting +             # true weighting effect
      boot_effects[bootstrap_sample] +
      task_effects[task_pair] +
      rnorm(n(), mean = 0, sd = 0.08)
  )

# Fit multilevel model
model <- lmer(
  fisher_z_r ~ weighting +
    (1 | bootstrap_sample) +
    (1 | task_pair),
  data = corr_data
)

summary(model)



# ---- wild bootstrapping ----
set.seed(123)

boot_out <- bootstrap(
  model,
  .f = fixef,
  type = "wild",
  hccme = 'hc3',
  aux.dist = "rademacher",
  B = 100
)

boot_out
confint(boot_out)
