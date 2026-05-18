# ---- loading packages ----
library(tidyverse)
library(psych)
library(purrr)
library(boot)



# ---- loading data sets ----
raw_data <- read_csv('../bachelor_data/data_study_1/sternberg_1.csv')



# ---- processing data ----

# grouping by ID, trialType (trialType), session (reactive/baseline), phase (test/retest)
# probeCorrect, RT
mean_data <- raw_data |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(probeCorrect),
    .groups = 'drop'
  )

# checking mean of all participants
overview_results <- raw_data |>
  group_by(trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(probeCorrect),
    .groups = 'drop'
  )
# View(overview_results)
rm(overview_results)

# pivoting data set to get 1 row per participant x modality
mean_data_wide <- mean_data |>
  pivot_wider(
    id_cols = c(ID, session),
    names_from = c(phase, trialType),
    values_from = c(mean_RT, ER)
  )




# ---- calculating boot strapped CIs for BIS in 0.25 distances ----

# function to get the intra class correlation ICC (3,1)
boot_icc <- function(data, indices, rt_weighting) {
  # draw bootstrap sample on participant level
  data_temp <- data[indices, ] |>
    mutate(ID = row_number()) # elsewise one ID from original data set can appear multiple times
  # pivot trialType back into long for scaling
  data_temp <- data_temp |>
    pivot_longer(
      cols = -c(ID, session),
      names_to = c(".value", "trialType"),
      names_pattern = "(.*)_(RN|NN)"
    )
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    mutate(
      z_RT_test = as.numeric(scale(mean_RT_test)),
      z_ER_test = as.numeric(scale(ER_test)),
      BIS_test = - (rt_weighting) * z_RT_test - (1 - rt_weighting) * z_ER_test,
      z_RT_retest = as.numeric(scale(mean_RT_retest)),
      z_ER_retest = as.numeric(scale(ER_retest)),
      BIS_retest = - (rt_weighting) * z_RT_retest - (1 - rt_weighting) * z_ER_retest
    ) |>
    select(-c('z_RT_test', 'z_ER_test', 'z_RT_retest', 'z_ER_retest', 'mean_RT_test', 'ER_test', 'mean_RT_retest', 'ER_retest'))
  # pivot wider and calculate the cognitive control measure
  data_temp <- data_temp |>
    pivot_wider(
      names_from = trialType,
      values_from = c(BIS_test, BIS_retest)
    ) |>
    mutate(
      CC_test = BIS_test_RN - BIS_test_NN,
      CC_retest = BIS_retest_RN - BIS_retest_NN
    )

  # calculate ICC; output NA for error in convergence
  icc_obj <- tryCatch(
    suppressWarnings(
      data_temp |>
        select(CC_test, CC_retest) |>
        psych::ICC()
    ),
    error = function(e) NA
  )
  if (is.na(icc_obj)[1]) return(NA)
  # extract ICC 3,1 if model converged
  ICC_3 <- icc_obj$results["Single_fixed_raters", "ICC"]
  return(ICC_3)
}



# weights to test
weights <- seq(0, 1, by = 0.25)
modalities <- unique(mean_data_wide$session)

# empty results table
weight_table <- tibble(
  modality = character(),
  weight_rt = numeric(),
  weight_er = numeric(),
  t0 = numeric(),
  upper_limit = numeric(),
  lower_limit = numeric(),
  per_not_converged = numeric()
)

set.seed(309)
for (curr_mod in modalities){
  for (curr_weight in weights) {

    # select data of current modality
    mod_data <- mean_data_wide |>
      filter(session == curr_mod)

    # bootstrap icc
    boot_ICC_obj <- boot(
      data = mod_data,
      statistic = boot_icc,
      rt_weighting = curr_weight,
      R = 1000
    )

    # check for convergence
    per_not_conv <- mean(is.na(boot_ICC_obj$t))

    # extract bias corrected probeCorrectelarated ci
    bca_ci <- boot.ci(boot_ICC_obj, type = "bca")

    t0 <- bca_ci$t0
    lower_limit <- bca_ci$bca[4]
    upper_limit <- bca_ci$bca[5]

    # append row
    weight_table <- bind_rows(
      weight_table,
      tibble(
        modality = curr_mod,
        weight_rt = curr_weight,
        weight_er = 1 - curr_weight,
        t0 = t0,
        upper_limit = upper_limit,
        lower_limit = lower_limit,
        per_not_converged = per_not_conv
      )
    )
  }
}


# ---- visualizing boot strapped intervals ----

sternberg_plot <- ggplot(weight_table, aes(x = weight_rt, y = t0, group = modality, color = modality)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = lower_limit, ymax = upper_limit),
    width = 0.03
  ) +
  labs(
    x = "RT Weight",
    y = "ICC (3,1)",
    color = 'Modality',
    title = "Cued TS: ICC Across BIS Weightings"
  ) +
  theme_minimal()

sternberg_plot

ggsave(plot = sternberg_plot, file = 'study_1_reliability/plots/bootstrap_50_excl/sternberg_boot.png', width = 8, height = 4)






# ====================================================
# ---- continous weights ----
# ====================================================


# ---- Function: compute ICC(3,1) for one session and one RT weight

icc_for_weight <- function(frt, data_session) {
  
  fer <- 1 - frt
  
  tmp <- data_session %>%
    mutate(
      stroop_test_bis =
        as.numeric(-frt * scale(stroop_test_rt) -
                   fer * scale(stroop_test_er)),
      stroop_retest_bis =
        as.numeric(-frt * scale(stroop_retest_rt) -
                   fer * scale(stroop_retest_er))
    )
  
  icc_obj <- tmp %>%
    select(stroop_test_bis, stroop_retest_bis) %>%
    psych::ICC()
  
  # ICC(3,1)
  icc_value <- icc_obj$results["Single_fixed_raters", "ICC"]
  
  return(as.numeric(icc_value))
}



# --------------------------------------------------
# 2) Continuous optimization of RT weight
# --------------------------------------------------
optimal_weights_continuous <- map_dfr(unique(mean_data_wide$session), function(sess) {
  
  data_session <- mean_data_wide %>%
    filter(session == sess)
  
  # optimize() searches continuously in [0,1]
  opt <- optimize(
    f = function(w) icc_for_weight(w, data_session),
    interval = c(0, 1),
    maximum = TRUE
  )
  
  tibble(
    session = sess,
    optimal_weight_rt = opt$maximum,
    optimal_weight_er = 1 - opt$maximum,
    max_ICC_3 = opt$objective
  )
})

optimal_weights_continuous
