library(tidyverse)
library(patchwork)



# ---- axcpt ----
raw_data <- read_csv('../bachelor_data/data_study_1/axcpt_1.csv')

# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
mean_data <- raw_data |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(probeReacTime, na.rm = TRUE),
    ER = 1 - mean(probeCorrect),
    .groups = 'drop'
  )

# checking mean of all participants
overview_results <- raw_data |>
  group_by(trialType, session, phase) |>
  summarise(
    mean_RT = mean(probeReacTime, na.rm = TRUE),
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

calc_ICC <- function(data_temp, rt_weighting){
  data_temp <- data_temp |>
    pivot_longer(
      cols = -c(ID, session),
      names_to = c(".value", "trialType"),
      names_pattern = "(.*)_(BX|BY)"
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
      CC_test = BIS_test_BX - BIS_test_BY,
      CC_retest = BIS_retest_BX - BIS_retest_BY
    )

  # calculate ICC
  icc_obj <- data_temp |>
      select(CC_test, CC_retest) |>
      psych::ICC()
  # extract ICC (3,1)
  ICC_3 <- icc_obj$results["Single_fixed_raters", "ICC"]
  lower_ci <- icc_obj$results["Single_fixed_raters", "lower bound"]
  upper_ci <- icc_obj$results["Single_fixed_raters", "upper bound"]
  # return ICC (3,1)
  return(c(ICC_3, lower_ci, upper_ci))
}

# weights to test
weights <- seq(0, 1, by = 0.25)
modalities <- unique(mean_data_wide$session)

# empty results table
weight_table <- tibble(
  modality = character(),
  weight_rt = numeric(),
  weight_er = numeric(),
  icc = numeric(),
  lower_limit = numeric(),
  upper_limit = numeric()
)

set.seed(309)
for (curr_mod in modalities){
  for (curr_weight in weights) {

    # select data of current modality
    mod_data <- mean_data_wide |>
      filter(session == curr_mod)

    ICC_3 <- calc_ICC(mod_data, curr_weight)

    # append row
    weight_table <- bind_rows(
      weight_table,
      tibble(
        modality = curr_mod,
        weight_rt = curr_weight,
        weight_er = 1 - curr_weight,
        icc = ICC_3[1],
        lower_limit = ICC_3[2],
        upper_limit = ICC_3[3]
      )
    )
  }
}

axcpt_plot <- ggplot(weight_table, aes(x = weight_rt, y = icc, group = modality, color = modality)) +
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
    title = "AXCPT: ICC Across BIS Weightings"
  ) +
  theme_minimal()

axcpt_plot
ggsave('study_1_reliability/plots/axcpt_try.png', width = 6, height = 4)

rm(list = setdiff(ls(), "axcpt_plot"))





# ---- sternberg ----
raw_data <- read_csv('../bachelor_data/data_study_1/sternberg_1.csv')

# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
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

calc_ICC <- function(data_temp, rt_weighting){
  data_temp <- data_temp |>
    pivot_longer(
      cols = -c(ID, session),
      names_to = c(".value", "trialType"),
      names_pattern = "(.*)_(NN|RN)"
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
      CC_test = BIS_test_NN - BIS_test_RN,
      CC_retest = BIS_retest_NN - BIS_retest_RN
    )

  # calculate ICC
  icc_obj <- data_temp |>
      select(CC_test, CC_retest) |>
      psych::ICC()
  # extract ICC (3,1)
  ICC_3 <- icc_obj$results["Single_fixed_raters", "ICC"]
  lower_ci <- icc_obj$results["Single_fixed_raters", "lower bound"]
  upper_ci <- icc_obj$results["Single_fixed_raters", "upper bound"]
  # return ICC (3,1)
  return(c(ICC_3, lower_ci, upper_ci))
}

# weights to test
weights <- seq(0, 1, by = 0.25)
modalities <- unique(mean_data_wide$session)

# empty results table
weight_table <- tibble(
  modality = character(),
  weight_rt = numeric(),
  weight_er = numeric(),
  icc = numeric(),
  lower_limit = numeric(),
  upper_limit = numeric()
)

set.seed(309)
for (curr_mod in modalities){
  for (curr_weight in weights) {

    # select data of current modality
    mod_data <- mean_data_wide |>
      filter(session == curr_mod)

    ICC_3 <- calc_ICC(mod_data, curr_weight)

    # append row
    weight_table <- bind_rows(
      weight_table,
      tibble(
        modality = curr_mod,
        weight_rt = curr_weight,
        weight_er = 1 - curr_weight,
        icc = ICC_3[1],
        lower_limit = ICC_3[2],
        upper_limit = ICC_3[3]
      )
    )
  }
}

sternberg_plot <- ggplot(weight_table, aes(x = weight_rt, y = icc, group = modality, color = modality)) +
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
    title = "Sternberg: ICC Across BIS Weightings"
  ) +
  theme_minimal()

sternberg_plot
ggsave('study_1_reliability/plots/sternberg_try.png', width = 6, height = 4)

rm(list = setdiff(ls(), c("axcpt_plot", "sternberg_plot")))





# ---- stroop ----
raw_data <- read_csv('../bachelor_data/data_study_1/stroop_1.csv')

# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
mean_data <- raw_data |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
    .groups = 'drop'
  )

# checking mean of all participants
overview_results <- raw_data |>
  group_by(trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
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

calc_ICC <- function(data_temp, rt_weighting){
  data_temp <- data_temp |>
    pivot_longer(
      cols = -c(ID, session),
      names_to = c(".value", "trialType"),
      names_pattern = "(.*)_(1|2)"
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
      CC_test = BIS_test_1 - BIS_test_2,
      CC_retest = BIS_retest_1 - BIS_retest_2
    )

  # calculate ICC
  icc_obj <- data_temp |>
      select(CC_test, CC_retest) |>
      psych::ICC()
  # extract ICC (3,1)
  ICC_3 <- icc_obj$results["Single_fixed_raters", "ICC"]
  lower_ci <- icc_obj$results["Single_fixed_raters", "lower bound"]
  upper_ci <- icc_obj$results["Single_fixed_raters", "upper bound"]
  # return ICC (3,1)
  return(c(ICC_3, lower_ci, upper_ci))
}

# weights to test
weights <- seq(0, 1, by = 0.25)
modalities <- unique(mean_data_wide$session)

# empty results table
weight_table <- tibble(
  modality = character(),
  weight_rt = numeric(),
  weight_er = numeric(),
  icc = numeric(),
  lower_limit = numeric(),
  upper_limit = numeric()
)

set.seed(309)
for (curr_mod in modalities){
  for (curr_weight in weights) {

    # select data of current modality
    mod_data <- mean_data_wide |>
      filter(session == curr_mod)

    ICC_3 <- calc_ICC(mod_data, curr_weight)

    # append row
    weight_table <- bind_rows(
      weight_table,
      tibble(
        modality = curr_mod,
        weight_rt = curr_weight,
        weight_er = 1 - curr_weight,
        icc = ICC_3[1],
        lower_limit = ICC_3[2],
        upper_limit = ICC_3[3]
      )
    )
  }
}

stroop_plot <- ggplot(weight_table, aes(x = weight_rt, y = icc, group = modality, color = modality)) +
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
    title = "Stroop: ICC Across BIS Weightings"
  ) +
  theme_minimal()

stroop_plot
ggsave('study_1_reliability/plots/stroop_try.png', width = 6, height = 4)

rm(list = setdiff(ls(), c("axcpt_plot", "sternberg_plot", "stroop_plot")))




# ---- cued task switching ----
raw_data <- read_csv('../bachelor_data/data_study_1/cuedts_1.csv')

# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
mean_data <- raw_data |>
  group_by(ID, congruency, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
    .groups = 'drop'
  )

# checking mean of all participants
overview_results <- raw_data |>
  group_by(congruency, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
    .groups = 'drop'
  )
# View(overview_results)
rm(overview_results)

# pivoting data set to get 1 row per participant x modality
mean_data_wide <- mean_data |>
  pivot_wider(
    id_cols = c(ID, session),
    names_from = c(phase, congruency),
    values_from = c(mean_RT, ER)
  )

calc_ICC <- function(data_temp, rt_weighting){
  data_temp <- data_temp |>
    pivot_longer(
      cols = -c(ID, session),
      names_to = c(".value", "congruency"),
      names_pattern = "(.*)_(Incongruent|Congruent)"
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
      names_from = congruency,
      values_from = c(BIS_test, BIS_retest)
    ) |>
    mutate(
      CC_test = BIS_test_Incongruent - BIS_test_Congruent,
      CC_retest = BIS_retest_Incongruent - BIS_retest_Congruent
    )

  # calculate ICC
  icc_obj <- data_temp |>
      select(CC_test, CC_retest) |>
      psych::ICC()
  # extract ICC (3,1)
  ICC_3 <- icc_obj$results["Single_fixed_raters", "ICC"]
  lower_ci <- icc_obj$results["Single_fixed_raters", "lower bound"]
  upper_ci <- icc_obj$results["Single_fixed_raters", "upper bound"]
  # return ICC (3,1)
  return(c(ICC_3, lower_ci, upper_ci))
}

# weights to test
weights <- seq(0, 1, by = 0.25)
modalities <- unique(mean_data_wide$session)

# empty results table
weight_table <- tibble(
  modality = character(),
  weight_rt = numeric(),
  weight_er = numeric(),
  icc = numeric(),
  lower_limit = numeric(),
  upper_limit = numeric()
)

set.seed(309)
for (curr_mod in modalities){
  for (curr_weight in weights) {

    # select data of current modality
    mod_data <- mean_data_wide |>
      filter(session == curr_mod)

    ICC_3 <- calc_ICC(mod_data, curr_weight)

    # append row
    weight_table <- bind_rows(
      weight_table,
      tibble(
        modality = curr_mod,
        weight_rt = curr_weight,
        weight_er = 1 - curr_weight,
        icc = ICC_3[1],
        lower_limit = ICC_3[2],
        upper_limit = ICC_3[3]
      )
    )
  }
}

cuedts_plot <- ggplot(weight_table, aes(x = weight_rt, y = icc, group = modality, color = modality)) +
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

cuedts_plot
ggsave('study_1_reliability/plots/cuedts_try.png', width = 8, height = 4)

rm(list = setdiff(ls(), c("axcpt_plot", "sternberg_plot", "stroop_plot", "cuedts_plot")))



general_plot <- (stroop_plot | axcpt_plot) /
(cuedts_plot | sternberg_plot)

ggsave('study_1_reliability/plots/general_plot.png', width = 16, height = 8)
