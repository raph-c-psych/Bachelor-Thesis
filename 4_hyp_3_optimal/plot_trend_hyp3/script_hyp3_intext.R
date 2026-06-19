library(tidyverse)
library(lme4)
library(sjPlot)
library(boot)
library(psych)


# ---- pre processing data -----
# -- axcpt
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
axcpt_data <- read_csv('../bachelor_data/data_study_2/axcpt_2.csv') |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(probeReacTime, na.rm = TRUE),
    ER = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    trialType = case_when(
      trialType == 'BX' ~ 'Incongruent',
      trialType == 'BY' ~ 'Congruent'
    ),
    task = 'axcpt'
  )

# -- sternberg
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
sternberg_data <- read_csv('../bachelor_data/data_study_2/sternberg_2.csv') |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    trialType = case_when(
      trialType == 'RN' ~ 'Incongruent',
      trialType == 'NN' ~ 'Congruent'
    ),
    task = 'sternberg'
  )


# -- stroop
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
stroop_data <- read_csv('../bachelor_data/data_study_2/stroop_2.csv') |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    trialType = case_when(
      trialType == '1' ~ 'Incongruent',
      trialType == '2' ~ 'Congruent'
    ),
    task = 'stroop'
  )

# -- cued task swtiching
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
cuedts_data <- read_csv('../bachelor_data/data_study_2/cuedts_2.csv') |>
  group_by(ID, congruency, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'cuedts'
  ) |>
  rename(trialType = congruency)


# -- adding together tasks
all_data <- rbind(
  axcpt_data,
  cuedts_data,
  sternberg_data,
  stroop_data
)
rm(axcpt_data, cuedts_data, sternberg_data, stroop_data)





# ---- calculating optimal weights for each task x modality ----
# function for correlating ICC
calc_ICCs <- function(data_temp, rt_weighting){
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task, phase) |>
    mutate(
      z_RT = as.numeric(scale(mean_RT)),
      z_ER = as.numeric(scale(ER)),
      BIS = - (rt_weighting) * z_RT - (1 - rt_weighting) * z_ER # afterwards could be test retest
    ) |>
    select(-c('z_RT', 'z_ER', 'mean_RT', 'ER')) |>
    ungroup()
  # pivot wider and calculate the cognitive control measure
  data_temp <- data_temp |>
    pivot_wider(
      names_from = trialType,
      values_from = BIS, # can be supplemented with test retest
      names_glue = "BIS_{trialType}"
    ) |>
    mutate(
      CC_measure = BIS_Incongruent - BIS_Congruent
      # CC_retest = BIS_retest_BX - BIS_retest_BY
    )

  # pivot data set for phase
  data_temp <- data_temp |>
    pivot_wider(
      id_cols = c(ID, task, session),
      names_from = phase,
      values_from = CC_measure,
      names_glue = "CC_{phase}"
    )
  # calculate corellations for each task and modality
  ICCs <- data_temp |>
    group_by(task, session) |>
    summarise(
      r = psych::ICC(data.frame(CC_test, CC_retest), lmer = FALSE)$results["Single_fixed_raters", "ICC"],
      n = sum(complete.cases(CC_test, CC_retest)),
      .groups = "drop"
    )

  # return correlation matrix
  return(ICCs)
}

# calculating the weighting in a finer grid
weights <- seq(0, 1, by = 0.01)
icc_list <- tibble(
  rt_weight = numeric(),
  task = character(),
  session = character(),
  r = numeric(),
  n = numeric()
)
for (curr_weight in weights) {
  mod_data <- all_data
  curr_iccs <- calc_ICCs(mod_data, curr_weight) |>
    mutate(rt_weight = curr_weight)
  icc_list <- rbind(icc_list, curr_iccs)
}
rm(curr_iccs, mod_data, curr_weight, weights, calc_ICCs)

# extracting maximal weights
optimal_weights <- icc_list |>
  group_by(task, session) |>
  slice_max(r, n = 1) |>
  select(-c(n, r))
rm(icc_list)





# ---- function: calculating correlations between sessions through pearson ----
calc_corrs <- function(data_temp, rt_weight_list){
  # match individual weighting with task x session
  data_temp <- data_temp |>
    right_join(rt_weight_list, by = c('task', 'session'))
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task) |>
    mutate(
      z_RT = as.numeric(scale(mean_RT)),
      z_ER = as.numeric(scale(ER)),
      BIS = - (rt_weight) * z_RT - (1 - rt_weight) * z_ER # afterwards could be test retest
    ) |>
    select(-c('z_RT', 'z_ER', 'mean_RT', 'ER')) |>
    ungroup()
  # pivot wider and calculate the cognitive control measure
  data_temp <- data_temp |>
    pivot_wider(
      names_from = trialType,
      values_from = BIS, # can be supplemented with test retest
      names_glue = "BIS_{trialType}"
    ) |>
    mutate(
      CC_measure = BIS_Incongruent - BIS_Congruent
      # CC_retest = BIS_retest_BX - BIS_retest_BY
    )

  # pivot data set for task and modality
  data_temp <- data_temp |>
    pivot_wider(
      id_cols = ID,
      names_from = c(task, session),
      values_from = CC_measure,
      names_glue = "CC_{task}_{session}"
    )
  # calculate correlation matrix
  all_corr <- data_temp |>
    select(contains("CC")) |>
    cor(use = "pairwise.complete.obs")

  # return correlation matrix
  return(all_corr)
}



# ---- testing function to create correlations for different weight types ----

# creating weight lists for rt and er
er_weights <- optimal_weights |>
  mutate(rt_weight = 0)
rt_weights <- optimal_weights |>
  mutate(rt_weight = 1)

# calculating correlation matrixes for different weights
types_weight <- c('rt', 'er', 'optimal')
corr_long <- tibble(
  weight_type = character(),
  correlation = numeric(),
  group = character()
)
for (curr_type in types_weight) {
  # take only test data
  mod_data <- all_data |>
    filter(phase == "test")
  # create correlation matrix for current weight between all sessions
  if(curr_type == 'rt'){
    corr_matrix <- calc_corrs(mod_data, rt_weights)
  } else if (curr_type == 'er'){
    corr_matrix <- calc_corrs(mod_data, er_weights)
  } else if (curr_type == 'optimal'){
    corr_matrix <- calc_corrs(mod_data, optimal_weights)
  } 
  
  # create clean long format of correlations
  corr_data <- corr_matrix |>
    as.data.frame() |>
    rownames_to_column("var1") |>
    pivot_longer(
      cols = -var1,
      names_to = "var2",
      values_to = "correlation"
    ) |>
    filter(var1 < var2) |> # exclude duplicates and auto correlations
    mutate(weight_type = curr_type) |>
    mutate(
      task_1 = str_match(var1, "^CC_(.*?)_(.*?)$")[,2],
      modality_1 = str_match(var1, "^CC_(.*?)_(.*?)$")[,3],
      task_2 = str_match(var2, "^CC_(.*?)_(.*?)$")[,2],
      modality_2 = str_match(var2, "^CC_(.*?)_(.*?)$")[,3]
    ) |>
    select(-c(var1, var2))
  corr_long <- rbind(corr_long, corr_data)
}
rm(corr_data, corr_matrix, mod_data, curr_type, types_weight, er_weights, rt_weights, optimal_weights)




# naming
corr_list <- corr_long |>
  mutate(
    correlation = fisherz(correlation),
    task_1 = case_when(
      task_1 == 'axcpt' ~ 'AX-CPT', 
      task_1 == 'cuedts' ~ 'Cued TS', 
      task_1 == 'sternberg' ~ 'Sternberg', 
      task_1 == 'stroop' ~ 'Stroop'
    ),
    task_2 = case_when(
      task_2 == 'axcpt' ~ 'AX-CPT', 
      task_2 == 'cuedts' ~ 'Cued TS', 
      task_2 == 'sternberg' ~ 'Sternberg', 
      task_2 == 'stroop' ~ 'Stroop'
    )
  ) |>
  mutate(
    weight_type = factor(
      weight_type,
      levels = c("er", "optimal", "rt"),
      labels = c("ER", "Optimal", "RT")
    )
  )

corr_list <- corr_list |>
  filter(modality_1 == modality_2) |>
  filter(task_1 != task_2) |>
  mutate(
    Combination = paste0(task_1, ' x ', task_2)
  )


# ---- visualize the trends ----

corr_trend_plot <- ggplot(data = corr_list, aes(x = weight_type, y = correlation, group = Combination, shape = Combination)) +
  geom_point(size = 2.5) +
  geom_line()+
  facet_wrap2(~modality_1, axes = "all", ncol = 2) +
  scale_y_continuous(
    limits = c(-0.5, 0.5)
  ) +
  labs(
    y = 'Correlation (Fisher-z trans.)',
    x = 'Weight of RT',
    shape = 'Task Combination'
  ) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    panel.grid = element_blank(),      # remove all grid lines
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line.x = element_line(
      color = "black"
    ),
    axis.line.y = element_line(
      color = "black",
      arrow = arrow(length = unit(0.7, "cm")))
  )

ggsave(
  "4_hyp_3_optimal/plot_trend_hyp3/trend_hyp3_intext.png",
  corr_trend_plot,
  width = 10,
  height = 8
)

system("open 4_hyp_3_optimal/plot_trend_hyp3/trend_hyp3_intext.png")

