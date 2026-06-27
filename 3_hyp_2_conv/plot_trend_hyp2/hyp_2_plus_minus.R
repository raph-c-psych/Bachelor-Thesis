library(tidyverse)
library(lme4)
library(sjPlot)
library(boot)
library(psych)
library(ggh4x)

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





# ---- function: calculating correlations between sessions through pearson ----
calc_corrs <- function(data_temp, rt_weighting, er_weighting){
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task) |>
    mutate(
      z_RT = as.numeric(scale(mean_RT)),
      z_ER = as.numeric(scale(ER)),
      BIS = - (rt_weighting) * z_RT - (er_weighting) * z_ER # afterwards could be test retest
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





# ---- testing function to create correlations for different weights ----
# calculating correlation matrixes for different weights
weights_rt <- seq(-1, 1, by = 0.05)
weights_er <- c(seq(0, 1, by = 0.05), seq(0.95,0, by = - 0.05))
corr_long <- tibble(
  rt_weight = numeric(),
  correlation = numeric(),
  group = character()
)
for (weight_index in seq_along(weights_rt)) {
  # take only test data
  mod_data <- all_data |>
    filter(phase == "test")
  # create correlation matrix for current weight between all sessions
  corr_matrix <- calc_corrs(mod_data, weights_rt[weight_index], weights_er[weight_index])
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
    mutate(rt_weight = as.numeric(weights_rt[weight_index])) |>
    mutate(
      task_1 = str_match(var1, "^CC_(.*?)_(.*?)$")[,2],
      modality_1 = str_match(var1, "^CC_(.*?)_(.*?)$")[,3],
      task_2 = str_match(var2, "^CC_(.*?)_(.*?)$")[,2],
      modality_2 = str_match(var2, "^CC_(.*?)_(.*?)$")[,3]
    ) |>
    select(-c(var1, var2))
    # filter(task_1 != task_2) |> # different tasks
    # filter(modality_1 == modality_2)# |>
    # mutate(group = paste0(str_sub(var1, 4), '-', str_sub(var2, 4))) |>
    # select(-c(var1, var2, task_1, task_2, modality_1, modality_2))
  corr_long <- rbind(corr_long, corr_data)
}
rm(corr_matrix, mod_data, weight_index, weights_rt, weights_er, corr_data)


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
  )

corr_list <- corr_list |>
  filter(modality_1 == modality_2) |>
  filter(task_1 != task_2) |>
  mutate(
    Combination = paste0(task_1, ' x ', task_2)
  )



# ---- visualize the trends ----
corr_trend_plot <- ggplot(data = corr_list, aes(x = rt_weight, y = correlation, group = Combination, shape = Combination)) +
  geom_point(size = 1.5) +
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
  "3_hyp_2_conv/plot_trend_hyp2/hyp2_plus_minus.png",
  corr_trend_plot,
  width = 14,
  height = 10
)

system("open 3_hyp_2_conv/plot_trend_hyp2/hyp2_plus_minus.png")

