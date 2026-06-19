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





# ---- function for ICC ----
calc_corrs <- function(data_temp, rt_weighting, er_weighting){
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task, phase) |>
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




# ---- testing ICC function -----
# calculating correlation matrixes for different weights
weights_rt <- seq(-1, 1, by = 0.05)
weights_er <- c(seq(0, 1, by = 0.05), seq(0.95,0, by = - 0.05))
corr_list <- tibble(
  rt_weight = numeric(),
  task = character(),
  session = character(),
  r = numeric(),
  n = numeric()
)
for (weight_index in seq_along(weights_rt)) {
  mod_data <- all_data
  curr_corrs <- calc_corrs(mod_data, weights_rt[weight_index], weights_er[weight_index]) |>
    mutate(rt_weight = weights_rt[weight_index])
  corr_list <- rbind(corr_list, curr_corrs)
}


corr_list <- corr_list |>
  mutate(
    r = fisherz(r),
    task = case_when(
      task == 'axcpt' ~ 'AX-CPT',
      task == 'cuedts' ~ 'Cued TS',
      task == 'sternberg' ~ 'Sternberg',
      task == 'stroop' ~ 'Stroop'
    )
  )


# ---- visualize the trends ----

icc_trend_plot <- ggplot(data = corr_list, aes(x = rt_weight, y = r, group = session, shape = session)) +
  geom_point(size = 1.5) +
  geom_line()+
  facet_wrap2(~task, axes = "all") +
  scale_y_continuous(
    limits = c(0, 1.3),
    breaks = seq(0, 1.3, by = 0.3)
  ) +
  labs(
    y = 'ICC (Fisher-z trans.)',
    x = 'Weight of RT',
    shape = 'Modality'
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

icc_trend_plot

ggsave(
  "2_hyp_1_icc/plot_trend_hyp1/minus_plus.png",
  icc_trend_plot,
  width = 10,
  height = 8
)

system("open 2_hyp_1_icc/plot_trend_hyp1/minus_plus.png")
