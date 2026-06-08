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




# ---- creating table -----

vec_task <- unique(all_data$task)
vec_cond <- unique(all_data$session)
vec_phase <- unique(all_data$phase)
vec_trialtype <- unique(all_data$trialType)

desc_list <- tibble(
  task = character(),
  condition = character(),
  phase = character(),
  trialtype = character(),
  mean = numeric(),
  variance = numeric()
)

desc_list <- all_data |>
  group_by(task, session, phase, trialType) |>
  summarise(
    mean_rt = mean(mean_RT),
    sd_rt = sd(mean_RT),
    mean_er = mean(ER * 100),
    sd_er = sd(ER * 100),
    .groups = 'drop'
  ) |>
  mutate(
    mean_rt = round(mean_rt),
    sd_rt = round(sd_rt),
    mean_er = round(mean_er, 2),
    sd_er = round(sd_er, 2)
  )

# for reporting
report_list <- desc_list |>
  mutate(
    rt_text = paste0(mean_rt, ' (', sd_rt, ')'),
    er_text = paste0(mean_er, ' (', sd_er, ')')
  ) |>
  pivot_wider(
    id_cols = c(task, session, trialType),
    names_from = phase,
    values_from = c(rt_text, er_text)
  )
