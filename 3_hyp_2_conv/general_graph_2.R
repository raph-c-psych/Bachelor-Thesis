library(tidyverse)
library(patchwork)
library(purrr)



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




calc_corrs <- function(data_temp, rt_weighting){
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task) |>
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

# calculating correlation matrixes for different weights
weights <- seq(0, 1, by = 0.25)
corr_list <- list()
for (curr_weight in weights) {
  mod_data <- all_data |>
    filter(phase == "test")
  corr_matrix <- calc_corrs(mod_data, curr_weight)
  corr_list[[as.character(curr_weight)]] <- corr_matrix
}


corr_long <- imap_dfr(
  corr_list,
  ~ .x |>
    as.data.frame() |>
    rownames_to_column("var1") |>
    pivot_longer(
      cols = -var1,
      names_to = "var2",
      values_to = "correlation"
    ) |>
    mutate(weight_rt = as.numeric(.y))
)

corr_long <- corr_long |>
  mutate(
    var1 = var1 |>
      gsub("^CC_", "", x = _) |>
      gsub("_", " ", x = _) |>
      tools::toTitleCase() |>
      gsub(" ", "\n", x = _),
    var2 = var2 |>
      gsub("^CC_", "", x = _) |>
      gsub("_", " ", x = _) |>
      tools::toTitleCase() |>
      gsub(" ", "\n", x = _)
  ) |>
  mutate(
    task1 = sub("^([A-Za-z]+).*", "\\1", var1),
    task2 = sub("^([A-Za-z]+).*", "\\1", var2),
    corr_type = ifelse(
      task1 == task2,
      "Within Task",
      "Between Task"
    )
  )


corr_plot <- ggplot(
  corr_long,
  aes(x = weight_rt, y = correlation, color = corr_type)
) +
  geom_line() +
  geom_point(size = 1.5) +
  facet_grid(var1 ~ var2) +
  labs(
    x = "RT Weight",
    y = "Correlation",
    color = "Correlation Type",
    title = "Correlations Across BIS Weightings"
  ) +
  scale_y_continuous(
    limits = c(-1, 1)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

corr_plot

ggsave(
  "study_2_validity/plots/correlation_grid.png",
  corr_plot,
  width = 20,
  height = 16
)

system("open study_2_validity/plots/correlation_grid.png")



corr_plot
