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



# ---- visualizing the rotation of BIS

plot_data <- all_data |>
  group_by(task, session) |>
  mutate(
    z_ER = scale(ER),
    z_RT = scale(mean_RT)
  ) |>
  ungroup() |>
  mutate(
    task = case_when(
      task == 'axcpt' ~ 'AX-CPT',
      task == 'cuedts' ~ 'Cued TS',
      task == 'sternberg' ~ 'Sternberg',
      task == 'stroop' ~ 'Stroop'
    )
  )

grid_plot <- ggplot(data = plot_data) + 
  # scatter plot of data
  geom_point(
    aes(x = scale(z_RT), y = scale(z_ER)),
    color = "black",
    size = 2.5,
    alpha = 0.5
  ) +
  # Labeling of axis and legend
  labs(
    x = "Reaction Time (z-standardized)", 
    y = "Error Rate (z-standardized)"
  ) +
  facet_grid2(task ~ session, axes = "all") +
  # scaling axis to be the same and setting ticks
  coord_fixed(ratio = 1) +
  scale_x_continuous(
  limits = c(-3, 3),
  breaks = seq(-3, 3, by = 1)
  ) +
  scale_y_continuous(
    limits = c(-3, 3),
    breaks = seq(-3, 3, by = 1)
  ) +
  # setting theme
  theme_minimal() +
  theme(
    # background
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.grid        = element_blank(),
    
    # keep standard axis
    axis.line         = element_line(color = "black", size = 1),
    
    # sizing of elements
    plot.title = element_text(size = 30, face = "bold", color = "black", hjust = 0.5),
    axis.title.x      = element_text(size = 26, face = "bold", color = "black"),
    axis.title.y      = element_text(size = 26, face = "bold", color = "black"),
    axis.text         = element_text(size = 26, color = "black"),
    legend.title      = element_text(size = 26, face = "bold", color = "black"),
    legend.text       = element_text(size = 26, color = "black"),
    axis.ticks        = element_line(size = 1, color = "black"),

    # add arrow heads to the axis lines
    axis.line.x = element_line(
      color = "black",
      arrow = arrow(length = unit(0.7, "cm"))
    ),
    axis.line.y = element_line(
      color = "black",
      arrow = arrow(length = unit(0.7, "cm"))
    ),
    panel.border = element_blank(),
    panel.spacing = unit(0.15, "cm"),
    strip.text.x = element_text(size = 26, face = "bold"),
    strip.text.y = element_text(size = 26, face = "bold")
  )

ggsave('5_other_scripts/explore_ceiling/ceiling_plot.png', plot = grid_plot, width = 20, height = 27)

system('open 5_other_scripts/explore_ceiling/ceiling_plot.png')
