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



# ---- loading optimal weighting ----

optimal_weights <- read_csv('4_hyp_3_optimal/analysis_hyp3/optimal_weights.csv')




# ---- calculate rotation of vector
bis_vector <- function(weight_rt,
                       weight_er = 1 - weight_rt,
                       bound = 2.8) {

  # scale so the vector touches the plot boundary
  scale_factor <- bound / max(abs(weight_rt), abs(weight_er))

  x_end <- -weight_rt * scale_factor
  y_end <- -weight_er * scale_factor

  x_start <- -x_end
  y_start <- -y_end

  angle <- atan2(y_end, x_end) * 180 / pi

  list(
    angle = angle,
    x_start = x_start,
    y_start = y_start,
    x_end = x_end,
    y_end = y_end
  )
}

bis_vectors <- optimal_weights |>
  rowwise() |>
  mutate(
    bis = list(bis_vector(rt_weight)),
    x_start_bis = bis$x_start,
    y_start_bis = bis$y_start,
    x_end_bis   = bis$x_end,
    y_end_bis   = bis$y_end
  ) |>
  ungroup() |>
  select(task, session, rt_weight, x_start_bis, y_start_bis, x_end_bis, y_end_bis) |>
  mutate(
    task = case_when(
      task == "axcpt" ~ "AX-CPT",
      task == "cuedts" ~ "Cued TS",
      task == "sternberg" ~ "Sternberg",
      task == "stroop" ~ "Stroop",
      TRUE ~ task
    )
  )



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
  geom_segment(
    data = bis_vectors,
    aes(
      x = x_start_bis,
      y = y_start_bis,
      xend = x_end_bis,
      yend = y_end_bis
    ),
    arrow = arrow(length = unit(1, "cm")),
    linewidth = 2,
    color = "black",
    # linetype = "dashed",
    inherit.aes = FALSE
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

ggsave('5_other_scripts/comp_distrib_bis/bis_distrib.png', plot = grid_plot, width = 20, height = 27)

system('open 5_other_scripts/comp_distrib_bis/bis_distrib.png')
