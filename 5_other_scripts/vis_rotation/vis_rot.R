library(tidyverse)
library(lme4)
library(sjPlot)
library(boot)
library(psych)
library(patchwork)


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



# ---- visualizing the rotation of BIS with weight 0.5 ----

temp_data <- all_data |>
  filter(phase == 'test') |>
  filter(task == 'cuedts' & session == 'proactive')

plot_vector50 <- ggplot(data = temp_data) + 
  # scatter plot of data
  geom_point(
    aes(x = scale(mean_RT), y = scale(ER)),
    color = "black",
    size = 3.5,
    alpha = 0.5
  ) +
  # vectors and their annotation
  annotate(
    "segment",
    x = 2.8, y = 2.8,
    xend = -2.8, yend = -2.8,
    arrow = arrow(length = unit(1, "cm")),
    linewidth = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  annotate(
    "segment",
    x = -2.8, y = 2.8,
    xend = 2.8, yend = -2.8,
    arrow = arrow(length = unit(1, "cm")),
    linewidth = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  annotate( # BIS
    "text", 
    x = -2.4, 
    y = -2.0, 
    label = "BIS",  
    hjust = 1.1, 
    vjust = 1.1, 
    size = 9, 
    angle = 45,
    fontface = "bold"
  ) +
  annotate("text", x = 2.65, y =  -2.5, label = "SAT",   hjust = 1.1, vjust = -0.1, size = 9, angle = -45, fontface = "bold") +
  # Labeling of axis and legend
  labs(
    x = "Reaction Time (z-standardized)", 
    y = "Error Rate (z-standardized)",
    title = 'a. Weight of RT = 0.5'
  ) +
  
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
    panel.border = element_blank()
  )

plot_vector50

ggsave('5_other_scripts/vis_rotation/vector50.png', plot = plot_vector50, width = 13, height = 10)

system('open 5_other_scripts/vis_rotation/vector50.png')



# ---- plot vector .20 ----

# -- calculate rotation of vector
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
bis_vector(0)
bis_vector(1)
bis_rot <- bis_vector(0.2)

bis_text_angle <- bis_rot$angle + 180
if (bis_text_angle > 90 && bis_text_angle < 270) {
  bis_text_angle <- bis_text_angle - 180
}

# BIS
x_start_bis <- bis_rot$x_start
y_start_bis <- bis_rot$y_start
x_end_bis <- bis_rot$x_end
y_end_bis <- bis_rot$y_end


# SAT: perpendicular to BIS
x_start_sat <- -y_start_bis
y_start_sat <-  x_start_bis

x_end_sat <- -y_end_bis
y_end_sat <-  x_end_bis


# -- create quarter circle

radius <- 2.9

angles <- seq(pi, 3 * pi / 2, length.out = 100)

# Filled sector
quarter_sector <- tibble(
  x = c(0, radius * cos(angles), 0),
  y = c(0, radius * sin(angles), 0)
)

# Arc only
quarter_circle <- tibble(
  x = radius * cos(angles),
  y = radius * sin(angles)
)

# -- plot graph

plot_vector20 <- ggplot(data = temp_data) + 
  # scatter plot of data
  geom_point(
    aes(x = scale(mean_RT), y = scale(ER)),
    color = "black",
    size = 3.5,
    alpha = 0.5
  ) +
  geom_path(
    data = quarter_circle,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_polygon(
    data = quarter_sector,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.4,
    color = NA
  ) +
  # vectors and their annotation
  annotate( # BIS
    "segment",
    x = x_start_bis, y = y_start_bis,
    xend = x_end_bis, yend = y_end_bis,
    arrow = arrow(length = unit(1, "cm")),
    linewidth = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  annotate( # SAT
    "segment",
    x = x_start_sat, y = y_start_sat,
    xend = x_end_sat, yend = y_end_sat,
    arrow = arrow(length = unit(1, "cm")),
    linewidth = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  annotate( # BIS
    "text", 
    x = -0.8, 
    y = -1.8, 
    label = "BIS",  
    hjust = 1.1, 
    vjust = 1.1, 
    size = 9, 
    angle = bis_text_angle,
    fontface = "bold"
  ) +
  annotate("text", x = 2.5, y =  -0.6, label = "SAT",   hjust = 1.1, vjust = -0.1, size = 9, angle = bis_text_angle - 90, fontface = "bold") +
  # Labeling of axis and legend
  labs(
    x = "Reaction Time (z-standardized)", 
    y = "Error Rate (z-standardized)",
    title = 'b. Weight of RT = 0.2'
  ) +
  
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
    panel.border = element_blank()
  )

ggsave('5_other_scripts/vis_rotation/vector20.png', plot = plot_vector20, width = 13, height = 10)

system('open 5_other_scripts/vis_rotation/vector20.png')



double_plot <- plot_vector50 | plot_vector20

ggsave('5_other_scripts/vis_rotation/double_plot.png', plot = double_plot, width = 20, height = 10)

system('open 5_other_scripts/vis_rotation/double_plot.png')
