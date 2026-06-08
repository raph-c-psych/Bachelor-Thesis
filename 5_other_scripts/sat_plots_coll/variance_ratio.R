# ---- loading packages ----
library(tidyverse)


# ---- create raw data  ----
set.seed(14)
raw_data <- tibble(
  ID = 1:10,
  true_var = rnorm(10),
  error_var = rep(0.2, times = 10)
)


# --- create no sat plot ---
plot_data_no_sat <- raw_data |>
  mutate(
    true_var_rt = true_var / 2,
    true_var_er = -true_var / 2,
    error_var_rt = error_var,
    error_var_er = -error_var
  ) |>
  select(ID, true_var, true_var_rt, error_var_rt,
         true_var_er, error_var_er) |>
  pivot_longer(
    cols = c(true_var_rt, error_var_rt,
             true_var_er, error_var_er),
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    value_plot = case_when(
      component %in% c("true_var_er", "error_var_er") ~ -abs(value),
      TRUE ~ abs(value)
    ),
    fill_group = case_when(
      component %in% c("error_var_rt", "error_var_er") ~ "error",
      true_var > 0 ~ "positive",
      true_var < 0 ~ "negative",
      TRUE ~ "zero"
    )
  )

no_plot <- ggplot(plot_data_no_sat,
       aes(x = factor(ID),
           y = value_plot,
           fill = fill_group)) +
  geom_col(position = "stack", width = 0.25) +
  annotate(
    "text",
    x = 10.3,
    y = -0.2,
    label = "ER True",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = 0.2,
    label = "RT True",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = -0.7,
    label = "ER Res",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = 0.7,
    label = "RT Res",
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(
    values = c(
      positive = "forestgreen",
      negative = "red",
      error = "black",
      zero = "grey50"
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    fill = 'variance component'
  ) +
  theme_minimal()+ 
  theme(
    # remove axis text
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    # remove axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # remove ticks
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(plot = no_plot, filename = 'other_plots/sat_plots/no_sat.png')



# ---- create clean sat plot ----
plot_data_clean_sat <- raw_data |>
  mutate(
    true_var_rt = true_var / 2,
    true_var_er = -true_var / 2,
    error_var_rt = error_var,
    error_var_er = -error_var
  ) |>
  select(ID, true_var, true_var_rt, error_var_rt,
         true_var_er, error_var_er) |>
  pivot_longer(
    cols = c(true_var_rt, error_var_rt,
             true_var_er, error_var_er),
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    value_plot = case_when(
      component %in% c("true_var_er", "error_var_er") ~ -abs(value),
      TRUE ~ abs(value)
    ),
    fill_group = case_when(
      component %in% c("error_var_rt", "error_var_er") ~ "error",

      component == "true_var_rt" & true_var > 0 ~ "green",
      component == "true_var_rt" & true_var < 0 ~ "red",

      component == "true_var_er" & true_var > 0 ~ "red",
      component == "true_var_er" & true_var < 0 ~ "green",

      TRUE ~ "grey50"
    )
  )

clean_plot <- ggplot(plot_data_clean_sat,
       aes(x = factor(ID),
           y = value_plot,
           fill = fill_group)) +
  geom_col(position = "stack", width = 0.25) +
  annotate(
    "text",
    x = 10.3,
    y = -0.2,
    label = "ER True",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = 0.2,
    label = "RT True",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = -0.7,
    label = "ER Res",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = 0.7,
    label = "RT Res",
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(
    values = c(
      green = "forestgreen",
      red   = "red",
      error = "black",
      grey50 = "grey50"
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    fill = 'variance component'
  ) +
  theme_minimal()+ 
  theme(
    # remove axis text
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    # remove axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # remove ticks
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(plot = clean_plot, filename = 'other_plots/sat_plots/clean_sat.png')


# --- create shifting sat plot ---
plot_data_shift_sat <- raw_data |>
  mutate(
    true_var_rt = true_var / 4,
    true_var_er = -true_var / sqrt(2),
    error_var_rt = error_var,
    error_var_er = -error_var
  ) |>
  select(ID, true_var, true_var_rt, error_var_rt,
         true_var_er, error_var_er) |>
  pivot_longer(
    cols = c(true_var_rt, error_var_rt,
             true_var_er, error_var_er),
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    value_plot = case_when(
      component %in% c("true_var_er", "error_var_er") ~ -abs(value),
      TRUE ~ abs(value)
    ),
    fill_group = case_when(
      component %in% c("error_var_rt", "error_var_er") ~ "error",

      component == "true_var_rt" & true_var > 0 ~ "green",
      component == "true_var_rt" & true_var < 0 ~ "red",

      component == "true_var_er" & true_var > 0 ~ "red",
      component == "true_var_er" & true_var < 0 ~ "green",

      TRUE ~ "grey50"
    )
  )

shift_plot <- ggplot(plot_data_shift_sat,
       aes(x = factor(ID),
           y = value_plot,
           fill = fill_group)) +
  geom_col(position = "stack", width = 0.25) +
  annotate(
    "text",
    x = 10.3,
    y = -0.2,
    label = "ER True",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = 0.2,
    label = "RT True",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = -0.7,
    label = "ER Res",
    size = 3
  ) +
  annotate(
    "text",
    x = 10.3,
    y = 0.7,
    label = "RT Res",
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(
    values = c(
      green = "forestgreen",
      red   = "red",
      error = "black",
      grey50 = "grey50"
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    fill = 'variance component'
  ) +
  theme_minimal()+ 
  theme(
    # remove axis text
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    # remove axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # remove ticks
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(plot = shift_plot, filename = 'other_plots/sat_plots/shift_sat.png')


# make create 10 little plots