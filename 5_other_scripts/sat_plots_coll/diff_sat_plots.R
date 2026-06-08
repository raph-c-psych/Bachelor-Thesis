# ---- loading packages ----
library(tidyverse)




# ---- create raw data ----
set.seed(14)
raw_data <- tibble(
  ID = 1:10,
  true_val = rnorm(10),
  sat_val = sample(c(-1, 1), size = 10, replace = TRUE)
)





# ----- no sat plot ----
no_sat_data <- raw_data |>
  mutate(
    rt_val = true_val / 2,
    er_val = true_val / 2,
  )

no_plot_data <- no_sat_data |>
  select(ID, rt_val, er_val) |>
  pivot_longer(
    cols = -ID,
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    component = factor(
      component,
      levels = c(
        "rt_val",
        "true_val",
        "er_val"
      )
    )
  )


no_sat <- ggplot(no_plot_data,
       aes(x = component,
           y = value,
           fill = component)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~ ID) +
  scale_y_continuous(labels = abs) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Component"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    # remove axis text
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    # remove axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )


ggsave(plot = no_sat, filename = 'other_plots/sat_plots/version2/no_sat.png', width = 8, height = 5)





# ----- clean sat plot ----
clean_sat_data <- raw_data |>
  mutate(
    rt_val = true_val / 2 * sat_val,
    er_val = true_val / 2 * (- sat_val),
  )

clean_plot_data <- clean_sat_data |>
  select(ID, rt_val, er_val) |>
  pivot_longer(
    cols = -ID,
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    component = factor(
      component,
      levels = c(
        "rt_val",
        "true_val",
        "er_val"
      )
    )
  )


clean_sat <- ggplot(clean_plot_data,
       aes(x = component,
           y = value,
           fill = component)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~ ID) +
  scale_y_continuous(labels = abs) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Component"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    # remove axis text
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    # remove axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(plot = clean_sat, filename = 'other_plots/sat_plots/version2/clean_sat.png', width = 8, height = 5)




# ----- shift sat plot ----
shift_sat_data <- raw_data |>
  mutate(
    rt_val = true_val / 8 * sat_val,
    er_val = true_val / sqrt(2) * (- sat_val),
  )

shift_plot_data <- shift_sat_data |>
  select(ID, rt_val, er_val) |>
  pivot_longer(
    cols = -ID,
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    component = factor(
      component,
      levels = c(
        "rt_val",
        "true_val",
        "er_val"
      )
    )
  )


shift_sat <- ggplot(shift_plot_data,
       aes(x = component,
           y = value,
           fill = component)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~ ID) +
  scale_y_continuous(labels = abs) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Component"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    # remove axis text
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    # remove axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(plot = shift_sat, filename = 'other_plots/sat_plots/version2/shift_sat.png', width = 8, height = 5)