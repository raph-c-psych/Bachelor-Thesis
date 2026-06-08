library(tidyverse)

icc_trends <- tibble(
  weighting = seq(0, 1, by = 0.05),
  fisher_z_icc = 0.7 - 0.2 * weighting - 0.50 * (weighting - 0.5)^2,
)

hyp_plot <- ggplot(icc_trends,
       aes(x = weighting,
           y = fisher_z_icc)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1)
  ) +
  labs(
    x = "RT Weighting",
    y = "Fisher z-standardized ICC",
    color = "Trend"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave(plot = hyp_plot, filename = 'other_plots/visualization_hyp/vis_hyp.png', width = 5, height = 3)
