library(tidyverse)

icc_trends <- tibble(
  weighting = seq(0, 1, by = 0.05),
  fisher_z_icc = 1 - 0.4 * weighting - 1.5 * (weighting - 0.5)^2,
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
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1)
  ) +
  labs(
    x = "Weight of RT",
    y = "Fisher z-standardized ICC",
    color = "Trend"
  ) +
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
    axis.title.x      = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y      = element_text(size = 20, face = "bold", color = "black"),
    axis.text         = element_text(size = 20, color = "black"),
    legend.title      = element_text(size = 20, face = "bold", color = "black"),
    legend.text       = element_text(size = 20, color = "black"),
    axis.ticks        = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(0.3, "cm"),

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

ggsave(plot = hyp_plot, filename = '5_other_scripts/visualization_hyp/vis_hyp.png', width = 10, height = 6)

system('open 5_other_scripts/visualization_hyp/vis_hyp.png')
