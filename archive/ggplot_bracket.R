library(ggplot2)

# Data for segments and labels
bracket_data <- data.frame(
  x = c(1, 1, 2, 2, 3, 3, 4, 4, 5),
  xend = c(2, 2, 3, 3, 4, 4, 5, 5, 5),
  y = c(8, 2, 8, 2, 6, 4, 6, 4, 5),
  yend = c(8, 2, 6, 4, 6, 4, 5, 5, 5)
)

labels <- data.frame(
  x = c(1, 1, 3, 3, 5),
  y = c(8, 2, 6, 4, 5),
  label = c("Team 1", "Team 2", "Winner 1", "Winner 2", "Champion")
)

# Plot
ggplot() +
  # Draw bracket segments
  geom_segment(data = bracket_data, aes(x = x, y = y, xend = xend, yend = yend), size = 1) +
  # Add labels for teams and winners
  geom_text(data = labels, aes(x = x, y = y, label = label), size = 5, hjust = 1.2) +
  # Customize the theme
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(clip = "off") +
  ggtitle("Tournament Bracket")


