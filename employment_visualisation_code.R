#========================================================#
# Yorkshire Industry Employment Analysis 2015–2024
# Composite Visualisation for IJC445
#========================================================#

#-------------------------#
# Libraries
#-------------------------#
library(readxl)      # read Excel files
library(tidyverse)   # data manipulation & plotting
library(stringr)     # string cleaning
library(ggplot2)     # plotting
library(ggpubr)      # combine multiple plots
library(ggrepel)     # prevent overlapping labels
library(scales)      # formatting numbers

#-------------------------#
# Load Data
#-------------------------#
bres <- read_excel("bres_yorkshire_industry.xlsx")

#-------------------------#
# Data Cleaning
#-------------------------#
bres <- bres %>%
  select(-contains("Flags")) %>%  # remove unnecessary flag columns
  mutate(
    Industry = Industry %>%
      str_replace("^\\d+\\s*:\\s*", "") %>%  # remove numbering
      str_replace("\\s*\\([^)]*\\)$", "")    # remove trailing brackets
  )

# Convert to long format for easier plotting
bres_long <- bres %>%
  pivot_longer(cols = `2015`:`2024`, names_to = "Year", values_to = "Employees") %>%
  mutate(Year = as.numeric(Year))

#-------------------------#
# Helper Function to Save Plots
#-------------------------#
save_plot <- function(plot, filename, width = 12, height = 12, dpi = 300){
  ggsave(filename, plot, width = width, height = height, dpi = dpi)
}

#-------------------------#
# Chart A / Figure 2: Line Chart – Employment Trends
#-------------------------#
line_chart <- ggplot(bres_long, aes(x = Year, y = Employees, colour = Industry)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(title = "Employment Trends by Industry (2015–2024)",
       x = "Year",
       y = "Number of Employees",
       colour = "Industry") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 20),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.25, "cm")
  ) +
  annotate("text", x = min(bres_long$Year), 
           y = max(bres_long$Employees)*1.05, 
           label = "A", size = 12, fontface = "bold", hjust = 0)

save_plot(line_chart, "line_chart.png", width = 16, height = 12)

#-------------------------#
# Chart B / Figure 3: Heatmap – Relative Employment Change
#-------------------------#
bres_change <- bres_long %>%
  filter(Year %in% c(2019, 2021)) %>%
  pivot_wider(names_from = Year, values_from = Employees) %>%
  mutate(Change_Percent = (`2021` - `2019`) / `2019` * 100) %>%
  arrange(Change_Percent) %>%
  mutate(Industry = factor(Industry, levels = Industry))

heatmap_chart <- ggplot(bres_change, aes(x = Industry, y = 1, fill = Change_Percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green", midpoint = 0,
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Relative Employment Change by Industry (2019–2021)",
    x = NULL,
    y = NULL,
    fill = "% Change"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 80, hjust = 1, size = 20),
    plot.title = element_text(face = "bold", size = 23, hjust = 0.5),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  annotate("text", x = 1, y = 1.2, label = "B", size = 12, fontface = "bold", hjust = 0)

save_plot(heatmap_chart, "heatmap_chart.png", width = 16, height = 12)

#-------------------------#
# Chart C / Figure 4: Small Multiples – Top 5 Industries
#-------------------------#
top5_industries <- bres_change %>%
  slice(1:5) %>%
  pull(Industry)

bres_top5 <- bres_long %>% filter(Industry %in% top5_industries)

small_multiples <- ggplot(bres_top5, aes(x = Year, y = Employees)) +
  geom_line(colour = "steelblue", size = 1.5) +
  geom_point(colour = "steelblue", size = 3) +
  facet_wrap(~Industry, scales = "free_y", labeller = label_wrap_gen(width = 15)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2015, 2024, 2)) +
  labs(title = "Employment Trends for Top 5 Most Affected Industries",
       x = "Year",
       y = "Number of Employees") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 20),
    panel.spacing = unit(1, "lines")
  ) +
  annotate("text", x = min(bres_top5$Year), 
           y = max(bres_top5$Employees)*1.05, 
           label = "C", size = 12, fontface = "bold", hjust = 0)

save_plot(small_multiples, "small_multiples.png", width = 13, height = 12)

#-------------------------#
# Chart D / Figure 5: Scatterplot – Pre-Pandemic Size vs Recovery Rate
#-------------------------#
bres_recovery <- bres_long %>%
  filter(Year %in% c(2019, 2024)) %>%
  pivot_wider(names_from = Year, values_from = Employees) %>%
  mutate(Recovery_Rate = `2024` / `2019`)

scatter_chart <- ggplot(bres_recovery, aes(x = `2019`, y = Recovery_Rate, label = Industry)) +
  geom_point(colour = "darkorange", size = 5) +
  geom_text_repel(size = 5, max.overlaps = 20, box.padding = 0.5, point.padding = 0.5, segment.color = "grey50") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  labs(title = "Pre-Pandemic Employment Size vs Recovery Rate (2024/2019)",
       x = "Employment in 2019",
       y = "Recovery Rate (%)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 16)
  ) +
  annotate("text", x = min(bres_recovery$`2019`), 
           y = max(bres_recovery$Recovery_Rate)*1.05, 
           label = "D", size = 12, fontface = "bold", hjust = 0)

save_plot(scatter_chart, "scatter_chart.png", width = 12, height = 12)

#-------------------------#
# Composite Visualisation
#-------------------------#
composite_plot <- ggarrange(
  line_chart, heatmap_chart, small_multiples, scatter_chart,
  ncol = 2, nrow = 2,
  common.legend = FALSE
)

save_plot(composite_plot, "composite_visualisation.png", width = 32, height = 24)

#========================================================#
# End of Script
#========================================================#
