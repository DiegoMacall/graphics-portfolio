## Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(extrafont)
library(ggplot2)
library(patchwork)
library(dplyr)

## Data prep -------------------------------

g7 <- c(
  "Canada", "France", "Germany", "Italy",
  "Japan", "United Kingdom", "United States"
)

brics <- c(
  "Brazil", "Russian Federation", "India",
  "China", "South Africa"
)

other_countries <- dumbbell0$Country[!dumbbell0$Country %in% c(g7, brics)]

dumbbell0 <- dumbbell0 %>%
  mutate(
    Country = factor(
      Country,
      levels = c(g7, brics, unique(other_countries))
    )
  )

seg_data <- dumbbell0 %>%
  group_by(Country) %>%
  summarise(
    xmin = min(`Births per woman`, na.rm = TRUE),
    xmax = max(`Births per woman`, na.rm = TRUE),
    .groups = "drop"
  )

lvl <- levels(dumbbell0$Country)

g7_idx     <- which(lvl %in% g7)
brics_idx  <- which(lvl %in% brics)
non_g7_idx <- which(!lvl %in% g7)

y_g7_mid    <- mean(g7_idx)
y_brics_mid <- mean(brics_idx)

x_min <- min(dumbbell0$`Births per woman`, na.rm = TRUE)
x_lab <- x_min - 0.25

## 1. Main dumbbell plot -------------------------------------------------

p_dumbbell <-
  ggplot() +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = min(non_g7_idx) - 0.5,
      ymax = max(non_g7_idx) + 0.5
    ),
    inherit.aes = FALSE,
    fill = "white",
    alpha = 0.6
  ) +
  geom_segment(
    data = seg_data,
    aes(x = xmin, xend = xmax, y = Country, yend = Country),
    linewidth = 1,
    color = "gray"
  ) +
  geom_hline(
    yintercept = which(lvl == "Brazil") - 0.5,
    linetype = "dashed",
    linewidth = 0.9,
    color = "black"
  ) +
  geom_point(
    data = dumbbell0,
    aes(x = `Births per woman`, y = Country, color = factor(Year)),
    size = 3.8
  ) +
  annotate(
    "text",
    x = x_lab, y = y_brics_mid,
    label = "BRICS",
    fontface = "bold",
    size = 4,
    hjust = 0.4
  ) +
  annotate(
    "text",
    x = x_lab, y = y_g7_mid,
    label = "G7",
    fontface = "bold",
    size = 4,
    hjust = 1
  ) +
  scale_color_manual(
    name = "Year",
    values = c("2018" = "#1B9E77", "2021" = "#D95F02")
  ) +
  labs(
    title = "Fertility Rates: BRICS vs G7",
    subtitle = "Births per woman in 2018 and 2021",
    x = "Births per woman",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(face = "bold", color = "black"),
    plot.title = element_text(face = "bold", color = "black", size = 16),
    plot.subtitle = element_text(face = "bold", color = "black", size = 12),
    legend.title = element_text(face = "bold", color = "black"),
    legend.text  = element_text(face = "bold", color = "black"),
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5.5, 5.5, 5.5, 35)
  ) +
  coord_cartesian(clip = "off")

## 2. Population data ----------------------------------------

pop_df <- tibble(
  Bloc   = c("BRICS", "G7"),
  Pop    = c(4395683907, 785614630),
  Share  = c(0.54, 0.10),
  ShareLabel = c("54%", "10%")
)

# same color as above, but now by bloc
bloc_cols <- c("BRICS" = "#1B9E77", "G7" = "#D95F02")

p_pie <-
  ggplot(pop_df, aes(x = "", y = Pop, fill = Bloc)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = bloc_cols) +
  geom_text(
    aes(
      label = paste0(Bloc, "\n", ShareLabel),
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4,
    lineheight = 0.9
  ) +
  labs(title = "Share of Total Global Population in 2024:\nBRICS vs G7") +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      color = "black",
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    text = element_text(face = "bold", color = "black"),
    legend.position = "none"
  )

## 3. Combine with patchwork ---------------------------------------------

final_plot <-
  (p_dumbbell | (p_pie)) +     
  plot_layout(
    widths = c(3, 2),                  
    heights = c(1.5, 1)                  
  ) +
  plot_annotation(
    caption = "Author: Diego M. Macall | Source: World Bank 2025"
  )

final_plot