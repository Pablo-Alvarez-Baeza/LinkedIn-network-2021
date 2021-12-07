library(pacman)
p_load(tidyverse, janitor, lubridate, ggbeeswarm, ggtext, showtext, sysfonts)

font_add("Fuzzy Bubbles", regular = "~~/Downloads/FuzzyBubbles-Bold.ttf")
showtext_auto()

theme_set(theme_minimal(base_family = "Lato"))



raw_data <- read_csv("result.csv")

df <- raw_data |>
  clean_names() |> 
  mutate(connection_since = as.Date(connection_since, "%d/%m/%y"),
         network = factor(network),
         connection = factor(connection)) |> 
  as.data.frame()

df |> glimpse()



df |>
  mutate(label_network = if_else(connection == 1, "", first_name)) |> 
  ggplot(aes(1, connection_since, color = network, size = connection)) +
  geom_beeswarm(alpha = .8,
                cex = 5) +
  geom_text(aes(label = label_network),
            position = position_beeswarm(cex = 5) ,
            size = 2,
            color = "black",
            family = "Fuzzy Bubbles") +
  coord_flip() +
  scale_y_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual(values = c(
    "w" = "#7e5000",
    "ld" = "#449eff",
    "dv" = "#fb3753",
    "other" = "#BBBBBB"
  ),
  labels = c(
    "Work", "L&D", "Data Viz", "Other"
  )) +
  scale_size_manual(values = c(2, 3.5),
                    guide = 'none') +
  theme(
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    panel.background = element_rect(fill = "grey98", color = "grey98"),
    legend.position = "top",
    legend.justification = c(0, 1),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      family = "Lato",
    )
    )
