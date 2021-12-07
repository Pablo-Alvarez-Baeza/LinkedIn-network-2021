library(pacman)
p_load(tidyverse, janitor, lubridate, ggbeeswarm, ggtext, showtext, sysfonts)

font_add("Fuzzy Bubbles", regular = "~~/Downloads/FuzzyBubbles-Bold.ttf")
showtext_auto()


raw_data <- read_csv("result.csv")

df <- raw_data |>
  clean_names() |> 
  mutate(connection_since = as.Date(connection_since, "%d/%m/%y"),
         month = month(connection_since),
         network = factor(network)) |> 
  as.data.frame()

df |> glimpse()



df |>
  mutate(label_network = if_else(connection == 1, "", first_name),
         connection = factor(connection)) |> 
  ggplot(aes(1, connection_since, color = network, size = connection)) +
  geom_beeswarm(alpha = .5,
                cex = 5) +
  scale_size_manual(values = c(1, 4)) +
  geom_text(aes(label = label_network),
            position=position_beeswarm(cex = 5) ,
            size = 2,
            color = "black",
            family="Fuzzy Bubbles") +
  coord_flip() +
  theme_minimal() +
  scale_color_manual(values = c(
    "w" = "grey25",
    "ld" = "blue",
    "dv" = "purple",
    "other" = "grey85"
  )) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank()
  ) +
  labs(x = NULL,
       y = NULL)
