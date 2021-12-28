# Load packages -----------------------------------------------------------
library(pacman)
p_load(tidyverse, janitor, lubridate, ggbeeswarm, ggtext, showtext, sysfonts, packcircles)

font_add_google("Lato")
showtext_auto()

# Set theme
theme_set(theme_void(base_family = "Lato"))


# Load data ---------------------------------------------------------------
raw_data <- read_csv("result.csv")


# Text plot ---------------------------------------------------------------
df <- raw_data |> 
  clean_names() |> 
  select(first_name, month, network) |>
  group_by(month) |> 
  arrange(month, desc(first_name)) |> 
  mutate(rank = row_number()) 
  
df |> 
  group_by(month) |> 
  slice_max(rank, n = 1)

df |> 
  ggplot(aes(month, rank, label = first_name, color = network)) +
  geom_text(size = 3,
            hjust = 0,
            family = "Lato",
            fontface = "bold")  +
  coord_cartesian(xlim = c(1, 12),
                    ylim = c(1, 35),
                    clip = "off") +
  scale_color_manual(values = c(
    "work" = "black",
    "ld" = "#273BD4",
    "dv" = "#D41159",
    "other" = "grey60")
    ) +
  labs(x = NULL,
       y = NULL,
       title = "2021: My LinkedIn connections",
       subtitle = "<span style='color:#273BD4'>L&D <span></span><span style='color:#D41159'> Data Viz </span><span style='color:black'> Work</span><span style='color:grey60'> Other</span>",
       caption = "Visualization by Pablo Alvarez") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.margin = margin(20, 40, 20, 40),
    plot.title = element_text(color = "black", 
                              size = 30, 
                              face = "bold",
                              hjust = 0,
                              margin = margin(t = 20)),
    plot.subtitle = element_markdown(size = 15, 
                                     hjust = 0,
                                     margin = margin(t = 10,
                                                     b = 80),
                                     family = "Lato",
                                     face = "bold"),
    plot.caption = element_text(color = "black",
                                size = 8,
                                hjust = 0.5,
                                margin = margin(t = 20, b = 20)),
    legend.position = "none"
  )

ggsave("linkedin_connections_2021.png", width = 10, height = 10, units = "in", dpi = 320)

 