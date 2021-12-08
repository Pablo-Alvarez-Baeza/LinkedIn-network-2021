# Load packages -----------------------------------------------------------


library(pacman)
p_load(tidyverse, janitor, lubridate, ggbeeswarm, ggtext, showtext, sysfonts, packcircles)

font_add_google("Lato")
font_add("Lato", regular = "~~/Downloads/Lato-Bold.ttf")
showtext_auto()

# Set theme
theme_set(theme_minimal(base_family = "Lato"))


# Load data ---------------------------------------------------------------


raw_data <- read_csv("result.csv")

df <- raw_data |>
  clean_names() |> 
  mutate(connection_since = as.Date(connection_since, "%d/%m/%y"),
         network = factor(network),
         connection = factor(connection)) |> 
  as.data.frame()

df |> glimpse()


# Beeswarm plot -----------------------------------------------------------


df |>
  mutate(label_network = if_else(connection == 1, "", first_name)) |> 
  ggplot(aes(1, connection_since, color = network, size = connection)) +
  geom_beeswarm(alpha = .8,
                cex = 5) +
  geom_text(aes(label = label_network),
            position = position_beeswarm(cex = 5) ,
            size = 2,
            color = "black",
            family = "Lato") +
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


# Pack circle -------------------------------------------------------------


df <- raw_data |>
  clean_names() |> 
  as.data.frame()

df <- df |>
  select(first_name, connection, network) |> 
  # Set size for circles
  mutate(connection = case_when(connection == 2 ~ 12,
                                TRUE ~ connection))
# Arrange connection to have bigger circles inside the smaller ones
  arrange(desc(connection), first_name)

# Generate the layout. sizetype can be area or radius, following your preference on what to be proportional to value.
packing <- circleProgressiveLayout(df$connection, sizetype='area')
# Radius for separating the circles
packing$radius <- 0.90 * packing$radius
data <- cbind(df, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Final plot
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "white") +
  scale_fill_manual(values = rep("white", 232)) +
  geom_text(data = filter(data, connection == 12),
            aes(x, y, size = connection, label = first_name),
            size = 5,
            family = "Lato",
            fontface = "bold") +
  scale_size_continuous(range = c(1,4)) +
  coord_equal() +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0), "null"),
        legend.position = "none") 

# Saving plot
ggsave('plot.jpg', width = 10, height = 10, dpi = 300)
