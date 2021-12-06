library(pacman)
p_load(tidyverse, janitor, lubridate, ggbeeswarm)

raw_data <- read_csv("result.csv")

df <- raw_data |>
  clean_names() |> 
  mutate(connection_since = as.Date(connection_since, "%d/%m/%y"),
         month = month(connection_since),
         network = factor(network)) |> 
  as.data.frame()

df |> glimpse()

df |>
  group_by(month) |> 
  mutate(value = rnorm(n()),
         factor_n = factor(1)) |> 
  ggplot(aes(factor_n, connection_since, color = network)) +
  geom_beeswarm(aes(size = connection), priority = "random", cex = 2) +
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



           

df |>
  group_by(month) |> 
  mutate(value = rnorm(n()),
         factor_n = factor(1),
         size_network = case_when(connection == 1 ~ 1,
                                  connection == 2 ~ 1)) |> 
  ggplot(aes(value, connection_since, color = network)) +
  geom_beeswarm(groupOnX = FALSE) +
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


         