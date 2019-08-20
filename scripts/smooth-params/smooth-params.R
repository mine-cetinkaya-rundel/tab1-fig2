# load packages ----------------------------------------------------------------
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(here)

# define colors ----------------------------------------------------------------
beluga <- "#4A4843"
living_coral <- "#FA7268"
turkish_sea <- "#195190"
limpet_shell <- "#98DDDE"
turtle_green <- "#81894E"

# filter out Setosas -----------------------------------------------------------
iris_nonsetosa <- iris %>%
  filter(Species != "setosa")

# visualize the relationship ---------------------------------------------------
p_smooth <- ggplot(iris_nonsetosa) +
  geom_point(
    aes(x = Sepal.Width, y = Petal.Length, color = Species)
  ) +
  geom_smooth(
    aes(x = Sepal.Width, y = Petal.Length), 
    method = "loess", span = 0.375, color = beluga
  ) +
  labs(
    x = "Sepal Width",
    y = "Petal Length"
  ) +
  scale_color_manual(
    values = c("versicolor" = turkish_sea, "virginica" = limpet_shell),
    labels = c("Versicolor", "Virginica")
  )

ggsave(p_smooth, 
       filename = here::here("scripts/smooth-params/", "iris-smooth.png"), 
       width = 7, height = 4, 
       dpi = "retina", bg = "transparent")
