# load packages ----------------------------------------------------------------
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(broom)
library(here)

# define colors ----------------------------------------------------------------
beluga <- "#4A4843"
living_coral <- "#FA7268"
turkish_sea <- "#195190"
limpet_shell <- "#98DDDE"
turtle_green <- "#81894E"

# split data -------------------------------------------------------------------
iris_setosa <- iris %>%
  filter(Species == "setosa")

iris_nonsetosa <- iris %>%
  filter(Species != "setosa")

# run regression ---------------------------------------------------------------
lm(Petal.Length ~ Sepal.Width, data = iris) %>% tidy()


# visualize non-setosas --------------------------------------------------------
p_nonsetosa <- ggplot(iris_nonsetosa) +
  geom_point(aes(x = Sepal.Width, y = Petal.Length, color = Species)) +
  geom_smooth(aes(x = Sepal.Width, y = Petal.Length), 
              method = "lm", color = beluga) +
  labs(
    x = "Sepal Width",
    y = "Petal Length"
  ) +
  scale_color_manual(
    values = c("versicolor" = turkish_sea, "virginica" = limpet_shell),
    labels = c("Versicolor", "Virginica")
  )

ggsave(p_nonsetosa, 
       filename = here::here("scripts/tab1-fig2/", "iris-nonsetosa.png"), 
       width = 7, height = 4, 
       dpi = "retina", bg = "transparent")

# plot with color --------------------------------------------------------------
p_nonsetosa_hl <- ggplot(iris_nonsetosa) +
  geom_point(aes(x = Sepal.Width, y = Petal.Length, color = Species)) +
  geom_smooth(aes(x = Sepal.Width, y = Petal.Length), 
              method = "lm", color = living_coral, size = 3) +
  labs(
    x = "Sepal Width",
    y = "Petal Length"
  ) +
  scale_color_manual(
    values = c("versicolor" = turkish_sea, "virginica" = limpet_shell),
    labels = c("Versicolor", "Virginica")
    )

ggsave(p_nonsetosa_hl, 
       filename = here::here("scripts/tab1-fig2/", "iris-nonsetosa-hl.png"), 
       width = 7, height = 4, 
       dpi = "retina", bg = "transparent")

# plot with all three species --------------------------------------------------
p_all <- ggplot(iris) +
  geom_point(aes(x = Sepal.Width, y = Petal.Length, color = Species)) +
  geom_smooth(aes(x = Sepal.Width, y = Petal.Length), 
              method = "lm", color = beluga) +
  labs(
    x = "Sepal Width",
    y = "Petal Length"
  ) +
  scale_color_manual(
    values = c("setosa" = turtle_green,
               "versicolor" = turkish_sea, 
               "virginica" = limpet_shell),
    labels = c("Setosa", "Versicolor", "Virginica")
  )

ggsave(p_all, 
       filename = here::here("scripts/tab1-fig2/", "iris-all.png"), 
       width = 7, height = 4, 
       dpi = "retina", bg = "transparent")

# how did this happen ----------------------------------------------------------
# https://carbon.now.sh

## 1

# fit model
model <- lm(Petal.Length ~ Sepal.Width, data = iris)

# print model summary
tidy(model)

## 2

# visualize the relationship
ggplot(iris) +
  geom_point(
    aes(x = Sepal.Width, y = Petal.Length, color = Species)
  ) +
  geom_smooth(
    aes(x = Sepal.Width, y = Petal.Length), 
    method = "lm"
  )

## 3

# filter out Setosas
iris_nonsetosa <- iris %>%
  filter(Species != "setosa")

# visualize the relationship
ggplot(iris_nonsetosa) +
  geom_point(
    aes(x = Sepal.Width, y = Petal.Length, color = Species)
  ) +
  geom_smooth(
    aes(x = Sepal.Width, y = Petal.Length), 
    method = "lm"
  )









