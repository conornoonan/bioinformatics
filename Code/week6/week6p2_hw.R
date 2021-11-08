libraries <- c("tidyverse", "knitr", "vroom", "multcomp","devtools", "patchwork", "multcomp", "countrycode", "wbstats")
lapply(libraries, library, character.only = TRUE)


data("iris")

## visualize the data

irplot <- ggplot(data = iris, aes(x= Petal.Length, y=Petal.Width, col=Species)) +
  geom_point() +
  xlab("Petal Length") +
  ylab("Petal WIdth") 

## see if data conforms to gaussian

gaus_plot <- ggplot(iris, aes(x=Petal.Length, fill=Species)) +
  geom_histogram(binwidth=.1, alpha=.5, position="identity")

#==========================================
#       MOD1
#==========================================

## make a model

mod1 <- glm(Petal.Width ~ Petal.Length,
            data = iris,
            family = "gaussian")

## calculate the residuals

iris$pred_gaussian <- predict(mod1,
                                   type="response")

iris$resid_gaussian <- resid(mod1)


## plot the residuals

resid_plot <- ggplot(iris, aes(x = resid_gaussian)) +
  geom_histogram(fill="goldenrod") +
  theme_minimal() +
  ggtitle("Histogram of residuals")

## plot the model

irplot2 <- irplot + geom_smooth(data=iris,
                       method="glm",
                       method.args = list(family = "gaussian"),
                       formula = y ~ x,
                       col = "dodgerblue",
                       fill = "lightblue")

#===========================================
#       MOD2
#===========================================
### make a model

mod2 <- glm(Petal.Width ~ Petal.Length,
            data = iris,
            family = gaussian(link = "inverse"))

## calculate the residuals

iris$pred_gaussian_inverse <- predict(mod2,
                              type="response")

iris$resid_gaussian_inverse <- resid(mod2)


## plot the residuals

resid_plot <- ggplot(iris, aes(x = resid_gaussian_inverse)) +
  geom_histogram(fill="goldenrod") +
  theme_minimal() +
  ggtitle("Histogram of residuals")

## plot the model

irplot3 <- irplot + geom_smooth(data=iris,
                                method="glm",
                                method.args = list(family = gaussian(link = "inverse")),
                                formula = y ~ x,
                                col = "dodgerblue",
                                fill = "lightblue")

#===========================================
#       MOD3
#===========================================
mod3 <- glm(Petal.Width ~ Petal.Length*Species,
            data = iris,
            family = "gaussian")

mod4 <- glm(Petal.Width ~ Petal.Length*Species,
            data = iris,
            family = gaussian(link = "inverse"))

summary(glht(mod1, mcp(Species="Tukey")))


## plotting three different lines

iris_split <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, col=Species)) +
  geom_point() +
  geom_smooth(method = "glm")
