## Exercise 2.1
libraries <- c("tidyverse", "shiny", "excelR", "readxl", "knitr", "vroom", "lubridate", "devtools", "patchwork", "ggthemes", "multcomp")
lapply(libraries, library, character.only = TRUE)


data("iris")

# Visualize using scatter plot

ggplot(data = iris, aes(x = Species, y = Sepal.Width)) + 
  geom_jitter(aes(col = Species)) + 
  theme_bw()

# Visualize using a histogram

ggplot(iris, aes(x=Sepal.Width, fill = Species)) +
  ## bin width determines how course the histogram is
  ## the alpha determines the transparency of the bars
  ## position allows you to determine what kind of histogram you plot (e.g. stacked vs overlapping). try changing to position="stack"
  geom_histogram(binwidth = .1, alpha = .5, position="identity")

##fit a glm()
mod_iris <- glm(Sepal.Width ~ Species,
                ##specify the data
                data = iris,
                ##specify the error structure
                family = "gaussian")

# Using multiple comparison tests

## run the multiple comparisons, and look at the summary output:
summary(glht(mod_iris, mcp(Species="Tukey")))

## Loading in data from week 3

tsp1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")

tsp2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv")

tsp_joined <- full_join(tsp1, tsp2)

tsp_long <- tsp_joined %>%
  
  pivot_longer(cols = -c(species:tertiary_threat),
               names_to = c("population", "date"),
               values_to = "abundance",
               values_drop_na = T, 
               names_pattern = "pop_(1|2)_(.*)"
  )

# Set correct date

tsp_long$date <- ymd(tsp_long$date)

# extract rows for Trichocolea tomentella

single_spp <- filter(tsp_long, species == "Trichocolea tomentella")

# visualize the data

##make the plot
p1 <- ggplot(single_spp, aes(x=date, y=abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("Year")
##add the loess smoothing:
p1 + geom_smooth(method="loess")

## calculate a new column (`standardised_time`) which is the difference between the
## starting date of the time series and each other date in weeks (see ?difftime)
## we will set this to a numeric vector
single_spp <- single_spp %>%
  mutate(standardised_time = as.numeric(difftime(as.Date(date),
                                                 min(as.Date(date)),
                                                 units = "weeks")))

print(single_spp[,c("abundance", "date", "standardised_time")], 30)

##fit a glm()
mod1 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "gaussian")

##return the predicted (response) values from the model
##and add them to the single species tibble:
single_spp$pred_gaussian <- predict(mod1,
                                    type="response")

##return the residuals from the model
##and add them to the single species tibble:
single_spp$resid_gaussian <- resid(mod1)

## plot the abundances through time
p2 <- ggplot(single_spp, aes(x = standardised_time,
                             y = abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")

##add in a line of the predicted values from the model
p2 <- p2 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian),
                     col = "dodgerblue",
                     size = 1)

## we can also add in vertical blue lines which show the redsidual error of the model
## (how far the observed points are from the predicted values).
## in geom_segement we specify where we want the start and end of the segments (lines)
## to be. Without any prompting ggplot assumes that we want the start of the lines to
## be taken from the x and y values we are plotting using the ggplot() function
## i.e. standardised_time and abundance, so we just need to specify the end points of
## the lines:
p2 <- p2 +
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian),
               col="lightblue")

## add a title
p2 <- p2 + ggtitle("Fitted model (gaussian with identity link)")

##print the plot
p2

##plot a histogram of the residuals from the model using geom_histogram()
p3 <- ggplot(single_spp, aes(x = resid_gaussian)) +
  geom_histogram(fill="goldenrod") +
  theme_minimal() +
  ggtitle("Histogram of residuals (gaussian with identity link)")
## print the plot
p3

p4 <- ggplot(single_spp, aes(x = pred_gaussian, y = resid_gaussian)) +
  geom_smooth() +
  geom_point() +
  xlab("Predicted Values") +
  ylab("Residuals") +
  ggtitle("Predicted (a.k.a. Fitted) vs Residual (Gaussian with Identity Link)")

p4

##plot the qq plot for the residuals from the model assuming a normal distribution,
## and add the straight line the points should fall along:
qqnorm(single_spp$resid_gaussian); qqline(single_spp$resid_gaussian)

## According to the skewness/tailedness application, it looks like we have negatice skewness and negative tailedness. 

# 2.2.1.1

## fit a glm with a poisson distribution
mod2 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "poisson")

## fit a glm with a gaussian distribution with a log link
mod3 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "log"))

## we could also try a guassian model with an inverse link
mod4 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "inverse"))

##compare the models
AIC_mods <- AIC(mod1,
                mod2,
                mod3,
                mod4)

## rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AIC),]



##return the predicted (response) values from the model
##and add them to the single species tibble:
single_spp$pred_gaussian_log <- predict(mod3,
                                    type="response")

##return the residuals from the model
##and add them to the single species tibble:
single_spp$resid_gaussian_log <- resid(mod3)



## plot the abundances through time
p5 <- ggplot(single_spp, aes(x = standardised_time,
                             y = abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")

##add in a line of the predicted values from the model
p5 <- p5 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian_log),
                     col = "dodgerblue",
                     size = 1)

## the lines:
p5 <- p5 +
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian_log),
               col="lightblue")

## add a title
p5 <- p5 + ggtitle("Fitted model (gaussian with identity link)")

## first off let's plot the data again
p6 <- ggplot(single_spp, aes(x=standardised_time, y=abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")

## use the geom_smooth() function to add the regression to the plot.
## unlike earlier here we are specifying the model type (glm), the formula,
## and the error structure and link
p6 <- p6 + geom_smooth(data=single_spp,
                       method="glm",
                       method.args = list(family = gaussian(link="log")),
                       formula = y ~ x,
                       col = "dodgerblue",
                       fill = "lightblue")

##print the plot
p6