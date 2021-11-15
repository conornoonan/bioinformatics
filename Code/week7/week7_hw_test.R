
libraries <- c("tidyverse", "knitr", "vroom", "gamlr", "DHARMa","fitdistrplus", "MASS")
lapply(libraries, library, character.only = TRUE)

data1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%206/data%201.csv")


#=======================================
#         Visualizing the Data
#=======================================

## converting table to long format for easier plotting 

df1_long <- data1 %>%
  pivot_longer(x1:x4, names_to = "series", values_to = "response" )

## looking at the outcome of the different independent variables

df1_filtered <- df1_long %>%
  filter(series == "x2")

df1_test_plot <- ggplot(df1_filtered, aes(x = response, y = y)) + geom_point()

## Looking at the distribution of your variables

df1_dist <- ggplot(df1_long, aes(x = y, fill = series)) +
  geom_histogram(alpha = 0.5, position = "identity")

## From these results it looks like all datasets are identically distributed and skewed to the right.

## plotting all variables and adding in a simple linear regression line to visualize any possible effects between 
## different predictors

df1_plot <- ggplot(data = df1_long, aes(x = response, y = y)) +
  geom_point(aes(col = series)) +
  geom_smooth(aes(col = series),
              method="glm",
              method.args = list(family = "gaussian"),
              formula = y ~ x,)
  
## Looks like there is no correlation at all ( slope ~ 0)
#=======================================
#         Test Some Models
#=======================================

df1_mod1 <- glm(y ~ x1, 
                data = data1,
                family = "gaussian")

df1_mod2 <- glm(y ~ x2, 
                data = data1,
                family = "gaussian")

df1_mod3 <- glm(y ~ x3, 
                data = data1,
                family = "gaussian")

df1_mod4 <- glm(y ~ x4, 
                data = data1,
                family = "gaussian")

df1_mod5 <- update(df1_mod1, family = gaussian(link = "log"))
df1_mod6 <- update(df1_mod2, family = gaussian(link = "log"))
df1_mod7 <- update(df1_mod3, family = gaussian(link = "log"))
df1_mod8 <- update(df1_mod4, family = gaussian(link = "log"))

AIC_mods <- data.frame(model = c("df1_mod1", "df1_mod2", "df1_mod3", "df1_mod4", "df1_mod5", "df1_mod6", "df1_mod7", "df1_mod8"),
                       AICc = c(AICc(df1_mod1), AICc(df1_mod2), AICc(df1_mod3), AICc(df1_mod4), AICc(df1_mod5), AICc(df1_mod6), AICc(df1_mod7), AICc(df1_mod8)))

AIC_mods[order(AIC_mods$AICc),]

## Try multiple linear regression

df1_mod5 <- glm(y ~ x1 + x2 + x3 + x4,
                  data = data1,
                  family = "gaussian")

## Based on this, also including summary, there's nothing of interest here




data2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%206/data%202.csv")

df2_long <- data2 %>%
  pivot_longer(x1:x3, names_to = "series", values_to = "response" )

## looking at the outcome of the different independent variables

df2_filtered <- df2_long %>%
  filter(series == "x2")

df2_test_plot <- ggplot(df2_filtered, aes(x = response, y = y)) + geom_point()

## Looking at the distribution of your variables

df2_dist <- ggplot(df2_long, aes(x = y, fill = series)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity")

## From these results it looks like all datasets are identically distributed and skewed to the right.

## plotting all variables and adding in a simple linear regression line to visualize any possible effects between 
## different predictors

df2_plot <- ggplot(data = df2_long, aes(x = response, y = y)) +
  geom_point(aes(col = series)) +
  geom_smooth(aes(col = series),
              method="glm",
              method.args = list(family = "gaussian"),
              formula = y ~ x)

## Looks like there is no correlation at all ( slope ~ 0)
#=======================================
#         Test Some Models
#=======================================

df2_mod1 <- glm(y ~ x1, 
                  data = data2,
                  family = "gaussian")

df2_mod2 <- glm(y ~ x2, 
                  data = data2,
                  family = "gaussian")

df2_mod3 <- glm(y ~ x3, 
                  data = data2,
                  family = "gaussian")

df2_mod4 <- update(df2_mod1, family = gaussian(link = "log"))
df2_mod5 <- update(df2_mod2, family = gaussian(link = "log"))
df2_mod6 <- update(df2_mod3, family = gaussian(link = "log"))


AIC_mods <- data.frame(model = c("df2_mod1", "df2_mod2", "df2_mod3", "df2_mod4", "df2_mod5", "df2_mod6"),
                       AICc = c(AICc(df2_mod1), AICc(df2_mod2), AICc(df2_mod3), AICc(df2_mod4), AICc(df2_mod5), AICc(df2_mod6)))

AIC_mods[order(AIC_mods$AICc),]

## Try multiple linear regression

df2_mod9 <- glm(y ~ x1 + x2 + x3,
                data = data1,
                family = "gaussian")








data3 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%206/data%203.csv")

df3_long <- data3 %>%
  pivot_longer(x1:x2, names_to = "series", values_to = "response" )

## looking at the outcome of the different independent variables

df3_filtered <- df3_long %>%
  filter(series == "x2")

df3_test_plot <- ggplot(df3_filtered, aes(x = response, y = y)) + geom_point()

## Looking at the distribution of your variables

df3_dist <- ggplot(df3_filtered, aes(x = y, fill = series)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity")

## From these results it looks like all datasets are identically distributed and skewed to the right.

## plotting all variables and adding in a simple linear regression line to visualize any possible effects between 
## different predictors

df3_plot <- ggplot(data = df3_long, aes(x = response, y = y)) +
  geom_point(aes(col = series)) +
  geom_smooth(aes(col = series),
              method="glm",
              method.args = list(family = "gaussian"),
              formula = y ~ x)









data4 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%206/data%204.csv")

df4_long <- data4 %>%
  pivot_longer(x1:x3, names_to = "series", values_to = "response" )

## looking at the outcome of the different independent variables

df4_filtered <- df4_long %>%
  filter(series == "x3")

df4_test_plot <- ggplot(df4_filtered, aes(x = response, y = y)) + geom_point()

## Looking at the distribution of your variables

df4_dist <- ggplot(df4_filtered, aes(x = y, fill = series)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity")

## From these results it looks like all datasets are identically distributed and skewed to the right.

## plotting all variables and adding in a simple linear regression line to visualize any possible effects between 
## different predictors

df4_plot <- ggplot(data = df4_long, aes(x = response, y = y)) +
  geom_point(aes(col = series)) +
  geom_smooth(aes(col = series),
              method="glm",
              method.args = list(family = gaussian(link = "log")),
              formula = y ~ x)

## Plotting the data, it looks like it's displaying the output of a constant function.


#=======================================
#         Test Some Models
#=======================================

df4_mod1 <- glm(y ~ x1, 
                data = data2,
                family = "gaussian")

df4_mod2 <- glm(y ~ x2, 
                data = data2,
                family = "gaussian")

df4_mod3 <- glm(y ~ x3, 
                data = data2,
                family = "gaussian")

df4_mod4 <- update(df2_mod1, family = gaussian(link = "log"))
df4_mod5 <- update(df2_mod2, family = gaussian(link = "log"))
df4_mod6 <- update(df2_mod3, family = gaussian(link = "log"))


AIC_mods <- data.frame(model = c("df4_mod1", "df4_mod2", "df4_mod3", "df4_mod4", "df4_mod5", "df4_mod6"),
                       AICc = c(AICc(df4_mod1), AICc(df4_mod2), AICc(df4_mod3), AICc(df4_mod4), AICc(df4_mod5), AICc(df4_mod6)))

AIC_mods[order(AIC_mods$AICc),]

## Try multiple linear regression

df4_mod9 <- glm(y ~ x1 + x2 + x3,
                data = data1,
                family = "gaussian")