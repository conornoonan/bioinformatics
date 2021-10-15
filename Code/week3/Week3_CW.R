matrix(, ncol = 4, crow = 6)

#maing a matrix using values obtained from a normal distribution?

matrix(rnorm(80, mean = 34, sd = 1), ncol = 8, nrow = 10)

#Create an array containing 100 random numbers drawn from a uniform distribution (see runif()) with at least 3 groups.

still_an_array <- array(1:100, dim = c(3,2,3))
still_an_array

#NOTE - difference between data frames and matrices are that matrices can only store one type of data (boolean, string, numeric etc)
#Data frames can store more than one type

#Create a data frame which includes the following four vectors

name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas')
score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19)
questions = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1)
qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')

df <- data.frame(
  "Names"= name,
  "Score" = score,
  "Questions" = questions,
  "Qualify" = qualify
)

df$Mean_Score <- df$Score / df$Questions
df

library("vroom")

wod_dat <-vroom("Data/wader_data.csv")
wod_dat

##first we set the working directory (which is the location of the current file you are working on):
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#loading in covid data set

cov_dat <- vroom("Data/Bioinformatics_data-master/Workshop 3/time_series_covid19_deaths_global.csv")

#CHanging the headers of the first two columns to remove the slash

names(cov_dat)[1:2] <- c("Province.State", "Country.Region")

##so this says take our data frame called covid_dat
covid_long <- cov_dat %>%
  ##and then apply this function 
  pivot_longer(cols = -c( Province.State, 
                          Country.Region, 
                          Lat, 
                          Long))

##our data frame
covid_long <- cov_dat %>%
  ##and then apply this function 
  pivot_longer(cols = -c(Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")
covid_long

##change long to wide
covid_long %>% 
  pivot_wider(names_from = Date,
              values_from = Deaths)
