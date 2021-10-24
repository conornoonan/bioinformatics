library("vroom")
library("tidyverse")
library("wbstats")
library("countrycode")
library("lubridate")

covid_dat <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/time_series_covid19_deaths_global.csv")

#renaming the first two columns

names(covid_dat)[1:2] <- c("Province.State", "Country.Region")

##so this says take our data frame called covid_dat
covid_long <- covid_dat %>%
  ##and then apply this function 
  pivot_longer(cols = -c( Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")

##tell lubridate what the current format is - month, day, year
covid_long$Date<-mdy(covid_long$Date)

##then convert it to day, month, year to make it easy to read
covid_long$Date <- format(covid_long$Date, "%d-%m-%Y")

##extract the population data for all countries
pop_data <- wb_data(indicator = "SP.POP.TOTL", 
                    start_date = 2002, 
                    end_date = 2020)


##convert it to a tibble
pop_data <- as_tibble(pop_data)

##finding the most recent pop counts

pop_2020 <- pop_data %>%
  filter(date == 2020)

## looking at the stats for Australia in 2020

pop_2020 %>% filter(country == "Australia")


## make a new data.frame from the old covid_long data.frame

covid_country <- covid_long %>% 
  
  ## we want to calculate the number of 
  ##deaths in each country and at each date:
  
  group_by(Country.Region, Date) %>% 
  
  ## and we want the sum of the "Death" column in these groups:
  
  summarise(Deaths = sum(Deaths))
  
## add a column to covid data containing the country code
covid_country$code <- countrycode(covid_country$Country.Region, 
                                  origin = "country.name", 
                                  destination = "iso3c")

## renaming the column with the pop number for ease

##rename the 5th column so it works with the following code

names(pop_2020)[5] <- "value"


## joining the data using the covid_country dt as the "left" table, using left_join allows us to add new column 

covid_w_pop <- left_join(covid_country,
                         pop_2020 %>% select(iso3c, value),
                         by = c("code" = "iso3c")
                         )
## changing the name of the "value" column. Using the Which() function allows you to return a boolean "true" value. This
## means you avoid having to use indexes, which might be an issue if the volume changes position

names(covid_w_pop)[which(names(covid_w_pop) == "value")] <- "Population"

## making a new data frame that contains the latest death counts

##filter to leave the most recent data
most_recent <- covid_country %>% 
  filter(Date == max(covid_country$Date))

##sum the deaths
sum(most_recent$Deaths)

## calculate the number of deaths per day and save as a new df 

## make a new data frame of the global deaths using group_by() and summarise()
global_deaths_day <- covid_country %>% 
  group_by(Date) %>%
  summarise("Global.deaths" = sum(Deaths))

##calculate deaths per million people and add it to the data.frame
covid_w_pop$Deaths.p.m <- (covid_w_pop$Deaths / covid_w_pop$Population) * 1000000

##look at the data
tail(covid_w_pop)

## tell R that the data is a date. 
##We need to specify the format the date data are given in using "%m-%d-%y" 
##(see ?as_date for help on what this means)

global_deaths_day$Date.corrected <- as_date(global_deaths_day$Date,
                                            format = "%d-%m-%y")

## make a ggplot object

## make a ggplot object
ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths)) + geom_point()

## a line plot
ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths)) + 
  ## lines
  geom_line()

## a scatter and line plot
ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths)) + 
  ## points
  geom_point(col = "darkgrey") +
  ## and lines! 
  geom_line(col = "red") 

covid_w_pop$Date.corrected <- as_date(covid_w_pop$Date,
                                            format = "%d-%m-%y")

by_country <- ggplot(data = covid_w_pop, aes(x = Date.corrected, y = Deaths)) 

by_country + geom_point(aes(col = Country.Region)) + theme(legend.position = "none")

##make a vector of countries we want to look at:
selec_countries <- c("United Kingdom", "China", "US", "Italy", "France", "Germany")

##use this to filter by for our plot. here using the %in% operature:
sel_country_plot <- ggplot(data = covid_w_pop %>% 
                             filter(Country.Region %in% selec_countries), 
                           aes(x = Date.corrected, y = Deaths)) 

##add a line geom specifying that the colours are dependant on the groups in `Country.Region`
sel_country_plot + geom_line(aes(col=Country.Region))

## with no grouping
sel_country_plot + geom_line()
