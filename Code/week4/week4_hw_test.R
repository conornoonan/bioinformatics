
## Loading in the data from week 3 homework

libraries <- c("tidyverse", "shiny", "excelR", "readxl", "knitr", "vroom", "lubridate", "devtools", "patchwork", "ggthemes")
lapply(libraries, library, character.only = TRUE)


tsp1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")

tsp2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv")

tsp_joined <- full_join(tsp1, tsp2)

tsp_long <- tsp_joined %>%
  
  pivot_longer(cols = -c(species:tertiary_threat),
               names_to = c("population", "date"),
               values_to = "abundance",
               values_drop_na = F, 
               names_pattern = "pop_(1|2)_(.*)",
  )

# First set the values under "date" to the correct format using the lubridate library.

##tell lubridate what the current format is - month, day, year
tsp_long$date <- ymd(tsp_long$date)

##then convert it so it is only the year (day and month are redundant)
tsp_long$date <- year(tsp_long$date)

## removing the date and abundance columns and removing duplicate rows so we have one of each threat per species

threats_only <- tsp_long %>%
  select(species:tertiary_threat) %>%
  distinct()

## removing the rows if there is an na in the abundance column

tsp_nona <- tsp_long %>%
  drop_na(abundance)

#removing the species which aren't endangered

tsp_endangered <- tsp_nona %>%
  drop_na(primary_threat)

#creating a data frame for each threat level

pthreat <-  table(threats_only$primary_threat)%>%
  as.data.frame()

names(pthreat)[2] = "freq_primary"

sthreat <-  table(threats_only$secondary_threat)%>%
  as.data.frame()

names(sthreat)[2] = "freq_secondary"

tthreat <-  table(threats_only$tertiary_threat)%>%
  as.data.frame()

names(tthreat)[2] = "freq_tertiary"

all_threats <- full_join(pthreat, sthreat, by="Var1") %>%
  full_join(tthreat, by="Var1")

names(all_threats)[2:4] <- c("Primary_Threat", "Secondary_Threat", "Tertiary_Threat")
names(all_threats)[1] <- "Threat"


tst1 <- all_threats %>%
  t() %>%
  as_tibble() 


colnames(tst1) <- slice(tst1, 1)
tst2 <- tst1[-c(1),]


tst2$Threats <- c("primary_threat", "secondary_threat", "tertiary_threat")
tst3 <- relocate(tst2, Threats)
tst4 <- rename(tst3, Urgency = Threats)

## making up a new data frame which has a column for urgency, threats, and threat frequency

Urgency <- rep(c("primary_threat", "secondary_threat", "tertiary_threat"), each=6)
Threats <- rep(c("Climate change", "Exploitation", "Habitat Destruction", "Habitat fragmentation", "Habitat loss", "Pollution"), 3)

plotting_df <- data.frame(Urgency, Threats)

## taking the frequency values from tst2 

d <- c(slice(tst1, 2), slice(tst1, 3), slice(tst1, 4))
d <- unlist(d)

plotting_df$Frequency <- d

plotting_df <- as_tibble(plotting_df)
plotting_df$Frequency <- as.numeric(plotting_df$Frequency)


long_test <- pivot_longer(tst4, cols = "Urgency")
