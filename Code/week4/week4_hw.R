## Loading in the data from week 3 homework

libraries <- c("tidyverse", "shiny", "excelR", "readxl", "knitr", "vroom", "lubridate", "devtools", "patchwork", "ggthemes")
lapply(libraries, library, character.only = TRUE)
install_github("thomasp85/patchwork")


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

## adding colour blind-friendly palettes

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## non-stacked chart

plot1 <- ggplot(data = plotting_df, aes(x = Urgency, y = Frequency, fill = Threats)) +
  geom_bar(aes(y = Frequency), position="dodge", stat="identity", colour = "black") +
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  labs(title = "Threats to Moss Species") +
  scale_fill_manual(values=cbbPalette) +
  scale_x_discrete(labels = c("Primary Threat", "Secondary Threat", "Tirtiary Threat"))

## stacked chart

plot2 <- ggplot(data = plotting_df, aes(x = Urgency, y = Frequency, fill = Threats)) +
  geom_bar(aes(y = Frequency), position="fill", stat="identity", colour = "black") +
  labs(title = "Threats to Moss Species") +
  scale_fill_manual(values=cbbPalette) +
  scale_x_discrete(labels = c("Primary Threat", "Secondary Threat", "Tirtiary Threat"))

## stitching them together with the patchwork library

plot1 + plot2

## Want to look at species which are primarily under threat due to habitat frag as it's the biggest 

hf <-  tsp_endangered %>%
  filter(primary_threat == "Habitat fragmentation")

hf_plot <- ggplot(data = hf, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(size = 2) +
  xlab("Date") + ylab("Abundance") +
  xlim(2000, 2020) +
  labs(title = "Population Counts of Moss Species at Risk of Habitat Fragmentation", fill = "Species") +
  scale_colour_discrete(name = "Species")


## Want to look at species which are primarily under threat due to exploitation as it's the second biggest 

ex <-  tsp_endangered %>%
  filter(primary_threat == "Exploitation" ) 

ex <- slice(ex, 1:97)

ex_plot <- ggplot(data = ex, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(size = 2) +
  xlab("Date") + ylab("Abundance") +
  xlim(2000, 2020) +
  labs(title = "Population Counts of Moss Species at Risk of Exploitation", fill = "Species") +
  scale_colour_discrete(name = "Species")

  
## Want to look at species which are primarily under threat due to Habitat loss as it's the third biggest 

hl <-  tsp_endangered %>%
  filter(primary_threat == "Habitat loss" ) 

hl_plot <- ggplot(data = hl, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(size = 2) +
  xlab("Date") + ylab("Abundance") +
  xlim(2000, 2020) +
  labs(title = "Population Counts of Moss Species at Risk of Habitat Loss", fill = "Species") +
  scale_colour_discrete(name = "Species")

## Want to look at species which are primarily under threat due to Pollution as it's the fourth biggest 

pol <-  tsp_endangered %>%
  filter(primary_threat == "Pollution" ) 

pol_plot <- ggplot(data = pol, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(method = "lm", size = 2) +
  xlab("Date") + ylab("Abundance") +
  xlim(2000, 2020) +
  labs(title = "Population Counts of Moss Species at Risk of Pollution", fill = "Species") +
  scale_colour_discrete(name = "Species")

## stitching the graphs together

(hf_plot | ex_plot) / (hl_plot | pol_plot)  
