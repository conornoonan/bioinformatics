Week 4 Homework
================

## Summary of Task

The goal of the homework for this week is to play around with the data
from Week 3. These data detail the various threats to moss species, as
well as population counts of multiple species over time.

Here’s an example:

| species                | primary\_threat     | secondary\_threat | tertiary\_threat | population | date       | abundance |
|:-----------------------|:--------------------|:------------------|:-----------------|:-----------|:-----------|----------:|
| Schistidium helveticum | Habitat destruction | NA                | NA               | 1          | 2003-01-01 |        NA |
| Schistidium helveticum | Habitat destruction | NA                | NA               | 1          | 2004-01-01 |        NA |
| Schistidium helveticum | Habitat destruction | NA                | NA               | 1          | 2005-01-01 |        NA |
| Schistidium helveticum | Habitat destruction | NA                | NA               | 1          | 2006-01-01 |        NA |
| Schistidium helveticum | Habitat destruction | NA                | NA               | 1          | 2007-01-01 |        NA |
| Schistidium helveticum | Habitat destruction | NA                | NA               | 1          | 2008-01-01 |        NA |

*Fig. 1. SUmmary of data from Week 3 homework.*

## Data Wrangling and Analysis

I wanted to look specifically at the species which were under threat. To
do so I first filtered the data so as to list each species and their
corresponding threats.

``` r
## First we're going to make sure that the values in the date column are recognized as dates. Tell lubridate 
## what the current format is - month, day, year (this will be relevant later on).

tsp_long$date <- ymd(tsp_long$date)

## then convert it so it is only the year (day and month are redundant)

tsp_long$date <- year(tsp_long$date)

## removing the date and abundance columns and removing duplicate rows so we have one of each threat per species

threats_only <- tsp_long %>%
  select(species:tertiary_threat) %>%
  distinct()
```

| species                    | primary\_threat     | secondary\_threat | tertiary\_threat |
|:---------------------------|:--------------------|:------------------|:-----------------|
| Schistidium helveticum     | Habitat destruction | NA                | NA               |
| Paraleucobryum longifolium | Exploitation        | Habitat loss      | NA               |
| Scapania paludosa          | Climate change      | NA                | NA               |
| Seligera recurvata         | Exploitation        | NA                | NA               |
| Tortula subulata           | Habitat loss        | Pollution         | Climate change   |
| Pohlia melanodon           | NA                  | NA                | NA               |

*Fig. 2. Summary of data from Week 3 homework.*

Next I wanted to summarize the frequency of each threat as it appears as
a primary, secondary, or tertiary threat. I did this by making three
separate tables (one for each level of “urgency” - i.e. primary,
secondary etc). I then made one master table containing all 3 levels and
the frequency they all appear.

``` r
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
```

| Threat                | Primary\_Threat | Secondary\_Threat | Tertiary\_Threat |
|:----------------------|----------------:|------------------:|-----------------:|
| Climate change        |               7 |                 6 |                6 |
| Exploitation          |               9 |                 3 |                2 |
| Habitat destruction   |               4 |                 6 |                2 |
| Habitat fragmentation |              10 |                 3 |                1 |
| Habitat loss          |               8 |                 4 |                3 |
| Pollution             |               8 |                 4 |                2 |

*Fig. 3. Frequency of each threat for each level of urgency.*

I wanted to graph these frequencies to show the breakdown of each
threat. To do so, I wrangled the data so that each level of urgency was
on the left side and each threat was a variable (column).

``` r
## transpose the data frame

tst1 <- all_threats %>%
  t() %>%
  as_tibble() 
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

``` r
colnames(tst1) <- slice(tst1, 1)
tst2 <- tst1[-c(1),]

 
tst2$Threats <- c("primary_threat", "secondary_threat", "tertiary_threat")
tst3 <- relocate(tst2, Threats)
```

| Threats           | Climate change | Exploitation | Habitat destruction | Habitat fragmentation | Habitat loss | Pollution |
|:------------------|:---------------|:-------------|:--------------------|:----------------------|:-------------|:----------|
| primary\_threat   | 7              | 9            | 4                   | 10                    | 8            | 8         |
| secondary\_threat | 6              | 3            | 6                   | 3                     | 4            | 4         |
| tertiary\_threat  | 6              | 2            | 2                   | 1                     | 3            | 2         |

*Fig. 4. Wide format table.*

To start plotting, I had to convert to a long format.

``` r
Urgency <- rep(c("primary_threat", "secondary_threat", "tertiary_threat"), each=6)
Threats <- rep(c("Climate change", "Exploitation", "Habitat Destruction", "Habitat fragmentation", "Habitat loss", "Pollution"), 3)

plotting_df <- data.frame(Urgency, Threats)

## taking the frequency values from tst2 

d <- c(slice(tst1, 2), slice(tst1, 3), slice(tst1, 4))
d <- unlist(d)

plotting_df$Frequency <- d

plotting_df <- as_tibble(plotting_df)
plotting_df$Frequency <- as.numeric(plotting_df$Frequency)
```

| Urgency           | Threats               | Frequency |
|:------------------|:----------------------|----------:|
| primary\_threat   | Climate change        |         7 |
| primary\_threat   | Exploitation          |         9 |
| primary\_threat   | Habitat Destruction   |         4 |
| primary\_threat   | Habitat fragmentation |        10 |
| primary\_threat   | Habitat loss          |         8 |
| primary\_threat   | Pollution             |         8 |
| secondary\_threat | Climate change        |         6 |
| secondary\_threat | Exploitation          |         3 |
| secondary\_threat | Habitat Destruction   |         6 |
| secondary\_threat | Habitat fragmentation |         3 |
| secondary\_threat | Habitat loss          |         4 |
| secondary\_threat | Pollution             |         4 |
| tertiary\_threat  | Climate change        |         6 |
| tertiary\_threat  | Exploitation          |         2 |
| tertiary\_threat  | Habitat Destruction   |         2 |
| tertiary\_threat  | Habitat fragmentation |         1 |
| tertiary\_threat  | Habitat loss          |         3 |
| tertiary\_threat  | Pollution             |         2 |

*Fig. 5. Long format table.*

## Plotting Threat Frequency

``` r
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
```

![*Fig. 6: Breakdown of threats to moss species and their relative
frequency.*](w4hw_merged_plots.png)

## Data Wrangling and Plotting for Abundance Data

These graphs show that the most common primary threat to moss species
are habitat fragmentation, exploitation, habitat loss, and pollution.

Because of this, I wanted to look at the abundance of the moss species
over time which have these factors listed as primary threats. To do
this, I filtered out any rows which didn’t have any value (NA) for
abundance. After this, I removed any species listed which didn’t have
any values for “primary threat” as it was assumed that they weren’t
endangered.

``` r
## removing the rows which have no values for abundance

tsp_nona <- tsp_long %>%
  drop_na(abundance)

## removing the species which aren't endangered

tsp_endangered <- tsp_nona %>%
  drop_na(primary_threat)
```

``` r
## Want to look at species which are primarily under threat due to habitat frag as it's the biggest 

hf <- tsp_endangered %>%
  filter(primary_threat == "Habitat fragmentation")

hf_plot <- ggplot(data = hf, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(size = 2) +
  xlab("Date") + ylab("Abundance") +
  labs(title = "Population Counts of Moss Species at Risk of Habitat Fragmentation", fill = "Species") +
  scale_colour_discrete(name = "Species")


## Want to look at species which are primarily under threat due to exploitation as it's the second biggest 

ex <- tsp_endangered %>%
  filter(primary_threat == "Exploitation" ) 

ex <- slice(ex, 1:97)

ex_plot <- ggplot(data = ex, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(size = 2) +
  xlab("Date") + ylab("Abundance") +
  labs(title = "Population Counts of Moss Species at Risk of Exploitation", fill = "Species") +
  scale_colour_discrete(name = "Species")

  
## Want to look at species which are primarily under threat due to Habitat loss as it's the third biggest 

hl <- tsp_endangered %>%
  filter(primary_threat == "Habitat loss" ) 

hl_plot <- ggplot(data = hl, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(size = 2) +
  xlab("Date") + ylab("Abundance") +
  labs(title = "Population Counts of Moss Species at Risk of Habitat Loss", fill = "Species") +
  scale_colour_discrete(name = "Species")

## Want to look at species which are primarily under threat due to Pollution as it's the fourth biggest 

pol <- tsp_endangered %>%
  filter(primary_threat == "Pollution" ) 

pol_plot <- ggplot(data = pol, aes(x = date, y = abundance)) + 
  geom_jitter(aes(col = species), size = 2) +
  geom_smooth(method = "lm") +
  xlab("Date") + ylab("Abundance") +
  labs(title = "Population Counts of Moss Species at Risk of Pollution", fill = "Species") +
  scale_colour_discrete(name = "Species")

## all graphs were stitched together using the patchwork package from thomasp85
```

![*Fig. 7: Breakdown of threats to moss species and their relative
frequency.*](threats_merged.png)

## Data Interpretation

These data suggest that moss species primarily at risk of habitat
fragmentation and habitat loss are, on average, actively declining in
population size. Species primarily at risk of exploitation and pollution
don’t appear to be declining on average. This disparity may be as a
result of the more immediate effects of habitat loss and fragmentation.
There are also a number of limitations to these results:

1.  Abundance counts for all species is not consistent. There is more
    data available regarding the abundance of some species compared to
    others.
2.  Some data were not available for some species. This includes
    population counts, as well as information on threat susceptibility
    (those with no threats listed were assumed to not be at risk).
3.  I only looked at the top 4 primary threats, there may also be
    iteresting trends for the remaining threats and at difference
    urgency levels.
4.  Data was admittedly cherry-picked out of convenience and time
    (sorry!).
