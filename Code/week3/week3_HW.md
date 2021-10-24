Week 3 Homework
================

# Summary

This document is broken down into subsections corresponding to the tasks
given for homework. The primary task is two write an R Markdown file
which contains code that carries out certain tasks.

## Task 1

Load `to_sort_pop_1.csv` and `to_sort_pop_1.csv` from
[bioinformatics\_data](https://github.com/chrit88/Bioinformatics_data/tree/master/Workshop%203)
provided by Dr. Chris Clements on github.

``` r
tsp1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")
```

    ## Rows: 30 Columns: 29

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "\t"
    ## chr  (4): species, primary_threat, secondary_threat, tertiary_threat
    ## dbl (24): pop_1_2003-01-01, pop_1_2004-01-01, pop_1_2005-01-01, pop_1_2006-0...
    ## lgl  (1): pop_1_1995-01-01

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(tsp1)
```

    ## # A tibble: 6 x 29
    ##   species      primary_threat  secondary_threat tertiary_threat `pop_1_2003-01-~
    ##   <chr>        <chr>           <chr>            <chr>                      <dbl>
    ## 1 Schistidium~ Habitat destru~ <NA>             <NA>                          NA
    ## 2 Paraleucobr~ Exploitation    Habitat loss     <NA>                          NA
    ## 3 Scapania pa~ Climate change  <NA>             <NA>                          NA
    ## 4 Seligera re~ Exploitation    <NA>             <NA>                          NA
    ## 5 Tortula sub~ Habitat loss    Pollution        Climate change                96
    ## 6 Pohlia mela~ <NA>            <NA>             <NA>                         288
    ## # ... with 24 more variables: pop_1_2004-01-01 <dbl>, pop_1_2005-01-01 <dbl>,
    ## #   pop_1_2006-01-01 <dbl>, pop_1_2007-01-01 <dbl>, pop_1_2008-01-01 <dbl>,
    ## #   pop_1_2009-01-01 <dbl>, pop_1_2010-01-01 <dbl>, pop_1_2011-01-01 <dbl>,
    ## #   pop_1_2012-01-01 <dbl>, pop_1_2013-01-01 <dbl>, pop_1_2014-01-01 <dbl>,
    ## #   pop_1_2015-01-01 <dbl>, pop_1_2016-01-01 <dbl>, pop_1_2017-01-01 <dbl>,
    ## #   pop_1_2018-01-01 <dbl>, pop_1_2019-01-01 <dbl>, pop_1_2000-01-01 <dbl>,
    ## #   pop_1_2001-01-01 <dbl>, pop_1_2002-01-01 <dbl>, pop_1_1997-01-01 <dbl>, ...

``` r
tsp2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv")
```

    ## Rows: 30 Columns: 28

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "\t"
    ## chr  (4): species, primary_threat, secondary_threat, tertiary_threat
    ## dbl (21): pop_2_2000-01-01, pop_2_2001-01-01, pop_2_2002-01-01, pop_2_2003-0...
    ## lgl  (3): pop_2_1996-01-01, pop_2_1997-01-01, pop_2_1998-01-01

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(tsp2)
```

    ## # A tibble: 6 x 28
    ##   species       primary_threat secondary_threat tertiary_threat `pop_2_2000-01-~
    ##   <chr>         <chr>          <chr>            <chr>                      <dbl>
    ## 1 Sphagnum pal~ <NA>           <NA>             <NA>                          NA
    ## 2 Pohlia wahle~ Habitat loss   Pollution        <NA>                          NA
    ## 3 Sphagnum les~ Pollution      Exploitation     <NA>                          NA
    ## 4 Marchantia p~ <NA>           <NA>             <NA>                          NA
    ## 5 Platyhypnidi~ Habitat loss   <NA>             <NA>                          NA
    ## 6 Scleropodium~ Pollution      <NA>             <NA>                          NA
    ## # ... with 23 more variables: pop_2_2001-01-01 <dbl>, pop_2_2002-01-01 <dbl>,
    ## #   pop_2_2003-01-01 <dbl>, pop_2_2004-01-01 <dbl>, pop_2_2005-01-01 <dbl>,
    ## #   pop_2_2006-01-01 <dbl>, pop_2_2007-01-01 <dbl>, pop_2_2008-01-01 <dbl>,
    ## #   pop_2_2009-01-01 <dbl>, pop_2_2010-01-01 <dbl>, pop_2_2011-01-01 <dbl>,
    ## #   pop_2_2012-01-01 <dbl>, pop_2_2013-01-01 <dbl>, pop_2_2014-01-01 <dbl>,
    ## #   pop_2_2015-01-01 <dbl>, pop_2_2016-01-01 <dbl>, pop_2_2017-01-01 <dbl>,
    ## #   pop_2_2018-01-01 <dbl>, pop_2_2019-01-01 <dbl>, pop_2_1996-01-01 <lgl>, ...

## Task 2

Using a `tidyverse` function, join both of these data together into a
single tibble.

``` r
tsp_joined <- full_join(tsp1, tsp2)
```

    ## Joining, by = c("species", "primary_threat", "secondary_threat", "tertiary_threat")

``` r
head(tsp_joined)
```

    ## # A tibble: 6 x 53
    ##   species      primary_threat  secondary_threat tertiary_threat `pop_1_2003-01-~
    ##   <chr>        <chr>           <chr>            <chr>                      <dbl>
    ## 1 Schistidium~ Habitat destru~ <NA>             <NA>                          NA
    ## 2 Paraleucobr~ Exploitation    Habitat loss     <NA>                          NA
    ## 3 Scapania pa~ Climate change  <NA>             <NA>                          NA
    ## 4 Seligera re~ Exploitation    <NA>             <NA>                          NA
    ## 5 Tortula sub~ Habitat loss    Pollution        Climate change                96
    ## 6 Pohlia mela~ <NA>            <NA>             <NA>                         288
    ## # ... with 48 more variables: pop_1_2004-01-01 <dbl>, pop_1_2005-01-01 <dbl>,
    ## #   pop_1_2006-01-01 <dbl>, pop_1_2007-01-01 <dbl>, pop_1_2008-01-01 <dbl>,
    ## #   pop_1_2009-01-01 <dbl>, pop_1_2010-01-01 <dbl>, pop_1_2011-01-01 <dbl>,
    ## #   pop_1_2012-01-01 <dbl>, pop_1_2013-01-01 <dbl>, pop_1_2014-01-01 <dbl>,
    ## #   pop_1_2015-01-01 <dbl>, pop_1_2016-01-01 <dbl>, pop_1_2017-01-01 <dbl>,
    ## #   pop_1_2018-01-01 <dbl>, pop_1_2019-01-01 <dbl>, pop_1_2000-01-01 <dbl>,
    ## #   pop_1_2001-01-01 <dbl>, pop_1_2002-01-01 <dbl>, pop_1_1997-01-01 <dbl>, ...

## Task 3

Reshape the tibble from wide to long format and include 3 new columns.

``` r
# take the tsp_long tibble and filter it into the pivot_longer function to convert from wide to long format

tsp_long <- tsp_joined %>%
                
# specifying the column titles for the converted tibble
  
                pivot_longer(cols = -c(species:tertiary_threat),
                             
                             # adding two new column titles under which unmatched character strings and our pop number will be
                             # assigned to
                             
                             names_to = c("population", "date"),
                             
                             # adding new column under which unmatched values will be assigned to
                             
                             values_to = "abundance",
                             
                             # using regular expressions to search for population number and date in character strings. The 
                             # brackets are the capture groups, and each group is assigned to one of the new columns.
                             names_pattern = "pop_(1|2)_(.*)",
                             )
head(tsp_long)
```

    ## # A tibble: 6 x 7
    ##   species     primary_threat  secondary_threat tertiary_threat population date  
    ##   <chr>       <chr>           <chr>            <chr>           <chr>      <chr> 
    ## 1 Schistidiu~ Habitat destru~ <NA>             <NA>            1          2003-~
    ## 2 Schistidiu~ Habitat destru~ <NA>             <NA>            1          2004-~
    ## 3 Schistidiu~ Habitat destru~ <NA>             <NA>            1          2005-~
    ## 4 Schistidiu~ Habitat destru~ <NA>             <NA>            1          2006-~
    ## 5 Schistidiu~ Habitat destru~ <NA>             <NA>            1          2007-~
    ## 6 Schistidiu~ Habitat destru~ <NA>             <NA>            1          2008-~
    ## # ... with 1 more variable: abundance <dbl>

## Author

Conor Noonan
