Citywide Crime Rate Data Set Creation, 2017-2019
================
Christopher Prener, Ph.D.
(November 03, 2019)

## Introduction

This notebook creates a table documenting changes in citywide crime rate
by year.

## Dependencies

This notebook requires the following `R` packages:

``` r
# tidyverse packages
library(dplyr)
library(readr)
library(stringr)

# spatial packages
library(tidycensus)

# other packages
library(compstatr)
library(here)
library(testthat)
```

## Load Data

The data for this table will be drawn from the three yearly `.csv` files
created by `01_CreateData_Crime.Rmd`. We’ll load those first:

``` r
part1_2017 <- read_csv(here("data", "crimes", "part1_2017.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   cs_year = col_double(),
    ##   complaint = col_character(),
    ##   date_occur = col_character(),
    ##   crime = col_double(),
    ##   description = col_character(),
    ##   ileads_address = col_double(),
    ##   ileads_street = col_character(),
    ##   neighborhood = col_double(),
    ##   region = col_character(),
    ##   gw_source = col_character(),
    ##   gw_address = col_character(),
    ##   gw_score = col_double(),
    ##   x = col_double(),
    ##   y = col_double()
    ## )

``` r
part1_2018 <- read_csv(here("data", "crimes", "part1_2018.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   cs_year = col_double(),
    ##   complaint = col_character(),
    ##   date_occur = col_character(),
    ##   crime = col_double(),
    ##   description = col_character(),
    ##   ileads_address = col_double(),
    ##   ileads_street = col_character(),
    ##   neighborhood = col_double(),
    ##   region = col_character(),
    ##   gw_source = col_character(),
    ##   gw_address = col_character(),
    ##   gw_score = col_double(),
    ##   x = col_double(),
    ##   y = col_double()
    ## )

``` r
part1_2019 <- read_csv(here("data", "crimes", "part1_2019.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   cs_year = col_double(),
    ##   complaint = col_character(),
    ##   date_occur = col_character(),
    ##   crime = col_double(),
    ##   description = col_character(),
    ##   ileads_address = col_double(),
    ##   ileads_street = col_character(),
    ##   neighborhood = col_double(),
    ##   region = col_character(),
    ##   gw_source = col_character(),
    ##   gw_address = col_character(),
    ##   gw_score = col_double(),
    ##   x = col_double(),
    ##   y = col_double()
    ## )

## Subset Data

Next, we’ll subset these data into violent crime and homicide objects:

``` r
# 2017
homicide_2017 <- cs_filter_crime(part1_2017, var = crime, crime = "Homicide")
violent_2017 <- cs_filter_crime(part1_2017, var = crime, crime = "Violent")

# 2018
homicide_2018 <- cs_filter_crime(part1_2018, var = crime, crime = "Homicide")
violent_2018 <- cs_filter_crime(part1_2018, var = crime, crime = "Violent")

# 2019
homicide_2019 <- cs_filter_crime(part1_2019, var = crime, crime = "Homicide")
violent_2019 <- cs_filter_crime(part1_2019, var = crime, crime = "Violent")
```

To make direct comparisons between the 2019 data and the earlier years,
we need to create partial year objects for 2017 and 2018:

``` r
# define last dates
last17 <- paste0("2017-", 
                 str_pad(as.character(params$months+1), width = 2, side = "left", pad = "0"), 
                 "-01")
last18 <- paste0("2018-", 
                 str_pad(as.character(params$months+1), width = 2, side = "left", pad = "0"), 
                 "-01")

# 2017
homicide_2017 %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  filter(date < last17) %>%
  select(-dateTime, -date, -time) -> homicide_2017p
  
violent_2017 %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  filter(date < last17) %>%
  select(-dateTime, -date, -time) -> violent_2017p

part1_2017 %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  filter(date < last17) %>%
  select(-dateTime, -date, -time) -> part1_2017p

# 2018
homicide_2018 %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  filter(date < last18) %>%
  select(-dateTime, -date, -time) -> homicide_2018p
  
violent_2018 %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  filter(date < last18) %>%
  select(-dateTime, -date, -time) -> violent_2018p

part1_2018 %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  filter(date < last18) %>%
  select(-dateTime, -date, -time) -> part1_2018p
```

## Create City-wide Rates

First, we want to create a tibble with just summary data:

``` r
city <- tibble(
  year = c(2017, 2017, 2017, 2017, 2017, 2017,
           2018, 2018, 2018, 2018, 2018, 2018,
           2019, 2019, 2019),
  range = c("full", "full", "full", "partial", "partial", "partial",
            "full", "full", "full", "partial", "partial", "partial",
            "partial", "partial", "partial"),
  type = c("homicide", "part 1", "violent", "homicide", "part 1", "violent",
           "homicide", "part 1", "violent", "homicide", "part 1", "violent",
           "homicide", "part 1", "violent"),
  count = c(nrow(homicide_2017), nrow(part1_2017), nrow(violent_2017),
            nrow(homicide_2017p), nrow(part1_2017p), nrow(violent_2017p),
            nrow(homicide_2018), nrow(part1_2018), nrow(violent_2018),
            nrow(homicide_2018p), nrow(part1_2018p), nrow(violent_2018p),
            nrow(homicide_2019), nrow(part1_2019), nrow(violent_2019))
)
```

Next, we’ll download the city’s total population using the 2013-2017 ACS
5-year estimate:

``` r
# download data
county <- get_acs(year = 2017, geography = "county", variables = "B01003_001", 
                state = 29, county = 510, geometry = FALSE)
```

    ## Getting data from the 2013-2017 5-year ACS

``` r
# store population value
county_pop <- county$estimate[1]
```

Finally, we’ll add the population and then calculate rates:

``` r
city %>%
  mutate(total_pop = county_pop) %>%
  mutate(rate = count/total_pop*1000) %>%
  select(year, range, type, count, rate, total_pop) -> city
```

## Write Data

Now we can write our city-wide estimates:

``` r
write_csv(city, path = here("data", "city", "city_wide.csv"))
```
