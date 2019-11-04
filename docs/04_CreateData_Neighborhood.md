Neighborhood Crime Rate Data Set Creation, 2017-2019
================
Christopher Prener, Ph.D.
(November 03, 2019)

## Introduction

This notebook creates a set of tables documenting changes in
neighborhood level crime rates for the City of St. Louis.

## Dependencies

This notebook requires the following `R` packages:

``` r
# tidyverse packages
library(dplyr)
library(readr)
library(stringr)

# spatial packages
library(areal)
library(gateway)
library(sf)
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

## Prepare Neighborhood Data

Our first task is to prepare a data set of neighborhoods along with
their total estimated populations.

### Census Data

To calculate rates, we’ll use total population estimates from the
2013-2017 5 year American Community Survey estimates. These data are
downloaded along with their geometry, and subset down to just the GEOID
value and the estimated population:

``` r
pop <- get_acs(year = 2017, geography = "tract", variables = "B01003_001", 
                state = 29, county = 510, geometry = TRUE) %>%
  st_transform(crs = 26915) %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate)
```

    ## Getting data from the 2013-2017 5-year ACS

    ## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.

We’ll also want the total estimated population for the City of St. Louis
from the same data source:

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

### Neighborhood Data

Next, we’ll download neighborhood boundaries for the City of St. Louis
and remove major parks (those neighborhoods with ID number values
greater than 79):

``` r
nhood <- gw_get_data(data = "Neighborhoods", class = "sf") %>%
  st_transform(crs = 26915) %>%
  select(NHD_NUM, NHD_NAME) %>%
  filter(NHD_NUM < 80)
```

### Estimate Neighborhood Total Population

With both the Census data and the neighborhood geometry, we can use a
technique called areal weighted interpolation to produce estimated
populations for each neighborhood based on the degree of overlap between
neighborhood boundaries and census tract boundaries:

``` r
# interpolate values
nhood <- aw_interpolate(nhood, tid = NHD_NUM, source = pop, sid = GEOID, 
                        weight = "sum", output = "tibble", extensive = "total_pop")

# unit test estimates
expect_equal(aw_verify(source = pop, sourceValue = total_pop, result = nhood, resultValue = total_pop), TRUE)
```

### Clean-up Enviornment

With our neighborhood estimates created, we can remove the initial `pop`
object we created with the tract-level data:

``` r
rm(pop, county)
```

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

## Summarize Data

With our objects created, we can group our data by neighborhood. and
them summarize them.

### 2017

First, homicides:

``` r
# full
homicide_2017 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2017,
    range = "full",
    type = "homicide"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> homicide_2017

# partial
homicide_2017p %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2017,
    range = "partial",
    type = "homicide"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> homicide_2017p
```

Next, violent crimes:

``` r
# full
violent_2017 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2017,
    range = "full",
    type = "violent"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> violent_2017

# partial
violent_2017p %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2017,
    range = "partial",
    type = "violent"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> violent_2017p
```

Finally, Part 1 crimes:

``` r
# full
part1_2017 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2017,
    range = "full",
    type = "part 1"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> part1_2017

# partial
part1_2017p %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2017,
    range = "partial",
    type = "part 1"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> part1_2017p
```

With these created, we can combine them into a single 2017 object:

``` r
# bind
bind_2017 <- bind_rows(homicide_2017, part1_2017, violent_2017,
                       homicide_2017p, part1_2017p, violent_2017p)

# clean-up
rm(homicide_2017, part1_2017, violent_2017,
                       homicide_2017p, part1_2017p, violent_2017p)
```

### 2018

First, homicides:

``` r
# full
homicide_2018 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2018,
    range = "full",
    type = "homicide"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> homicide_2018

# partial
homicide_2018p %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2018,
    range = "partial",
    type = "homicide"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> homicide_2018p
```

Next, violent crimes:

``` r
# full
violent_2018 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2018,
    range = "full",
    type = "violent"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> violent_2018

# partial
violent_2018p %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2018,
    range = "partial",
    type = "violent"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> violent_2018p
```

Finally, Part 1 crimes:

``` r
# full
part1_2018 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2018,
    range = "full",
    type = "part 1"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> part1_2018

# partial
part1_2018p %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2018,
    range = "partial",
    type = "part 1"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> part1_2018p
```

With these created, we can combine them into a single 2018 object:

``` r
# bind
bind_2018 <- bind_rows(homicide_2018, part1_2018, violent_2018,
                       homicide_2018p, part1_2018p, violent_2018p)

# clean-up
rm(homicide_2018, part1_2018, violent_2018,
                       homicide_2018p, part1_2018p, violent_2018p)
```

### 2019

First, homicides:

``` r
# partial
homicide_2019 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2019,
    range = "partial",
    type = "homicide"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> homicide_2019
```

Next, violent crimes:

``` r
# partial
violent_2019 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2019,
    range = "partial",
    type = "violent"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> violent_2019
```

Finally, Part 1 crimes:

``` r
# partial
part1_2019 %>%
  rename(NHD_NUM = neighborhood) %>%
  group_by(NHD_NUM) %>%
  summarise(count = n()) %>%
  filter(NHD_NUM > 0) %>%
  mutate(
    year = 2019,
    range = "partial",
    type = "part 1"
  ) %>%
  select(year, range, type, NHD_NUM, count) -> part1_2019
```

With these created, we can combine them into a single 2019 object:

``` r
# bind
bind_2019 <- bind_rows(homicide_2019, part1_2019, violent_2019)

# clean-up
rm(homicide_2019, part1_2019, violent_2019)
```

## Combine and Calculate Rates

We can collapse our three objects further:

``` r
# bind
nhood_crime <- bind_rows(bind_2017, bind_2018, bind_2019)

# clean-up
rm(bind_2017, bind_2018, bind_2019)
```

Before we calculate rates, we’ll subset our `nhood_crime` object into
two - one for major parks (that do not have residential populations and
therefore cannot have rates calculated), and one for residential
neighborhoods.

``` r
majorPark_crime <- filter(nhood_crime, NHD_NUM >= 80) %>%
  rename(nhd_num = NHD_NUM)
  
nhood_crime <- filter(nhood_crime, NHD_NUM < 80)
```

Then we can calculate rates:

``` r
# calculate
nhood %>%
  select(-NHD_NAME) %>%
  left_join(nhood_crime, ., by = "NHD_NUM") %>%
  mutate(rate = count/total_pop*1000) %>%
  select(year, range, type, NHD_NUM, count, rate, total_pop) %>%
  rename(nhd_num = NHD_NUM) -> nhood

# clean-up
rm(nhood_crime)
```

## Write Data

Now we can write our city-wide estimates:

``` r
write_csv(nhood, path = here("data", "region", "neighborhoods.csv"))
write_csv(majorPark_crime, path = here("data", "region", "major_parks.csv"))
```
