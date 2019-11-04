Regional Crime Data, 2017-2019
================
Christopher Prener, Ph.D.
(November 03, 2019)

## Introduction

This notebook creates a set of `.csv` files containing estimates at the
“regional” level - several experimental groupings of City of St. Louis
neighborhoods to make direct comparisons to Hayden’s Rectangle.

## Dependencies

This notebook requires the following `R` packages:

``` r
# tidyverse packages
library(dplyr)
library(readr)
library(stringr)

# spatial packages
library(areal)
library(sf)
library(tidycensus)

# other packages
library(compstatr)
library(here)
library(testthat)
```

## Load Data

We have a shapefile containing these regions in the `data/raw`
subdirectory:

``` r
regions <- st_read(here("data", "raw", "haydenRegions"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915) %>%
  select(-Shape_Leng, -Shape_Area)
```

    ## Reading layer `haydenRegions' from data source `/Users/chris/GitHub/chris-prener/STL_CRIME_Nhood/data/raw/haydenRegions' using driver `ESRI Shapefile'
    ## Simple feature collection with 7 features and 4 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 871512.3 ymin: 982994.4 xmax: 912850.5 ymax: 1070957
    ## epsg (SRID):    102696
    ## proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs

The data for these table will be drawn from the three yearly `.csv`
files created by `01_CreateData_Crime.Rmd`. We’ll load those first:

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

## Prepare Regional Data

Our first task is to calculate the total estimated population for each
region.

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

### Estimate Neighborhood Region Population

With both the Census data and the regional geometry, we can use a
technique called areal weighted interpolation to produce estimated
populations for each region based on the degree of overlap between
regional boundaries and census tract boundaries:

``` r
# interpolate values
regions <- aw_interpolate(regions, tid = ID, source = pop, sid = GEOID, 
                        weight = "sum", output = "tibble", extensive = "total_pop")

# unit test estimates
expect_equal(aw_verify(source = pop, sourceValue = total_pop, 
                       result = regions, resultValue = total_pop), TRUE)
```

### Clean-up Enviornment

Next, we’ll remove the `pop` object from our enviornment:

``` r
rm(pop)
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

With our objects created, we can group our data by region and them
summarize them.

### 2017

First, homicides:

``` r
# full
homicide_2017 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2017,
    range = "full",
    type = "homicide"
  ) %>%
  select(year, range, type, region, count) -> homicide_2017

# partial
homicide_2017p %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2017,
    range = "partial",
    type = "homicide"
  ) %>%
  select(year, range, type, region, count) -> homicide_2017p
```

Next, violent crimes:

``` r
# full
violent_2017 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2017,
    range = "full",
    type = "violent"
  ) %>%
  select(year, range, type, region, count) -> violent_2017

# partial
violent_2017p %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2017,
    range = "partial",
    type = "violent"
  ) %>%
  select(year, range, type, region, count) -> violent_2017p
```

Finally, Part 1 crimes:

``` r
# full
part1_2017 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2017,
    range = "full",
    type = "part 1"
  ) %>%
  select(year, range, type, region, count) -> part1_2017

# partial
part1_2017p %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2017,
    range = "partial",
    type = "part 1"
  ) %>%
  select(year, range, type, region, count) -> part1_2017p
```

With these created, we can combine them into a single 2017 object:

``` r
# bind
bind_2017 <- bind_rows(homicide_2017, part1_2017, violent_2017,
                       homicide_2017p, part1_2017p, violent_2017p)

# clean-up
rm(homicide_2017, part1_2017, violent_2017, homicide_2017p, part1_2017p, violent_2017p)
```

### 2018

First, homicides:

``` r
# full
homicide_2018 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2018,
    range = "full",
    type = "homicide"
  ) %>%
  select(year, range, type, region, count) -> homicide_2018

# partial
homicide_2018p %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2018,
    range = "partial",
    type = "homicide"
  ) %>%
  select(year, range, type, region, count) -> homicide_2018p
```

Next, violent crimes:

``` r
# full
violent_2018 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2018,
    range = "full",
    type = "violent"
  ) %>%
  select(year, range, type, region, count) -> violent_2018

# partial
violent_2018p %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2018,
    range = "partial",
    type = "violent"
  ) %>%
  select(year, range, type, region, count) -> violent_2018p
```

Finally, Part 1 crimes:

``` r
# full
part1_2018 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2018,
    range = "full",
    type = "part 1"
  ) %>%
  select(year, range, type, region, count) -> part1_2018

# partial
part1_2018p %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2018,
    range = "partial",
    type = "part 1"
  ) %>%
  select(year, range, type, region, count) -> part1_2018p
```

With these created, we can combine them into a single 2018 object:

``` r
# bind
bind_2018 <- bind_rows(homicide_2018, part1_2018, violent_2018,
                       homicide_2018p, part1_2018p, violent_2018p)

# clean-up
rm(homicide_2018, part1_2018, violent_2018, homicide_2018p, part1_2018p, violent_2018p)
```

### 2019

First, homicides:

``` r
# partial
homicide_2019 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2019,
    range = "partial",
    type = "homicide"
  ) %>%
  select(year, range, type, region, count) -> homicide_2019
```

Next, violent crimes:

``` r
# partial
violent_2019 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2019,
    range = "partial",
    type = "violent"
  ) %>%
  select(year, range, type, region, count) -> violent_2019
```

Finally, Part 1 crimes:

``` r
# partial
part1_2019 %>%
  group_by(region) %>%
  summarize(count = n()) %>%
  mutate(
    year = 2019,
    range = "partial",
    type = "part 1"
  ) %>%
  select(year, range, type, region, count) -> part1_2019
```

With these created, we can combine them into a single 2018 object:

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
regional_crime <- bind_rows(bind_2017, bind_2018, bind_2019)

# clean-up
rm(bind_2017, bind_2018, bind_2019)
```

Then we can calculate rates:

``` r
# calculate
regions %>%
  select(-ID) %>%
  rename(region = NAME) %>%
  left_join(regional_crime, ., by = "region") %>%
  mutate(rate = count/total_pop*1000) %>%
  select(year, range, type, region, count, rate, total_pop) -> regions

# clean-up
rm(regional_crime)
```

## Write Data

Now we can write our city-wide estimates:

``` r
write_csv(regions, path = here("data", "region", "region.csv"))
```
