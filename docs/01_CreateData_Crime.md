Initial Crime Rate Data Set Creation, 2017-2019
================
Christopher Prener, Ph.D.
(November 03, 2019)

## Introduction

This notebook creates a set of three initial `.csv` files containing
**all** Part 1 crimes for 2017, 2018, and 2019. These are used as the
basis for creating neighborhood, regional, and city-wide crime rate
files.

## Dependencies

This notebook requires the following `R` packages:

``` r
# tidyverse packages
library(dplyr)
library(readr)
library(stringr)

# spatial packages
library(gateway)
library(sf)

# other packages
library(compstatr)
library(here)
library(knitr)
library(testthat)
```

## Build Geocoders

One of the steps we’ll be taking below is to geocode crimes that have
missing x,y coordinate data. To do that, we’ll need to build three local
geocoders that will be part of the composite geocoder workflow:

``` r
geocoder <- gw_build_geocoder(style = "full", return = "coords")
geocoder_s <- gw_build_geocoder(style = "short", return = "coords")
geocoder_p <- gw_build_geocoder(style = "placename", return = "coords")
```

## Crime Data

Our first goal is to download and validate crime data for each of the
three focal years.

### Download Crime Data

The initial step is to create an index of data available from the
St. Louis Metropolitan Police Department’s website:

``` r
i <- cs_create_index()
```

With the index created, we can download a full year’s worth of crime
data from 2018 as well as the January through August tables for 2019:

``` r
# 2017
data2017_raw <- cs_get_data(year = 2017)

# 2018
data2018_raw <- cs_get_data(year = 2018)

# 2019
data2019_raw <- cs_get_data(year = 2019)
```

The data are downloaded as “year-list” objects - a list containing a
separate data frame for each month within the given year. Before we
collapse these into single tables for each year, we need to validate
them.

### Validate 2019

The validation process ensures that the tables within each year-list are
formatted correctly. This is done using a unit test to ensure that
changes to the data over time do not break subsequent analyses below.

``` r
expect_equal(cs_validate(data2019_raw, year = "2019"), TRUE)
```

We’ll also confirm that there are 9 months in our object:

``` r
expect_equal(length(data2019_raw), params$months)
```

Since the 2019 data are valid, we can collapse them into a single object
and then produce a slightly smaller table containing only crimes that
occured in 2019 (there are some crimes from prior years that were not
reported until 2019, and the SLMPD data include crimes based on year and
month reported, not year and month committed).

``` r
# collapse into single object
data2019_raw <- cs_collapse(data2019_raw)

# combine and filter
cs_combine(type = "year", date = 2019, data2019_raw) %>%
  cs_filter_count(var = count) -> crime2019
```

The `crime2019` object contains all crimes for 2019 (through August).

### 2018

We’ll follow the same process, first unit testing to ensure that we have
valid data:

``` r
expect_equal(cs_validate(data2018_raw, year = "2018"), TRUE)
```

Since the validation result is a value of `TRUE`, we can proceed to
collapsing the year-list object into a single tibble and processing it
as we did with the 2019 data:

``` r
# collapse into single object
data2018_raw <- cs_collapse(data2018_raw)

# combine and filter
cs_combine(type = "year", date = 2018, data2019_raw, data2018_raw) %>%
  cs_filter_count(var = count) -> crime2018
```

We use both the 2018 and 2019 raw data to do this to capture any crimes
reported in 2019 that occured in 2018. These make the 2018 data set
created not directly comparable to the FBI’s UCR values for the city,
since those reflect data that do not take more recently reported crimes
into account.

### 2017

We’ll repeat the validation process with the 2017 data:

``` r
expect_equal(cs_validate(data2017_raw, year = "2017"), FALSE)
```

Since we fail the validation, we can use the `verbose = TRUE` option to
get a summary of where validation issues are occurring.

``` r
cs_validate(data2017_raw, year = "2017", verbose = TRUE)
```

    ## # A tibble: 12 x 8
    ##    namedMonth codedMonth valMonth codedYear valYear oneMonth varCount
    ##    <chr>      <chr>      <lgl>        <int> <lgl>   <lgl>    <lgl>   
    ##  1 January    January    TRUE          2017 TRUE    TRUE     TRUE    
    ##  2 February   February   TRUE          2017 TRUE    TRUE     TRUE    
    ##  3 March      March      TRUE          2017 TRUE    TRUE     TRUE    
    ##  4 April      April      TRUE          2017 TRUE    TRUE     TRUE    
    ##  5 May        May        TRUE          2017 TRUE    TRUE     FALSE   
    ##  6 June       June       TRUE          2017 TRUE    TRUE     TRUE    
    ##  7 July       July       TRUE          2017 TRUE    TRUE     TRUE    
    ##  8 August     August     TRUE          2017 TRUE    TRUE     TRUE    
    ##  9 September  September  TRUE          2017 TRUE    TRUE     TRUE    
    ## 10 October    October    TRUE          2017 TRUE    TRUE     TRUE    
    ## 11 November   November   TRUE          2017 TRUE    TRUE     TRUE    
    ## 12 December   December   TRUE          2017 TRUE    TRUE     TRUE    
    ## # … with 1 more variable: valVars <lgl>

The data for May 2017 do not pass the validation checks. We can extract
this month and confirm that there are too many columns in the May 2017
release. Once we have that confirmed, we can standardize that month and
re-run our validation.

``` r
# extract data and unit test column numbers
expect_equal(ncol(cs_extract_month(data2017_raw, month = "May")), 26)

# standardize months
data2017_raw <- cs_standardize(data2017_raw, month = "May", config = 26)

# validate data
expect_equal(cs_validate(data2017_raw, year = "2017"), TRUE)
```

We now get a `TRUE` value for `cs_validate()` and can move on to
collapsing the 2017 and 2018 raw data objects to create a new object,
`data2017`, that contains all known 2017 crimes including those that
were reported or upgraded in 2018.

``` r
# collapse into single object
data2017_raw <- cs_collapse(data2017_raw)

# combine and filter
cs_combine(type = "year", date = 2017, data2019_raw, data2018_raw, data2017_raw) %>%
  cs_filter_count(var = count) -> crime2017
```

### Subset Part 1 Crimes

Now that we have validated data, we can create three smaller objects
containing only the Part 1 crimes for each year:

``` r
part1_2017 <- cs_filter_crime(crime2017, var = crime, crime = "Part 1")
part1_2018 <- cs_filter_crime(crime2018, var = crime, crime = "Part 1")
part1_2019 <- cs_filter_crime(crime2019, var = crime, crime = "Part 1")
```

### Clean-up Environment

We’ll clean-up our environment to get rid of now unneeded objects:

``` r
rm(data2019_raw, data2018_raw, data2017_raw, crime2017, crime2018, crime2019, i)
```

## Remove Unneeded Columns

There are a number of columns that we do not need. Removing them will
keep our data sets smaller and more manageable, both on disk and while
we are working with them:

``` r
part1_2017 <- select(part1_2017, -coded_month, -flag_crime, -flag_unfounded, -flag_administrative,
                     -count, -flag_cleanup, -district, -neighborhood, -location_name, -location_comment,
                     -cad_address, -cad_street)

part1_2018 <- select(part1_2018, -coded_month, -flag_crime, -flag_unfounded, -flag_administrative,
                     -count, -flag_cleanup, -district, -neighborhood, -location_name, -location_comment,
                     -cad_address, -cad_street)

part1_2019 <- select(part1_2019, -coded_month, -flag_crime, -flag_unfounded, -flag_administrative,
                     -count, -flag_cleanup, -district, -neighborhood, -location_name, -location_comment,
                     -cad_address, -cad_street)
```

## Improve Sample Size

There are missing spatial data in each of the crime objects:

``` r
# identify missing
part1_2017 %>% 
  cs_missingXY(varX = x_coord, varY = y_coord, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> part1_2017

part1_2018 %>% 
  cs_missingXY(varX = x_coord, varY = y_coord, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> part1_2018

part1_2019 %>% 
  cs_missingXY(varX = x_coord, varY = y_coord, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> part1_2019

# summarize
missing <- tibble(
  year = c(2017, 2018, 2019),
  pct_missing_pre = c(mean(part1_2017$missingXY)*100, mean(part1_2018$missingXY)*100,
                  mean(part1_2019$missingXY)*100))

# print summary
kable(missing, digits = 2, col.names = c("Year", "% Missing"))
```

| Year | % Missing |
| ---: | --------: |
| 2017 |      1.98 |
| 2018 |      2.06 |
| 2019 |      1.80 |

While these numbers are absolutely within the confines of acceptable
missing data, we will make some initial efforts to geocode them. Before
we do this, we’ll store the count of each years’ records so that we can
execute unit tests after the geocoding process is complete.

``` r
count_2017 <- nrow(part1_2017)
count_2018 <- nrow(part1_2018)
count_2019 <- nrow(part1_2019)
```

### Prepare for Geocoding

First, we need to subset them to isolate those that are missing and
create a single address variable:

``` r
part1_2017 %>% 
  filter(missingXY == TRUE) %>%
  mutate(address = paste(ileads_address, ileads_street)) %>%
  mutate(address = str_replace(address, pattern = "[/@]", replacement = "at")) %>%
  mutate(address = ifelse(word(address, start = 1) == "0", 
                          word(address, start = 2, end = -1), address)) %>%
  select(-x_coord, -y_coord) -> part1_2017_miss

part1_2018 %>% 
  filter(missingXY == TRUE) %>%
  mutate(address = paste(ileads_address, ileads_street)) %>%
  mutate(address = str_replace(address, pattern = "[/@]", replacement = "at")) %>%
  mutate(address = ifelse(word(address, start = 1) == "0", 
                          word(address, start = 2, end = -1), address)) %>%
  select(-x_coord, -y_coord) -> part1_2018_miss

part1_2019 %>% 
  filter(missingXY == TRUE) %>%
  mutate(address = paste(ileads_address, ileads_street)) %>%
  mutate(address = str_replace(address, pattern = "[/@]", replacement = "at")) %>%
  mutate(address = ifelse(word(address, start = 1) == "0", 
                          word(address, start = 2, end = -1), address)) %>%
  select(-x_coord, -y_coord) -> part1_2019_miss
```

Next, we’ll remove the missing rows from the original objects:

``` r
part1_2017 <- filter(part1_2017, missingXY == FALSE)
part1_2018 <- filter(part1_2018, missingXY == FALSE)
part1_2019 <- filter(part1_2019, missingXY == FALSE)
```

### Geocode

Then we’ll pass these addresses through the City’s batch geocoder:

``` r
part1_2017_miss <- gw_geocode(part1_2017_miss, type = "composite, full", 
                              var = address, class = "tibble", 
                              local = geocoder, local_short = geocoder_s, 
                              local_place = geocoder_p, threshold = 90)
```

    ## Warning: `cols` is now required.
    ## Please use `cols = c(geo)`

``` r
part1_2018_miss <- gw_geocode(part1_2018_miss, type = "composite, full", 
                              var = address, class = "tibble", 
                              local = geocoder, local_short = geocoder_s, 
                              local_place = geocoder_p, threshold = 90)
```

    ## Warning: `cols` is now required.
    ## Please use `cols = c(geo)`

``` r
part1_2019_miss <- gw_geocode(part1_2019_miss, type = "composite, full", 
                              var = address, class = "tibble", 
                              local = geocoder, local_short = geocoder_s, 
                              local_place = geocoder_p, threshold = 90)
```

    ## Warning: `cols` is now required.
    ## Please use `cols = c(geo)`

### Clean-up Geocoded Data

There are two issues with these objects - the coordinate columns in the
valid and geocoded data sets do not line up anymore, and they use
different coordinate systems. To combine them, we’ll project our
original data separately, transform the coordinate system, and then bind
them. First, we’ll project the valid data:

``` r
part1_2017 %>% 
  select(-missingXY) %>%
  cs_projectXY(varX = x_coord, varY = y_coord, crs = 4269) %>%
  gw_get_coords() -> part1_2017

st_geometry(part1_2017) <- NULL
  
part1_2018 %>% 
  select(-missingXY) %>%
  cs_projectXY(varX = x_coord, varY = y_coord, crs = 4269) %>%
  gw_get_coords() -> part1_2018

st_geometry(part1_2018) <- NULL

part1_2019 %>% 
  select(-missingXY) %>%
  cs_projectXY(varX = x_coord, varY = y_coord, crs = 4269) %>%
  gw_get_coords() -> part1_2019

st_geometry(part1_2019) <- NULL
```

Then, we’ll clean the missing but geocoded data:

``` r
part1_2017_miss %>%
  rename(x = gw_x, y = gw_y) %>%
  select(-missingXY, -address, -gw_addrrecnum, -gw_id) -> part1_2017_miss

part1_2018_miss %>%
  rename(x = gw_x, y = gw_y) %>%
  select(-missingXY, -address, -gw_addrrecnum, -gw_id) -> part1_2018_miss

part1_2019_miss %>%
  rename(x = gw_x, y = gw_y) %>%
  select(-missingXY, -address, -gw_addrrecnum, -gw_id) -> part1_2019_miss
```

Next, we’ll bind the data objects together and re-order them by date and
time:

``` r
part1_2017 <- bind_rows(part1_2017, part1_2017_miss)

part1_2018 <- bind_rows(part1_2018, part1_2018_miss)

part1_2019 <- bind_rows(part1_2019, part1_2019_miss) 
```

Finally, we’ll run our unit tests to make sure we have the same sample
size we began with:

``` r
expect_equal(nrow(part1_2017), count_2017)
expect_equal(nrow(part1_2018), count_2018)
expect_equal(nrow(part1_2019), count_2019)
```

### Clean-up Environment

With our objects combined and tested, we can get rid of the objects
we’ve created:

``` r
rm(geocoder, geocoder_p, geocoder_s, part1_2017_miss, part1_2018_miss, part1_2019_miss)
```

### Update Missing Data Statistics

With our data put back together, we can also check to see the degree to
which we’ve included the amount of spatial data we have:

``` r
# identify missing
part1_2017 %>% 
  cs_missingXY(varX = x, varY = y, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> part1_2017

part1_2018 %>% 
  cs_missingXY(varX = x, varY = y, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> part1_2018

part1_2019 %>% 
  cs_missingXY(varX = x, varY = y, newVar = missingXY) %>%
  mutate(missingXY = as.logical(missingXY)) -> part1_2019

# summarize
missing <- tibble(
  year = missing$year,
  pct_missing_pre = missing$pct_missing_pre,
  pct_missing_post = c(mean(part1_2017$missingXY)*100, mean(part1_2018$missingXY)*100,
                  mean(part1_2019$missingXY)*100)
  )

# remove missing data variable
part1_2017 <- select(part1_2017, -missingXY)
part1_2018 <- select(part1_2018, -missingXY)
part1_2019 <- select(part1_2019, -missingXY)

# add percent change
missing <- mutate(missing, delta = (pct_missing_post-pct_missing_pre)/pct_missing_pre*100)

# print summary
kable(missing, digits = 2, col.names = c("Year", "% Missing, Pre", "% Missing, Post", "Delta"))
```

| Year | % Missing, Pre | % Missing, Post |   Delta |
| ---: | -------------: | --------------: | ------: |
| 2017 |           1.98 |            0.59 | \-70.37 |
| 2018 |           2.06 |            0.58 | \-71.66 |
| 2019 |           1.80 |            0.58 | \-67.58 |

## Add Neighborhoods and Regions

To add neighborhood identifiers and region names, we need to open these
data sources, transform their coordinate systems to a projected system,
and prepare them for the spatial join:

``` r
nhood <- gw_get_data(data = "Neighborhoods", class = "sf") %>%
  st_transform(crs = 26915) %>%
  select(NHD_NUM) %>%
  rename(neighborhood = NHD_NUM)

region <- st_read(here("data", "raw", "haydenRegions"), stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915) %>%
  select(NAME) %>%
  rename(region = NAME)
```

    ## Reading layer `haydenRegions' from data source `/Users/chris/GitHub/chris-prener/STL_CRIME_Nhood/data/raw/haydenRegions' using driver `ESRI Shapefile'
    ## Simple feature collection with 7 features and 4 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 871512.3 ymin: 982994.4 xmax: 912850.5 ymax: 1070957
    ## epsg (SRID):    NA
    ## proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs

Next, we need to project each year’s data, transform the crs to a
matching projected coordinate system, apply both spatial joins, re-order
based on the date and time the crime occured, re-arrange our columns,
and get our `x` and `y` coordinate columns back. First, we’ll do it for
the 2017 data:

``` r
# create ids
part1_2017 <- tibble::rowid_to_column(part1_2017, var = "id")

# spatial joins
part1_2017 %>%
  filter(is.na(x) == FALSE) %>%
  st_as_sf(coords = c("x", "y"), crs = 4269) %>%
  st_transform(crs = 26915) %>%
  st_intersection(., nhood) %>%
  st_intersection(., region) %>%
  select(id, cs_year, complaint, date_occur, crime, description, ileads_address,
         ileads_street, neighborhood, region, gw_source, gw_address, gw_score) %>%
  gw_get_coords() -> part1_2017_joined
```

    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries
    
    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries

``` r
# convert back to data frame
st_geometry(part1_2017_joined) <- NULL

# store count of joined data
count_geo_2017 <- nrow(part1_2017_joined)

# store joined ids in vector
part1_2017_joined_ids <- part1_2017_joined$id

# find missing records
part1_2017 <- filter(part1_2017, id %in% part1_2017_joined_ids == FALSE)

# bind with missing
bind_rows(part1_2017_joined, part1_2017) %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  arrange(dateTime) %>%
  select(-id, -dateTime, -date, -time) -> part1_2017
```

Next, we’ll do it for the 2018 data:

``` r
# create ids
part1_2018 <- tibble::rowid_to_column(part1_2018, var = "id")

# spatial joins
part1_2018 %>%
  filter(is.na(x) == FALSE) %>%
  st_as_sf(coords = c("x", "y"), crs = 4269) %>%
  st_transform(crs = 26915) %>%
  st_intersection(., nhood) %>%
  st_intersection(., region) %>%
  select(id, cs_year, complaint, date_occur, crime, description, ileads_address,
         ileads_street, neighborhood, region, gw_source, gw_address, gw_score) %>%
  gw_get_coords() -> part1_2018_joined
```

    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries
    
    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries

``` r
# convert back to data frame
st_geometry(part1_2018_joined) <- NULL

# store count of joined data
count_geo_2018 <- nrow(part1_2018_joined)

# store joined ids in vector
part1_2018_joined_ids <- part1_2018_joined$id

# find missing records
part1_2018 <- filter(part1_2018, id %in% part1_2018_joined_ids == FALSE)

# bind with missing
bind_rows(part1_2018_joined, part1_2018) %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  arrange(dateTime) %>%
  select(-id, -dateTime, -date, -time) -> part1_2018
```

Next, we’ll do it for the 2019 data:

``` r
# create ids
part1_2019 <- tibble::rowid_to_column(part1_2019, var = "id")

# spatial joins
part1_2019 %>%
  filter(is.na(x) == FALSE) %>%
  st_as_sf(coords = c("x", "y"), crs = 4269) %>%
  st_transform(crs = 26915) %>%
  st_intersection(., nhood) %>%
  st_intersection(., region) %>%
  select(id, cs_year, complaint, date_occur, crime, description, ileads_address,
         ileads_street, neighborhood, region, gw_source, gw_address, gw_score) %>%
  gw_get_coords() -> part1_2019_joined
```

    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries
    
    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries

``` r
# convert back to data frame
st_geometry(part1_2019_joined) <- NULL

# store count of joined data
count_geo_2019 <- nrow(part1_2019_joined)

# store joined ids in vector
part1_2019_joined_ids <- part1_2019_joined$id

# find missing records
part1_2019 <- filter(part1_2019, id %in% part1_2019_joined_ids == FALSE)

# bind with missing
bind_rows(part1_2019_joined, part1_2019) %>%
  cs_parse_date(var = date_occur, dateVar = date, timeVar = time) %>%
  arrange(dateTime) %>%
  select(-id, -dateTime, -date, -time) -> part1_2019
```

Finally, we’ll run our unit tests to make sure we have the same sample
size we began with:

``` r
expect_equal(nrow(part1_2017), count_2017)
expect_equal(nrow(part1_2018), count_2018)
expect_equal(nrow(part1_2019), count_2019)
```

## Final Check of How Data Are Changing

Before we write our data, we’ll print a final summary of how our data
are changing:

``` r
# summarize
missing <- tibble(
  year = missing$year,
  pct_missing_pre = missing$pct_missing_pre,
  pct_missing_post = missing$pct_missing_post,
  delta = missing$delta,
  pct_not_joined =  c(100-(count_geo_2017/count_2017*100), 
                      100-(count_geo_2018/count_2018*100),
                      100-(count_geo_2019/count_2019*100))
  )

# print summary
kable(missing, digits = 2, col.names = c("Year", "% Missing, Pre", "% Missing, Post", "Delta", "% Not Joined"))
```

| Year | % Missing, Pre | % Missing, Post |   Delta | % Not Joined |
| ---: | -------------: | --------------: | ------: | -----------: |
| 2017 |           1.98 |            0.59 | \-70.37 |         0.64 |
| 2018 |           2.06 |            0.58 | \-71.66 |         0.68 |
| 2019 |           1.80 |            0.58 | \-67.58 |         0.64 |

## Write Data

With our initial data acquisition and cleaning completed, we’ll write
our crime tables to the `data/crimes` sub-folder:

``` r
write_csv(part1_2017, here("data", "crimes", "part1_2017.csv"))
write_csv(part1_2018, here("data", "crimes", "part1_2018.csv"))
write_csv(part1_2019, here("data", "crimes", "part1_2019.csv"))
```
