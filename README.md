# Divvy case study


# Introduction

In this case study we take a look at the dataset provided by Divvy, a
company that is in the business of bicycle rent. We will try to find
useful insights about differences between members and casual users of
the service that the marketing team could use find useful to develop
better marketing campaigns to persuade casual user to become members of
the service. In order to do this we will focus on the next questions:

- What are the problems the dataset presents? Is there any way to
  overcome or mitigate those problems?
- Are there any clear distinction between members and casual users of
  the service that could be helpful?
- What suggestions could be made to the marketing team?

With those goals settled, we can start our analysis of the data.

# Setting up packages

First, we will setup all the R packages that we will use.

``` r
library(tidyverse)
library(knitr)
library(mapview)
library(lubridate)
library(sf)
library(jsonlite)
library(geosphere)
library(furrr)
```

Loading data from source, if not it is available locally.

``` r
dates <- list("202310", "202311", "202312", "202401", "202402", "202403", "202404", "202405", "202406", "202407", "202408", "202409")
df <- tibble()
for (date in dates) {
  file <- sprintf("%s-divvy-tripdata.zip", date)
  if (!file.exists(file)) {
    link <- sprintf("https://divvy-tripdata.s3.amazonaws.com/%s", file)
    download.file(link, file)
  }
  df <- df |> bind_rows(read_csv(file, show_col_types = FALSE))
}
```

# Exploratory face

Read file as a tibble and first look at data.

``` r
kable(head(df))
```

| ride_id          | rideable_type | started_at          | ended_at            | start_station_name                   | start_station_id | end_station_name            | end_station_id | start_lat | start_lng |  end_lat |   end_lng | member_casual |
|:-----------------|:--------------|:--------------------|:--------------------|:-------------------------------------|:-----------------|:----------------------------|:---------------|----------:|----------:|---------:|----------:|:--------------|
| 4449097279F8BBE7 | classic_bike  | 2023-10-08 10:36:26 | 2023-10-08 10:49:19 | Orleans St & Chestnut St (NEXT Apts) | 620              | Sheffield Ave & Webster Ave | TA1309000033   |  41.89820 | -87.63754 | 41.92154 | -87.65382 | member        |
| 9CF060543CA7B439 | electric_bike | 2023-10-11 17:23:59 | 2023-10-11 17:36:08 | Desplaines St & Kinzie St            | TA1306000003     | Sheffield Ave & Webster Ave | TA1309000033   |  41.88864 | -87.64441 | 41.92154 | -87.65382 | member        |
| 667F21F4D6BDE69C | electric_bike | 2023-10-12 07:02:33 | 2023-10-12 07:06:53 | Orleans St & Chestnut St (NEXT Apts) | 620              | Franklin St & Lake St       | TA1307000111   |  41.89807 | -87.63751 | 41.88584 | -87.63550 | member        |
| F92714CC6B019B96 | classic_bike  | 2023-10-24 19:13:03 | 2023-10-24 19:18:29 | Desplaines St & Kinzie St            | TA1306000003     | Franklin St & Lake St       | TA1307000111   |  41.88872 | -87.64445 | 41.88584 | -87.63550 | member        |
| 5E34BA5DE945A9CC | classic_bike  | 2023-10-09 18:19:26 | 2023-10-09 18:30:56 | Desplaines St & Kinzie St            | TA1306000003     | Franklin St & Lake St       | TA1307000111   |  41.88872 | -87.64445 | 41.88584 | -87.63550 | member        |
| F7D7420AFAC53CD9 | electric_bike | 2023-10-04 17:10:59 | 2023-10-04 17:25:21 | Orleans St & Chestnut St (NEXT Apts) | 620              | Sheffield Ave & Webster Ave | TA1309000033   |  41.89812 | -87.63753 | 41.92154 | -87.65382 | member        |

``` r
kable(summary(df))
```

|     | ride_id          | rideable_type    | started_at                     | ended_at                       | start_station_name | start_station_id | end_station_name | end_station_id   | start_lat     | start_lng      | end_lat       | end_lng         | member_casual    |
|:----|:-----------------|:-----------------|:-------------------------------|:-------------------------------|:-------------------|:-----------------|:-----------------|:-----------------|:--------------|:---------------|:--------------|:----------------|:-----------------|
|     | Length:5854544   | Length:5854544   | Min. :2023-10-01 00:00:05.00   | Min. :2023-10-01 00:02:02.00   | Length:5854544     | Length:5854544   | Length:5854544   | Length:5854544   | Min. :41.64   | Min. :-87.94   | Min. :16.06   | Min. :-144.05   | Length:5854544   |
|     | Class :character | Class :character | 1st Qu.:2024-02-27 05:34:06.75 | 1st Qu.:2024-02-27 05:50:02.00 | Class :character   | Class :character | Class :character | Class :character | 1st Qu.:41.88 | 1st Qu.:-87.66 | 1st Qu.:41.88 | 1st Qu.: -87.66 | Class :character |
|     | Mode :character  | Mode :character  | Median :2024-06-06 12:55:34.65 | Median :2024-06-06 13:17:11.44 | Mode :character    | Mode :character  | Mode :character  | Mode :character  | Median :41.90 | Median :-87.64 | Median :41.90 | Median : -87.64 | Mode :character  |
|     | NA               | NA               | Mean :2024-05-08 12:56:33.76   | Mean :2024-05-08 13:13:52.17   | NA                 | NA               | NA               | NA               | Mean :41.90   | Mean :-87.65   | Mean :41.90   | Mean : -87.65   | NA               |
|     | NA               | NA               | 3rd Qu.:2024-08-05 11:12:35.00 | 3rd Qu.:2024-08-05 11:35:24.94 | NA                 | NA               | NA               | NA               | 3rd Qu.:41.93 | 3rd Qu.:-87.63 | 3rd Qu.:41.93 | 3rd Qu.: -87.63 | NA               |
|     | NA               | NA               | Max. :2024-09-30 23:54:05.54   | Max. :2024-09-30 23:59:52.55   | NA                 | NA               | NA               | NA               | Max. :42.07   | Max. :-87.52   | Max. :87.96   | Max. : 1.72     | NA               |
|     | NA               | NA               | NA                             | NA                             | NA                 | NA               | NA               | NA               | NA            | NA             | NA’s :7441    | NA’s :7441      | NA               |

``` r
dim(df)
```

    [1] 5854544      13

``` r
df |>
  select(ride_id) |>
  unique() |>
  dim()
```

    [1] 5854333       1

``` r
kable(colSums(is.na(df)))
```

|                    |       x |
|:-------------------|--------:|
| ride_id            |       0 |
| rideable_type      |       0 |
| started_at         |       0 |
| ended_at           |       0 |
| start_station_name | 1056535 |
| start_station_id   | 1056535 |
| end_station_name   | 1091792 |
| end_station_id     | 1091792 |
| start_lat          |       0 |
| start_lng          |       0 |
| end_lat            |    7441 |
| end_lng            |    7441 |
| member_casual      |       0 |

# Data cleaning

The dataset has columns with 18% of their content missing. Since those
columns are the name or the id of the stations, we can try to obtain
that information from other sources. Also, there are some rows that does
not have their geographical info on where the ride ended, let’s have a
look at those.

One may suspect that the rides that have missing end latitude and
longitude would be at the end of the month, thinking of this cut on the
dataset publishing as the main source of missing information. Let’s
check a graph that could sustain this hypothesis

``` r
df |>
  filter(is.na(end_lng)) |>
  ggplot() +
  aes(x = mday(started_at)) +
  geom_histogram(binwidth = 1, color = "#000000", fill = "#06CEFD") +
  global_theme() +
  facet_wrap(vars(month(started_at, label = TRUE, abbr = FALSE))) +
  labs(
    x = "Day of month",
    y = "Number of rides",
    title = "Nunber of rides with missing geografical location",
    subtitle = "by day of month"
  )
```

![We expected almost all rides of this kind to be at the end of the
month to prove our
hypothesis](README_files/figure-commonmark/Graph%20of%20rides%20with%20missing%20end%20location%20info-1.png)

We can’t see anything that suggest anything conclusive. Since we can’t
confidently recover the data lost, we should drop those entries.

``` r
df <- filter(df, !is.na(end_lng))
colSums(is.na(df))
```

               ride_id      rideable_type         started_at           ended_at 
                     0                  0                  0                  0 
    start_station_name   start_station_id   end_station_name     end_station_id 
               1056535            1056535            1084351            1084351 
             start_lat          start_lng            end_lat            end_lng 
                     0                  0                  0                  0 
         member_casual 
                     0 

The other data that is missing is naming and id info from the stations.
Reading how the company operates, if you leave a divvy in any other
place that is not a station, you will be charged a fee, we can consider
this the cause for those missing entries. This will be used as another
parameter in our analysis.

Looking back to the data summary at the beginning, we can see that some
coordinates are very far away from the range, let’s have some deeper
look at the stations info

``` r
df |>
  select(start_station_id) |>
  unique() |>
  dim()
```

    [1] 1701    1

``` r
df |>
  select(end_station_id) |>
  unique() |>
  dim()
```

    [1] 1711    1

``` r
df |>
  select(start_station_id, start_lat, start_lng) |>
  unique() |>
  dim()
```

    [1] 1342701       3

We can deduce that this dataset has multiple coordinates for some
stations. This might be due to how the coordinates and stations are
added to the database. I think that the coordinates are given by the
bicycle, and if the lock is activated in a station the name of the
station is added to the form before it is uploaded. This might be a
problem, since we detected that there are some coordinates that are very
far away from the city where Divvy is working. We may want to use the
location of the stations to look for some trends. Let’s find an example
of this problematic.

``` r
distm_v <- Vectorize(function(x1, y1, x2, y2) {
  distm(c(x1, y1), c(x2, y2), fun = distHaversine)
})

df |>
  select(start_station_id, start_station_name, start_lng, start_lat) |>
  filter(start_station_id == 13029) |>
  mutate(mean_lng = mean(start_lng), mean_lat = mean(start_lat), most_distant =
    distm_v(mean_lat, mean_lng, start_lat, start_lng)) |>
  filter(most_distant > 15) |>
  ggplot() +
  aes(x = most_distant) +
  scale_x_log10() +
  global_theme() +
  geom_histogram(bins = 100, color = "#000000", fill = "#06CEFD") +
  labs(
    x = "Distance (meters) to mean",
    y = "Number of rides",
    title = "Distribution of distance to mean of start coordinates",
    subtitle = 'Station "Station Flied Museum"',
    caption = "(only rides with distance > 15m)"
  )
```

![](README_files/figure-commonmark/unnamed-chunk-8-1.png)

This graph shows that the coordinates present in our data are
unreliable. Thus we need to look for another data source for information
about coordinates of stations.

Looking at the data page from divvy, there is a link for a json with
information about location of the stations, let’s incorporate that.

``` r
stations_json <- "stations.json"
if (!file.exists(stations_json)) {
  download.file(
    "https://gbfs.lyft.com/gbfs/2.3/chi/en/station_information.json",
    stations_json
  )
}
stations <- fromJSON(stations_json) |>
  _$data$stations |>
  as_tibble() |>
  select(station_id, short_name, name, lon, lat) |>
  mutate(lon = num(lon, digits = 6), lat = num(lat, digits = 6))

stations |>
  distinct(station_id) |>
  dim()
```

    [1] 1801    1

``` r
stations |>
  distinct(lat, lon) |>
  dim()
```

    [1] 1798    2

``` r
stations |>
  group_by(lat, lon) |>
  filter(n() > 1) |>
  ungroup()|>
  kable()
```

| station_id                           | short_name   | name                              |        lon |       lat |
|:-------------------------------------|:-------------|:----------------------------------|-----------:|----------:|
| a3a3a282-a135-11e9-9cda-0a87ae2ba916 | TA1306000014 | Wilton Ave & Diversey Pkwy        | -87.652705 | 41.932418 |
| d53ae727-5265-4b8e-a6ca-2a36dc0345c4 | chargingstx2 | Wilton Ave & Diversey Pkwy\*      | -87.652705 | 41.932418 |
| 1827484051430132402                  | NA           | Public Rack - Forest Glen Station | -87.755520 | 41.978710 |
| 1715823821144840768                  | NA           | Public Rack - Laflin St &51st ST  | -87.662076 | 41.801354 |
| 1677249871073777806                  | NA           | Public Rack - Laflin St & 51st St | -87.662076 | 41.801354 |
| 1827474404723843690                  | NA           | Public Rack - Peterson Park       | -87.755520 | 41.978710 |

``` r
stations |>
  group_by(name) |>
  filter(n() > 1) |>
  ungroup() |>
  kable()
```

| station_id          | short_name | name                   |        lon |       lat |
|:--------------------|:-----------|:-----------------------|-----------:|----------:|
| 1978857650118994914 | 24409      | Indiana Ave & 133rd St | -87.617190 | 41.653800 |
| 1967727360320698512 | 24211      | Western Ave & Lake St  | -87.686680 | 41.884810 |
| 1984042930424753006 | 24394      | Steelworkers Park      | -87.530910 | 41.737930 |
| 1448642188027369086 | NA         | Indiana Ave & 133rd St | -87.617054 | 41.653564 |
| 1594046379513303720 | NA         | Western Ave & Lake St  | -87.685853 | 41.884606 |
| 1448642188027369090 | NA         | Steelworkers Park      | -87.531067 | 41.738246 |

This data is mostly clean, just some typos in the system that are an
easy fix. Since I can count those errors with one hand, I manually check
them on Google Maps. Here are my findings:

1)  Wilton Ave & Diversey Pkwy has many places to park, maybe the
    company takes those as 2 different stations
2)  Forest Glen station and Peterson Park are 2 stations that are just a
    meter or 2 of distance, we can consider both as one station
3)  Laflin St & 51st St appears to be a typo, since there is just one
    station nearby.
4)  For those stations that share name but not coordinates, I’ve found
    that they are just stations that are very close to each other.
    Luckily, they have a different station id, and more over, only 1 of
    each pair have shortname, which is the column used as station id in
    our original dataframe.

Given those observations, we can merge

``` r
stations |>
  filter(
    station_id == "a3a3a282-a135-11e9-9cda-0a87ae2ba916" | station_id == ""
  )
```

    # A tibble: 1 x 5
      station_id                           short_name   name           lon       lat
      <chr>                                <chr>        <chr>    <num:.6!> <num:.6!>
    1 a3a3a282-a135-11e9-9cda-0a87ae2ba916 TA1306000014 Wilton~ -87.652705 41.932418

Awesome, now we have a clean dataset of the stations info

``` r
distm_v <- Vectorize(function(x1, y1, x2, y2) {
  distm(c(x1, y1), c(x2, y2), fun = distHaversine)
})

assert_value <- function(value) {
  if (dim(value)[1] == 0) {
    tibble(station_id = "")
  } else {
    value
  }
}

closest_station_margin <- function(margin) {
  function(latitude, longitude) {
    station_closest <- stations |>
      select(lat, lon, station_id) |>
      filter(
        lat < latitude + margin &
          lat > latitude - margin &
          lon < longitude + margin &
          lon > longitude - margin
      ) |>
      mutate(distance = distm_v(longitude, latitude, lon, lat)) |>
      slice_min(distance, n = 1, with_ties = FALSE) |>
      assert_value() |>
      _$station_id
  }
}

stations_geoinfo <- select(df, start_lat, start_lng) |>
  rename(lat = start_lat, lng = start_lng) |>
  bind_rows(select(df, end_lat, end_lng) |>
    rename(lat = end_lat, lng = end_lng)) |>
  unique()

stations_get_closest <- closest_station_margin(0.001)

plan(multicore, workers = 3)
stations_with_id <- if (file.exists("coords_with_names.csv")) {
  read_csv("coords_with_names.csv")
} else {
  stations.geoinfo |>
    rename(latitude = lat, longitude = lng) |>
    slice_sample(n = 50000) |>
    mutate(
      station_id = future_map2_chr(latitude, longitude, stations_get_closest)
    )
}
```

# Data cleaning

``` r
# graoh <- ggplot(data = df) +
#  aes(x = member_casual, fill = rideable_type) +
#  geom_bar()
#
# distm_v   <-  Vectorize(function(x1,y1,x2,y2) {
#  distm(c(x1, y1), c(x2, y2), fun = distHaversine)
# }
# )
#
# data  <- df |>
#  mutate(distance = distm_v(end_lng, end_lat, start_lng, end_lat), time = as.integer(ended_at - started_at)) |>
#  mutate(avg_velocity = distance/ time) |>
#  select(ride_id, start_station_id, started_at, time, distance, member_casual, rideable_type, start_lng, start_lat, avg_velocity)
#
# visual  <- data |>
#  select(member_casual, avg_velocity, distance, time) |>
#  group_by(member_casual) |>
#  summarise(avg_time_used = mean(time), avg_distance = mean(distance), count = n())
#
# p  <- ggplot(data = visual) + aes(x = member_casual, y= avg_time_used) + geom_col()
#
# p1  <- ggplot(data = visual) + aes(x = member_casual, y= avg_distance) + geom_col()
#
# p2  <- ggplot(data = visual) + aes(x = member_casual, y= count) + geom_col()
#
# p
# p1
# p2
#
# ggplot(data=data) + aes(x = distance) + geom_histogram(bins=60) + facet_grid(vars(member_casual))
# per_hour_day  <- ggplot(data=data) + aes(x = hour(started_at)) + geom_histogram(bins = 24, fill = 'lightblue', color = 'darkblue') + facet_grid(cols=vars(member_casual), rows=vars(wday(started_at, week_start = 1, label = TRUE, abbr = FALSE))) + labs(title = 'Rides per hour of day', subtitle='Data from Agust 2024', x='Hour',y='Number of rides')+  theme_linedraw()
# per_day <-  ggplot(data=data) + aes(x = wday(started_at, week_start = 1), fill = rideable_type) + geom_histogram() + facet_grid(cols=vars(member_casual), rows =vars(week(started_at))) + labs(x='day of the week', title = 'Rides per day of the week', subtitle = 'Data from Agust, 2024', y = 'Number of rides') + theme_linedraw()
# ggplot(data=filter(data, time < 3000)) + aes(x=hms::as_hms(started_at), y=time, colour=rideable_type) + geom_point() + facet_grid(rows=vars(wday(started_at)), cols=vars(member_casual))
# per_day
# per_hour_day
#
# data |>
#  select(started_at, time) |>
#  mutate(hour = hour(started_at), day  )
#
# ggplot() + aes(x=distance, y=time, colour=rideable_type  )+ geom_point()
# data |> slice_min(time)
# glimpse(data)
# map  <- data |>
#  group_by(start_station_id, start_lng, start_lat, member_casual, rideable_type) |>
#  summarise(avg_dist = mean(distance), avg_time = mean (time), rides = n(), avg_vel = mean(avg_velocity), .groups = "keep")
#
# pos  <- map |>
#  arrange(desc(avg_dist))
#
#
# loc_sf  <- st_as_sf(map, coords = c('start_lng', 'start_lat'))
# st_crs(loc_sf) = 4236
#
# mapview(loc_sf, zcol='rides')
# mapview(loc_sf, zcol='avg_dist')
#
#
```
