# Cyclist data analysis


# Preparing control

First, we will prepare the necesary libraries to make an initial
exploration.

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(mapview)
library(lubridate)
library(sf)
```

    Linking to GEOS 3.12.2, GDAL 3.8.5, PROJ 9.3.1; sf_use_s2() is TRUE

``` r
library(geosphere)
```

Loading data from source if not available locally.

``` r
if(!file.exists("202408-divvy-tripdata.zip")) {
  download.file("https://divvy-tripdata.s3.amazonaws.com/202408-divvy-tripdata.zip","202408-divvy-tripdata.zip")
}
```

# Exploratory face

Read file as a tibble and first look at data.

``` r
df <- read_csv("202408-divvy-tripdata.zip")
```

    Multiple files in zip: reading '202408-divvy-tripdata.csv'
    Rows: 755639 Columns: 13
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    dbl  (4): start_lat, start_lng, end_lat, end_lng
    dttm (2): started_at, ended_at

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(df)
```

    Rows: 755,639
    Columns: 13
    $ ride_id            <chr> "BAA154388A869E64", "8752245932EFF67A", "44DDF9F57A…
    $ rideable_type      <chr> "classic_bike", "electric_bike", "classic_bike", "e…
    $ started_at         <dttm> 2024-08-02 13:35:14, 2024-08-02 15:33:13, 2024-08-…
    $ ended_at           <dttm> 2024-08-02 13:48:24, 2024-08-02 15:55:23, 2024-08-…
    $ start_station_name <chr> "State St & Randolph St", "Franklin St & Monroe St"…
    $ start_station_id   <chr> "TA1305000029", "TA1309000007", "TA1309000007", "TA…
    $ end_station_name   <chr> "Wabash Ave & 9th St", "Damen Ave & Cortland St", "…
    $ end_station_id     <chr> "TA1309000010", "13133", "TA1307000039", "TA1306000…
    $ start_lat          <dbl> 41.88462, 41.88032, 41.88032, 41.90297, 41.96640, 4…
    $ start_lng          <dbl> -87.62783, -87.63519, -87.63519, -87.63128, -87.688…
    $ end_lat            <dbl> 41.87077, 41.91598, 41.90297, 41.89259, 41.95606, 4…
    $ end_lng            <dbl> -87.62573, -87.67733, -87.63128, -87.61729, -87.668…
    $ member_casual      <chr> "member", "member", "member", "member", "casual", "…

``` r
summary(df)
```

       ride_id          rideable_type        started_at                    
     Length:755639      Length:755639      Min.   :2024-07-30 23:06:26.89  
     Class :character   Class :character   1st Qu.:2024-08-08 16:52:03.75  
     Mode  :character   Mode  :character   Median :2024-08-16 09:29:33.76  
                                           Mean   :2024-08-16 10:53:22.75  
                                           3rd Qu.:2024-08-24 12:15:39.46  
                                           Max.   :2024-08-31 23:58:30.89  
                                                                           
        ended_at                      start_station_name start_station_id  
     Min.   :2024-08-01 00:00:16.84   Length:755639      Length:755639     
     1st Qu.:2024-08-08 17:07:32.32   Class :character   Class :character  
     Median :2024-08-16 09:45:41.80   Mode  :character   Mode  :character  
     Mean   :2024-08-16 11:12:07.51                                        
     3rd Qu.:2024-08-24 12:38:19.82                                        
     Max.   :2024-08-31 23:59:53.88                                        
                                                                           
     end_station_name   end_station_id       start_lat       start_lng     
     Length:755639      Length:755639      Min.   :41.65   Min.   :-87.90  
     Class :character   Class :character   1st Qu.:41.88   1st Qu.:-87.66  
     Mode  :character   Mode  :character   Median :41.90   Median :-87.64  
                                           Mean   :41.90   Mean   :-87.65  
                                           3rd Qu.:41.93   3rd Qu.:-87.63  
                                           Max.   :42.07   Max.   :-87.52  
                                                                           
        end_lat         end_lng       member_casual     
     Min.   :16.06   Min.   :-97.47   Length:755639     
     1st Qu.:41.88   1st Qu.:-87.66   Class :character  
     Median :41.90   Median :-87.64   Mode  :character  
     Mean   :41.90   Mean   :-87.65                     
     3rd Qu.:41.93   3rd Qu.:-87.63                     
     Max.   :46.16   Max.   :-79.02                     
     NA's   :1027    NA's   :1027                       

``` r
df %>% unique() %>% summary()
```

       ride_id          rideable_type        started_at                    
     Length:755639      Length:755639      Min.   :2024-07-30 23:06:26.89  
     Class :character   Class :character   1st Qu.:2024-08-08 16:52:03.75  
     Mode  :character   Mode  :character   Median :2024-08-16 09:29:33.76  
                                           Mean   :2024-08-16 10:53:22.75  
                                           3rd Qu.:2024-08-24 12:15:39.46  
                                           Max.   :2024-08-31 23:58:30.89  
                                                                           
        ended_at                      start_station_name start_station_id  
     Min.   :2024-08-01 00:00:16.84   Length:755639      Length:755639     
     1st Qu.:2024-08-08 17:07:32.32   Class :character   Class :character  
     Median :2024-08-16 09:45:41.80   Mode  :character   Mode  :character  
     Mean   :2024-08-16 11:12:07.51                                        
     3rd Qu.:2024-08-24 12:38:19.82                                        
     Max.   :2024-08-31 23:59:53.88                                        
                                                                           
     end_station_name   end_station_id       start_lat       start_lng     
     Length:755639      Length:755639      Min.   :41.65   Min.   :-87.90  
     Class :character   Class :character   1st Qu.:41.88   1st Qu.:-87.66  
     Mode  :character   Mode  :character   Median :41.90   Median :-87.64  
                                           Mean   :41.90   Mean   :-87.65  
                                           3rd Qu.:41.93   3rd Qu.:-87.63  
                                           Max.   :42.07   Max.   :-87.52  
                                                                           
        end_lat         end_lng       member_casual     
     Min.   :16.06   Min.   :-97.47   Length:755639     
     1st Qu.:41.88   1st Qu.:-87.66   Class :character  
     Median :41.90   Median :-87.64   Mode  :character  
     Mean   :41.90   Mean   :-87.65                     
     3rd Qu.:41.93   3rd Qu.:-87.63                     
     Max.   :46.16   Max.   :-79.02                     
     NA's   :1027    NA's   :1027                       

As we can see, there is no key colums. Fortunally, each row is unique,
and we only have 1027 NA. We can drop them, since they represent less
than a .1% of the total data. We have some colums that represent
geographical data from our stations, lets have a deeper look at them.

``` r
df %>% select(start_station_id) %>% unique() %>% count()
```

    # A tibble: 1 × 1
          n
      <int>
    1  1346

``` r
df %>% select(end_station_id) %>% unique() %>% count()
```

    # A tibble: 1 × 1
          n
      <int>
    1  1368

``` r
df %>% select(start_station_id, start_lat, start_lng) %>%
  unique() %>%
  count()
```

    # A tibble: 1 × 1
          n
      <int>
    1 24956

We have multiple greographical values for some stations. Altought this
could be intented (to have precise data on where was the bycicle
taken/returned), we are not interested in those especifics, lets fix
them.

``` r
df <- df %>%
  drop_na() %>%
  unique()

graoh <- ggplot(data = df) +
  aes(x = member_casual, fill = rideable_type) +
  geom_bar()

distm_v   <-  Vectorize(function(x1,y1,x2,y2) {
  distm(c(x1, y1), c(x2, y2), fun = distHaversine)
}
)

data  <- df %>%
  mutate(distance = distm_v(end_lng, end_lat, start_lng, end_lat), time = as.integer(ended_at - started_at)) %>%
  mutate(avg_velocity = distance/ time) %>%
  select(ride_id, start_station_id, started_at, time, distance, member_casual, rideable_type, start_lng, start_lat, avg_velocity)

visual  <- data %>%
  select(member_casual, avg_velocity, distance, time) %>%
  group_by(member_casual) %>%
  summarise(avg_time_used = mean(time), avg_distance = mean(distance), count = n())

p  <- ggplot(data = visual) + aes(x = member_casual, y= avg_time_used) + geom_col()

p1  <- ggplot(data = visual) + aes(x = member_casual, y= avg_distance) + geom_col()

p2  <- ggplot(data = visual) + aes(x = member_casual, y= count) + geom_col()

p
```

![](README_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
p1
```

![](README_files/figure-commonmark/unnamed-chunk-5-2.png)

``` r
p2
```

![](README_files/figure-commonmark/unnamed-chunk-5-3.png)

``` r
ggplot(data=data) + aes(x = distance) + geom_histogram(bins=60) + facet_grid(vars(member_casual))
```

![](README_files/figure-commonmark/unnamed-chunk-5-4.png)

``` r
per_hour_day  <- ggplot(data=data) + aes(x = hour(started_at)) + geom_histogram(bins = 24, fill = 'lightblue', color = 'darkblue') + facet_grid(cols=vars(member_casual), rows=vars(wday(started_at, week_start = 1, label = TRUE, abbr = FALSE))) + labs(title = 'Rides per hour of day', subtitle='Data from Agust 2024', x='Hour',y='Number of rides')+  theme_linedraw()
per_day <-  ggplot(data=data) + aes(x = wday(started_at, week_start = 1), fill = rideable_type) + geom_histogram() + facet_grid(cols=vars(member_casual), rows =vars(week(started_at))) + labs(x='day of the week', title = 'Rides per day of the week', subtitle = 'Data from Agust, 2024', y = 'Number of rides') + theme_linedraw()
ggplot(data=filter(data, time < 3000)) + aes(x=hms::as_hms(started_at), y=time, colour=rideable_type) + geom_point() + facet_grid(rows=vars(wday(started_at)), cols=vars(member_casual))
```

![](README_files/figure-commonmark/unnamed-chunk-5-5.png)

``` r
per_day
```

    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-commonmark/unnamed-chunk-5-6.png)

``` r
per_hour_day
```

![](README_files/figure-commonmark/unnamed-chunk-5-7.png)

``` r
#data %>%
#  select(started_at, time) %>%
#  mutate(hour = hour(started_at), day  )
#
#ggplot() + aes(x=distance, y=time, colour=rideable_type  )+ geom_point()
#data %>% slice_min(time)
#glimpse(data)
#map  <- data %>%
#  group_by(start_station_id, start_lng, start_lat, member_casual, rideable_type) %>%
#  summarise(avg_dist = mean(distance), avg_time = mean (time), rides = n(), avg_vel = mean(avg_velocity), .groups = "keep")
#
#pos  <- map %>%
#  arrange(desc(avg_dist))
#
#
#loc_sf  <- st_as_sf(map, coords = c('start_lng', 'start_lat')) 
#st_crs(loc_sf) = 4236 
#
#mapview(loc_sf, zcol='rides')
#mapview(loc_sf, zcol='avg_dist')
#
#
```
