library(tidyverse)
library(knitr)
library(mapview)
library(lubridate)
library(sf)
library(jsonlite)
library(geosphere)
library(furrr)
library(scales)

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

opts <- options(knitr.kable.NA = "")

global_theme <- function() {
  theme_linedraw() +
    theme(
      axis.text.y = element_text(colour = "white", face = "bold"), axis.text.x = element_text(colour = "white", face = "bold"),
      axis.title.y = element_text(colour = "white", face = "bold"),
      axis.title.x = element_text(colour = "white", face = "bold"),
      plot.title = element_text(colour = "white", face = "bold"),
      plot.subtitle = element_text(colour = "white", face = "bold"),
      strip.background = element_rect(fill = "#262852", color = "#262852"),
      strip.placement = "inside",
      strip.text = element_text(colour = "white", face = "bold"),
      panel.background = element_rect(fill = "#262852"),
      plot.background = element_rect(fill = "#150F3A"),
      plot.caption = element_text(colour = "white", face = "bold")
    )
}

df <- filter(df, !is.na(end_lng))

distm_v <- Vectorize(function(x1, y1, x2, y2) {
  distm(c(x1, y1), c(x2, y2), fun = distHaversine)
})

stations_json <- "stations.json"
if (!file.exists(stations_json)) {
  download.file(
    "https://gbfs.lyft.com/gbfs/2.3/chi/en/station_information.json",
    stations_json
  )
}

stations <- fromJSON(stations_json) |>
  _$data$stations |>
  as_tibble()

stations <- stations |>
  select(station_id, short_name, name, lon, lat)

stations <- stations |>
  filter(station_id != "1448642188027369090") |>
  filter(station_id != "1594046379513303720") |>
  filter(station_id != "1448642188027369086")
# NOTE: WTF!!! If we connect those statements with the | operator, with multiple
# lines, it doesn't work

df <- df |>
  select(ride_id:start_station_name, end_station_name, member_casual) |>
  left_join(stations, by = join_by(start_station_name == name)) |>
  mutate(start_station_id = station_id, start_lon = lon, start_lat = lat) |>
  select(!station_id:lat) |>
  left_join(stations, by = join_by(end_station_name == name)) |>
  mutate(end_station_id = station_id, end_lon = lon, end_lat = lat) |>
  select(!station_id:lat) |>
  mutate(
    start_station_id = if_else(
      start_station_id == "d53ae727-5265-4b8e-a6ca-2a36dc0345c4",
      "a3a3a282-a135-11e9-9cda-0a87ae2ba916",
      start_station_id
    ),
    start_station_name = if_else(
      start_station_name == "Wilton Ave & Diversey Pkwy*",
      "Wilton Ave & Diversey Pkwy", start_station_name
    ),
    end_station_id = if_else(
      end_station_id == "d53ae727-5265-4b8e-a6ca-2a36dc0345c4",
      "a3a3a282-a135-11e9-9cda-0a87ae2ba916", end_station_id
    ),
    end_station_name = if_else(
      end_station_name == "Wilton Ave & Diversey Pkwy*",
      "Wilton Ave & Diversey Pkwy", end_station_name
    )
  ) |>
  mutate(
    start_station_id = if_else(
      start_station_id == "1715823821144840768",
      "1677249871073777806",
      start_station_id
    ),
    start_station_name = if_else(
      start_station_name == "Public Rack - Laflin St & 51st ST",
      "Public Rack - Laflin St & 51st St", start_station_name
    ),
    end_station_id = if_else(
      end_station_id == "1715823821144840768",
      "1677249871073777806", end_station_id
    ),
    end_station_name = if_else(
      end_station_name == "Public Rack - Laflin St & 51st ST",
      "Public Rack - Laflin St & 51st St", end_station_name
    )
  ) |>
  mutate(
    start_station_id = if_else(
      start_station_id == "1827484051430132402",
      "1827474404723843690",
      start_station_id
    ),
    start_station_name = if_else(
      start_station_name == "Public Rack - Forest Glen Station",
      "Public Rack - Peterson Park", start_station_name
    ),
    end_station_id = if_else(
      end_station_id == "1827484051430132402",
      "1827474404723843690", end_station_id
    ),
    end_station_name = if_else(
      end_station_name == "Public Rack - Forest Glen Station",
      "Public Rack - Peterson Park", end_station_name
    )
  ) |>
  mutate(ride_duration = as.integer((ended_at - started_at) / 60)) |>
  filter(ride_duration > 0) |>
  mutate(station_fee = is.na(end_station_name))

seasons <- c("1" = "Spring", "2" = "Summer", "3" = "Autumn", "4" = "Winter")
weekdays <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")


df |>
  anti_join(
    y = stations |>
      select(name) |>
      unique(),
    by = join_by(start_station_name == name)
  ) |>
  filter(start_station_name != "NA") |>
  ggplot() +
  aes(x = started_at) +
  geom_histogram(bins = 12) +
  facet_wrap(vars(start_station_name), ncol = 5)
