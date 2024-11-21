stations.geoinfo <- select(df, start_lat, start_lng) |>
  rename(lat = start_lat, lng = start_lng) |>
  bind_rows(select(df, end_lat, end_lng) |>
    rename(lat = end_lat, lng = end_lng)) |>
  unique()
