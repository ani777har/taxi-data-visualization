library(dplyr)
library(sf)

load("data/gg_raw_data.rda")
yerevan_districts <- st_read("data/Yerevan-Districts/Yerevan-Districts.shp", quiet=T)


 
# 1. remove all rows, where taxi was canceled & destination coordinates are missing

df <- df[!is.na(df$destLng) & !is.na(df$destLat) & is.na(df$canceled_at), ]

# 2. remove the column canceled_at

df$canceled_at <- NULL

# 3. exclude the cases where distance was less than 500m, since we assumed that those 
# observations were not real, they were for testings

df <- df[df$distance > 500,]

# 4. filling missing distance values based on the coordinates of origin and destination

threshold <- 0.003

fill_missing_distance <- function(df) {
  df %>%
    mutate(
      distance = ifelse(
        is.na(distance), 
        
        df %>%
          filter(!is.na(distance) & 
                   abs(originLng - cur_data()$originLng) <= threshold &
                   abs(oroginLat - cur_data()$oroginLat) <= threshold &
                   abs(destLng - cur_data()$destLng) <= threshold &
                   abs(destLat - cur_data()$destLat) <= threshold) %>%
          summarise(mean_distance = mean(distance, na.rm = TRUE)) %>%
          pull(mean_distance),
        distance
      )
    )
}

df <- fill_missing_distance(df)


# 5. filtering to have only Yerevan's data

lng_min <- 44.43
lng_max <- 44.65
lat_min <- 40.06
lat_max <- 40.24

df <- df %>%
  filter(originLng >= lng_min & originLng <= lng_max & 
           oroginLat >= lat_min & oroginLat <= lat_max &
           destLng >= lng_min & destLng <= lng_max &
           destLat >= lat_min & destLat <= lat_max)

# 6. adding columns priceperkm, date, hour, duration, speed_kmh, day, response_time, is_late, month, season

df <- df %>%
  mutate(
    date=as.Date(accepted_at),
    priceperkm = ifelse(date<as.Date("2016-04-01"), 100, as.integer(fare * 1000 / distance)),
    hour = hour(as.POSIXct(accepted_at)),
    duration = as.numeric(difftime(completed_at, processing_at, units = "mins")),
    speed_kmh = distance / (duration * 1000 / 60),
    day = weekdays(date),
    day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    response_time = as.numeric(difftime(processing_at, arrived_at, units = "mins")),
    is_late = ifelse(response_time > 10, 1, 0),
    month = as.numeric(format(as.Date(accepted_at), "%m")), 
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn",
      TRUE ~ "Unknown"
    )
  )

# 7. adding the column distance_category

df <- df %>%
  mutate(distance_category = case_when(
    distance <= 2000 ~ "Very Short",
    distance <= 5000 ~ "Short",
    distance <= 10000 ~ "Medium",
    distance <= 20000 ~ "Long",
    TRUE ~ "Very Long"
  ))

# 8. adding the column duration category

df <- df %>%
  mutate(duration_category = case_when(
    duration <= 5 ~ "very_short",
    duration <= 15 ~ "short",
    duration <= 30 ~ "medium",
    duration <= 60 ~ "long",
    TRUE ~ "very_long"
  ))

# 9. adding the column period of day

df <- df %>%
  mutate(
    period = case_when(
      is.na(accepted_at) ~ "Unknown",  
      format(accepted_at, "%H") >= "00" & format(accepted_at, "%H") < "05" ~ "Night",
      format(accepted_at, "%H") >= "05" & format(accepted_at, "%H") < "12" ~ "Morning",
      format(accepted_at, "%H") >= "12" & format(accepted_at, "%H") < "17" ~ "Afternoon",
      format(accepted_at, "%H") >= "17" & format(accepted_at, "%H") <= "23" ~ "Evening"
    )
  )

# 10. getting the origin and destination district names for each ride

taxi_origin_sf <- st_as_sf(df, coords = c("originLng", "oroginLat"), crs = 4326)
taxi_dest_sf <- st_as_sf(df, coords = c("destLng", "destLat"), crs = 4326)

districts_sf <- st_transform(yerevan_districts, crs = st_crs(taxi_origin_sf))

taxi_origin_with_district <- st_join(taxi_origin_sf, districts_sf, join = st_within)
taxi_dest_with_district <- st_join(taxi_dest_sf, districts_sf, join = st_within)

taxi_origin_with_district <- taxi_origin_with_district[, c("orderId", "Name_en")]
colnames(taxi_origin_with_district)[colnames(taxi_origin_with_district) == "Name_en"] <- "origin_district"


taxi_dest_with_district <- taxi_dest_with_district[, c("orderId", "Name_en")]
colnames(taxi_dest_with_district)[colnames(taxi_dest_with_district) == "Name_en"] <- "dest_district"

taxi_origin_df <- st_drop_geometry(taxi_origin_with_district)
taxi_dest_df <- st_drop_geometry(taxi_dest_with_district)

taxi_with_both_districts <- merge(
  taxi_origin_df, 
  taxi_dest_df, 
  by = "orderId", 
  all = TRUE
)

final_df <- merge(df, taxi_with_both_districts, by = "orderId", all.x = TRUE)
final_df <- final_df[!is.na(final_df$dest_district) & !is.na(final_df$origin_district), ]

# data is ready, congrats !

save(final_df, file = "data/gg_processed_data.RData")

