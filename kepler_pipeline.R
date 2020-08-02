# Transforms the Dynamic Export Flights (Tab) from LogTenPro into a format
# that is usable by kepler.gl.  

# in use at: https://kepler.gl/demo/map?mapUrl=https://dl.dropboxusercontent.com/s/do2d57d2gsq8ltm/keplergl_4tayh2.json

library(tidyverse)

df <- as_tibble(read.csv("all_flights.csv", stringsAsFactors = F))
df$From <- as.character(df$From)
df$To <- as.character(df$To)


# Filter  ExpressJet Only Flying
# filter E145 flying - un-comment following 2 lines
# embraers <- c("E135", "E145", "E45X")
# df <- df[df$Aircraft.Type %in% embraers, ]
# End Filter  ExpressJet Only Flying

coords <- as_tibble(read.csv("airports.csv", stringsAsFactors = F)) %>%
  select("icao", "lat", "lon")

match_coord <- function (a, coord_type) {
  coordinates <- filter(coords, icao == a)
  s <- c(coordinates$lat, coordinates$lon)
  if(coord_type == "lat") {
    return (as.numeric(s[1]))
  }
  else if (coord_type == "lon") {
    return (as.numeric(s[2]))
  }
}


log <- select(df, "Date", "From", "To")  %>%
  group_by(From,To) %>%
  summarize(value=n()) %>%
  rename(frequency = value) %>% 
  mutate("from_lat" = map(From, match_coord, "lat")) %>%
  mutate("from_lon" = map(From, match_coord, "lon")) %>%
  mutate("to_lat" = map(To, match_coord, "lat")) %>%
  mutate("to_lon" = map(To, match_coord, "lon"))


log$from_lat <- as.numeric(log$from_lat)
log$from_lon <- as.numeric(log$from_lon)
log$to_lat <- as.numeric(log$to_lat)
log$to_lon <- as.numeric(log$to_lon)

log <- drop_na(log)

write.csv(log, "kepler_logook.csv")



