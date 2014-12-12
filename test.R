library(RJSONIO)
locationHistory = fromJSON("LocationHistory.json")


timeStamp =
    as.Date(
        sapply(locationHistory[[2]],
               FUN = function(x){
                   as.numeric(x$timestampMs)/1000/60/60/24
               }
               ),
        origin = "1970-01-01"
        )
latitude = sapply(locationHistory[[2]], FUN = function(x) x$latitudeE7)
longitude = sapply(locationHistory[[2]], FUN = function(x) x$longitudeE7)

library(ggplot2)
library(ggmap)
library(maps)
worldMap = map_data("world")

myLocations.df =
    data.frame(timeStamp = timeStamp,
               longitude = longitude/10000000,
               latitude = latitude/10000000)

trip.df = myLocations.df[myLocations.df$timeStamp >=
                             as.Date("2014-10-03"), ]


franceMap = map_data("france")

myLocation = ggplot(franceMap, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="white", colour="black") +
        geom_point(data = trip.df,
                   aes(x = longitude, y = latitude, col = "red",
                       group = NA))
myLocation


myLocation = ggplot(worldMap, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="white", colour="black") +
        geom_point(data = myLocations.df,
                   aes(x = longitude, y = latitude, col = "red",
                       group = NA))
myLocation

mapgilbert =
    get_map(location = c(lon = mean(myLocations.df$longitude),
                         lat = mean(myLocations.df$latitude)),
            zoom = 3, maptype = "satellite", scale = 2)


mapgilbert =
    get_map(location = c(lon = ,
                         lat = mean(myLocations.df$latitude)),
            zoom = 3, maptype = "satellite", scale = 2)


ggmap(mapgilbert) +
  geom_point(data = myLocations.df,
             aes(x = longitude, y = latitude, fill = "red",
                 alpha = 0.8), size = 5, shape = 21) +
                     guides(fill = FALSE, alpha = FALSE, size = FALSE)
