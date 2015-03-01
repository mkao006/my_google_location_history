library(RJSONIO)
library(ggplot2)
library(ggmap)
library(maps)
library(MASS)

## You can download your own locatoin history from
## https://www.google.com/settings/takeout after signing in.
locationHistory = fromJSON("LocationHistory.json")


## Extract time stamp
timeStamp =
    as.Date(
        sapply(locationHistory[[1]],
               FUN = function(x){
                   as.numeric(x$timestampMs)/1000/60/60/24
               }
               ),
        origin = "1970-01-01"
        )

## Extract logitude and latitude
latitude = sapply(locationHistory[[1]], FUN = function(x) x$latitudeE7)
longitude = sapply(locationHistory[[1]], FUN = function(x) x$longitudeE7)

## create data frame
myLocations.df =
    data.frame(timeStamp = timeStamp,
               longitude = longitude/10000000,
               latitude = latitude/10000000)


## Extract my location within the map of Rome
romeLocations.df =
    myLocations.df[myLocations.df$longitude >= 12.45 &
                   myLocations.df$longitude <= 12.55 &
                   myLocations.df$latitude >= 41.86 &
                   myLocations.df$latitude <= 41.92, ]

## Extract the map of Rome
mapRome =
    get_map(location = c(lon = mean(romeLocations.df$longitude),
                         lat = mean(romeLocations.df$latitude)),
            zoom = 13, maptype = "satellite", scale = 2) 

## Estimate the density
dens = with(romeLocations.df,
    kde2d(longitude, latitude, n = 60))
densdf = data.frame(expand.grid(lon = dens$x, lat = dens$y),
                     z = as.vector(dens$z))

## Plot the map
m = ggmap(mapRome) +
  geom_point(data = romeLocations.df,
             aes(x = longitude, y = latitude, color = "red"),
             alpha = 1/30, size = 5) +
  guides(col = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  geom_contour(data = densdf,
               aes(x = lon, y = lat, z = z^(1/4)), bins = 8, size = 1)
m

ggsave("rome_location.png")
