long1 <- gmpdprot$long[1:1242]
lat1 <- gmpdprot$lat[1:1242]
long2 <- gmpdprot$long[1243:2484]
lat2 <- gmpdprot$lat[1243:2484]

df <- data.frame(cbind(long1, lat1, long2, lat2))

p1 <- cbind(df$long1, df$lat1)
p2 <- cbind(df$long2, df$lat2)

library(geosphere)

df$distance <- distHaversine(p1, p2)
