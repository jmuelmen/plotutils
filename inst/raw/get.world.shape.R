#### get world map shape file from Natural Earth, save it in
#### sysdata.rda

library(maptools)
library(devtools)

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip",
              "ne_110m_land.zip")
unzip("ne_110m_land.zip")
shp <- readShapePoly("ne_110m_land.shp", proj4string = CRS("+proj=longlat +ellps=WGS84"))
devtools::use_data(shp, internal = TRUE)
