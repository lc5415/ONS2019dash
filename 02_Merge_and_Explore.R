library(rgdal)
library(leaflet)
library(dplyr)
library(ezplot)
library(rmapshaper)

#Load ONS data
ons = read.csv('CleanData/ONS.csv')

#Load the shapefile
#'
#'from http://geoportal1-ons.opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson?outSR={%22latestWkid%22:27700,%22wkid%22:27700}
#'
#'see this link for other formats: https://data.gov.uk/dataset/51878530-7dd4-45df-b36b-9a0b01f3c136/local-authority-districts-december-2019-boundaries-uk-bgc

geojson = readOGR('http://geoportal1-ons.opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson?outSR={%22latestWkid%22:27700,%22wkid%22:27700}')

simplify = FALSE
# below steps reduces the size of the geojson file from 16 to 2 MB 
# signficantly improving the loading time of the shiny app later
if (simplify) geojson = ms_simplify(geojson, sys = TRUE)

# Polygons and data are linked by order not by id or anything similar so I've got to make sure
#' that the mapping of data to polygons remains
agg = geojson
agg@data = inner_join(agg@data, ons,by = c('lad19cd' = 'areaCode'))

saveRDS(agg, 'DashData/geo_ons.rds')

# Example map
pal <- colorNumeric("viridis", NULL)
leaflet(agg) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0.3,
              fillOpacity = 1,
              fillColor = ~pal(log10(TotalPeople_All)),
              label = ~paste0(lad19nm, ": ",
                              ez_labels(TotalPeople_All, signif = 3))
  ) %>%
  addLegend(pal = pal,
            values = ~log10(TotalPeople_All),
            opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))
  )



