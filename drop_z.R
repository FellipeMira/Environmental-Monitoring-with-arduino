sf::st_zm(pontos, drop=T, what='ZM') %>% 

sf::st_write(obj = .,"interpolate/pontos_interpolação.shp")

rgdal::writeOGR(pontos,dsn = "interpolate/pontos_interpolação.shp",driver = "ESRI Shapefile")

"interpolate/"