idw <- function(bot, update){
  library(spatstat)  # Used for the dirichlet tessellation function
  library(maptools)  # Used for conversion from SPDF to ppp
  library(raster)    # Used to clip out thiessen polygons
  
  sjc <- sf::st_read("sjc.shp")
  P <- rgdal::readOGR("interpolate/pontos_interpolação.shp")
  sanja <- rgdal::readOGR("sjc.shp")
  
  crs_sirgas <- sp::CRS(SRS_string = "EPSG:31983")
  
  # Create a tessellated surface
  th  <-  as(dirichlet(as.ppp(P)), "SpatialPolygons")
  
  summary(th)
  summary(P)
  # The dirichlet function does not carry over projection information
  # requiring that this information be added manually
  crs_sirgas <- sp::CRS(SRS_string = "EPSG:31983")
  slot(P, "proj4string") <- crs_sirgas
  slot(th, "proj4string") <- crs_sirgas
  slot(sanja, "proj4string") <- crs_sirgas
  # The tessellated surface does not store attribute information
  # from the point data layer. We'll use the over() function (from the sp
  # package) to join the point attributes to the tesselated surface via
  # a spatial join. The over() function creates a dataframe that will need to
  # be added to the `th` object thus creating a SpatialPolygonsDataFrame object
  th.z     <- over(th, P, fn=mean)
  th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
  slot(th.spdf, "proj4string") <- crs_sirgas
  
  # Finally, we'll clip the tessellated  surface to the Texas boundaries
  th.clp   <- raster::intersect(sanja,th.spdf)
  
  # Map the data
  tm_shape(th.spdf) + 
    tm_polygons(col="umdd_sl", palette=viridis::magma(6), auto.palette.mapping=FALSE,
                title="Predicted precipitation \n(in inches)") +
    tm_legend(legend.outside=TRUE)
  
  
  
  
  
  ##################################################################################
  library(gstat) # Use gstat's idw routine
  library(sp)    # Used for the spsample function
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(P, "regular", n=50000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  crs_sirgas <- sp::CRS(SRS_string = "EPSG:31983")
  slot(P, "proj4string") <- crs_sirgas
  slot(th, "proj4string") <- crs_sirgas
  slot(sanja, "proj4string") <- crs_sirgas
  slot(grd, "proj4string") <- crs_sirgas
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(umdd_sl ~ 1, P, newdata=grd, idp=2.0)
  
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, sanja)
  
  # Plot
  tmap_mode("plot")
  qq <-tm_shape(sjc)+
    tm_borders("gray3")+
    tm_shape(r) + 
    tm_raster(n=10,
              palette = viridis::magma(10), auto.palette.mapping = FALSE,
              title="Umidade do solo") + 
    tm_shape(P) + 
    tm_dots(size=0.05,col = "white") +
    tm_legend(legend.outside=TRUE)
  
  tmap::tmap_save(qq,filename = "qq.png")
  bot$sendPhoto(chat_id = update$message$chat_id, photo = 'qq.png')
  
}

idw_handler <- CommandHandler("idw", idw)

updater <- updater + idw_handler

