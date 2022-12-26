#' Finds the correct UTM projection for your aoi.shp
#' 
#' @author Jurie Theron
#' 
#' @param shp_aoi Area of interest - shapefile as sf object.

grab_UTM_proj4 = function(shp_aoi) {
  
  suppressPackageStartupMessages(require(sf))
  suppressPackageStartupMessages(require(terra))
  suppressPackageStartupMessages(require(dplyr))
  
  # Make sure aoi is in geographic coordinate system
  shp_aoi<-shp_aoi %>%
    st_transform(crs=4326)
  
  # Find centroid of aoi
  Centroid<-st_as_sfc(st_bbox(shp_aoi),crs=st_crs(4326)) %>%
    st_centroid(Centroid) %>%
    st_as_sf()
  rm(shp_aoi)
  
  # Identify UTM zone of centroid
  utm_grid<-st_read(paste0(
    usr_parent_directory,"Data/World_UTM_Grid/World_UTM_Grid.shp"),
    quiet=TRUE) %>%
    st_as_sf() %>%
    st_make_valid()
  utm<-st_join(Centroid,utm_grid) %>%
    dplyr::select(ZONE) %>%
    st_drop_geometry() %>%
    as.character()
  rm(utm_grid)
  
  # Determine hemisphere of centroid
  hemisphere<-Centroid %>%
    st_coordinates() %>%
    as.data.frame()
  if (hemisphere$Y > 0) {
    hemisphere<-as.character("northern")
  } else {
    hemisphere<-as.character("south")
  }
  
  # Grab proj4 code
  if (hemisphere=="south") {
    proj4<-paste0(
      "+proj=utm +zone=",utm," +",hemisphere,
      " +datum=WGS84 +units=m +no_defs +type=crs")
  } else {
    proj4<-paste0(
      "+proj=utm +zone=",utm,
      " +datum=WGS84 +units=m +no_defs +type=crs")
  }
  rm(utm,hemisphere,Centroid)
  
  return(proj4)
  gc()
}
