#' Dissaggregate woody cover by Global Ecological Zone
#'
#' Disaggregate the woody cover data of a given raster by the Global Ecological Zones for the same extent.
#'
#' @param rast a raster of the woody cover in the area of interest.
#' @param poly_gez a simple features (sf) polygon of the GEZ. This should be projected to the relevant CRS of the raster.
#' @param scaling a vector by which the raster data will be scaled. If the raster has the woody cover as a percentage (0-100), then scaling = 1 (default). If the raster has decimals (0-1), then scaling = 100.
#'
#' @return a data frame of the total woody area per GEZ.
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter
#' @import terra
#' @import sf

disaggregate_gez = function(rast,
                            poly_gez,
                            scaling=1) 
  {
  aoi_zones<-poly_gez$gez_name
  aoi_cell_size<-res(rast) %>%
    prod()
  aoi_output<-data.frame(gez_name=aoi_zones,
                         Cover_ha=NA,
                         Area_ha=NA,
                         Total_ha=NA)
  
  # Calculate cover per GEZ
  for(i in 1:length(aoi_zones)) {
    aoi_zone_single<-poly_gez %>%
      dplyr::filter(gez_name==aoi_zones[i])
    message(paste0("Disaggregating GEZ: ",poly_gez$gez_name[i],". ",length(aoi_zones)-i," zones remaining."))
    
    # Cover
    aoi_cover_p<-terra::mask(rast,aoi_zone_single) %>%
      terra::values()
    aoi_output[i,2]<-sum((aoi_cover_p[!is.na(aoi_cover_p)]*scaling)*aoi_cell_size,na.rm=TRUE)
    
    # Area
    aoi_area_each<-aoi_cover_p
    aoi_output[i,3]<-length(aoi_area_each[!is.na(aoi_area_each)])*aoi_cell_size
    
    # Total area
    aoi_output[i,4]<-st_area(aoi_zone_single) %>%
      as.numeric()
  }
  
  return(aoi_output)
  gc()
}
