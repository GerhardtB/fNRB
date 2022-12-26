#' Process woody cover change data.
#'
#' Mask, reclassify, aggregate and write woody cover change data from Hansen et al.
#'
#' @author Ruan Parrott, Nicholas Salonen & Jurie Theron
#' 
#' @param cover_raster a raster of 2000 woody cover for the aoi.
#' @param loss_raster a raster of 2000-2018 woody loss for the aoi. (this should be the output of process_coverchange)
#' @param gain_raster a raster of 2000-2012 woody gain for the aoi. (this should be the output of process_coverchange)
#' @param remote_shape a (sf) polygon of the remote areas. (this should be the output of disaggregate_access)
#' @param aoi_shape a (sf) polygon of the area of interest
#' @param aggregation a vector by which the raster data will be aggregated. Default = 8.
#' @param aoi_prefix a character string of the prefix supplied to write the resulting raster. The default is "aoi_".
#' @param destination a character string of the directory to save the results to. The default is the working directory.
#'
#' @return three raster images (.tif) written to the destination provided.These consist of the 2000 total, 2018 total and 2018 accessible woody cover data.
#'
#' @importFrom magrittr `%>%`
#' @import terra
#' @import sf

process_cover = function(cover_raster,
                         loss_raster,
                         gain_raster,
                         aggregation = 8,
                         remote_shape,
                         aoi_shape,
                         aoi_prefix = "aoi_",
                         destination = getwd())
  {
  
  # Mask and gggregate 2000 woody cover
  message("Mask and aggregating...")
  aoi_cover_total<-cover_raster %>%
    terra::mask(aoi_shape) %>% 
    aggregate(fact=aggregation) 
  gc()
  rm(cover_raster)
  
  # Calculate cover
  message("Inverting...")
  inverse_loss_raster<-1-loss_raster
  gc()
  rm(loss_raster)
  
  # Process and calculate tree cover using tree loss and gain
  message("Getting total cover...")
  aoi_2018cover_total<-inverse_loss_raster %>%
    prod(aoi_cover_total,na.rm=TRUE)
  datastack<-sds(aoi_2018cover_total,(gain_raster*100))
  aoi_2018cover_total<-terra::lapp(datastack,fun=function(r1,r2){return(r1+r2)}) %>%
    terra::mask(aoi_shape) %>%
    terra::classify(matrix(c(100,Inf,100),ncol=3,byrow=TRUE))
  gc()
  rm(inverse_loss_raster,gain_raster,datastack)
  
  # Grabbing tree cover in accessible areas
  message("Getting accessible...")
  if (nrow(remote_shape) == 0) {
    aoi_2018cover_access<-aoi_2018cover_total
  } else {
    aoi_2018cover_access<-terra::mask(aoi_2018cover_total,remote_shape,inverse=TRUE)
  }
  
  # Save to disk
  message("Writing rasters...")
  writeRaster(aoi_cover_total,paste0(destination,aoi_prefix,"agg and reclass_2000cover_total.tif"),overwrite=TRUE)
  rm(aoi_cover_total)
  gc()
  writeRaster(aoi_2018cover_total,paste0(destination,aoi_prefix,"agg and reclass_2021cover_total.tif"),overwrite=TRUE)
  rm(aoi_2018cover_total)
  gc()
  writeRaster(aoi_2018cover_access,paste0(destination,aoi_prefix,"agg and reclass_2021cover_access.tif"),overwrite=TRUE)
  rm(aoi_2018cover_access)
  gc()
}
