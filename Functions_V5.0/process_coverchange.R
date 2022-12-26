#' Process woody cover change data.
#'
#' Mask, reclassify, aggregate and write woody cover change data from Hansen et al.
#'
#' @author Ruan Parrott, Nicholas Salonen & Jurie Theron
#' 
#' @param change_raster a raster of woody gain/loss for the aoi.
#' @param aoi_shape a (sf) polygon of the area of interest
#' @param aggregation a vector by which the raster data will be aggregated. Default = 8.
#' @param aoi_prefix a character string of the prefix supplied to write the resulting raster. The default is "aoi_".
#' @param var_suffix a character string of the suffix supplied to write the resulting raster.. The default is "_coverchange".
#' @param destination a character string of the directory to save the results to. The default is the working directory.
#'
#' @return a raster image (.tif) written to the destination provided.
#'
#' @importFrom magrittr `%>%`
#' @import terra
#' @import sf

process_coverchange = function(change_raster,
                               aggregation = 8,
                               aoi_shape,
                               aoi_prefix,
                               var_suffix = "_coverchange",
                               destination = getwd())
  {
  
  # Create matrix to reclassify pixels
  reclass_binary<-matrix(c(1, Inf, 1),ncol=3,byrow=TRUE)
  
  # Crop, reclassify, aggregate
  aoi_coverchange_total<-change_raster %>%
    terra::crop(aoi_shape) %>%
    terra::classify(reclass_binary) %>%
    terra::aggregate(fact=aggregation)
  
  # Write to disk
  writeRaster(aoi_coverchange_total,paste0(
    destination,aoi_prefix,"agg and reclass",var_suffix,"_total.tif"),
    overwrite=TRUE)
  
  # Select return object
  return(aoi_coverchange_total)
  gc()
}
