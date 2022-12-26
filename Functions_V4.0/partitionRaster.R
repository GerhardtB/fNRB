#' Split a Raster* object into strips.
#'
#' Partition a raster into vertical strips. Useful for parallel processing.
#'
#' @param raster A *Raster object to be partitioned.
#' @param nchunks Integer representing number of partitions.
#' @param chunk Integer representing which of the paritions to create.
#' @param subextent Optional extent object indicating a subdomain of the raster
#'   that should be partitioned.
#' @return A cropped strip of the input raster object.
partitionRaster<-function(raster,nchunks,chunk,subextent=NA){
  e<-extent(raster)
  if(!is.na(subextent)) e<-subextent
  x<-seq(e@xmin,e@xmax,(e@xmax-e@xmin)/(nchunks))
  e@xmin<-x[chunk] - 2 * res(raster)[1]
  e@xmax<-x[chunk+1]  + 2 * res(raster)[1]
  return(trim(crop(raster, e)))
}
