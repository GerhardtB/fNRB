#' Dissaggregate accessible - and remote or protected - areas
#'
#' Disaggregate a given area of interest (aoi), such as a national or regional project area polygon, into accessible and remote areas, according to the defined buffer distance from the roads and the protected areas. The results are saved to the directory provided, rather than loading it into the workspace.
#'
#' @param aoi_border a simple features (sf) polygon of the area of interest border or boundary. This should be projected to the relevant CRS of the aoi.
#' @param roads_projected a simple features (sf) lines of the roads in the area of interest. This should be projected to the relevant CRS of the aoi.
#' @param PAs_union a simple features (sf) polygon of the protected areas in the area of interest, not as a multipolygon, but as a single, unified shape. This should be projected to the relevant CRS of the aoi.
#' @param proj_crs the UTM projection specified by user
#' @param buffer_dist a vector of the buffer distance from the road that should be considered accessible in the projection units. The default is 2.5 km from the road (in m).
#' @param aoi_prefix a character string of the prefix supplied to write the resulting sf objects. The default is "aoi_".
#' @param destination a character string of the directory to save the results to. The default is the working directory.
#'
#' @return two shapefiles written to the destination provided.
#'
#' @importFrom magrittr `%>%`
#' @import terra
#' @import sf

disaggregate_access = function(aoi_border,
                               roads_projected,
                               PAs_union,
                               proj_crs,
                               buffer_dist = 2500,
                               aoi_prefix = "aoi_",
                               destination = getwd()) 
  {
  
  # Calculate area of PA's
  border_area<-as.double(st_area(aoi_border))
  if (length(PAs_union) > 0) {
    PA_area<-as.double(st_area(PAs_union))
  } else {
    PA_area<-0
  }
  
  if (nrow(roads_projected) > 0 & !(PA_area >= border_area)) {
  # Create buffer
  aoi_road_buff<-roads_projected %>%
    st_union() %>%
    st_sf() %>%
    st_buffer(dist=buffer_dist)
  
  # Calculate area of buffer
  road_buff_area<-as.double(st_area(aoi_road_buff))
  
  # Define remote and accessible areas
  if (length(PAs_union) > 0 & (road_buff_area < border_area)) {
    aoi_remote<-st_difference(aoi_border,aoi_road_buff) %>%
      st_union(PAs_union) %>%
      st_sf()
  } else if (length(PAs_union) > 0 & (road_buff_area >= border_area)) {
    aoi_remote<-st_intersection(aoi_border,PAs_union)
  } else {
    aoi_remote<-st_difference(aoi_border,aoi_road_buff)
  }
  if ((nrow(aoi_remote) > 0) || (length(aoi_remote) > 0)) {
    aoi_accessible<-st_difference(aoi_border,aoi_remote)
  } else {
    aoi_accessible<-aoi_border 
  }
  aoi_accessible_trim<-st_intersection(aoi_border,aoi_accessible)
  
  # Project
  aoi_remote_ll<-st_transform(aoi_remote,proj_crs)
  aoi_access_ll<-st_transform(aoi_accessible_trim,proj_crs)
  
  } else {
    message("The province is fully remote.")
    aoi_remote_ll<-st_transform(aoi_border,proj_crs)
    aoi_access_ll<-NA
  }
  
  # Correct geometry type
  if (!is.na(aoi_access_ll)) {
    if (st_geometry_type(aoi_access_ll,by_geometry=FALSE) != "POLYGON" &
        st_geometry_type(aoi_access_ll,by_geometry=FALSE) != "MULTIPOLYGON") {
      aoi_access_ll<-st_collection_extract(aoi_access_ll,type=c("POLYGON","POINT","LINESTRING"))
    }
  }
  
  # Write to disk
  if (is.na(aoi_access_ll)) {
    st_write(aoi_remote_ll,paste0(destination,aoi_prefix,"remote.shp"),delete_layer=TRUE)
  } else {
    st_write(aoi_remote_ll,paste0(destination,aoi_prefix,"remote.shp"),delete_layer=TRUE)
    st_write(aoi_access_ll,paste0(destination,aoi_prefix,"accessible.shp"),delete_layer=TRUE)
  }
  
  # Select object to return
  return(aoi_remote_ll)
  gc()
}
