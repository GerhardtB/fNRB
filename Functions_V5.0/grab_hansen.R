#' Identifies corresponding Hansen data, downloads, loads and clips them.
#' Returns a list of Hansen products
#' 
#' @author Jurie Theron
#' 
#' @param shp_aoi Area of interest - shapefile as sf object.
#' @param resize Factor by which to aggregate pixels

grab_hansen = function(shp_aoi,resize=1) {
  
  suppressPackageStartupMessages(require(sf))
  suppressPackageStartupMessages(require(terra))
  suppressPackageStartupMessages(require(dplyr))
  
  # Make sure aoi is in geographic coordinate system
  shp_aoi<-shp_aoi %>%
    st_transform(crs=4326)
  
  # Identify IDs of corresponding hansen
  # data from fishnet grid
  fishnet<-st_read(
    paste0(usr_parent_directory,"Data/fishnet_Named.shp"),
    quiet=TRUE)
  ID<-st_join(
    shp_aoi,fishnet) %>%
    select(Name) %>%
    st_drop_geometry()
  rm(fishnet)
  
  # Convert to list
  ID<-split(ID,seq(nrow(ID)))
  
  # Grab corresponding Hansen data
  treecover_list<-list()
  lossyear_list<-list()
  gain_list<-list()
  i<-1
  
  # Loop
  for (id in ID) {
    id<-as.character(id)
    
    # Check if Hansen has been downloaded
    han<-list.files(
      path=paste0(usr_parent_directory,"Data/Hansen/"),
      pattern=paste0("Hansen_GFC-2021-v1.9_treecover2000_",id,".tif"))
    
    # Load and or download Hansen
    message("Loading and/aggregating Hansen data")
    if (length(han)==0){
      # Download URL
      treecover<-paste0(
        "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2021-v1.9/Hansen_GFC-2021-v1.9_treecover2000_",
        id,".tif")
      lossyear<-paste0(
        "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2021-v1.9/Hansen_GFC-2021-v1.9_lossyear_",
        id,".tif")
      gain<-paste0(
        "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2021-v1.9/Hansen_GFC-2021-v1.9_gain_",
        id,".tif")
      fun_download_urls<-c(treecover,lossyear,gain)
      
      # Loop and download files
      for (URL in fun_download_urls) {
        file_name<-basename(httr::HEAD(URL)$url)
        message(paste0("Downloading ",file_name))
        curl::curl_download(
          URL,paste0(
            usr_parent_directory,"Data/Hansen/",file_name),
          quiet=FALSE)
      }
      rm(fun_download_urls)
      
      # Load
      treecover<-rast(
        paste0(usr_parent_directory,
               "Data/Hansen/Hansen_GFC-2021-v1.9_treecover2000_",
               id,".tif")) %>%
        terra::aggregate(fact=resize,fun="mean")
      lossyear<-rast(
        paste0(usr_parent_directory,
               "Data/Hansen/Hansen_GFC-2021-v1.9_lossyear_",
               id,".tif")) %>%
        terra::aggregate(fact=resize,fun="modal")
      gain<-rast(
        paste0(usr_parent_directory,
               "Data/Hansen/Hansen_GFC-2021-v1.9_gain_",
               id,".tif")) %>%
        terra::aggregate(fact=resize,fun="max")
      
    } else {
      # Load data
      treecover<-rast(
        paste0(usr_parent_directory,
               "Data/Hansen/Hansen_GFC-2021-v1.9_treecover2000_",
               id,".tif")) %>%
        terra::aggregate(fact=resize,fun="mean")
      lossyear<-rast(
        paste0(usr_parent_directory,
               "Data/Hansen/Hansen_GFC-2021-v1.9_lossyear_",
               id,".tif")) %>%
        terra::aggregate(fact=resize,fun="modal")
      gain<-rast(
        paste0(usr_parent_directory,
               "Data/Hansen/Hansen_GFC-2021-v1.9_gain_",
               id,".tif")) %>%
        terra::aggregate(fact=resize,fun="max")
    }
    rm(han)
    
    # Append to list
    treecover_list[[i]]<-treecover
    lossyear_list[[i]]<-lossyear
    gain_list[[i]]<-gain
    
    # Continue loop
    i<-i+1
    rm(treecover,lossyear,gain)
  }
  rm(ID,id,i)
  
  # Merge hansen data and clip to AOI
  message("Merging and clipping Hansen data")
  treecover<-lapply(treecover_list,terra::crop,y=shp_aoi) %>%
    terra::sprc() %>%
    terra::merge() %>%
    terra::mask(shp_aoi)
  message("treecover done")
  lossyear<-lapply(lossyear_list,terra::crop,y=shp_aoi) %>%
    terra::sprc() %>%
    terra::merge() %>%
    terra::mask(shp_aoi)
  message("lossyear done")
  gain<-lapply(gain_list,terra::crop,y=shp_aoi) %>%
    terra::sprc() %>%
    terra::merge() %>%
    terra::mask(shp_aoi)
  message("gain done")
  rm(treecover_list,lossyear_list,gain_list)
  
  # Return stacked object in order of:
  # 1) Treecover, 2) lossyear, 3) gain
  hansen<-list(treecover,lossyear,gain)
  names(hansen)<-c("treecover","lossyear","gain")
  
  # Select return object
  return(hansen)
  rm(treecover,lossyear,gain)
  gc()
}
