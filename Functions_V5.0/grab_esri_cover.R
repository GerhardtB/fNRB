#' Identifies corresponding ESRI land cover data, downloads, loads and clips them.
#' 
#' @author Jurie Theron
#' 
#' @param shp_aoi Area of interest - shapefile as sf object.
#' @param resize Factor by which to aggregate pixels
#' @param year Year of land cover product: https://livingatlas.arcgis.com/landcover/

grab_esri_cover = function(shp_aoi,
                           year = 2021) {
  
  suppressPackageStartupMessages(require(sf))
  suppressPackageStartupMessages(require(terra))
  suppressPackageStartupMessages(require(dplyr))
  
  # Make sure aoi is in geographic coordinate system
  shp_aoi<-shp_aoi %>%
    st_transform(crs=4326)
  
  # Create grid for projecting
  tmp_rast<-terra::rast(
    ext=ext(shp_aoi),
    resolution=0.001,
    crs=crs(shp_aoi))
  
  # Identify IDs of corresponding ESRI
  # data from grid
  utm_grid<-st_read(paste0(
    usr_parent_directory,"Data/World_UTM_Grid/World_UTM_Grid.shp"),
    quiet=TRUE) %>%
    st_as_sf() %>%
    st_make_valid()
  grid<-st_join(shp_aoi,utm_grid) %>%
    dplyr::select(ZONE,ROW_) %>%
    st_drop_geometry() %>%
    dplyr::mutate(tile=paste0(ZONE,ROW_)) %>%
    dplyr::select(tile)
  grid<-as.character(grid$tile)
  rm(utm_grid)
  
  # Loop
  esri_list<-list()
  i<-1
  year_end<-year+1
  for (id in grid) {
    # Check if ESRI has been downloaded
    esri<-list.files(
      path=paste0(usr_parent_directory,"Data/ESRI_Land_Cover/"),
      pattern=paste0(id,"_",year,"0101-",year_end,"0101.tif"))
    
    # Load and or download ESRI
    message("Loading and/aggregating ESRI data")
    if (length(esri)==0){
      # Download URL
      land<-paste0(
        "https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc",
        year,"/",id,"_",year,"0101-",year_end,"0101.tif")
      fun_download_urls<-c(land)
      
      # Loop and download files
      message("Downloading: ESRI restricts download speed...")
      for (URL in fun_download_urls) {
        file_name<-basename(httr::HEAD(URL)$url)
        message(paste0("Downloading ",file_name))
        curl::curl_download(
          URL,paste0(
            usr_parent_directory,"Data/ESRI_Land_Cover/",file_name),
          quiet=FALSE)
      }
      rm(fun_download_urls)
      
      # Load data
      land<-rast(
        paste0(usr_parent_directory,
               "Data/ESRI_Land_Cover/",id,"_",year,
               "0101-",year_end,"0101.tif")) %>%
        terra::project(tmp_rast,method="near") %>%
        terra::resample(tmp_rast,method="near") %>%
        terra::crop(shp_aoi) %>%
        terra::mask(shp_aoi)
      
    } else {
      # Load data
      land<-rast(
        paste0(usr_parent_directory,
               "Data/ESRI_Land_Cover/",id,"_",year,
               "0101-",year_end,"0101.tif")) %>%
        terra::project(tmp_rast,method="near") %>%
        terra::resample(tmp_rast,method="near") %>%
        terra::crop(shp_aoi) %>%
        terra::mask(shp_aoi)
    }
    rm(esri)
    
    # Append to list
    esri_list[[i]]<-land
    
    # Continue loop
    i<-i+1
    rm(land)
  }
  rm(id,i)
  
  # Merge ESRI data and clip to AOI
  message("Merging and clipping ESRI data")
  esri<-terra::sprc(esri_list) %>%
    terra::merge()
  rm(esri_list)
  
  # Select return object
  return(esri)
  gc()
}
