#' Creates maps of woody cover dynamics
#' 
#' @author Jurie Theron
#' 
#' @param proj_loc Location of project directory, to save results
#' @param usr_aoi_iso3 ISO 3 code of the country of interest
#' @custom_aoi user specified shapefile object / sf object
#' @aoi_provis provincial boundary of the country of interest

calc_maps = function(proj_loc = proj_loc,
                     usr_aoi_iso3 = usr_aoi_iso3,
                     custom_aoi = custom_aoi,
                     aoi_window_unproj = aoi_window_unproj) {
  
  suppressPackageStartupMessages(require(sf))
  suppressPackageStartupMessages(require(terra))
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(tmap))
  suppressPackageStartupMessages(require(tmaptools))
  
  # Select all countries within selected continent
  conti_filt<-country_codes() %>%
    dplyr::filter(continent==usr_aoi_cont) %>%
    dplyr::select("ISO3") %>%
    as.list(as.data.frame(t()))
  conti_filt<-as.character(conti_filt[["ISO3"]])
  
  # Load world borders
  world_bord<-st_read(
    paste0(
      usr_parent_directory,
      "Data/World_Borders.shp"),
    quiet=TRUE) %>%
    st_transform(crs=4326)
  
  # Select country of interest
  if(is.null(custom_aoi)){
    country<-world_bord %>%
      dplyr::filter(GID_0==usr_aoi_iso3)
    
  }else{
    country<-aoi_window_unproj
  }
  
  # Filter continent borders
  continent<-world_bord %>%
    filter(
      str_detect(
        GID_0,str_c(
          conti_filt,
          collapse="|")))
  
  # Create square bounding box
  bbox<-st_buffer(country,dist=80000) %>%
    st_bbox(country)
  x_range<-(bbox[[3]]+180)-(bbox[[1]]+180)
  y_range<-(bbox[[4]]+180)-(bbox[[2]]+180)
  
  # Calculate half distance
  add_dist<-(y_range-x_range)/2
  if(add_dist>0){
    add_dist<-add_dist
  }else{
    add_dist<-add_dist*-1
  }
  
  # Calculate new xy min max
  if(x_range < y_range){
    # Add / subtract to x values
    xmin<-bbox[[1]]-add_dist
    ymin<-bbox[[2]]
    xmax<-bbox[[3]]+add_dist
    ymax<-bbox[[4]]
  } else{
    # Add / subtract to y values
    xmin<-bbox[[1]]
    ymin<-bbox[[2]]-add_dist
    xmax<-bbox[[3]]
    ymax<-bbox[[4]]+add_dist
  }
  rm(add_dist,y_range,x_range,bbox)
  
  message("Merging tifs...")
  
  # Load 2000 tree cover final
  tree_cover_2000<-lapply(
    list.files(
      path=proj_loc,
      pattern=glob2rx("*_2000*treecov*_final.tif*"),
      full.names=TRUE),
    terra::rast)
  tree_cover_2000<-lapply(
    tree_cover_2000,terra::aggregate,
    fact=4,fun="mean") %>%
    terra::sprc() %>%
    terra::merge()
  if (file.exists(paste0(proj_loc,"/Products/Merged_tifs/"))) {
    cat("")
  } else {
    dir.create(paste0(proj_loc,"/Products/Merged_tifs/"))
  }
  writeRaster(
    tree_cover_2000,
    paste0(proj_loc,
           "/Products/Merged_tifs/tree_cover_2000_resampled.tif"))
  
  # Load 2021 tree cover final
  tree_cover_2021<-lapply(
    list.files(
      path=proj_loc,
      pattern=glob2rx("*_2021*treecov*_final.tif*"),
      full.names=TRUE),
    terra::rast)
  tree_cover_2021<-lapply(
    tree_cover_2021,terra::aggregate,
    fact=4,fun="mean") %>%
    terra::sprc() %>%
    terra::merge()
  writeRaster(
    tree_cover_2021,
    paste0(proj_loc,
           "/Products/Merged_tifs/tree_cover_2021_resampled.tif"))
  
  # Load Primary forest cover final
  Prim_forst<-lapply(
    list.files(
      path=proj_loc,
      pattern=glob2rx("*Primary*Forest*Extent.tif*"),
      full.names=TRUE),
    terra::rast)
  Prim_forst<-lapply(
    Prim_forst,terra::aggregate,
    fact=4,fun="max") %>%
    terra::sprc() %>%
    terra::merge() %>%
    terra::classify(cbind(0,NA))
  writeRaster(
    Prim_forst,
    paste0(proj_loc,
           "/Products/Merged_tifs/primary_forest_extent_resampled.tif"))
  
  # Load tree gain final
  tree_gain<-lapply(
    list.files(
      path=proj_loc,
      pattern=glob2rx("*treegain*final.tif*"),
      full.names=TRUE),
    terra::rast)
  tree_gain<-lapply(
    tree_gain,terra::aggregate,
    fact=4,fun="modal") %>%
    terra::sprc() %>%
    terra::merge()
  writeRaster(
    tree_gain,
    paste0(proj_loc,
           "/Products/Merged_tifs/tree_gain_resampled.tif"))
  
  # Load tree loss final
  tree_loss<-lapply(
    list.files(
      path=proj_loc,
      pattern=glob2rx("*treeloss*final.tif*"),
      full.names=TRUE),
    terra::rast)
  tree_loss<-lapply(
    tree_loss,terra::aggregate,
    fact=4,fun="modal") %>%
    terra::sprc() %>%
    terra::merge()
  writeRaster(
    tree_loss,
    paste0(proj_loc,
           "/Products/Merged_tifs/tree_loss_resampled.tif"))
  
  # Mini map
  if (file.exists(paste0(proj_loc,"/Products/Maps/"))) {
    cat("")
  } else {
    dir.create(paste0(proj_loc,"/Products/Maps/"))
  }
  mini<-tm_shape(continent) +
    tm_sf("white") +
    tm_shape(continent) +
    tm_borders("black") +
    tm_shape(country) +
    tm_sf("red") +
    tm_shape(country) +
    tm_borders("red") +
    tm_layout(frame=FALSE)
  tmap_save(mini,
    paste0(
      proj_loc,
      "/Products/Maps/",
      usr_aoi_iso3,
      "_MINI_map.png"),
    units="mm",
    width=90,
    height=70,
    dpi=300)
  
  # Create tree cover map 2000   
  cov_2000<-tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey") +
    tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("white") +
    tm_text("NAME_0") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey50") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("grey50") +
    tm_shape(tree_cover_2000)+
    tm_raster(palette="Greens",
              title="Woody cover, 2000 (%)",
              breaks = c(0,20,40,60,80,100)) +
    tm_layout(legend.outside=TRUE,
              legend.outside.position="right",
              fontface=2) +
    tm_grid(lines=FALSE) +
    tm_scale_bar(position=c("left","bottom")) +
    tm_compass(type="arrow")
  tmap_save(cov_2000,
            paste0(
              proj_loc,
              "/Products/Maps/",
              usr_aoi_iso3,
              "_Woody2000_map.png"),
            units="mm",
            width=230,
            height=180,
            dpi=300)
  
  # Create tree cover map 2021
  cov_2021<-tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey") +
    tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("white") +
    tm_text("NAME_0") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey50") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("grey50") +
    tm_shape(tree_cover_2021)+
    tm_raster(palette="Greens",
              title="Woody cover, 2021 (%)",
              breaks = c(0,20,40,60,80,100)) +
    tm_layout(legend.outside=TRUE,
              legend.outside.position="right",
              fontface=2) +
    tm_grid(lines=FALSE) +
    tm_scale_bar(position=c("left","bottom")) +
    tm_compass(type="arrow")
  tmap_save(cov_2021,
            paste0(
              proj_loc,
              "/Products/Maps/",
              usr_aoi_iso3,
              "_Woody2021_map.png"),
            units="mm",
            width=230,
            height=180,
            dpi=300)
  
  # Create tree loss map
  treeloss<-tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey") +
    tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("white") +
    tm_text("NAME_0") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey50") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("grey50") +
    tm_shape(tree_loss)+
    tm_raster(palette="Reds",
              title="Woody loss, 2000-2021 (%)",
              breaks = c(0,20,40,60,80,100)) +
    tm_layout(legend.outside=TRUE,
              legend.outside.position="right",
              fontface=2) +
    tm_grid(lines=FALSE) +
    tm_scale_bar(position=c("left","bottom")) +
    tm_compass(type="arrow")
  tmap_save(treeloss,
            paste0(
              proj_loc,
              "/Products/Maps/",
              usr_aoi_iso3,
              "_WoodyLoss_map.png"),
            units="mm",
            width=230,
            height=180,
            dpi=300)
  
  # Create tree gain map
  treegain<-tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey") +
    tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("white") +
    tm_text("NAME_0") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey50") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("grey50") +
    tm_shape(tree_gain)+
    tm_raster(palette="plasma",
              title="Woody gain, 2000-2012") +
    tm_layout(legend.outside=TRUE,
              legend.outside.position="right",
              fontface=2) +
    tm_grid(lines=FALSE) +
    tm_scale_bar(position=c("left","bottom")) +
    tm_compass(type="arrow")
  tmap_save(treegain,
            paste0(
              proj_loc,
              "/Products/Maps/",
              usr_aoi_iso3,
              "_WoodyGain_map.png"),
            units="mm",
            width=230,
            height=180,
            dpi=300)
  
  # Create primary forest map
  primfor<-tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey") +
    tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("white") +
    tm_text("NAME_0") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey50") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("grey50") +
    tm_shape(Prim_forst)+
    tm_raster(palette="Greens",
              title="Primary tree cover") +
    tm_layout(legend.outside=TRUE,
              legend.outside.position="right",
              fontface=2) +
    tm_grid(lines=FALSE) +
    tm_scale_bar(position=c("left","bottom")) +
    tm_compass(type="arrow")
  tmap_save(primfor,
            paste0(
              proj_loc,
              "/Products/Maps/",
              usr_aoi_iso3,
              "_PrimForest_2021_map.png"),
            units="mm",
            width=230,
            height=180,
            dpi=300)
  
  # Create GEZ map
  gez<-st_read(paste0(
    usr_parent_directory,
    "Data/gez2010/gez_2010_wgs84.shp"),
    quiet=TRUE) %>%
    sf::st_make_valid()
  gez_names<-gez$gez_name
  cols<-c("#0a6900","#324530","#75e868","#9de0db","#90ab90","#334733",
          "#186356","#62948b","#c6e3b6","#b0f041","#a8860c","#e6d085",
          "#bf66d4","#b52ba7","#a10a2a","#ab5916","#599eb3","#748387",
          "#6e5b80","#c596f2","#0216cc")
  gez_index<-data.frame(name=gez_names,col=cols)
  rm(gez_names,cols)
  gez<-gez %>%
    sf::st_intersection(country)
  gez_names<-as.list(as.data.frame(t(gez$gez_name)))
  gez_index<-gez_index %>%
    filter(name %in% gez_names)
  rm(gez_names)
  gez_map<-tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey") +
    tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("white") +
    tm_text("NAME_0") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey50") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("grey50") +
    tm_shape(gez,bbox=c(xmin,ymin,xmax,ymax))+
    tm_sf(col="gez_name",
          palette=gez_index$col,
          title="Global Ecological Zones") +
    tm_layout(legend.outside=TRUE,
              legend.outside.position="right",
              fontface=2) +
    tm_grid(lines=FALSE) +
    tm_scale_bar(position=c("left","bottom")) +
    tm_compass(type="arrow")
  tmap_save(gez_map,
            paste0(
              proj_loc,
              "/Products/Maps/",
              usr_aoi_iso3,
              "_GEZ_map.png"),
            units="mm",
            width=230,
            height=180,
            dpi=300)
  
  # ESRI land cover
  esri<-grab_esri_cover(shp_aoi = country,
                        year = 2021)
  esri_cov<-as.data.frame(
    unique(esri))
  colnames(esri_cov)<-c("cov")
  
  # Create legend
  name<-c("Water","Trees","Flooded vegetation",
          "Crops","Built Area","Bare ground",
          "Snow/Ice","Clouds","Rangeland")
  col<-c("#419bdf","#397d49","#7a87c6",
         "#e49635","#c4281b","#a59b8f",
         "#a8ebff","#616161","#e3e2c3")
  val<-c(1,2,4,5,7,8,9,10,11)
  leg<-data.frame(Name=name,Col=col,Value=val)
  rm(name,col,val)
  
  # Remove elements not present in map
  esri_cov<-as.list(as.data.frame(t(esri_cov$cov)))
  leg<-leg %>%
    filter(Value %in% esri_cov)
  
  # Create ESRI land cover
  esri_land<-tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey") +
    tm_shape(continent,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("white") +
    tm_text("NAME_0") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_sf("grey50") +
    tm_shape(country,bbox=c(xmin,ymin,xmax,ymax)) +
    tm_borders("grey50") +
    tm_shape(esri)+
    tm_raster(style="cat",
              palette=leg$Col,
              labels=leg$Name,
              title="ESRI land cover, 2021") +
    tm_layout(legend.outside=TRUE,
              legend.outside.position="right",
              fontface=2) +
    tm_grid(lines=FALSE) +
    tm_scale_bar(position=c("left","bottom")) +
    tm_compass(type="arrow")
  tmap_save(esri_land,
            paste0(
              proj_loc,
              "/Products/Maps/",
              usr_aoi_iso3,
              "_ESRI_LandCover.png"),
            units="mm",
            width=230,
            height=180,
            dpi=300)
  
  message("Map making done!")
  rm(continent,country,gez,gez_index,lossyear,treecover,treegain,
     treecover_2021,world_bord,conti_filt,xmax,xmin,ymin,ymax)
}
