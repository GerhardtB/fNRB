#' @author Ruan Parrott, Nicholas Salonen & Jurie Theron.
#' Jurie Theron translated Raster functions to Terra function for version 4 of this script.
#' 
#' @param parent_directory User specified location within which an "fNRB_analysis" directory is used or created if it doesn't already exist.
#' @param aoi_iso3 The ISO3 character string for the country of interest.
#' @param usr_provinces_ofinterest The province of interest.
#' @param custom_aoi Name of user provided AOI stored in the "Custom_aoi" folder
#' @param month The month corresponding to the latest available protected areas data set.
#' @param download Specify whether you need to download data.
#' @param buf_dist Specify distance of buffer to calculate accessibility.
#' @param Create_Maps_Tables TRUE/FALSE Whether to calculate fNRB value and create maps

# Load packages
library("sf")
library("terra")
library("tidyverse")
library("tmap")
library("tmaptools")
library("geodata")
library("curl")

# User defined input arguments
usr_parent_directory    <-"/home/c4datasciencevm/projDir/fNRB_Ver5.0/"
usr_aoi_iso3            <-"IDN"
usr_provinces_ofinterest<-NULL
custom_aoi              <-NULL #"aoi.shp"
usr_month               <-"Dec"
data_download           <-TRUE
buf_dist                <-2500
Create_Maps_Tables      <-TRUE

# Run Code within {} as one chunk
{
  # Load other parameters
  parms_csv<-read.csv(file.path(paste0(usr_parent_directory,"Data/CSV_Databases/Parameters.csv")))
  if (isTRUE((parms_csv %>% filter(ISO3_Code %in% usr_aoi_iso3) %>% select(ISO3_Code) == usr_aoi_iso3))) {
    parms_csv<-parms_csv %>%
      filter(ISO3_Code %in% usr_aoi_iso3)
    usr_aoi_name<-parms_csv$Country
    usr_aoi_cont<-parms_csv$Continent
  } else {
    message("Please check that you entered a valid ISO 3 country code.\nVisit https://www.iban.com/country-codes")
  }
  rm(parms_csv)
  
  # Load functions
  fNRBsuppRt_funcs<-list.files(paste0(
    usr_parent_directory,"Functions_V5.0"),
    pattern=".R",all.files=TRUE,full.names=TRUE)
  for(i in 1:length(fNRBsuppRt_funcs)){
    source(fNRBsuppRt_funcs[i])
  }
  rm(i,fNRBsuppRt_funcs)
}

# Set up woody cover calculations
get_fNRB<-function (parent_directory = usr_parent_directory,
                    aoi_name = usr_aoi_name,
                    aoi_iso3 = usr_aoi_iso3,
                    aoi_cont = usr_aoi_cont,
                    provinces_ofinterest = usr_provinces_ofinterest,
                    month = usr_month,
                    download = data_download) {
  
  proj_loc<-dir_fNRB(fun_parent_directory = parent_directory,
                     fun_aoi_name = aoi_name,
                     fun_aoi_iso3 = aoi_iso3,
                     fun_aoi_cont = aoi_cont,
                     fun_provinces_ofinterest = provinces_ofinterest,
                     fun_month = month,
                     fun_download = download)
  parent_directory<-file.path(paste0(parent_directory,"fNRB_analysis/"))
  proj_directory<-file.path(paste0(parent_directory,"/",aoi_name,"/"))
  in_loc<-file.path(paste0(proj_directory,"/","Input_Data/"))
  
  # Loading shapefiles
  message("Loading and projecting shapefiles...")
  aoi_national<-st_read(
    file.path(in_loc,paste0(
      aoi_iso3, "_adm/", aoi_iso3, "_adm0.shp")),
    quiet=TRUE) %>%
    st_as_sf() %>%
    sf::st_make_valid()
  
  # Check whether to use user aoi or country shapefile
  if(is.null(custom_aoi)){
    # Load provincial shapefile
    aoi_provis<-st_read(
      file.path(in_loc,paste0(
        aoi_iso3,"_adm/",aoi_iso3,"_adm1.shp")),
      quiet=TRUE) %>% 
      st_as_sf() %>%
      sf::st_make_valid()
  }else{
    # Load custom shapefile
    aoi_provis<-st_read(
      paste0(usr_parent_directory,"Data/Custom_aoi/",custom_aoi),
      quiet=TRUE) %>% 
      st_as_sf() %>%
      st_transform(crs=4326) %>%
      sf::st_make_valid()
    aoi_provis<-aoi_provis %>% 
      dplyr::mutate(NAME_1=1:nrow(aoi_provis))
  }
  
  # Roads
  aoi_roads<-st_read(
    file.path(in_loc,paste0(
      aoi_iso3,"_rds/",aoi_iso3,"_roads.shp")),
    quiet=TRUE) %>%
    st_as_sf() %>%
    sf::st_make_valid()
  
  # Protected areas
  aoi_PAname<-paste0(
    "WDPA_WDOECM_",month,"2022_Public_",aoi_iso3,"_shp")
  aoi_PAs0<-st_read(
    file.path(in_loc,aoi_PAname,paste0(
      aoi_PAname, "_0")),
    quiet=TRUE,
    layer=paste0(aoi_PAname,"-polygons")) %>%
    st_as_sf() %>%
    sf::st_make_valid()
  aoi_PAs1<-st_read(
    file.path(in_loc,aoi_PAname,paste0(
      aoi_PAname,"_1")),
    quiet=TRUE,
    layer=paste0(aoi_PAname,"-polygons")) %>%
    st_as_sf() %>%
    sf::st_make_valid()
  aoi_PAs2<-st_read(
    file.path(in_loc,aoi_PAname,paste0(
      aoi_PAname,"_2")),
    quiet=TRUE,
    layer=paste0(aoi_PAname,"-polygons")) %>%
    st_as_sf() %>%
    sf::st_make_valid()
  aoi_PAs<-rbind(aoi_PAs0,aoi_PAs1,aoi_PAs2)
  rm(aoi_PAs0,aoi_PAs1,aoi_PAs2)
  
  if (is.null(provinces_ofinterest) == FALSE){
    aoi_provis<-aoi_provis %>%
      filter(NAME_1 %in% provinces_ofinterest)
    }
  
  # Perform calculations per province within country
  for (i in 1:nrow(aoi_provis)){
  tryCatch({
		provinces_ofinterest<-gsub("[^[:alnum:]]"," ",as.character(aoi_provis$NAME_1[i]))
		message(paste0("Processing fNRB values for the ",provinces_ofinterest,
				" province.\nNumber ",i," of the ",length(aoi_provis$NAME_1), 
				" provinces to be processed in ",aoi_name))
		
		# Subset to provincial level
		message("Subsetting to provincial level...")
		
		# Window for clipping first before projecting
		aoi_window_unproj<-aoi_provis %>% 
		  filter(NAME_1 %in% NAME_1[i]) %>%
		  select('geometry') %>% 
		  st_as_sf()
		
		# Find UTM and project
		crs_proj<-grab_UTM_proj4(aoi_window_unproj)
		
		# Project
		aoi_window<-aoi_provis %>% 
		  filter(NAME_1 %in% NAME_1[i]) %>%
		  select('geometry') %>% 
		  st_as_sf() %>%
		  st_transform(crs=crs_proj)
		
		# Crop roads
		aoi_poi_roads<-aoi_roads %>%
		  st_intersection(aoi_window_unproj) %>%
		  st_transform(crs=crs_proj)
		
		# Crop and load global GEZ
		message("Cropping and projecting GEZ...")
		aoi_fao_gez<-st_read(
		  file.path(usr_parent_directory,"Data/gez2010/"),
		  quiet=TRUE,layer="gez_2010_wgs84") %>%
		  sf::st_make_valid() %>%
		  st_intersection(aoi_window_unproj) %>%
		  st_transform(crs=crs_proj) %>%
		 #st_intersection(aoi_window) %>%
		  dplyr::mutate(gez_name=as.character(gez_name))
		
		# Check if water is present
		water_Check<-as.character(aoi_fao_gez$gez_name)
		if("Water" %in% water_Check){
		  aoi_fao_gez$area<-as.numeric(st_area(aoi_fao_gez))
		  # ID largers other GEZ
		  if(nrow(aoi_fao_gez)>1){
		    rename<-aoi_fao_gez %>%
		      dplyr::filter(gez_name!="Water") %>%
		      dplyr::slice_max(area) %>%
		      dplyr::select("gez_name") %>% 
		      st_drop_geometry() %>%
		      as.character()
		  }
		  # Reclassify water
		  aoi_fao_gez<-aoi_fao_gez %>%
		    dplyr::mutate(gez_name=case_when(
		      gez_name=="Water" ~ rename,
		      TRUE ~ gez_name)) %>%
		    st_as_sf()
		  rm(rename)
		}
		rm(water_Check)
		
		# Check geometry
		if (st_geometry_type(aoi_fao_gez,by_geometry=FALSE) != "POLYGON" &
		   st_geometry_type(aoi_fao_gez,by_geometry=FALSE) != "MULTIPOLYGON") {
		   aoi_fao_gez<-st_collection_extract(
		     aoi_fao_gez,type=c("POLYGON","POINT","LINESTRING"))
		   aoi_fao_gez<-aggregate(
		     aoi_fao_gez,list(aoi_fao_gez$gez_code), function(x) x[1])
		}
    aoi_fao_gez<-aoi_fao_gez %>%
      dplyr::group_by(gez_name) %>%
      dplyr::summarise()
    
		# GLAD PRIMARY FOREST COVER 
		message("Cropping and projecting primary forest...")
		rst_template<-terra::rast(
		  ext=ext(aoi_window),resolution=30,crs=crs(aoi_window))
		aoi_primary<-terra::rast(
		  file.path(
		    paste0(usr_parent_directory,"Data/Primary"),
		    paste0(aoi_cont,"_2001_primary.tif"))) %>%
		  terra::project(rst_template,method="bilinear") %>%
		  terra::crop(aoi_window) %>%
		  terra::classify(cbind(NA,0))
		
		# Writing initial files
		message(paste0("Writing initial files to:", proj_loc))
		st_write(aoi_fao_gez,file.path(
		  proj_loc,paste0(
		    aoi_iso3,"_",provinces_ofinterest,"_fao_gez.shp")),quiet=TRUE)
		  
		# Buffer PAs
		# Create list for calculating remote and accessible areas
		aoi_PAs_union_list<-aoi_PAs %>%
		  st_intersection(aoi_window_unproj) %>%
		  st_transform(crs=crs_proj) %>%
		  st_buffer(dist=20) %>%
		  st_union()
		# Merge list to create sf extent for clipping and masking rasters
		aoi_PAs_union<-aoi_PAs %>%
		  st_intersection(aoi_window_unproj) %>%
		  st_transform(crs=crs_proj) %>%
		  st_buffer(dist=20) %>%
		  st_union() %>%
		  st_as_sf()
		st_write(aoi_PAs_union,file.path(
		  proj_loc,paste0(
		    aoi_iso3,"_",provinces_ofinterest,"_union_Protected Areas.shp")),quiet=TRUE)
		
		message("Disaggregating remote and accessible areas...")
		aoi_remote<-disaggregate_access(aoi_border = aoi_window, 
										roads_projected = aoi_poi_roads,
										buffer_dist = buf_dist, 
										PAs_union = aoi_PAs_union_list,
										proj_crs = crs_proj,
										aoi_prefix = paste0("/",aoi_iso3,"_",provinces_ofinterest,"_"),
										destination = proj_loc)	
		rm(aoi_PAs_union_list)
		
		# Hansen data
		message("Downloading/loading Hansen data...")
		hansen_data<-grab_hansen(aoi_window_unproj)
		# 1) Treecover, 2) lossyear, 3) gain
		aoi_treecov<-hansen_data[[1]]
		aoi_treegain<-hansen_data[[3]]
		aoi_treeloss<-hansen_data[[2]]
		rm(hansen_data)
		
		message("Cropping and projecting Hansen treecover...")
		aoi_treecov<-aoi_treecov %>%
		  terra::crop(aoi_window_unproj,snap="out") %>%
		  terra::project(rst_template,method="bilinear") %>%
		  terra::crop(aoi_window,snap="out")
		aoi_treegain<-aoi_treegain %>%
		  terra::crop(aoi_window_unproj,snap="out") %>%
		  terra::project(rst_template,method="near") %>%
		  terra::crop(aoi_window,snap="out")
		aoi_treeloss<-aoi_treeloss %>%
		  terra::crop(aoi_window_unproj,snap="out") %>%
		  terra::project(rst_template,method="near") %>%
		  terra::crop(aoi_window,snap="out")
		
		# Tree loss
		message("Processing treecover loss...")
		aoi_treeloss_processed<-process_coverchange(
		  change_raster = aoi_treeloss,
		  aggregation = 1,
		  aoi_shape = aoi_window,
		  aoi_prefix = ifelse(is.null(provinces_ofinterest),
		                      paste0("/", aoi_iso3, "_"),
		                      paste0("/", aoi_iso3, "_",
		                      provinces_ofinterest, "_")),
		  var_suffix = "_treeloss",
		  destination = proj_loc)
		gc()
		rm(aoi_treeloss)
		
		# Create matrix for reclassifying pixels after aggregating
		resclass_trim_frac<-matrix(c(-Inf,0,0,1,Inf,1),ncol=3,byrow=TRUE)
		aoi_treeloss_total<-terra::classify(aoi_treeloss_processed,resclass_trim_frac)
		gc()
		rm(aoi_treeloss_processed)
		
		# Tree gain
		message("Processing treecover gain...")
		aoi_treegain_processed<-process_coverchange(
		  change_raster = aoi_treegain,
		  aggregation = 1,
		  aoi_shape = aoi_window,
		  aoi_prefix = ifelse(is.null(provinces_ofinterest),
		                      paste0("/", aoi_iso3, "_"),
		                      paste0("/", aoi_iso3, "_",
		                      provinces_ofinterest, "_")),
		  var_suffix = "_treegain",
		  destination = proj_loc)
		gc()
		rm(aoi_treegain)
		
		# Reclassifying pixels after aggregating
		aoi_treegain_total<-terra::classify(aoi_treegain_processed,resclass_trim_frac)
		gc()
		rm(aoi_treegain_processed)
		
		# Save to disk
		writeRaster(aoi_treeloss_total,file.path(
		  proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,"_treeloss_total.tif")))
		writeRaster(aoi_treegain_total,file.path(
		  proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,"_treegain_total.tif")))
		
		# Tree cover
		message("Processing cover...")
		process_cover(cover_raster = aoi_treecov,
		              loss_raster = aoi_treeloss_total,
		              gain_raster = aoi_treegain_total,
		              aggregation = 1,
		              remote_shape = aoi_remote,
		              aoi_shape = aoi_window,
		              aoi_prefix = ifelse(is.null(provinces_ofinterest),
		                                  paste0("/", aoi_iso3, "_"),
		                                  paste0("/", aoi_iso3, "_",
		                                         provinces_ofinterest, "_")),
		              destination = proj_loc)
		gc()
		rm(aoi_remote,aoi_treecov,aoi_treeloss_total)
		message("Cover has been processed...")
		
		# Accessible
		message("Reclassifying...")
		message("Accessible cover...")
		resclass_trim<-matrix(c(-Inf,0,0,100,Inf,100),ncol=3,byrow=TRUE)
		aoi_treecov2021_access<-terra::rast(file.path(
		  proj_loc,paste0(
		    aoi_iso3,ifelse(
		      is.null(provinces_ofinterest),
		      "",paste0("_",provinces_ofinterest)),
		    "_agg and reclass_2021cover_access.tif"))) %>%
		  terra::classify(resclass_trim) %>% 
		  terra::crop(aoi_window)
		gc()
		
		# 2000 cover
		message("2000 cover...")
		aoi_treecov2000_total<-terra::rast(file.path(
		  proj_loc,paste0(
		    aoi_iso3,ifelse(
		      is.null(provinces_ofinterest),
		      "",paste0("_",provinces_ofinterest)),
		    "_agg and reclass_2000cover_total.tif"))) %>% 
		  terra::classify(resclass_trim) %>% 
		  terra::crop(aoi_window)
		gc()
		
		# 2021 cover
		message("2021 cover...")
		aoi_treecov2021_total<-terra::rast(file.path(
		  proj_loc,paste0(
		    aoi_iso3,ifelse(
		      is.null(provinces_ofinterest),
		      "",paste0("_",provinces_ofinterest)),
		    "_agg and reclass_2021cover_total.tif"))) %>% 
		  terra::classify(resclass_trim) %>% 
		  terra::crop(aoi_window)
		gc()
		
		# Calculate absolute loss
		message("Loss...")
		datastack<-sds(aoi_treecov2000_total,aoi_treecov2021_total)
		aoi_treeloss_absolute<-terra::lapp(datastack,fun=function(r1,r2) {r1-r2}) %>%
		  terra::classify(matrix(c(-Inf,0,NA),ncol=3,byrow=TRUE)) %>%
		  terra::crop(aoi_window)
		gc()
		rm(datastack)
		
		# Gain
		message("Gain...")
		aoi_treegain_absolute<-aoi_treegain_total %>%
		  terra::classify(matrix(c(-Inf,0,NA),ncol=3,byrow=TRUE)) %>%
		  terra::crop(aoi_window) %>%
		  terra::mask(aoi_window)
		rm(aoi_treegain_total)
		gc()
		
		# PAs
		message("PAs...")
		if (length(aoi_PAs_union)>0) {
		  sp_aoi_PAs_union<-aoi_PAs_union
		  aoi_2021_PAcov_m<-terra::mask(aoi_treecov2021_total,sp_aoi_PAs_union)
		  rm(sp_aoi_PAs_union)
		} else {
		  aoi_2021_PAcov_m<-aoi_treecov2021_total %>%
		    terra::classify(matrix(c(-Inf,Inf,0),ncol=3,byrow=TRUE))
		}
		
		# Create trimming function
		trim_rast<-function(r,max=100) {
		  resclass_trim<-matrix(c(-Inf,0,0,max,Inf,max),ncol=3,byrow=TRUE)
			rast_trimmed<-r %>%
			  terra::classify(resclass_trim)
		return(rast_trimmed)
		}
		
		# Trim rasters
		message("Trimming...")
		aoi_treecov2021_access<-trim_rast(aoi_treecov2021_access)
		aoi_treecov2021_total<-trim_rast(aoi_treecov2021_total)
		aoi_treecov2000_total<-trim_rast(aoi_treecov2000_total)
		aoi_2021_PAcov_m<-trim_rast(aoi_2021_PAcov_m)
		aoi_treegain_absolute<-trim_rast(aoi_treegain_absolute,max=1)
		aoi_treeloss_absolute<-trim_rast(aoi_treeloss_absolute)
		
	  # Save to disk
		message("Writing cover rasters...")
		writeRaster(aoi_treecov2021_access,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_treecov_access_final.tif")))
		writeRaster(aoi_treecov2021_total,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_2021_treecov_final.tif")))
		writeRaster(aoi_treecov2000_total,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_2000_treecov_final.tif")))
		writeRaster(aoi_2021_PAcov_m,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_2021_PAcov_final.tif")))
		writeRaster(aoi_treegain_absolute,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_treegain_final.tif")))
		writeRaster(aoi_treeloss_absolute,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_treeloss_final.tif")))
		
	  # Disaggregate rasters to GEZ
		message("Compiling final treecover results...")
		message("Writing total treecover 2021 per GEZ...")
		aoi_gez_cover_total<-disaggregate_gez(
		  aoi_treecov2021_total,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2021 Total woody cover")
		message("Writing total treecover 2000 per GEZ...")
		aoi_gez_cover_tot200<-disaggregate_gez(
		  aoi_treecov2000_total,poly_gez=aoi_fao_gez,scaling=1/100) %>% 
		  dplyr::mutate(Variable="2000 Total woody cover")
		message("Writing 2021 accessible treecover per GEZ...")
		aoi_gez_cover_access<-disaggregate_gez(
		  aoi_treecov2021_access,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2021 Accessible wood cover")
		message("Writing 2021 protected treecover per GEZ...")
		aoi_gez_cover_protected<-disaggregate_gez(
		  aoi_2021_PAcov_m,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2021 Protected wood cover")
		message("Writing 2000-2012 treecover gain per GEZ...")
		aoi_gez_cover_gain<-disaggregate_gez(
		  aoi_treegain_absolute,poly_gez=aoi_fao_gez) %>%
		  dplyr::mutate(Variable="2000-2012 Woody cover gain")
		message("Writing 2000-2021 treecover loss per GEZ...")
		aoi_gez_cover_loss<-disaggregate_gez(
		  aoi_treeloss_absolute,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2000-2021 Woody cover loss")
	  
		# Create dataframe
		message("Combining output...")
		aoi_gez_cover<-rbind.data.frame(aoi_gez_cover_total,
		                                aoi_gez_cover_tot200,
		                                aoi_gez_cover_access,
		                                aoi_gez_cover_protected,
		                                aoi_gez_cover_gain,
		                                aoi_gez_cover_loss)
		
		# m2 to ha
		message("Converting from square meters to hectares...")
		m2_ha<-1/10000
		aoi_gez_cover[2:4]<-aoi_gez_cover[2:4]*m2_ha
		
		# Saving to disk
		message(paste0("Writing output to: ",file.path(
		  proj_loc,"CSVs",paste0(
		    aoi_iso3,"_",provinces_ofinterest,
		    "_Tree cover disaggregated by GEZ_ha.csv"))))
		write.csv(aoi_gez_cover,file.path(
		  proj_loc,"CSVs",paste0(
		    aoi_iso3,"_",provinces_ofinterest,
		    "_Tree cover disaggregated by GEZ_ha.csv")))
		message("Treecover compiled.")
		
		# Primary Forest
		message("Processing primary cover...")
		aoi_primary_reclass<-aoi_primary %>%
		  terra::classify(matrix(c(0,Inf,1),ncol=3))
		rm(aoi_primary)
		
		# Primary forest cover
		aoi_treecov2021_access<-aoi_treecov2021_access*aoi_primary_reclass
		aoi_treecov2021_total<-aoi_treecov2021_total*aoi_primary_reclass
		aoi_treecov2000_total<-aoi_treecov2000_total*aoi_primary_reclass
		aoi_2021_PAcov_m<-aoi_2021_PAcov_m*aoi_primary_reclass
		aoi_treegain_absolute<-aoi_treegain_absolute*aoi_primary_reclass
		aoi_treeloss_absolute<-aoi_treeloss_absolute*aoi_primary_reclass
		writeRaster(aoi_primary_reclass,file.path(
		  proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,
		                   "_Primary Forest Extent.tif")))
		rm(aoi_primary_reclass)
		message("Compiling final primary cover results...")
		
		# Primary tree cover per GEZ
		message("Writing total primary treecover 2021...")
		aoi_gez_cover_total<-disaggregate_gez(
		  aoi_treecov2021_total,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2021 Total woody cover")
		message("Writing total primary treecover 2000...")
		aoi_gez_cover_tot200<-disaggregate_gez(
		  aoi_treecov2000_total,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2000 Total woody cover")
		message("Writing 2021 accessible primary treecover...")
		aoi_gez_cover_access<-disaggregate_gez(
		  aoi_treecov2021_access,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2021 Accessible wood cover")
		message("Writing 2021 protected primary treecover...")
		aoi_gez_cover_protected<-disaggregate_gez(
		  aoi_2021_PAcov_m,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2021 Protected wood cover")
		message("Writing 2000-2012 primary treecover gain...")
		aoi_gez_cover_gain<-disaggregate_gez(
		  aoi_treegain_absolute,poly_gez=aoi_fao_gez) %>%
		  dplyr::mutate(Variable="2000-2012 Woody cover gain")
		message("Writing 2000-2021 primary treecover loss...")
		aoi_gez_cover_loss<-disaggregate_gez(
		  aoi_treeloss_absolute,poly_gez=aoi_fao_gez,scaling=1/100) %>% 
		  dplyr::mutate(Variable="2000-2021 Woody cover loss")
		rm(aoi_fao_gez)
	  
		# Create dataframe
		message("Combining output...")
		aoi_gez_cover<-rbind.data.frame(aoi_gez_cover_total,
		                                aoi_gez_cover_tot200,
		                                aoi_gez_cover_access,
		                                aoi_gez_cover_protected,
		                                aoi_gez_cover_gain,
		                                aoi_gez_cover_loss)
		rm(aoi_gez_cover_total,aoi_gez_cover_tot200,aoi_gez_cover_access,
		   aoi_gez_cover_protected,aoi_gez_cover_gain,aoi_gez_cover_loss)
		
		# m2 to ha
		message("Converting from square meters to hectares...")
		aoi_gez_cover[2:4]<-aoi_gez_cover[2:4]*m2_ha
		message(paste0("Writing output to:",file.path(
		  proj_loc,"CSVs",paste0(
		    aoi_iso3,"_",provinces_ofinterest,
		    "_Primary tree cover disaggregated by GEZ_ha.csv"))))
		write.csv(aoi_gez_cover,file.path(
		  proj_loc,"CSVs",paste0(
		    aoi_iso3,"_",provinces_ofinterest,
		    "_Primary tree cover disaggregated by GEZ_ha.csv")))
		message("Primary tree cover results compiled.")
	  
	  Sys.sleep(0.1)
										
		message(paste0(
		  "Pre-processing for ",aoi_name,", ",provinces_ofinterest,
		  " complete!\n",length(aoi_provis$NAME_1) - i,
		  " provinces left to do."))
  },
  
  # Add error message to the error log file
  error = function(err.msg){
    write(toString(err.msg),
          file=file.path(
            proj_loc,"CSVs",paste0(
              i,"_",provinces_ofinterest,
              "_Error Report.csv")),append=TRUE)
  })
  }
  message("Woody cover calculations done!")
  
  # Check for error in csv
  ERRORS<-list.files(
    path=paste0(proj_loc,"/CSVs/"),
    pattern="*_Error Report.csv",
    full.names=FALSE)
  if(length(ERRORS)>0){
    message("There are errors for the following provinces/districts:")
    # Grab names
    for(x in ERRORS){
      x<-sub("_Error Report.csv.*","",x)
      x<-sub(".*_","",x)
      message(paste0("Error for : ",x))
      rm(x)
    }
  }else{
    if(Create_Maps_Tables==TRUE){
      message("fNRB Calculations")
      
      # Calculate Accessible Woody Cover tables
      options(scipen=999)
      calc_fNRB(proj_loc = proj_loc,
                usr_aoi_iso3 = usr_aoi_iso3)
      
      # Map making
      calc_maps(proj_loc = proj_loc,
                usr_aoi_iso3 = usr_aoi_iso3,
                custom_aoi = custom_aoi,
                aoi_window_unproj = aoi_window_unproj)
    }else{
      message("fNRB and map processing is switched off")
    }
  }
}

get_fNRB()
