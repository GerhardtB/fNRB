#' @author Ruan Parrott & Nicholas Salonen. Jurie Theron translated Raster functions to Terra function for version 4 of this script.
#' 
#' @param parent_directory User specified location within which an "fNRB_analysis" directory is used or created if it doesn't already exist.
#' @param crs_proj Projection specific to the location.
#' @param aoi_name Country of interest name.
#' @param aoi_iso3 The ISO3 character string for the country of interest.
#' @param aoi_cont The continent of the country of interest.
#' @param month The month corresponding to the latest available protected areas data set.
#' @param download Specify whether you need to download data.
#' 
#' @source Inland water - https://biogeo.ucdavis.edu/data/diva/wat/VNM_wat.zip
#' @source Road network - https://biogeo.ucdavis.edu/data/diva/rds/VNM_rds.zip
#' @source Elevation - https://biogeo.ucdavis.edu/data/diva/alt/VNM_alt.zip
#' @source Primary forest cover - https://glad.umd.edu/sites/default/files/Asia_2001_primary.tif
#' @source Protected Areas - https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Aug2021_Public_VNM_shp.zip
#' @source Global ecological zones - https://storage.googleapis.com/fao-maps-catalog-data/uuid/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b/resources/gez2010.zip

##### Packages and support functions #####
require("stringr")
library("terra")
library("tidyverse")
library("sf")
library("curl")

# User defined input arguments
usr_parent_directory    <-"/home/c4ubuntu/projDir/fNRB_C4"
fNRB_Functions          <-"/home/c4ubuntu/projDir/fNRB_C4/Functions_V4.0"
usr_crs_proj            <-"+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs +type=crs" # Define the CRS for the area
usr_aoi_name            <-"Togo" # Country
usr_aoi_iso3            <-"TGO" # Country Code
usr_aoi_cont            <-"Africa" # Continent
usr_provinces_ofinterest<-NULL
usr_month               <-"Oct"
data_download           <-FALSE

# Load functions
fNRBsuppRt_funcs = list.files(fNRB_Functions,pattern = ".R",
                              all.files = T, full.names = T)
for(i in 1:length(fNRBsuppRt_funcs)){
  source(fNRBsuppRt_funcs[i])
}


# Set up fNRB calculations
get_fNRB<-function (parent_directory = usr_parent_directory,
                    crs_proj = usr_crs_proj,
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
  
  parent_directory<-file.path(parent_directory, "fNRB_analysis")
  proj_directory<-file.path(parent_directory, aoi_name)
  in_loc<-file.path(proj_directory, "Input_Data")
  
  if (length(provinces_ofinterest) > 1 |
      is.null(provinces_ofinterest)) {
    csv_merge<-TRUE
  } else {
    csv_merge<-FALSE
  }
  
  # Loading shapefiles
  message("Loading and projecting shapefiles...")
  aoi_national<-st_read(file.path(in_loc,paste0(aoi_iso3, "_adm/", aoi_iso3, "_adm0.shp"))) %>%
    st_as_sf() %>%
    sf::st_make_valid() %>%
    st_transform(crs = crs_proj)
  # Use a unprojected admin 1 for clipping large raster and shp
  aoi_provis<-st_read(file.path(in_loc,paste0(aoi_iso3, "_adm/", aoi_iso3, "_adm1.shp"))) %>% 
    st_as_sf() %>%
    sf::st_make_valid()
  aoi_roads<-st_read(file.path(in_loc,paste0(aoi_iso3, "_rds/", aoi_iso3, "_roads.shp"))) %>%
    st_as_sf() %>%
    sf::st_make_valid() %>%
    st_transform(crs = crs_proj)
  aoi_PAname<-paste0("WDPA_WDOECM_", month,"2022_Public_", aoi_iso3, "_shp")
  aoi_PAs0<-st_read(file.path(in_loc,aoi_PAname,paste0(aoi_PAname, "_0")), 
                      layer = paste0(aoi_PAname, "-polygons")) %>%
    st_as_sf() %>%
    sf::st_make_valid() %>%
    st_transform(crs = crs_proj)
  aoi_PAs1<-st_read(file.path(in_loc, aoi_PAname, paste0(aoi_PAname, "_1")),
                      layer = paste0(aoi_PAname, "-polygons")) %>%
    st_as_sf() %>%
    sf::st_make_valid() %>%
    st_transform(crs = crs_proj)
  aoi_PAs2<-st_read(file.path(in_loc, aoi_PAname, paste0(aoi_PAname, "_2")),
                      layer = paste0(aoi_PAname, "-polygons")) %>%
    st_as_sf() %>%
    sf::st_make_valid() %>%
    st_transform(crs = crs_proj)
  aoi_PAs<-rbind(aoi_PAs0,aoi_PAs1,aoi_PAs2)
  rm(aoi_PAs0,aoi_PAs1,aoi_PAs2)
  
  if (download == T) {
    hh<-"Yes"
    if (hh == "No") {
      stop(paste0("Hansen woody file cover data not found in ", in_loc, "/Hansen"))
    }
  }
	
  if (is.null(provinces_ofinterest) == FALSE){
    aoi_provis<-aoi_provis %>%
      filter(NAME_1 %in% provinces_ofinterest)
    }

  for (i in 1:length(aoi_provis$NAME_1)){
    
  tryCatch({
		provinces_ofinterest<-gsub("[^[:alnum:]]", " ", as.character(aoi_provis$NAME_1[i]))
		message(paste0("Processing fNRB values for the ", provinces_ofinterest,
				" province.\nNumber ",i," of the ",length(aoi_provis$NAME_1), 
				" provinces to be processed in ",aoi_name))
		
		# Subset to provincial level
		message("Susetting to provincial level...")
		
		# Project
		aoi_window<-aoi_provis %>% 
		  filter(NAME_1 %in% NAME_1[i]) %>%
		  select('geometry') %>% 
		  st_as_sf() %>%
		  st_transform(crs = crs_proj)
		
		# Window for clipping first before projecting
		aoi_window_unproj<-aoi_provis %>% 
		  filter(NAME_1 %in% NAME_1[i]) %>%
		  select('geometry') %>% 
		  st_as_sf()
		
		# Crop roads
		aoi_poi_roads<-aoi_roads %>%
		  st_intersection(aoi_window)
		
		# Crop and load global GEZ
		message("Cropping and projecting GEZ...")
		aoi_fao_gez<-st_read(file.path(in_loc,"gez2010"),layer="gez_2010_wgs84") %>%
		  sf::st_make_valid() %>%
		  st_intersection(aoi_window_unproj) %>%
		  st_transform(crs=crs_proj) %>%
		  st_intersection(aoi_window) %>%
		  dplyr::mutate(gez_name=as.character(gez_name))
		if (st_geometry_type(aoi_fao_gez,by_geometry=FALSE) != "POLYGON" &
		   st_geometry_type(aoi_fao_gez,by_geometry=FALSE) != "MULTIPOLYGON") {
		   aoi_fao_gez<-st_collection_extract(aoi_fao_gez,type=c("POLYGON","POINT","LINESTRING"))
		   aoi_fao_gez<-aggregate(aoi_fao_gez,list(aoi_fao_gez$gez_code), function(x) x[1])
		}
    aoi_fao_gez<-aoi_fao_gez %>%
      dplyr::group_by(gez_name) %>%
      dplyr::summarise()
    
		# GLAD PRIMARY FOREST COVER 
		message("Cropping and projecting primary forest...")
		rst_template<-terra::rast(ext=ext(aoi_window),resolution=30,crs=crs(aoi_window))
		aoi_primary<-terra::rast(file.path(in_loc, paste0(aoi_cont, "_2001_primary.tif"))) %>%
		  terra::project(rst_template,method="near") %>%
		  terra::crop(aoi_window) %>%
		  terra::classify(cbind(NA,0))
		
		# Writing initial files
		message(paste0("Writing initial files to:", proj_loc))
		st_write(aoi_fao_gez,file.path(proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,"_fao_gez.shp")))
		
		# Buffer PAs
		# Create list for calculating remote and accessible areas
		aoi_PAs_union_list<-aoi_PAs %>%
		  st_intersection(aoi_window) %>%
		  st_buffer(dist=20) %>%
		  st_union()
		# Merge list to create sf extent for clipping and masking rasters
		aoi_PAs_union<-aoi_PAs %>%
		  st_intersection(aoi_window) %>%
		  st_buffer(dist=20) %>%
		  st_union() %>%
		  st_as_sf()
		st_write(aoi_PAs_union,file.path(proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,"_union_Protected Areas.shp")))
		
		message("Disaggregating remote and accessible areas...")
		aoi_remote<-disaggregate_access(aoi_border = aoi_window, 
										roads_projected = aoi_poi_roads,
										buffer_dist = 2500, 
										PAs_union = aoi_PAs_union_list,
										proj_crs = usr_crs_proj,
										aoi_prefix = paste0("/",aoi_iso3,"_",provinces_ofinterest,"_"),
										destination = proj_loc)	
		rm(aoi_PAs_union_list)
		
		# Loading Hansen data
		message("Cropping and projecting Hansen treecover...")
		aoi_treecov<-terra::rast(file.path(in_loc,paste0("Hansen/",aoi_name,"_treecov2000.tif"))) %>%
		  terra::crop(aoi_window_unproj,snap="out") %>%
		  terra::project(rst_template,method="bilinear") %>%
		  terra::crop(aoi_window,snap="out")
		aoi_treegain<-terra::rast(file.path(in_loc, paste0("Hansen/", aoi_name, "_gain.tif"))) %>%
		  terra::crop(aoi_window_unproj,snap="out") %>%
		  terra::project(rst_template,method="near") %>%
		  terra::crop(aoi_window,snap="out")
		aoi_treeloss<-terra::rast(file.path(in_loc, paste0("Hansen/", aoi_name, "_lossyear.tif"))) %>%
		  terra::crop(aoi_window_unproj,snap="out") %>%
		  terra::project(rst_template,method="near") %>%
		  terra::crop(aoi_window,snap="out")
		
		# Tree loss
		message("Processing treecover loss...")
		aoi_treeloss_processed<-process_coverchange(change_raster = aoi_treeloss,
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
		aoi_treegain_processed<-process_coverchange(change_raster = aoi_treegain,
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
		writeRaster(aoi_treeloss_total,file.path(proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,"_treeloss_total.tif")))
		writeRaster(aoi_treegain_total,file.path(proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,"_treegain_total.tif")))
		
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
		aoi_treecov2020_access<-terra::rast(file.path(
		  proj_loc,paste0(
		    aoi_iso3,ifelse(
		      is.null(provinces_ofinterest),
		      "",paste0("_",provinces_ofinterest)),
		    "_agg and reclass_2020cover_access.tif"))) %>%
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
		
		# 2020 cover
		message("2020 cover...")
		aoi_treecov2020_total<-terra::rast(file.path(
		  proj_loc,paste0(
		    aoi_iso3,ifelse(
		      is.null(provinces_ofinterest),
		      "",paste0("_",provinces_ofinterest)),
		    "_agg and reclass_2020cover_total.tif"))) %>% 
		  terra::classify(resclass_trim) %>% 
		  terra::crop(aoi_window)
		gc()
		
		# Calculate absolute loss
		message("Loss...")
		datastack<-sds(aoi_treecov2000_total,aoi_treecov2020_total)
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
		  aoi_2020_PAcov_m<-terra::mask(aoi_treecov2020_total,sp_aoi_PAs_union)
		  rm(sp_aoi_PAs_union)
		} else {
		  aoi_2020_PAcov_m<-aoi_treecov2020_total %>%
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
		aoi_treecov2020_access<-trim_rast(aoi_treecov2020_access)
		aoi_treecov2020_total<-trim_rast(aoi_treecov2020_total)
		aoi_treecov2000_total<-trim_rast(aoi_treecov2000_total)
		aoi_2020_PAcov_m<-trim_rast(aoi_2020_PAcov_m)
		aoi_treegain_absolute<-trim_rast(aoi_treegain_absolute,max=1)
		aoi_treeloss_absolute<-trim_rast(aoi_treeloss_absolute)
		
	  # Save to disk
		message("Writing cover rasters...")
		writeRaster(aoi_treecov2020_access,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_treecov_access_final.tif")))
		writeRaster(aoi_treecov2020_total,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_2020_treecov_final.tif")))
		writeRaster(aoi_treecov2000_total,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_2000_treecov_final.tif")))
		writeRaster(aoi_2020_PAcov_m,
		            file.path(
		              proj_loc,paste0(
		                aoi_iso3,"_",provinces_ofinterest,
		                "_2020_PAcov_final.tif")))
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
		message("Writing total treecover 2020 per GEZ...")
		aoi_gez_cover_total<-disaggregate_gez(aoi_treecov2020_total,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2020 Total woody cover")
		message("Writing total treecover 2000 per GEZ...")
		aoi_gez_cover_tot200<-disaggregate_gez(aoi_treecov2000_total,poly_gez=aoi_fao_gez,scaling=1/100) %>% 
		  dplyr::mutate(Variable="2000 Total woody cover")
		message("Writing 2020 accessible treecover per GEZ...")
		aoi_gez_cover_access<-disaggregate_gez(aoi_treecov2020_access,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2020 Accessible wood cover")
		message("Writing 2020 protected treecover per GEZ...")
		aoi_gez_cover_protected<-disaggregate_gez(aoi_2020_PAcov_m,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2020 Protected wood cover")
		message("Writing 2000-2012 treecover gain per GEZ...")
		aoi_gez_cover_gain<-disaggregate_gez(aoi_treegain_absolute,poly_gez=aoi_fao_gez) %>%
		  dplyr::mutate(Variable="2000-2012 Woody cover gain")
		message("Writing 2000-2020 treecover loss per GEZ...")
		aoi_gez_cover_loss<-disaggregate_gez(aoi_treeloss_absolute,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2000-2020 Woody cover loss")
	  
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
		
		# Uh?
		aoi_treecov2020_access<-aoi_treecov2020_access*aoi_primary_reclass
		aoi_treecov2020_total<-aoi_treecov2020_total*aoi_primary_reclass
		aoi_treecov2000_total<-aoi_treecov2000_total*aoi_primary_reclass
		aoi_2020_PAcov_m<-aoi_2020_PAcov_m*aoi_primary_reclass
		aoi_treegain_absolute<-aoi_treegain_absolute*aoi_primary_reclass
		aoi_treeloss_absolute<-aoi_treeloss_absolute*aoi_primary_reclass
		writeRaster(aoi_primary_reclass,file.path(
		  proj_loc,paste0(aoi_iso3,"_",provinces_ofinterest,
		                   "_Primary Forest Extent.tif")))
		rm(aoi_primary_reclass)
		message("Compiling final primary cover results...")
		
		# Primary tree cover per GEZ
		message("Writing total primary treecover 2020...")
		aoi_gez_cover_total<-disaggregate_gez(aoi_treecov2020_total,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2020 Total woody cover")
		message("Writing total primary treecover 2000...")
		aoi_gez_cover_tot200<-disaggregate_gez(aoi_treecov2000_total,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2000 Total woody cover")
		message("Writing 2020 accessible primary treecover...")
		aoi_gez_cover_access<-disaggregate_gez(aoi_treecov2020_access,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2020 Accessible wood cover")
		message("Writing 2020 protected primary treecover...")
		aoi_gez_cover_protected<-disaggregate_gez(aoi_2020_PAcov_m,poly_gez=aoi_fao_gez,scaling=1/100) %>%
		  dplyr::mutate(Variable="2020 Protected wood cover")
		message("Writing 2000-2012 primary treecover gain...")
		aoi_gez_cover_gain<-disaggregate_gez(aoi_treegain_absolute,poly_gez=aoi_fao_gez) %>%
		  dplyr::mutate(Variable="2000-2012 Woody cover gain")
		message("Writing 2000-2020 primary treecover loss...")
		aoi_gez_cover_loss<-disaggregate_gez(aoi_treeloss_absolute,poly_gez=aoi_fao_gez,scaling=1/100) %>% 
		  dplyr::mutate(Variable="2000-2020 Woody cover loss")
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
		  " complete!\n", length(aoi_provis$NAME_1) - i," provinces left to do."))
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
  
	# Combine CSV 
	if (isTRUE(csv_merge)) {
	  message("Merging provincial cover data...")
  	temp_prim<-list.files(path=file.path(proj_loc,"CSVs"),pattern="*_Prim")
  	temp_cov<-list.files(path=file.path(proj_loc,"CSVs"),pattern="*_Tree")
  	setwd(file.path(proj_loc,"CSVs"))
  	prim<-lapply(temp_prim,read.csv)
  	cov<-lapply(temp_cov,read.csv)
  	
  	#
  	prim_bind<-do.call(rbind,prim)
  	prim_bind_ag<-prim_bind %>%
  	  group_by(gez_name,Variable) %>%
  	  summarise(Cover_ha=sum(Cover_ha),
  	            Area_ha=sum(Area_ha),
  	            Total_ha=sum(Total_ha))
  	
  	#
  	cov_bind<-do.call(rbind,cov)
  	cov_bind_ag<-cov_bind %>%
  	  group_by(gez_name,Variable) %>%
  	  summarise(Cover_ha=sum(Cover_ha),
  	            Area_ha=sum(Area_ha),
  	            Total_ha=sum(Total_ha))
  	
  	# Save to disk
  	write.csv(cov_bind_ag,file.path(proj_loc,paste0(aoi_iso3,"_Tree cover disaggregated by GEZ_ha.csv")))
	  write.csv(prim_bind_ag,file.path(proj_loc,paste0(aoi_iso3,"_Primary tree cover disaggregated by GEZ_ha.csv")))
	}
  message("Job done!")
}

get_fNRB()
