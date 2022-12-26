#' @author Ruan Parrott & Nicholas Salonen
#' 
#' @param fun_parent_directory User specified location within which an "fNRB_analysis" directory is used or created if it doesn't already exist.
#' @param fun_aoi_name Country of interest name.
#' @param fun_aoi_iso3 The ISO3 character string for the country of interest.
#' @param fun_aoi_cont The continent of the country of interest.
#' @param fun_month The fun_month corresponding to the latest available protected areas data set.
#' @param fun_download Specify whether you need to fun_download data.

dir_fNRB = function(fun_parent_directory = parent_directory, 
                    fun_aoi_name = aoi_name,
                    fun_aoi_iso3 = aoi_iso3,
                    fun_aoi_cont = aoi_cont,
                    fun_provinces_ofinterest = provinces_ofinterest,
                    fun_month = month,
                    fun_download = download) {
  
  setwd(fun_parent_directory)
  
  if (grepl("*fNRB_analysis",fun_parent_directory)) {
    
    if (dir.exists(file.path(fun_parent_directory,fun_aoi_name))){
      proj_directory<-file.path(fun_parent_directory,fun_aoi_name) 
    } else {
      suppressWarnings(dir.create(file.path(fun_parent_directory,fun_aoi_name)))
      proj_directory<-file.path(fun_parent_directory,fun_aoi_name)
    }
    
    if (dir.exists(file.path(proj_directory,"Input_Data"))){
      in_loc<-file.path(proj_directory,"Input_Data")
    } else {
      suppressWarnings(dir.create(file.path(proj_directory,"Input_Data")))
      in_loc<-file.path(proj_directory,"Input_Data")
    }
    
    if (is.null(fun_provinces_ofinterest) == FALSE) {
      suppressWarnings(dir.create(file.path(proj_directory,
                           paste0(fun_aoi_iso3,"_",fun_provinces_ofinterest,"_output_",str_replace_all(
                             format(Sys.time(),"%d %b %Y %X"),"[:]","-")))))
      proj_loc<-file.path(proj_directory,
                            paste0(fun_aoi_iso3,"_",fun_provinces_ofinterest,"_output_",str_replace_all(
                              format(Sys.time(),"%d %b %Y %X"),"[:]","-"))) 
      suppressWarnings(dir.create(file.path(proj_loc,"CSVs")))
    } else {
      suppressWarnings(dir.create(file.path(proj_directory,
                           paste0(fun_aoi_iso3,"_output_",str_replace_all(format(Sys.time(),"%d %b %Y %X"),"[:]","-")))))
      proj_loc<-file.path(proj_directory,
                            paste0(fun_aoi_iso3,"_output_",str_replace_all(format(Sys.time(),"%d %b %Y %X"),"[:]","-")))
      suppressWarnings(dir.create(file.path(proj_loc,"CSVs")))
    }
    
  } else {
    suppressWarnings(dir.create(file.path(fun_parent_directory,"fNRB_analysis")))
    fun_parent_directory<-file.path(fun_parent_directory,"fNRB_analysis")
    suppressWarnings(dir.create(file.path(fun_parent_directory,fun_aoi_name)))
    proj_directory<-file.path(fun_parent_directory,fun_aoi_name)
    suppressWarnings(dir.create(file.path(proj_directory,"Input_Data")))
    in_loc<-file.path(proj_directory,"Input_Data")
    
    if (is.null(fun_provinces_ofinterest) == FALSE) {
      dir.create(file.path(proj_directory,
                           paste0(fun_aoi_iso3,"_",fun_provinces_ofinterest,"_output_",str_replace_all(format(Sys.time(),"%d %b %Y %X"),"[:]", "-"))))
      proj_loc<-file.path(proj_directory,
                            paste0(fun_aoi_iso3,"_",fun_provinces_ofinterest,"_output_",str_replace_all(format(Sys.time(),"%d %b %Y %X"),"[:]","-")))
      suppressWarnings(dir.create(file.path(proj_loc,"CSVs")))
    } else {
      dir.create(file.path(proj_directory,
                           paste0(fun_aoi_iso3,"_output_",str_replace_all(format(Sys.time(),"%d %b %Y %X"),"[:]","-"))))
      proj_loc<-file.path(proj_directory,
                            paste0(fun_aoi_iso3,"_output_",str_replace_all(format(Sys.time(),"%d %b %Y %X"),"[:]","-")))
      suppressWarnings(dir.create(file.path(proj_loc,"CSVs")))
    }
  }

  #Specify URL
  if (fun_download == TRUE) {
    
    #Specify URL
    fao_gez_url    <-"https://storage.googleapis.com/fao-maps-catalog-data/uuid/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b/resources/gez2010.zip"
    prim_forest_url<-paste0("https://glad.umd.edu/sites/default/files/",fun_aoi_cont,"_2001_primary.tif")
    wdpa_url       <-paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_",fun_month,"2022_Public_",fun_aoi_iso3,"_shp.zip")
    roads_url      <-paste0("https://biogeo.ucdavis.edu/data/diva/rds/",fun_aoi_iso3,"_rds.zip")
    in_water_url   <-paste0("https://biogeo.ucdavis.edu/data/diva/wat/",fun_aoi_iso3,"_wat.zip")
    SRTM_url       <-paste0("https://biogeo.ucdavis.edu/data/diva/alt/",fun_aoi_iso3,"_alt.zip")
    #GADM_url       <-paste0("https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_",fun_aoi_iso3,"_shp.zip")
    GADM_url       <-paste0("https://biogeo.ucdavis.edu/data/diva/adm/",fun_aoi_iso3,"_adm.zip")
    
    fun_download_urls<-c(fao_gez_url, 
                       prim_forest_url, 
                       wdpa_url, 
                       roads_url, 
                       in_water_url, 
                       SRTM_url, 
                       GADM_url)
    
    # fun_download files
    for (i in 1:length(fun_download_urls)) {
      file_name<-basename(httr::HEAD(fun_download_urls[i])$url)
      file_path<-file.path(in_loc,file_name)
      print(paste0("fun_downloading ",file_name," from ",fun_download_urls[i]))
      curl::curl_download(fun_download_urls[i],file_path,quiet=FALSE)  
        }
  }
  
  ##### Unzip & Load data ####
  # Unzipping
  message("Unzipping...")
  unzip(file.path(in_loc,"gez2010.zip"),overwrite=TRUE,exdir=file.path(in_loc,"gez2010"))
  #unzip(file.path(in_loc,paste0("gadm40_",fun_aoi_iso3,"_shp.zip")),overwrite=TRUE,exdir=file.path(in_loc,paste0(fun_aoi_iso3,"_adm")))
  unzip(file.path(in_loc,paste0(fun_aoi_iso3,"_adm.zip")),overwrite=TRUE,exdir=file.path(in_loc,paste0(fun_aoi_iso3,"_adm")))
  unzip(file.path(in_loc,paste0(fun_aoi_iso3,"_alt.zip")),overwrite=TRUE,exdir=file.path(in_loc,paste0(fun_aoi_iso3,"_alt")))
  unzip(file.path(in_loc,paste0(fun_aoi_iso3,"_rds.zip")),overwrite=TRUE,exdir=file.path(in_loc,paste0(fun_aoi_iso3,"_rds")))
  unzip(file.path(in_loc,paste0(fun_aoi_iso3,"_wat.zip")),overwrite=TRUE,exdir=file.path(in_loc,paste0(fun_aoi_iso3,"_wat")))
  aoi_PAname<-paste0("WDPA_WDOECM_",fun_month,"2022_Public_",fun_aoi_iso3,"_shp")
  unzip(file.path(in_loc,paste0(aoi_PAname,".zip")),exdir=file.path(in_loc,aoi_PAname))
  unzip(file.path(in_loc,aoi_PAname,paste0(aoi_PAname,"_0.zip")),exdir=file.path(in_loc,aoi_PAname,paste0(aoi_PAname,"_0")))
  unzip(file.path(in_loc,aoi_PAname,paste0(aoi_PAname,"_1.zip")),exdir=file.path(in_loc,aoi_PAname,paste0(aoi_PAname,"_1")))  
  unzip(file.path(in_loc,aoi_PAname,paste0(aoi_PAname,"_2.zip")),exdir=file.path(in_loc,aoi_PAname,paste0(aoi_PAname,"_2")))
return(proj_loc)
}
