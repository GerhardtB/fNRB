#' Creates fNRB tables
#' 
#' @author Jurie Theron
#' 
#' @param proj_loc Location of project directory, to save results
#' @param usr_aoi_iso3 ISO 3 code of the country of interest

calc_fNRB = function(proj_loc = proj_loc,
                     usr_aoi_iso3 = usr_aoi_iso3) {
  
  suppressPackageStartupMessages(require(sf))
  suppressPackageStartupMessages(require(dplyr))
  
  # Load Tree cover
  tree_cover<-lapply(
    list.files(
      path=paste0(proj_loc,"/CSVs/"),
      pattern=glob2rx("*Tree*GEZ_ha.csv*"),
      full.names=TRUE),
    read.csv)
  
  # Merge and sum per GEZ
  tree_cover<-do.call(rbind,tree_cover) %>%
    dplyr::select(c("gez_name","Cover_ha","Variable")) %>% 
    dplyr::group_by(gez_name,Variable) %>% 
    dplyr::summarise(Cover_ha_sum=sum(Cover_ha,na.rm=TRUE)) %>%
    dplyr::arrange(gez_name) %>%
    as.data.frame()
  
  # Create Total Woody Cover table
  Colnames<-c("Global Ecological Zone",
              unique(tree_cover$Variable))
  tree_cover<-tree_cover %>%
    pivot_wider(names_from=Variable,
                values_from=Cover_ha_sum) %>%
    as.data.frame()
  colnames(tree_cover)<-Colnames
  tree_cover<-tree_cover %>%
    dplyr::mutate(
      # Total 2021 - Access 2021 - Protected 2021
      Remote_Unprotected=tree_cover[,7]-tree_cover[,5]-tree_cover[,6])
  tree_cover_Totals<-tree_cover %>%
    dplyr::select(-c("Global Ecological Zone")) %>%
    summarise(across(everything(),~sum(.,is.na(.),0)))
  tree_cover_Totals$`Global Ecological Zone`<-"Total"
  tree_cover<-as.data.frame(rbind(tree_cover,tree_cover_Totals))
  rm(tree_cover_Totals)
  
  # Save
  if (file.exists(paste0(proj_loc,"/Products/"))) {
    cat("")
  } else {
    dir.create(paste0(proj_loc,"/Products/"))
  }
  if (file.exists(paste0(proj_loc,"/Products/Tables/"))) {
    cat("")
  } else {
    dir.create(paste0(proj_loc,"/Products/Tables/"))
  }
  write.csv(
    tree_cover,row.names=FALSE,
    paste0(proj_loc,"/Products/Tables/",usr_aoi_iso3,
           "_Accessible_Woody_Cover_Total_Woody_Cover.csv"))
  
  # Load Primary tree cover
  primary_cover<-lapply(
    list.files(
      path=paste0(proj_loc,"/CSVs/"),
      pattern=glob2rx("*Primary*GEZ_ha.csv*"),
      full.names=TRUE),
    read.csv)
  
  # Merge and sum per GEZ
  primary_cover<-do.call(rbind,primary_cover) %>%
    dplyr::select(c("gez_name","Cover_ha","Variable")) %>% 
    group_by(gez_name,Variable) %>% 
    summarise(Cover_ha_sum=sum(Cover_ha,na.rm=TRUE)) %>%
    dplyr::arrange(gez_name) %>%
    as.data.frame()
  
  # Create Primary Forest Cover table
  Colnames<-c("Global Ecological Zone",
              unique(primary_cover$Variable))
  primary_cover<-primary_cover %>%
    pivot_wider(names_from=Variable,
                values_from=Cover_ha_sum) %>%
    as.data.frame()
  colnames(primary_cover)<-Colnames
  primary_cover<-primary_cover %>%
    dplyr::mutate(
      # Total 2021 - Access 2021 - Protected 2021
      Remote_Unprotected=primary_cover[,7]-primary_cover[,5]-primary_cover[,6])
  primary_cover_Totals<-primary_cover %>%
    dplyr::select(-c("Global Ecological Zone")) %>%
    summarise(across(everything(),~sum(.,is.na(.),0)))
  primary_cover_Totals$`Global Ecological Zone`<-"Total"
  primary_cover<-as.data.frame(rbind(primary_cover,primary_cover_Totals))
  rm(primary_cover_Totals)
  
  # Save
  write.csv(
    primary_cover,row.names=FALSE,
    paste0(proj_loc,"/Products/Tables/",usr_aoi_iso3,
           "_Accessible_Woody_Cover_Primary_Forest_Cover.csv"))
  
  # Create MAI table
  Colnames<-c("Global Ecological Zone",
              "Total forest extent (ha)",
              "Primary forest (ha)",
              "12 yr forest gain (ha)",
              "Extrapolated 20 yr forest gain (ha)",
              "Total forest extent <20 yr (%)",
              "Total primary forest extent (%)",
              "Total forest extent >20 yr (%)",
              "Growth rate primary forest (t/ha/yr)",
              "Growth rate <20 yr forest (t/ha/yr)",
              "Growth rate >20 yr forest (t/ha/yr)",
              "Age-weighted growth rate (t/ha/yr)")
  mai<-tree_cover[,c(1,7)] %>%
    dplyr::mutate(Primary_Forest=primary_cover[,7]) %>%
    dplyr::mutate(Forest_Gain=tree_cover[,3]) %>%
    dplyr::slice_head(n=-1)
  mai$ExtrpoGain<-(mai[,4]/12)*20
  mai$TotExtLess<-mai[,5]/mai[,2]
  mai$TotPrimExt<-mai[,3]/mai[,2]
  mai$TotExtMore<-1-mai[,6]-mai[,7]
  
  # Grab growth rate from database
  grab_gez<-mai$`Global Ecological Zone`
  if(usr_aoi_cont=="SouthAmerica"){
    usr_aoi_cont<-"America"
  }else if(usr_aoi_cont=="Madagascar"){
    usr_aoi_cont<-"Africa"
  }else{
    usr_aoi_cont<-usr_aoi_cont
  }
  
  
  
  
  # IF iso3 = isular asia, convert Asia to Asia insular
  
  
  
  
  db<-read.csv(paste0(usr_parent_directory,"Data/CSV_Databases/Net_Biomass_Growth_Forests.csv"))
  db<-db %>%
    dplyr::filter(Continent==usr_aoi_cont) %>%
    dplyr::filter(GEZ_Equivalent %in% grab_gez) %>%
    dplyr::select(c("GEZ_Equivalent",
                    "Status_Condition",
                    "Aboveground_Biomass_growth")) %>%
    dplyr::arrange(GEZ_Equivalent) %>%
    as.data.frame()
  db<-db %>%
    pivot_wider(names_from=Status_Condition,
                values_from=Aboveground_Biomass_growth) %>%
    as.data.frame()
  mai<-as.data.frame(cbind(mai,db[,2:4]))
  
  # Calculate weighted growth rate
  mai<-mai %>%
    dplyr::mutate(Weighted=mai[,6]*mai[,11]+mai[,8]*mai[,10]+mai[,7]*mai[,9])
  colnames(mai)<-Colnames
  rm(Colnames,db,grab_gez)
  
  # Save
  write.csv(
    mai,row.names=FALSE,
    paste0(proj_loc,"/Products/Tables/",usr_aoi_iso3,
           "_MAI.csv"))
  
  # Create Total Consumption table
  # Load population data
  pop_new<-read.csv(paste0(usr_parent_directory,"Data/CSV_Databases/Population.csv")) %>%
    dplyr::filter(Country.Code==usr_aoi_iso3) %>%
    dplyr::select(tail(names(.),1)) %>% # Grab last columns
    as.numeric()
  
  # Load consumption values
  consumption<-read.csv(paste0(usr_parent_directory,"Data/CSV_Databases/Consumption_Rate.csv")) %>%
    dplyr::filter(ISO3==usr_aoi_iso3) %>%
    dplyr::select(c("Type","X2019_Capital"))
  consumption<-consumption %>%
    dplyr::mutate(unite_per_capita=X2019_Capital/1000)
  Type<-consumption$Type
  
  # Check if there are 0's
  consumption<-as.list(as.data.frame(t(consumption$unite_per_capita)))
  consumption_updated<-list()
  for(i in 1:6){
    con<-consumption[[i]]
    if(con==0){
      pop_2019<-read.csv(paste0(usr_parent_directory,"Data/CSV_Databases/Population.csv")) %>%
        dplyr::filter(Country.Code==usr_aoi_iso3) %>%
        dplyr::select(c("X2019"))
      consump_2019<-read.csv(paste0(usr_parent_directory,"Data/CSV_Databases/Consumption_Rate.csv")) %>%
        dplyr::filter(ISO3==usr_aoi_iso3) %>%
        dplyr::select(c("X2019_Consumption"))
      consump_2019<-as.numeric(consump_2019[i,])
      con<-consump_2019/pop_2019 %>%
        as.numeric()
      rm(pop_2019,consump_2019)
    }else{
      con<-con
    }
    consumption_updated[[i]]<-con
  }
  consumption_updated<-do.call(rbind,consumption_updated)
  consumption_updated<-data.frame(Type=Type,
                                  X2019_Capital=consumption_updated)
  rm(Type,con,i,consumption)
  
  # Create consumption table
  Variable<-c("Domestic fuelwood consumption","Wood fuel",
              "Non-domestic (energy) wood consumption",
              "Wood charcoal","Non-domestic (non-energy) wood consumption",
              "Industrial roundwood","Sawnwood","Veneer sheets",
              "Wood-based panels","Wood density conversion factor",
              "Biomasss conversion and expansion factor",
              "Charcoal to wood biomass factor",
              "Total woody biomass consumption")
  Unit<-c("t/yr","m3/yr","t/yr","t/yr","t/yr",
          "m3/yr","m3/yr","m3/yr","m3/yr",
          "t/m3","t/m3","-","t/yr")
  x10<-0.725
  x11<-read.csv(paste0(usr_parent_directory,"Data/CSV_Databases/BCEF.csv")) %>%
    dplyr::filter(ISO_3==usr_aoi_iso3) %>%
    dplyr::select("BCEF") %>%
    as.numeric()
  x12<-4
  x2<-consumption_updated %>%
    dplyr::filter(Type=="Wood fuel") %>%
    dplyr::select("X2019_Capital") %>%
    as.numeric()
  x2<-x2*pop_new
  x1<-x2*x10
  x4<-consumption_updated %>%
    dplyr::filter(Type=="Wood charcoal") %>%
    dplyr::select("X2019_Capital") %>%
    as.numeric()
  x4<-pop_new*x4
  x3<-x4*x12
  x6<-consumption_updated %>%
    dplyr::filter(Type=="Industrial roundwood") %>%
    dplyr::select("X2019_Capital") %>%
    as.numeric()
  x6<-pop_new*x6
  x7<-consumption_updated %>%
    dplyr::filter(Type=="Sawnwood") %>%
    dplyr::select("X2019_Capital") %>%
    as.numeric()
  x7<-pop_new*x7
  x8<-consumption_updated %>%
    dplyr::filter(Type=="Veneer sheets") %>%
    dplyr::select("X2019_Capital") %>%
    as.numeric()
  x8<-pop_new*x8
  x9<-consumption_updated %>%
    dplyr::filter(Type=="Wood-based panels") %>%
    dplyr::select("X2019_Capital") %>%
    as.numeric()
  x9<-pop_new*x9
  x5<-(x6+x7+x8+x9)*x11
  x13<-x1+x3+x5
  Values<-c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
  rm(x1,x2,x3,x4,x5,x6,x7, x8,x9,x10,x11,x12,x13)
  Consumption_table<-data.frame(
    Variable=Variable,Unit=Unit,Value=Values)
  rm(Variable,Unit,Values,consumption_updated,pop_new)
  
  # Save
  write.csv(
    Consumption_table,row.names=FALSE,
    paste0(proj_loc,"/Products/Tables/",usr_aoi_iso3,
           "_Total_Consumption.csv"))
  
  # Create fNRB table
  # Summary table
  Colnames<-c("FAO Global Ecological Zone","Total forest cover (ha)",
              "Protected area cover (ha)","Remote area cover (ha)",
              "MAI (t/ha/yr)","Annual growth (t/yr)")
  summary<-as.data.frame(
    cbind(tree_cover[,1],tree_cover[,7],
          tree_cover[,6],tree_cover[,8])) %>%
    dplyr::slice_head(n=-1) %>%
    dplyr::mutate(V5=mai[,12])
  summary$V2<-as.numeric(summary$V2)
  summary$V3<-as.numeric(summary$V3)
  summary$V4<-as.numeric(summary$V4)
  summary$V5<-as.numeric(summary$V5)
  summary$V6<-summary$V5*(summary$V2-summary$V3-summary$V4)
  colnames(summary)<-Colnames
  rm(Colnames)
  
  # Calculate total
  sum<-summary[,2:6] %>%
    summarise(
      across(everything(),~sum(.,is.na(.),0))) %>%
    as.numeric()
  sum<-c("Total",sum)
  summary<-as.data.frame(rbind(summary,sum))
  rm(sum)
  summary$`Total forest cover (ha)`<-as.numeric(summary$`Total forest cover (ha)`)
  summary$`Protected area cover (ha)`<-as.numeric(summary$`Protected area cover (ha)`)
  summary$`Remote area cover (ha)`<-as.numeric(summary$`Remote area cover (ha)`)
  summary$`MAI (t/ha/yr)`<-as.numeric(summary$`MAI (t/ha/yr)`)
  summary$`Annual growth (t/yr)`<-as.numeric(summary$`Annual growth (t/yr)`)
  
  # Save
  write.csv(
    summary,row.names=FALSE,
    paste0(proj_loc,"/Products/Tables/",usr_aoi_iso3,
           "_fnrb_summary.csv"))
  
  # fNRB table
  Variables<-c("Total woody biomass consumption (H)",
               "Renewable biomass (RB)",
               "Non-renewable biomass (NRB)","fNRB")
  Units<-c("t/yr","t/yr","t/yr","-")
  x1<-Consumption_table %>%
    dplyr::select("Value") %>%
    dplyr::slice_tail() %>%
    as.numeric()
  x2<-summary %>%
    dplyr::slice_tail() %>%
    dplyr::select(6) %>%
    as.numeric()
  x3<-x1-x2
  x4<-x3/(x2+x3)
  Values<-c(x1,x2,x3,x4)
  rm(x1,x2,x3,x4)
  fNRB<-data.frame(Variable=Variables,
                   Unit=Units,
                   Value=Values)
  rm(Variables,Units,Values)
  
  # Save
  write.csv(
    fNRB,row.names=FALSE,
    paste0(proj_loc,"/Products/Tables/",usr_aoi_iso3,
           "_fNRB_calculations.csv"))
  message("fNRB calcuations done!")
  rm(Consumption_table,fNRB,mai,primary_cover,summary,tree_cover)
  gc()
}
