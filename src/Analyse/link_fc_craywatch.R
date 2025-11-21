library(here)
library(dplyr)
library(tidyverse)
library(sf)
library(units)
library(mapview)
options(qgisprocess.path = "C:/Program Files/QGIS 3.40.4/bin/qgis_process-qgis-ltr.bat")
library(qgisprocess)
library(lwgeom)
library(readxl)


#===============================================================================
#laad kreeft data
data <- read.csv(here("data","analyse_dataset_stmer.csv"))%>%
  select(-c(geometry1,geometry2))%>%
  st_as_sf( coords = c("longitude", "latitude"),
              crs = 4326, 
              remove = FALSE)%>%
  st_transform(31370)%>%
  select(-c(VHAG,CATC,WVLC))

load(file=here("data","verwerkt","analyses_dataset_VHAG_WVLC_CATC_rivier_breedte.Rdata"))


# laad fc data (enkel fysico-chemie)
load(file=here("G:/Mijn Drive/NARA_VIS","data","VMM","verwerkt","fc_fc_data_breed.Rdata"))
load(file=here("data","verwerkt","fc_fc_data_breed_VHAG_WVLC_rivier_breedte.Rdata"))


# laad waterloop shapefile
waterloop <- read_sf(here("G:/Mijn Drive/NARA_VIS","data","VHA","VhaCattraj.shp"))%>%
  st_transform(31370)

# # laad waterloop segmenten shapefile
waterloopsegmenten <- read_sf(here("G:/Mijn Drive/NARA_VIS","data","VHA","VHA_waterlopen_VHA_waterloopsegment.shp"))%>%
  st_transform(31370)

# laad watervlakken shapefile
watervlakken <- read_sf(here("G:/Mijn Drive/NARA_VIS","data","watervlakken","watervlakken.shp"))%>%
  st_transform(31370)

# laad overstorten shapefile
overstorten <- read_sf(here("G:/Mijn Drive/NARA_VIS","data","overstorten","P_OS_uitlaat_VHA.shp"))%>%
  st_transform(31370)

# laad bekken shapefile
bekken <- read_sf(here("G:/Mijn Drive/NARA_VIS","data","shape_files","Wsbekken.shp"))%>%
  st_transform(31370)

# laad alle fc data
load("G:/Mijn Drive/NARA_VIS/data/VMM/verwerkt/fc_data_cleaner.rdata")
fc_data <- fc_data_clean
#laad watergang laag
watergang <- read_sf(here("data","GRB_Wtz_watergang.shp"))%>%
  st_transform(31370)

#-------------------------------------------------------------------------------
#link fc data met waterloop vhag en watervlakken

# vindt de dichtste waterlopen en watervlakken 
fc_loc <- fc_breed%>%
  select(sample_point,geometry, lambert_x,lambert_y)%>%
  distinct(sample_point,.keep_all = T)%>%
  mutate(# bepaal dichtstbijzijnste waterloopsegmenten
         nearest_river_index=st_nearest_feature(., waterloopsegmenten),
         # voeg VHAS toe
         VHAS=waterloopsegmenten$VHAS[nearest_river_index],
         # voeg VHAG toe
         VHAG=waterloopsegmenten$VHAG[nearest_river_index],
         # bepaal afstand tussen waterloopsegmenten en VMM punt
         distances_vhag=st_distance(., waterloopsegmenten[nearest_river_index, ], by_element = TRUE),
         # bepaal dichtstbijzijnste watervlak
         nearest_waterbody_index=st_nearest_feature(., watervlakken),
         # voeg WVLC toe
         WVLC=watervlakken$WVLC[nearest_waterbody_index],
         # bepaal afstand tussen watervlak en VMM punt
         distances_wv=st_distance(., watervlakken[nearest_waterbody_index, ], by_element = TRUE)
         )


# bepaal of watervlak of waterloopsegmenten het dichtstebij ligt

fc_loc <- fc_loc%>%
  mutate(VHASFinal=ifelse(distances_vhag<=distances_wv,VHAS,NA),
         VHAGFinal=ifelse(distances_vhag<=distances_wv,VHAG,NA),
         WVLCFinal=ifelse(distances_wv<distances_vhag,WVLC,NA),
         distances=ifelse(distances_vhag<=distances_wv,distances_vhag,distances_wv))

# check afstanden
summary(fc_loc$distances)

ggplot(fc_loc,aes(x=distances))+
  geom_histogram(bins=100)+xlim(0,20)


# check waterloopsegmentenpunt
Dist <- fc_loc%>%
  dplyr::filter(distances<10)%>%
  dplyr::filter(row_number()==1)

waterloopsegmenten_sub <- waterloopsegmenten%>%
  dplyr::filter(VHAG%in%Dist$VHAGFinal)

mapview(waterloopsegmenten_sub)+mapview(Dist,cex=2,col.region="red")

# check watervlakpunt
Dist <- fc_loc%>%
  dplyr::filter(distances<10)%>%
  dplyr::filter(row_number()==439)

watervlakken_sub <- watervlakken%>%
  dplyr::filter(WVLC%in%Dist$WVLCFinal)

mapview(watervlakken_sub)+mapview(watervlakken,col.regions="purple")+mapview(Dist,col.region="red")


# check afstanden <10 m
Dist <- fc_loc%>%
  dplyr::filter(distances<10)

waterloopsegmenten_sub <- waterloopsegmenten%>%
  dplyr::filter(VHAG%in%Dist$VHAGFinal)

watervlakken_sub <- watervlakken%>%
  dplyr::filter(WVLC%in%Dist$WVLCFinal)


mapview(waterloopsegmenten_sub)+mapview(watervlakken_sub)+mapview(Dist,cex=2,col.region="red")

# verwijder wanneer afstand >=10m bij smalle waterlopen en kleiner dan 25 bij breede
fc_loc_10 <- fc_loc%>%
  dplyr::filter(distances<=10)

# filter locaties met grotere afstanden
fc_loc_large<- fc_loc%>%
  dplyr::filter(distances>10)

# houd locaties bij die binnen een afstand van (de breedte van de rivier/2) + 10 m liggen
fc_loc_large <- fc_loc_large%>%
  distinct(sample_point,.keep_all = T)%>%
  st_join(watergang, join = st_nearest_feature, suffix = c("","wg"))%>%
  dplyr::filter(VHAGFinal==VHAGwg)%>%
  mutate(breedteschatting=OPPERVL/LENGTE)%>%
  arrange(desc(breedteschatting))%>%
  dplyr::filter(distances<=(breedteschatting/2+10))%>%
  dplyr::filter(!sample_point%in%c('OW990066','OW300100'))

# check de overgebleven punten (ik heb enkel de eerste 15 punten gescheckt, best de rest ook eens nakijken)
for(i in 1:15){print(mapview(fc_loc_large[i,])+mapview(waterloopsegmenten[waterloopsegmenten$VHAG==fc_loc_large$VHAG[i],]))}


#combineer de twee datasets
fc_loc <- fc_loc_10%>%
  bind_rows(fc_loc_large)


# save locatie file
save(fc_loc,file=here("data","verwerkt","fc_fc_data_breed_VHAG_WVLC_rivier_breedte.Rdata"))

#-------------------------------------------------------------------------------

# merge fc_loc met fc data
fc_breed <- fc_breed%>%
  left_join(select(fc_loc,sample_point,VHASFinal,VHAGFinal,WVLCFinal,distances)%>%
              st_drop_geometry())%>%
  rename(VHAG=VHAGFinal,
         VHAS=VHASFinal,
         WVLC=WVLCFinal)


#-------------------------------------------------------------------------------
# voeg VHAG, WVLC en CATC aan rivierkreeften data toe

# vindt de dichtste waterlopen en watervlakken 
data<- data%>%
  mutate(# bepaal dichtstbijzijnste waterloopsegmenten
    nearest_river_index=st_nearest_feature(., waterloopsegmenten),
    # voeg VHAS toe
    VHAS=waterloopsegmenten$VHAS[nearest_river_index],
    # voeg VHAG toe
    VHAG=waterloopsegmenten$VHAG[nearest_river_index],
    # bepaal afstand tussen waterloopsegmenten en VMM punt
    distances_vhag=st_distance(., waterloopsegmenten[nearest_river_index, ], by_element = TRUE),
    # bepaal dichtstbijzijnste watervlak
    nearest_waterbody_index=st_nearest_feature(., watervlakken),
    # voeg WVLC toe
    WVLC=watervlakken$WVLC[nearest_waterbody_index],
    # bepaal afstand tussen watervlak en VMM punt
    distances_wv=st_distance(., watervlakken[nearest_waterbody_index, ], by_element = TRUE),
    #voeg een entry ID toe
    ID=1:nrow(data)
  )

# bepaal of watervlak of waterloopsegmenten het dichtstebij ligt

data <- data%>%
  mutate(VHAS=ifelse(distances_vhag<=distances_wv,VHAS,NA),
         VHAG=ifelse(distances_vhag<=distances_wv,VHAG,NA),
         WVLC=ifelse(distances_wv<distances_vhag,WVLC,NA),
         distances=ifelse(distances_vhag<=distances_wv,distances_vhag,distances_wv))%>%
  select(-c(distances_vhag,distances_wv))%>%
  left_join(select(waterloop,VHAG,CATC)%>%st_drop_geometry(),by="VHAG")


# check afstanden
summary(data$distances)

ggplot(data,aes(x=distances))+
  geom_histogram(bins=100)+xlim(0,20)

# check waterloopsegmentenpunt
Dist <- data%>%
  dplyr::filter(distances<10)%>%
  dplyr::filter(row_number()==1)

waterloopsegmenten_sub <- waterloopsegmenten%>%
  dplyr::filter(VHAG%in%Dist$VHAG)

mapview(waterloopsegmenten_sub)+mapview(Dist,cex=2,col.region="red")

# check watervlakpunt
Dist <- data%>%
  dplyr::filter(distances<10)%>%
  dplyr::filter(row_number()==936)

watervlakken_sub <- watervlakken%>%
  dplyr::filter(WVLC%in%Dist$WVLC)

mapview(watervlakken_sub)+mapview(watervlakken,col.regions="purple")+mapview(Dist,col.region="red")


# check afstanden <10 m
Dist <- data%>%
  dplyr::filter(distances<10)

waterloopsegmenten_sub <- waterloopsegmenten%>%
  dplyr::filter(VHAG%in%Dist$VHAG)

watervlakken_sub <- watervlakken%>%
  dplyr::filter(WVLC%in%Dist$WVLC)


mapview(waterloopsegmenten_sub)+mapview(watervlakken_sub)+mapview(Dist,cex=2,col.region="red")

# waterloopsegmenten large distance
data_large <- data%>%
  dplyr::filter(distances>10&!is.na(VHAG))

# bepaal breedte schatting op basis van watergang en behoud de punten waarbij de distance kleiner is dan de breedte/2 + 10 m

data_large <- data_large%>%
  distinct(ID,.keep_all = T)%>%
  st_join(watergang, join = st_nearest_feature, suffix = c("","wg"))%>%
  dplyr::filter(VHAG==VHAGwg)%>%
  mutate(breedteschatting=OPPERVL/LENGTE)%>%
  arrange(desc(breedteschatting))%>%
  dplyr::filter(distances<=(breedteschatting/2+10))

# check de overgebleven punten (ik heb enkel de eerste 15 punten gescheckt, best de rest ook eens nakijken en verder filteren indien nodig)
for(i in 1:15){print(mapview(data_large[i,])+mapview(waterloopsegmenten[waterloopsegmenten$VHAG==data_large$VHAG[i],]))}

# verwijder wanneer afstand >10m
data <- data%>%
  dplyr::filter(distances<10)


# combineer datasets
data <- data%>%
  bind_rows(data_large)


# check of punt tegen het einde van een VHAG ligt


# creeer merged by VHAG waterloopsegmenten file
# snap polylines met behulp van threshold (threshold -> trial and error)
waterloop_merge <- qgis_run_algorithm(
  "grass:v.clean",
  type="line",
  input=waterloop,
  tool="snap",
  threshold=0.01
)

waterloop_merge <- sf::st_as_sf(waterloop_merge)

#na snap dissolve doen
waterloop_merge <- qgis_run_algorithm(
  "native:dissolve",
  INPUT=waterloop_merge,
  SEPARATE_DISJOINT=TRUE,
  FIELD="VHAG"
)

waterloop_merge <- sf::st_as_sf(waterloop_merge) 

# functie dat de afstand van het punt tot de eindpunten van de VHAG bepaald en als deze 
# kleiner is dan 2 km (meer dan 1 km om te corrigeren voor kronkelingen in de rivier)
# dan worden de VHAG's die geconnecteerd zijn met de VHAG van het punt binnen een
# straal van 1 km toegevoegd
dist_end_point <- function(x, data_point = data, polyline = waterloop_merge){
  # enkel uitvoeren als VHAG niet NA is
  if(!is.na(data_point$VHAG[x])){
    
    # maak data subsets
    data_sub <- data_point%>%
      dplyr::filter(row_number()==x)
    
    waterloop_sub <- polyline%>%
      dplyr::filter(VHAG==data_sub$VHAG)
    
    # vorm datasets om naar sfc format
    data_sub_sfc <- data_sub%>%
      st_as_sfc()
    
    waterloop_sub_sfc <- waterloop_sub%>%
      st_as_sfc()
    
    # bereken afstand van projectie tot eindpunt lijn (of lijn segmenten)
    measure_data <- st_line_project(waterloop_sub_sfc, data_sub_sfc)
    
    # bereken lengte lijn of lijn segmenten
    waterloop_length <- st_length(waterloop_sub_sfc)
    
    # verwijder segmenten en lijn lengten waar het punt niet op ligt
    waterloop_length <- waterloop_length[measure_data!=0]
    measure_data<- measure_data[measure_data!=0]
    
    # voeg afstanden tot begin en eindpunt samen
    measure_data_two_direct <- c(measure_data,as.numeric(waterloop_length)-measure_data)
    
    # als een afstand tot een eindpunt kleiner is dan 2 km bereken de 2de dichtste VHAG
    if(any(measure_data_two_direct<2000)){
      
      #segment buffer 2 km around point out of VHAG
      buffer_point <- qgis_run_algorithm(
        "native:buffer",
        INPUT=data_sub,
        DISTANCE=1000
      )
      buffer_point <- sf::st_as_sf(buffer_point) 
      
      waterloop_sub <- st_intersection(waterloop_sub,buffer_point)
      
      #add buffer of 2 m to make sure that connected VHAG's are picked up (in case of gaps)
      waterloop_sub <- qgis_run_algorithm(
        "native:buffer",
        INPUT=waterloop_sub,
        DISTANCE=2
      )
      waterloop_sub <- sf::st_as_sf(waterloop_sub) 
      
        #add VHAG that touches the endpoints
      waterloop_sub_extra <- qgis_run_algorithm(
        "native:joinattributesbylocation",
        INPUT=waterloop_sub,
        PREDICATE="cross",
        JOIN=waterloop,
        JOIN_FIELDS="VHAG"
      )
      
      waterloop_sub_extra <- sf::st_as_sf(waterloop_sub_extra) 
      waterloop_sub_extra <- waterloop%>%
        dplyr::filter(VHAG %in% waterloop_sub_extra$VHAG_2)
      
      data_buffer <- data_sub%>%
        st_join(.,select(waterloop_sub_extra,VHAG),join=st_is_within_distance,
                dist=1000, suffix=c("","_2"))
      

      # voeg VHAG toe
      VHAG=list(data_buffer$VHAG_2)
      
    }else{
      VHAG=NA
    }
    
  }else{
    VHAG=NA
  }  
  return(VHAG)
}


data <- data%>%
  mutate(VHAG2=map(1:nrow(.),dist_end_point))

# check extra waterloop
Dist <- data%>%
  dplyr::filter(!is.na(VHAG2))%>%
  distinct(VHAG,.keep_all = T)%>%
  dplyr::filter(row_number()==10)

waterloop_sub <- waterloop%>%
  dplyr::filter(VHAG%in%c(Dist$VHAG,unlist(Dist$VHAG2)))

mapview(waterloop_sub)+mapview(Dist%>%select(-VHAG2),cex=2,col.region="red")




# save kreeft data file
save(data,file=here("data","verwerkt","analyses_dataset_VHAG_WVLC_CATC_rivier_breedte.Rdata"))

#-------------------------------------------------------------------------------
# voeg chlorofyl a aan fc_breed toe
fc_breed <- fc_breed%>%
  full_join(fc_data%>%
              dplyr::filter(parameter_symbool=='Clfyl a')%>%
              select(-c(parameter_omschrijving,teken,eenheid,
                        resultaat,sample_point_omschrijving:gemeente))%>%
              pivot_wider(names_from = parameter_symbool,
                          values_from = resultaat_detectielimiet),
            by=c("sample_point"="sample_point",
                 "sample_datum_monstername"="sample_datum_monstername",
                 "sample_tijdstip_monstername"="sample_tijdstip_monstername"))

# selecteer fc variabelen
fc_breed_sub <- fc_breed%>%
  select(sample_point:geometry,'Cl-','N t',O2,BZV5,
         'EC 20','T',pH, Secchi, 'P t', 'Ca o',
         'Clfyl a', ZS, oPO4,WVLC,VHAG,distances)

#Cl- = Chloride (mg/l)
#N t = totaal stikstof (mg N/l)
#O2 = zuurstof (mg/l)
#BZV5 = biochemisch zuurstofverbruik na 5d (mg o2/l)
#EC 20 = geleidbaarheid bij 20°C (µS/cm)
#T = temperatuur (°C)
#pH
#Secchi =doorzichtigheid (cm)
#P t = Totaal fosfor (mg P/l)
#Ca o = Calcium opgelost (µg/l)
#Clfyl a = Chlorofyl a (µg/l)
#ZS = zwevende stoffen (mg/l)
#oPO4 = orthofosfaat (mg P/l)

# Voeg Jaar aan FC data toe en bereken zomergemiddelde
fc_breed_sub <- fc_breed_sub%>%
  mutate(Year=as.numeric(format(as.Date(sample_datum_monstername),"%Y")),
         MaandNr=as.numeric(format(as.Date(sample_datum_monstername),"%m")),
         entryID=1:nrow(fc_breed_sub))

#-------------------------------------------------------------------------------
#voeg Jaar groep variabele aan craywatch data toe

data <- data%>%
  mutate(yearGroup=year)

data <- data%>%
  bind_rows(data%>%
              mutate(yearGroup=yearGroup-1))%>%
  bind_rows(data%>%
              mutate(yearGroup=yearGroup-2))

#-------------------------------------------------------------------------------
# merge data op basis van WVLC (code moet nog verder uitgewerkt worden). Als er
# meerdere fc punten in een watervlak liggen wordt het zomer gemiddelde per 
#watervlak berekend

# calculate mean or concatinate
meancon <- function(x){
  if(is.numeric(x)){
    mean(x,na.rm=T)
  }else{
    paste(unique(x), collapse="; ")
  }
  
}

data_fc_wvlc_notAvg <- data%>%
  dplyr::filter(!is.na(WVLC))%>%
  inner_join(fc_breed_sub%>%
               dplyr::filter(!is.na(WVLC))%>%
              st_drop_geometry,by=c("WVLC"="WVLC","yearGroup"="Year"), suffix=c("","fc"))%>%
    mutate(distance_cray_FC=st_distance(., fc_breed_sub[entryID, ],
                                        by_element = TRUE),
           distance_cray_FC=as.numeric(distance_cray_FC))

  

data_fc_wvlc_notAvg_all <- data%>%
  dplyr::filter(!is.na(WVLC))%>%
  inner_join(fc_breed_sub%>%
               dplyr::filter(!is.na(WVLC))%>%
               st_drop_geometry,by=c("WVLC"="WVLC"), suffix=c("","fc"))%>%
  mutate(distance_cray_FC=st_distance(., fc_breed_sub[entryID, ],
                                      by_element = TRUE),
         distance_cray_FC=as.numeric(distance_cray_FC))



  
#-------------------------------------------------------------------------------
# data is gelinkt door voor elke variabele het dichtste bijzijnste punt binnen
# een straal van  500m op dezelfde waterloop te bepalen. De afstand tussen de 2 
# punten wordt ook berekend. 

# filter op data punten met VHAG
data_fc_vhag <- data%>%
  dplyr::filter(!is.na(VHAG))

# verwijder dubbele locaties uit fc dataset
fc_loc <- fc_breed_sub%>%
  select(sample_point, VHAG, geometry)%>%
  distinct(sample_point,.keep_all=T)%>%
  dplyr::filter(!is.na(VHAG))

#functie om dichtstbijzijnste waterloop punt te vinden
VHAG_nf <- function(x,data = data_fc_vhag,fc = fc_loc){
  
  fc_sub <- fc[fc$VHAG%in%c(data$VHAG[x],unlist(data$VHAG2[x])),]
  nearest_river_index <- st_nearest_feature(data[x,], fc_sub)
  distances <- st_distance(data[x,], fc_sub[nearest_river_index, ], by_element = TRUE)
  return(list(sample_point=fc_sub$sample_point[nearest_river_index],Distance=distances))
} 


# vindt dichtstbijzijnste waterloop
data_fc_vhag <- data_fc_vhag%>%
  mutate(nearestf=map(1:nrow(.),VHAG_nf),
         sample_point=map_chr(nearestf,1),
         distance_cray_FC=map_dbl(nearestf,2))

# filter op punten binnen de 500m (straal 1 km) 
data_fc_vhag <- data_fc_vhag%>%
  dplyr::filter(distance_cray_FC<1000)

# visualiseer locaties 

data_fc_vhag_sub <- data_fc_vhag%>%
  select(geometry)

fc_loc_sub <- fc_loc%>%
  dplyr::filter(sample_point %in% data_fc_vhag$sample_point)

mapview(waterloop)+mapview(data_fc_vhag_sub, col.regions="orange")+mapview(fc_loc_sub, col.regions="red")

#-------------------------------------------------------------------------------
# punten waarbij een overstort tussen het INBO en VMM meetpunt ligt worden
# verwijderd.

# link vhag aan overstorten via vhag_segm
overstorten <- overstorten%>%
  left_join(select(waterloopsegmenten,VHAS,VHAG)%>%
              st_drop_geometry(),by=join_by(vha_segm==VHAS))


# filter overstorten die op VHAG van data liggen
overstorten_data <- overstorten%>%
  dplyr::filter(VHAG %in% data_fc_vhag$VHAVispuntWaterloopGewestCode)
# 
# # functie ligt overstort tussen staalnamepunten en verwijder
# overstort_filter <- function(x, overstort=overstorten_data, data=data_fc_vhag, fc=fc_loc){
#   
#   # create data subsets (enkel overstorten die op dezelfde VHAG liggen)
#   overstort_sub <- overstort[overstort$VHAG%in%c(data$VHAG[x],unlist(data$VHAG2[x])) & !is.na(overstort$VHAG),]
#   data_sub <- do.call("rbind", replicate(nrow(overstort_sub), data[x,], simplify = FALSE))
#   
#   if(nrow(overstort_sub)>0){ # check of er een overstort op de waterloop ligt
#     
#     # bepaal afstanden tussen overstorten en data punt
#     distances <- st_distance(data_sub, overstort_sub, by_element = TRUE)
#     
#     # filter op overstorten die dichter bij liggen dan het VMM punt
#     overstort_sub <- overstort_sub[as.numeric(distances)<data$distance_cray_FC[x], ]
#     
#     
#     if(nrow(overstort_sub)>0){ # check of er overstorten dichter dan het VMM punt liggen
#       
#       # maak VMM subset data
#       fc_sub <- do.call("rbind", replicate(nrow(overstort_sub), fc[fc$sample_point==data$sample_point[x],], simplify = FALSE))
#       # bepaal afstand tussen overstort en VMM punt
#        distances2 <- st_distance(fc_sub, overstort_sub, by_element = TRUE)
#        
#        # als afstand oversort VMM < afstand datapunt VMM -> issue
#         if(any(as.numeric(distances2)<data$distance_cray_FC[x])){
#           
#           overstort_issue=T
#           
#         }else{overstort_issue=F}
#         
#     }else{overstort_issue=F}
#     
#   }else{overstort_issue=F}
#   
#   return(overstort_issue)
# }
# 
# data_fc_vhag <- data_fc_vhag%>%
#   mutate(overstort_issue=map(1:nrow(.),overstort_filter))
# 
# #visualiseer oversort issues
# waterloop_sub <- waterloop%>%
#   dplyr::filter(VHAG%in%data_fc_vhag$VHAG[data_fc_vhag$overstort_issue==T])
  # er zijn geen oversorten aanwezig tussen de kreeften en FC punten


# merge data_fc_vhag en fc_breed_sub
data_fc_vhag_notAvg <- data_fc_vhag%>%
  left_join(fc_breed_sub%>%
              dplyr::filter(!is.na(VHAG))%>%
              st_drop_geometry(),by=c("sample_point"="sample_point","yearGroup"="Year", "VHAG"="VHAG"),suffix=c("","fc"))


data_fc_vhag_notAvg_all <- data_fc_vhag%>%
  left_join(fc_breed_sub%>%
              dplyr::filter(!is.na(VHAG))%>%
              st_drop_geometry(),by=c("sample_point"="sample_point","VHAG"="VHAG"),suffix=c("","fc"))



 
#==============================================================================

# maak ruwe data file voor locaties
data_fc_cray_notAvg <- data_fc_wvlc_notAvg_all%>%
  bind_rows(data_fc_vhag_notAvg_all)%>%
  select(-c(nearest_waterbody_index,WVLCfc, entryID,
            distancesfc, VHAGfc))%>%
  rename(distances_cray_VHAG_WVLC=distances)%>%
  select(-c(nearestf,VHAG2))%>%
  dplyr::filter(!is.na(MaandNr))%>%
  st_drop_geometry()

write.table(data_fc_cray_notAvg,file=here("data","verwerkt","data_fc_cray_notAvg_rivierbreedte.txt"),sep="\t",row.names=F)

# filter op zomer maanden en jaren
data_fc_cray_notAvg_zomer <- data_fc_wvlc_notAvg%>%
  bind_rows(data_fc_vhag_notAvg)%>%
  select(-c(nearest_waterbody_index,WVLCfc,  entryID,
            distancesfc, VHAGfc))%>%
  rename(distances_cray_VHAG_WVLC=distances)%>%
  select(-c(nearestf,VHAG2))%>%
  dplyr::filter(!is.na(MaandNr))%>%
  dplyr::filter(MaandNr%in%5:10)%>%
  st_drop_geometry()

write.table(data_fc_cray_notAvg_zomer,file=here("data","verwerkt","data_fc_cray_notAvg_zomer_jaar_rivierbreedte.txt"),sep="\t",row.names=F)



# bereken mediaan mei:oktober metingen

  mediancon <- function(x){
    if(is.numeric(x)){
      median(x,na.rm=TRUE)
    }else{
      paste(unique(x), collapse="; ")
    }
    
  }



data_fc_cray <- data_fc_cray_notAvg_zomer%>%
  group_by(ID)%>%
  mutate(across(c(sample_point:oPO4,distance_cray_FC),~mediancon(.x)))%>%
  distinct(ID,.keep_all = T)%>%
  st_drop_geometry()%>%
  mutate_all(.,~ifelse(is.nan(.), NA, .))%>%
  select(-c(BZV5,`Clfyl a`,`Ca o`, oPO4))



write.table(data_fc_cray,file=here("data","verwerkt","data_fc_cray_rivierbreedte.txt"),sep="\t",row.names=F)




