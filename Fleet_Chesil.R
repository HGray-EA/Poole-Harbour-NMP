# Becky's Fleet & Chesil

library(sf)
library(leaflet)
library(tidyverse)

list.files("C:/Users/hg000051/Downloads/Poole_Harbour")

cf <- read_sf("C:/Users/hg000051/Downloads/Poole_Harbour/Chesil + Fleet Catchment/Chesil + Fleet Catchment/Chesil + The Fleet catchment.shp") %>% 
            st_transform(4326)
 c1 <-    read_sf("C:/Users/hg000051/Downloads/Poole_Harbour/Countryside_Stewardship_Agreement_Options_England historic/Countryside_Stewardship_Agreement_Options_England.gdb")%>% 
   st_transform(4326)
 c2 <- read_sf("C:/Users/hg000051/Downloads/Poole_Harbour/Countryside_Stewardship_Scheme_2016_Management_Areas_England/wr_England.gdb")%>% 
   st_transform(4326)
 c3 <- read_sf("C:/Users/hg000051/Downloads/Poole_Harbour/Environmental_Stewardship_Scheme_Options_England/Environmental_Stewardship_Scheme_Options_England.gdb")%>% 
   st_transform(4326)
 c4 <- read_sf("C:/Users/hg000051/Downloads/Poole_Harbour/Environmental_Stewardship_Scheme_Agreements_England/Environmental_Stewardship_Scheme_Agreements_England.gdb")%>% 
   st_transform(4326)
 
 DRN <- read_sf("C:/Users/hg000051/OneDrive - Defra/Projects/04_Misc_Data/WFD Data/Whole England/Hydrology/rivers_50k.shp") %>% 
   st_transform(4326)
 

 DRN <- DRN[cf,]
 
 leaflet() %>% 
  addProviderTiles(providers$Esri) %>% 
  addPolygons(data=cf,
              fill=NA,
              color = "black",
              weight = 1) %>% 

   addPolylines(data=DRN,
                col="blue",
                weight = 3) %>% 
   addPolygons(data=c4, 
               col="purple",
               weight = 1,
               popup = c4$nca)



plot(cf)
