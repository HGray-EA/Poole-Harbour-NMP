# Monitoring in Poole Harbour
library(sf)

# Load in WIMS for CAT
source("WIMS_Transform_Script.R")
source("Catch_Set_Up.R")

# Load your data
CDE <- read.csv("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/CEP/WFD_Wessex_2024.csv")

CDE %<>% 
  filter(Operational.Catchment == unique(CAT$OPCAT_NAME)) %>% 
  inner_join(CAT_geo, ., by = c("WB_ID" = "Water.Body.ID"))


# We display most recent 2 years after the last classifications. 
# We display the 3 years before then which aligns with the 2022 interim WFD classification.

WIMS_CAT_22 <- WIMS_CAT %>%  filter(Year >= 2022)             
WIMS_CAT_20 <- WIMS_CAT %>%  filter(Year >= 2019 & Year < 2022) 

#1 Wims
WIMS_P <- WIMS_CAT_22 %>% filter(
  determinand.definition == "Orthophosphate, reactive as P") %>% 
  group_by(sample.samplingPoint.label) %>% 
  mutate(Mean_P = round(mean(result),2),
         Mean_count = n()) %>% 
  ungroup()

#2 CDE Transforms
CDE_P <- CDE %>% filter(Classification.Item == "Phosphate" & Year == "2022")

CDE_P_2019 <- CDE %>% filter(Classification.Item == "Phosphate" & Year == "2019")

WIMS_am <- WIMS_CAT_22 %>% filter(
  determinand.definition== "Ammoniacal Nitrogen as N"
) %>% 
  group_by(sample.samplingPoint.label) %>% 
  mutate(Mean_A = round(mean(result),2),
         Mean_count = n())
#CDE Transforms
CDE_Am<- CDE %>% filter(Classification.Item == "Ammonia (Phys-Chemical)" & Year == "2022")

CDE_Am_2019 <- CDE %>% filter(Classification.Item == "Ammonia (Phys-Chemical)" & Year == "2019")

WIMS_DO <- WIMS_CAT_22 %>% filter(
  determinand.definition == "Oxygen, Dissolved, % Saturation") %>% 
  group_by(sample.samplingPoint.label) %>% 
  mutate(Mean_DO = round(mean(result),2),
         Mean_count = n()) %>% 
  ungroup()

CDE_DO <- CDE %>% filter(Classification.Item == "Dissolved oxygen" & Year == "2022")
CDE_DO_2019 <- CDE %>% filter(Classification.Item == "Dissolved oxygen" & Year == "2019")

WIMS_BOD <- WIMS_CAT_22 %>% filter(
  determinand.definition == "BOD : 5 Day ATU") %>% 
  group_by(sample.samplingPoint.label) %>% 
  mutate(BOD_90th = round(quantile(result, probs = 0.9, na.rm=TRUE),2),
         Quantile_count = n()) %>% 
  ungroup()

CDE_BOD <- CDE %>% filter(Classification.Item == "Biochemical Overall Oxygen Demand (BOD)" & Year == "2022")
CDE_BOD_2019 <- CDE %>% filter(Classification.Item == "Biochemical Overall Oxygen Demand (BOD)" & Year == "2019")

WIMS_NTU <- WIMS_CAT_22 %>% filter(
  determinand.definition == "Turbidity") %>% 
  group_by(sample.samplingPoint.label) %>% 
  mutate(Mean_NTU = round(mean(result),2),
         Mean_count = n()) %>% 
  ungroup()


Phys_Chem_Site <- readxl::read_excel("/dbfs/FileStore/WSX_HGray/2022_Phys_Chem_Classification_QA_document__RAW_.xlsx", sheet= "Site Class")

# Filter by Wessex area and convert to sf
Phys_Chem_Site <- Phys_Chem_Site %>% filter(SITE_EA_AREA == "Wessex") %>% 
  st_as_sf(coords=c("SITE_EASTING","SITE_NORTHING"), crs=27700) %>% 
  st_transform(4326)

Phys_Chem_Site <- Phys_Chem_Site[CAT,]
# Phosphate
Site_P <- Phys_Chem_Site %>% filter(Name == "Phosphate") 

# Ammonia
Site_Am <- Phys_Chem_Site %>% filter(Name== "Ammonia")

# DO
Site_DO <- Phys_Chem_Site %>% filter(Name=="DO")

# BOD
Site_BOD <- Phys_Chem_Site %>% filter(Name=="BOD")



### 

# Violin plot
v <- ggplot(WIMS_P, aes(x = WB_NAME, y = result)) +
  # geom_violin(aes(fill = WB_NAME), color = "black", alpha = 0.5) +  # Violin plot with some transparency
  geom_boxplot(width = 0.05, color = "black", aes(fill = WB_NAME), alpha = 0.7) +  # Boxplot on top
  geom_text(aes(x = WB_NAME, y = -Inf, label = paste("Count: ", Mean_count)), vjust = -0.5, hjust = 0.5, size = 4) +  # Add counts below the x-axis if available
  labs(title = "2022-2025 Phosphate Distribution by WB", x = "Waterbody", y = "Phosphate (mg/L)") +
  #scale_y_continuous(limits = c(0,20), breaks = c(0,0.1,0.2,0.3,0.5,1)) + 
  geom_hline(yintercept = 0.3, col = "red")+ # this is wfd poor
  geom_hline(yintercept = 0.1, col = "green")+# this is wfd good
  theme_minimal() +  # Use a minimal theme for a cleaner look
  theme(legend.position = "none", axis.text.x = element_text(angle=45, size=9)) +
  labs(caption = "WFD Phosphate Reactive P, good and poor indicated by horizontal lines")+
  coord_flip()

#a <- ggplot(WIMS_p, aes(x= WB_NAME, y=result, col=result))+
#  geom_point()+theme(axis.text.x = element_text(angle=90, size=5))+labs(y= "Orthophosphate (mg/L)", x= "Waterbodies")

# Add the caption manually using layout
plotly::ggplotly(v)  %>%
  plotly::layout(
    annotations = list(
      x = 0.5,  # Horizontal center
      y = -5,  # Position below the plot
      text = "WFD Phosphate Reactive P, good and poor indicated by horizontal lines",
      showarrow = FALSE,
      xref = "paper",
      yref = "paper",
      font = list(size = 12)  # Adjust the font size if needed
    )
  )


##### Monitoring points on a map 2022-2025


# We now have a dataset for hourly spills within 2022 which is linked to locations. 
library(leaflet.extras)

# Define log-scaled bins for numeric data
binz <-c(0,0.05,0.1,0.2,0.5,1,2,5,10,Inf)

# Create a color palette using colorBin for numeric data
pal_p <- colorBin(palette = "inferno", domain = WIMS_P$result, bins = binz, reverse=TRUE )


leaflet() %>%
  addProviderTiles(providers$Esri, group="Esri Basemap") %>% 
  addPolygons(data=CAT_Union,
              fillOpacity = 0.0001,
              fillColor = NA,
              color = "black",
              weight = 3,
              options = pathOptions(zIndex = 600)
  ) %>% 
  addPolygons(data=CAT,
              fillOpacity = 0.0001,
              fillColor = NA,
              color = "black",
              weight = 3,
              highlightOptions = highlightOptions(color = "white", weight = 4,
                                                  bringToFront = FALSE),
              options = pathOptions(zIndex = 600),
              group = "Waterbodies"
  ) %>% 
  
  addPolylines(data=DRN, col="#2832C2", weight=1, 
               opacity = 0.5,
               options = pathOptions(zIndex = 600)
  ) %>%
  addCircleMarkers(
    data=WIMS_P,
    radius = 10,
    color = "black",
    weight = 1,
    fillColor = ~pal_p(Mean_P),
    fillOpacity = 1,
    popup = ~paste0("Site: ", WIMS_P$sample.samplingPoint.label, "<br> Mean PO4-P (mg/L): ", WIMS_P$Mean_P, "<br> Sample Count: ", WIMS_P$Mean_count,
                    "<br> from/to: ",
                    min(WIMS_P$Date), "/", max(WIMS_P$Date)),
    options = pathOptions(zIndex = 999),
    group = "PO4-P" ) %>% 
  addLegend("bottomleft", 
            pal = pal_p, 
            values = binz, 
            title = "Mean PO4-P 2022-2025",
            group = "PO4-P") %>% 
  addScaleBar()  %>% 
  addLayersControl(overlayGroups = c(
    
    "Waterbodies"
  ),
  position="topright",
  options= layersControlOptions(collapsed=FALSE)) %>% 
  htmlwidgets::onRender(Layers_JS) %>% 
  hideGroup(c( 
    "Waterbodies"))
