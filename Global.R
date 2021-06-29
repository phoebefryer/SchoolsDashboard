library(classInt); library(viridis);library(leaflet); library (dplyr); library(tidyverse) ; library(sf) ; 
library(shiny); library(htmlwidgets); library(readxl); library(fingertipsR); library(leaflet.extras); library(httr);
library(jsonlite); library(rjson); library(rgdal);library(geojsonio); library(mapview); library(leafem); library(ukpolice);
library(nomisr); library(shinydashboard);library(shinyWidgets);library(tidyverse);library(sf);library(ggiraph);
library(scales); library(htmltools);library(htmlwidgets);library(nomisr);library(readxl);
library(mapview);library(fresh); library(leaflet.extras); library(rsconnect); library(shinyscreenshot);
library(dashboardthemes); 


# Boundaries --------------------------------------------------------------

lads <- st_read("data/GM_LADS.geojson")
gm_la <- st_read("data/GMLAs.shp")
msoa <- st_read("data/GM_MSOA.geojson")
lsoa <- st_read("data/GM_LSOA.geojson")
constituency <- st_read("data/GM_Constituency.geojson")

lookup <- read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>%
  filter (LAD17NM %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  pull(LSOA11CD)

lookupname <- read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>%
  filter (LAD17NM %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  pull(LAD17NM)

lookupmsoa <- read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>%
  filter (LAD17NM %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  pull(MSOA11NM) 

# Datasets ----------------------------------------------------------------

# IMD
imd <- read.csv("data/imd.csv") %>% 
  mutate(decile = factor(decile, levels = c(1:10), ordered = TRUE))

imd <- imd %>%
  filter(lsoa11cd %in% lookup) %>%
  mutate(decile = as.numeric(decile)) 

imd <- left_join(lsoa, imd, by = "lsoa11cd")

idaci <- imd %>%
  filter(index_domain =="Income Deprivation Affecting Children")

housing <- imd %>%
  filter(index_domain == "Barriers to Housing and Services")

crime <- imd %>% 
  filter(index_domain == "Crime")

education <- imd %>%
  filter(index_domain == "Education, Skills and Training")

employment <- imd %>%
  filter(index_domain == "Employment")

idaoa <- imd %>%
  filter(index_domain == "Income Deprivation Affecting Older People")

imd19 <- imd %>%
  filter(index_domain =="Index of Multiple Deprivation")

environ <- imd %>%
  filter(index_domain == "Living Environment")

#Active Lives
CYPAL <- read_excel("data/Active Lives Template v20.xlsx", 
                    sheet = "Full CYP Active Lives")

CYPAL <- left_join(lads, CYPAL, by = c("lad17nm" = "Area"))

CYPAL <- CYPAL %>%
  mutate (Active = Active*100) %>% mutate(`Fairly Active`= `Fairly Active`*100)%>%mutate(`Less Active`=`Less Active`*100)%>%
  mutate(`Active 60 minutes every day`=`Active 60 minutes every day`*100)%>%mutate(`Active 30 minutes+ at and outside school`=`Active 30 minutes+ at and outside school`*100)%>%
  mutate(`Active 30+ minutes outside school`=`Active 30+ minutes outside school`*100)%>%mutate(`Active 30+ minutes at school`=`Active 30+ minutes at school`*100)%>%mutate(Moving=Moving*100)

CYPAL <- CYPAL %>%
  mutate (Active = as.numeric(Active)) %>% mutate(`Fairly Active`= as.numeric(`Fairly Active`))%>%mutate(`Less Active`=as.numeric(`Less Active`))%>%
  mutate(`Active 60 minutes every day`= as.numeric(`Active 60 minutes every day`))%>%mutate(`Active 30 minutes+ at and outside school`=as.numeric(`Active 30 minutes+ at and outside school`))%>%
  mutate(`Active 30+ minutes outside school`=as.numeric(`Active 30+ minutes outside school`))%>%mutate(`Active 30+ minutes at school`=as.numeric(`Active 30+ minutes at school`))%>%mutate(Moving=as.numeric(Moving))

cypal1718 <- CYPAL %>%
  filter(Release == "2017-18")

cypal1819 <- CYPAL %>%
  filter (Release == "2018-19")

cypal1920 <- CYPAL %>%
  filter (Release == "2019-20")

bins <- c(25, 30, 35, 40, 45, 50)
col <- colorBin(c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#BA1084", "#8C1D82"), domain = cypal1718$Active, bins = bins)

#NCMP Reception: Prevalence of obesity (including severe obesity)
NCMPRec <- read.csv("data/NCMPRec.csv")
NCMPRec <- left_join(lads, NCMPRec, by = c("lad17nm" = "AreaName"))

ncmpbins <- c(5, 10, 15, 20, 25, 30)
ncmppal <- colorBin(c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#BA1084", "#8C1D82"), domain = NCMPRec$Value, bins = ncmpbins)


obesityRec <- read.csv("data/obesityRec.csv")
obesityRec <- left_join(msoa, obesityRec, by = c("MSOA11CD" = "AreaCode"))

obesitybins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
obesitypal <- colorBin(c("#FFFFFF", "#FFD8E4", "#FEB0C9", "#F887AF", "#F05996",
                         "#E5007E", "#d40481", "#C30C83", "#B11384", "#9E1984", "#8C1D82"), domain = obesityRec$Value, bins = obesitybins)

#NCMP Year 6: Prevalence of obesity (including severe obesity)

NCMP6 <- read.csv("data/NCMP6.csv")
NCMP6 <- left_join(lads, NCMP6, by = c("lad17nm" = "AreaName"))


obesity6 <- read.csv("data/obesity6.csv")
obesity6 <- left_join(msoa, obesity6, by = c("MSOA11CD" = "AreaCode"))

#School readiness: percentage of children achieving a good level of development at the end of Reception

readiness <- read.csv("data/readiness.csv")
readiness <- left_join(lads, readiness, by = c("lad17nm" = "AreaName"))

readiness_bins <- c(64, 66, 68, 70, 72, 74, 76)
readiness_pal <- colorBin(c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#C90983", "#AB1584", "#8C1D82"), domain = readiness, bins = readiness_bins)

#School Readiness: percentage of children with free school meal status achieving a good level of development at the end of Reception

readiness_fsm <- read.csv("data/readinessFSM.csv")
readiness_fsm <- left_join(lads, readiness_fsm, by = c("lad17nm" = "AreaName"))

readiness_fsm_bins <- c(44, 47, 50, 53, 56, 59, 62)
readiness_fsm_pal <- colorBin(c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#C90983", "#AB1584", "#8C1D82"), 
                              domain = readiness_fsm, bins = readiness_fsm_bins)

#Children in care

care <- read.csv("data/care.csv")
care <- left_join(lads, care,  by = c("lad17nm" = "AreaName"))

care_bins <- c(40,60,80,100,120,140)
care_pal <- colorBin(c("#FFFFFF", "#FFBED2", "#f679a7", "#E5007E", "#BA1084", "#8C1D82"), domain = care$Value, bins = care_bins)

#16-17 year olds NEETs

neets <- read.csv("data/neets.csv")
neets <- left_join(lads, neets,  by = c("lad17nm" = "AreaName"))

neets_bins <- c(3,4,5,6,7,8,9)
neets_pal <- colorBin(c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#C90983", "#AB1584", "#8C1D82"), domain = neets$Value, bins = neets_bins)

#Youth Homelessness
homeless <- read.csv("data/Youth Homelessness.csv")
homeless <- left_join(lads, homeless, by = c("lad17nm" = "Area.Name"))

homeless_bins <- c(0.5, 1, 1.5 ,2, 2.5, 3, 3.5)

homeless_pal <- colorBin(c("#FFFFFF", "#FFBED2", "#f679a7", "#E5007E", "#BA1084", "#8C1D82"),
                         domain = homeless$Rate.of.youth.homelessness..16.24.,
                         bins = homeless_bins)
#Poverty
poverty <- read.csv("data/poverty.csv")
poverty <- left_join(constituency, poverty, by = c("pcon17cd" = "Area.Code"))

poverty_bins <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)

poverty_pal <- colorBin(c("#FFFFFF", "#FFCEDD", "#FB9CBC", "#F2669C", "#E5007E",
                          "#D00682", "#BA1084", "#A31884", "#8C1D82"),
                        domain = poverty$`Percentage.2018.19`,
                        bins = poverty_bins)

#Crime

ss19 <- read.csv("data/ss19.csv")

#Greenspace

privateGreenspace <- read.csv("data/Outdoor Private Space MSOA.csv")
privateGreenspace <- left_join(msoa, privateGreenspace, by = c("MSOA11CD" = "MSOA.code"))

privateGreenspace_bins <- c(0, 10,20,30,40,50,60,70,80,90,100)
privateGreenspace_pal <- colorBin(c("#FFFFFF", "#FFE0F1", "#FFC1E3", "#FFA1D5", "#FF82C7",
                                    "#FF63B9", "#FF44AB", "#FF249D", "#FF058F", "#E5007E"),
                                  domain = privateGreenspace$Total..Percentage.of.adresses.with.private.outdoor.space,
                                  bins = privateGreenspace_bins)

publicGreenspace <- read.csv("data/publicGreenspace.csv")
publicGreenspace <- left_join(lsoa, publicGreenspace, by = c("lsoa11cd" = "LSOA.code"))

publicGreenspace_bins <- c(0,2,4,6,8,10,12,14)
publicGreenspace_pal <- colorBin(c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#C90983", "#AB1584", "#8C1D82"),
                                 domain = publicGreenspace$Average.number.of..Parks..Public.Gardens..or.Playing.Fields.within.1.000.m.radius,
                                 bins = publicGreenspace_bins)

sat_clubs <- read.csv("data/sat_clubs.csv")

# Graphs Data -------------------------------------------------------------

#neets
neets_time <- fingertips_data(IndicatorID = 93203, AreaTypeID = 202) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons")

neets_time_Eng <- fingertips_data(IndicatorID = 93203, AreaTypeID = 6) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName == "England") %>% filter(Sex == "Persons")

neets_time <- bind_rows(neets_time, neets_time_Eng)

#Obesity
NCMPRec_time <- fingertips_data(IndicatorID = 90319, AreaTypeID = 202) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("England", "Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons")
  

NCMP6_time <- fingertips_data(IndicatorID = 90323, AreaTypeID = 202) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("England", "Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons")


#School Readiness

readiness_time <- fingertips_data(IndicatorID = 90631, AreaTypeID = 202) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons")

readiness_time_Eng <- fingertips_data(IndicatorID = 90631, AreaTypeID = 6) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(Sex == "Persons") %>% filter(AreaName == "England")

readiness_time <- bind_rows(readiness_time, readiness_time_Eng)

readiness_fsm_time <- fingertips_data(IndicatorID = 90632, AreaTypeID = 202) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons")

readiness_fsm_time_Eng <- fingertips_data(IndicatorID = 90632, AreaTypeID = 6) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(Sex == "Persons") %>% filter(AreaName == "England")

readiness_fsm_time <- bind_rows(readiness_fsm_time, readiness_fsm_time_Eng)

#Care

care_time <- fingertips_data(IndicatorID = 90803, AreaTypeID = 202) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) 

care_time_Eng<- fingertips_data(IndicatorID = 90803, AreaTypeID = 6) %>%
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName == "England") %>% filter(Sex == "Persons")

care_time <- bind_rows(care_time, care_time_Eng)

#Povertyf

poverty_time <- read.csv("data/Poverty Time.csv")
poverty_time <- rename(poverty_time, "Value"="Percentage")

poverty_time_Eng <- read.csv("data/Poverty_Eng.csv")

poverty_time <- bind_rows(poverty_time, poverty_time_Eng)

#Inactivity

cypal_time <- read_excel("data/Active Lives Template v20.xlsx", 
                         sheet = "Full CYP Active Lives") %>%
  mutate (Active = Active*100) %>% mutate(`Fairly Active`= `Fairly Active`*100)%>%mutate(`Less Active`=`Less Active`*100)%>%
  mutate(`Active 60 minutes every day`=`Active 60 minutes every day`*100)%>%mutate(`Active 30 minutes+ at and outside school`=`Active 30 minutes+ at and outside school`*100)%>%
  mutate(`Active 30+ minutes outside school`=`Active 30+ minutes outside school`*100)%>%mutate(`Active 30+ minutes at school`=`Active 30+ minutes at school`*100)%>%mutate(Moving=Moving*100)
cypal_time <- rename(cypal_time, c("AreaName"="Area", "Timeperiod" = "Release", "Value" = "Less Active"))

al_time <- read_excel("data/Active Lives Template v19.xlsx", 
                      sheet = "Full Adult Active Lives") %>%
  subset(Release!="Total change")
al_time[c(3, 5:33, 35:63, 65:102)] = al_time[c(3, 5:33, 35:63, 65:102)]*100
al_time <- rename(al_time, c("AreaName"="Area", "Timeperiod" = "Release", "Value" = "Inactive - Total Population"))

# Database ----------------------------------------------------------------


postcodes <- read.csv("data/ukpostcodes.csv")
db <- read_csv("data/SchoolsDb150121.csv")
schools <- read_csv("data/Schools.csv")
schoolslatlong <- left_join(schools, postcodes, by = c("Postcode" ="postcode"))
schoolsdb <- left_join(db, schoolslatlong, by = c("EntityID" = "GroupID"))
schoolsdb <- schoolsdb[-c(1:2,4:9, 65:68)]


# Other -------------------------------------------------------------------

gstheme <- create_theme(
  adminlte_color(
    light_blue = "#FFFFFF"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#3c3c3b",
    dark_hover_bg = "#5B2D86",
    dark_color = "#D21C60"
  ),
  adminlte_global(
    content_bg = "#FFFFFF",
    box_bg = "#5B2D86", 
    info_box_bg = "#D21C60"
  )
)
u25 <- read.csv("data/u25.csv")
u25 <- left_join(lads, u25, by = c("lad17nm" = "GEOGRAPHY_NAME"))

u16 <- read.csv("data/u16.csv")
u16 <- left_join(lads, u16, by = c("lad17nm" = "GEOGRAPHY_NAME")) 

yp <- read.csv("data/yp.csv")
yp <- left_join(lads, yp, by = c("lad17nm" = "GEOGRAPHY_NAME"))

u16_bins <- c(18,19,20,21,22,23)
u16_pal <- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"),
                    domain = u16$OBS_VALUE, bins = u16_bins)

yp_bins <- c(8,10,12,14,16,18)
yp_pal<- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"),
                  domain = yp$OBS_VALUE, bins = yp_bins)

no_U25 <- sum(yp$OBS_VALUE)

DM <- length(which(schoolsdb$`Is this School signed up to the Daily Mile?` == "Yes"))
SGL2 <- length(which(schoolsdb$`Does this school get involved with the School Games at Level 2?` == "Yes"))
schools <- length(unique(schoolsdb$URN.x))
schools <- prettyNum(schools, big.mark = ",", scientific = FALSE)
SGL3 <- length(which(schoolsdb$`Has this school ever attended Level 3 School Games (Post Sept 2018)?` == "Yes"))
ALC <- length(which(schoolsdb$`Have they completed the survey` == "Yes"))
no_U25 <- read.csv("data/no_u25.csv") 
no_U25 <- sum(no_U25$OBS_VALUE)
no_U25 <- prettyNum(no_U25, big.mark = ",", scientific = FALSE)
trusts <- length(unique(schoolsdb$`Which academy/trust are they part of`))

#Colour Palettes
col_6 <- c("#FFFFFF", "#FFBED2", "#f679a7", "#E5007E", "#BA1084", "#8C1D82")
col_7 <- c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#C90983", "#AB1584", "#8C1D82")
col_9 <- c("#FFFFFF", "#FFCEDD", "#FB9CBC", "#F2669C", "#E5007E",
           "#D00682", "#BA1084", "#A31884", "#8C1D82")
col_11 <- c("#FFFFFF", "#FFD8E4", "#FEB0C9", "#F887AF", "#F05996",
            "#E5007E", "#d40481", "#C30C83", "#B11384", "#9E1984", "#8C1D82")
col_10 <- c("#FFFFFF", "#FFD8E4", "#FEB0C9", "#F887AF", "#F05996",
            "#E5007E", "#D00682", "#BA1084", "#A31884", "#8C1D82")

ss192 <- ss19 %>%
  filter(!is.na(lng)) %>%
  filter (!is.na(lat))

ss192 <- ss192 %>%
  mutate(longitude = as.numeric(longitude)) %>% mutate(latitude = as.numeric(latitude))

imd <- imd %>%
  mutate(year = as.numeric(year)) %>% filter(year == 2019)

imd_pal <- colorBin(col_10 , domain = imd$decile, bins =10)




