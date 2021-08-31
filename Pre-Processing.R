library(classInt); library(viridis);library(leaflet); library (dplyr); library(tidyverse) ; library(sf) ; 
library(shiny); library(htmlwidgets); library(readxl); library(fingertipsR); library(leaflet.extras); library(httr);
library(jsonlite); library(rjson); library(rgdal);library(geojsonio); library(mapview); library(leafem); library(ukpolice);
library(nomisr); library(shinydashboard);library(shinyWidgets);library(tidyverse);library(sf);library(ggiraph);
library(scales); library(htmltools);library(htmlwidgets);library(nomisr);library(readxl);
library(mapview);library(fresh); library(leaflet.extras); library(rsconnect); library(shinyscreenshot);
library(dashboardthemes); library(shinycssloaders); library(shinycustomloader); library(extrafont)

# Boundaries --------------------------------------------------------------

lookup <- read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>%
  filter (LAD17NM %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  pull(LSOA11CD)

lookupname <- read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>%
  filter (LAD17NM %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  pull(LAD17NM)

lookupmsoa <- read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>%
  filter (LAD17NM %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  pull(MSOA11NM) 

lsoa <- st_read("data/best_fit_lsoa.geojson")

lsoa %>%
  filter(lsoa11cd %in% lookup) %>%
  st_write("GM_LSOA.geojson")

la <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2017_Boundaries/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json")

gm_la <- la %>%
  filter(lad17nm %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))

lads <- st_read("data/regions.geojson")

lads <- lads %>%
  filter(lad17nm %in% lookupname) %>%
  st_write("data/GM_LADS.geojson")

gm_la <- st_read("data/GMLAs.shp")

MSOAimport <- st_read("https://opendata.arcgis.com/datasets/5d4e4cc075ef4a40acbe6e50735451ef_0.geojson")

msoa <- MSOAimport %>%
  filter(MSOA11NM %in% lookupmsoa) 

msoa %>% st_write("data/GM_MSOA.geojson")

constituency <- st_read("https://opendata.arcgis.com/datasets/5ce27b980ffb43c39b012c2ebeab92c0_2.geojson")
wardtola <- read.csv("data/Parliamentary Constituency to LA.csv")
constituency <- left_join(constituency, wardtola, by = c("pcon17cd" = "PCON16CD"))
constituency <- constituency %>%
  filter (LAD16NM %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))

constituency %>% st_write("data/GM_Constituency.geojson")
# Datasets ----------------------------------------------------------------

## IMD ----
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

## Active Lives ----
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
col <- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"), domain = cypal1718$Active, bins = bins)

## Obesity ----
#### NCMP Reception: Prevalence of obesity (including severe obesity) ----
NCMPRec <- fingertips_data(IndicatorID = 90319, AreaTypeID = 102) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  filter(Timeperiod =="2019/20")

NCMPRec %>% write_csv("data/NCMPRec.csv")

NCMPRec <- left_join(lads, NCMPRec, by = c("lad17nm" = "AreaName"))

ncmpbins <- c(5, 10, 15, 20, 25, 30)
ncmppal <- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"), domain = NCMPRec$Value, bins = ncmpbins)

obesityRec <- fingertips_data(IndicatorID = 93106, AreaTypeID = 3) %>%
  filter(AreaType == "MSOA") %>%
  filter(ParentName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Timeperiod == "2017/18 - 19/20")

obesityRec %>% write_csv("data/obesityRec.csv")

obesityRec <- left_join(msoa, obesityRec, by = c("MSOA11CD" = "AreaCode"))

obesitybins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
obesitypal <- colorBin(c("#FFFFFF", "#F6D1DF", "#EDA4BF", "#E4769F", "#DB497F",
                         "#D21C60", "#BA1F67", "#A2226F", "#8A2676", "#72297E", "#5B2D86"), domain = obesityRec$Value, bins = obesitybins)

### NCMP Year 6: Prevalence of obesity (including severe obesity) ----

NCMP6 <- fingertips_data(IndicatorID = 90323, AreaTypeID = 101) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  filter(Timeperiod =="2019/20")

NCMP6 %>% write_csv("data/NCMP6.csv")

NCMP6 <- left_join(lads, NCMP6, by = c("lad17nm" = "AreaName"))

obesity6 <- fingertips_data(IndicatorID = 93108, AreaTypeID = 3) %>%
  filter(AreaType == "MSOA") %>%
  filter(ParentName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Timeperiod == "2017/18 - 19/20")

obesity6%>% write_csv("data/obesity6.csv")

obesity6 <- left_join(msoa, obesity6, by = c("MSOA11CD" = "AreaCode"))

## School readiness ----
### School readiness: percentage of children achieving a good level of development at the end of Reception ----

readiness <- fingertips_data(IndicatorID = 90631, AreaTypeID = 202) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons") %>%
  filter(Timeperiod == "2018/19") 

readiness %>% write_csv("data/readiness.csv")

readiness <- left_join(lads, readiness, by = c("lad17nm" = "AreaName"))

readiness_bins <- c(64, 66, 68, 70, 72, 74, 76)
readiness_pal <- colorBin(c("#FFFFFF", "#F0B3CA", "#E16795", "#D21C60", "#AA216C", "#822779", "#5B2D86"), domain = readiness_fsm, bins = readiness_bins)

### School Readiness: percentage of children with free school meal status achieving a good level of development at the end of Reception ----

readiness_fsm <- fingertips_data(IndicatorID = 90632, AreaTypeID = 202) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons") %>%
  filter(Timeperiod == "2018/19") 

readiness_fsm %>% write_csv("data/readinessFSM.csv")

readiness_fsm <- left_join(lads, readiness_fsm, by = c("lad17nm" = "AreaName"))

readiness_fsm_bins <- c(44,47,50,53,56,59,62)
readiness_fsm_pal <- colorBin(c("#FFFFFF", "#F0B3CA", "#E16795", "#D21C60", "#AA216C", "#822779", "#5B2D86"), 
                              domain = readiness_fsm, bins = readiness_fsm_bins)

## Children in care ----

care <- fingertips_data(IndicatorID = 90803, AreaTypeID = 202) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Timeperiod == "2020")

care %>% write_csv("data/care.csv")

care <- left_join(lads, care,  by = c("lad17nm" = "AreaName"))

care_bins <- c(40,60,80,100,120,140)
care_pal <- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"), domain = care$Value, bins = care_bins)

## 16-17 year olds NEETs ----

neets <- fingertips_data(IndicatorID = 93203, AreaTypeID = 202) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons") %>%
  filter(Timeperiod == "2019")

neets %>% write_csv("data/neets.csv")

neets <- left_join(lads, neets,  by = c("lad17nm" = "AreaName"))

neets_bins <- c(3,4,5,6,7,8,9)
neets_pal <- colorBin(c("#FFFFFF", "#F0B3CA", "#E16795", "#D21C60", "#AA216C", "#822779", "#5B2D86"), domain = neets$Value, bins = neets_bins)

## Youth Homelessness ----
homeless <- read.csv("data/Youth Homelessness.csv")
homeless <- left_join(lads, homeless, by = c("lad17nm" = "Area.Name"))

homeless_bins <- c(0.5, 1, 1.5 ,2, 2.5, 3, 3.5)

homeless_pal <- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"),
                         domain = homeless$`Rate of youth homelessness (16-24)`,
                         bins = homeless_bins)

## Childhood Poverty ----

poverty <- read.csv("data/poverty.csv")
poverty <- left_join(constituency, poverty, by = c("pcon17cd" = "Area Code"))
poverty <- poverty %>%
  mutate(`Percentage 2018/19` = `Percentage 2018/19`*100) %>% mutate(`Percentage 2017/18` = `Percentage 2017/18`*100) %>%
  mutate(`Percentage 2016/17` = `Percentage 2016/17`*100) %>% mutate(`Percentage 2015/16` = `Percentage 2015/16`*100) %>%
  mutate(`Percentage 2014/15` = `Percentage 2014/15`*100) %>% mutate(...13 = ...13*100)

poverty %>% write.csv("data/poverty.csv")

poverty_bins <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)

poverty_pal <- colorBin(c("#FFFFFF", "#F3C6D7", "#E88DAF", "#DD5487", "#D21C60",
                          "#B42069", "#962473", "#78287C", "#5B2D86"),
                        domain = poverty$`Percentage 2018/19`,
                        bins = poverty_bins)

## Crime ----

ss_0619 <- ukc_stop_search_force(force = "greater-manchester",
                                 date = "2019-06") %>%
  filter(age_range == "under 10" | age_range == "10-17" | age_range == "18-24") %>%
  mutate(lng = as.numeric(longitude)) %>% mutate(lat = as.numeric(latitude))

ss_0519 <- ukc_stop_search_force(force = "greater-manchester",
                                 date = "2019-05") %>%
  filter(age_range == "under 10" | age_range == "10-17" | age_range == "18-24") %>%
  mutate(lng = as.numeric(longitude)) %>% mutate(lat = as.numeric(latitude))

ss_0419 <- ukc_stop_search_force(force = "greater-manchester",
                                 date = "2019-04") %>%
  filter(age_range == "under 10" | age_range == "10-17" | age_range == "18-24") %>%
  mutate(lng = as.numeric(longitude)) %>% mutate(lat = as.numeric(latitude))

ss_0319 <- ukc_stop_search_force(force = "greater-manchester",
                                 date = "2019-03") %>%
  filter(age_range == "under 10" | age_range == "10-17" | age_range == "18-24") %>%
  mutate(lng = as.numeric(longitude)) %>% mutate(lat = as.numeric(latitude))

ss_0219 <- ukc_stop_search_force(force = "greater-manchester",
                                 date = "2019-05") %>%
  filter(age_range == "under 10" | age_range == "10-17" | age_range == "18-24") %>%
  mutate(lng = as.numeric(longitude)) %>% mutate(lat = as.numeric(latitude))

ss_0119 <- ukc_stop_search_force(force = "greater-manchester",
                                 date = "2019-01") %>%
  filter(age_range == "under 10" | age_range == "10-17" | age_range == "18-24") %>%
  mutate(lng = as.numeric(longitude)) %>% mutate(lat = as.numeric(latitude))

rbind(ss_0119, ss_0219, ss_0319, ss_0419, ss_0519, ss_0619) %>%
  write_csv("data/ss19.csv")

## Greenspace ----

privateGreenspace <- read.csv("data/Outdoor Private Space MSOA.csv")

privateGreenspace <- left_join(msoa, privateGreenspace, by = c("MSOA11CD" = "MSOA.code"))

privateGreenspace %>% write.csv("data/privateGreenspace.csv")

privateGreenspace_bins <- c(100, 90,80,70,60,50,50,40,30,20,10)
privateGreenspace_pal <- colorBin(c("#5B2D86", "#75297D", "#8F2575", "#AA216C", "#C41D64",
                                    "#D73571", "#E16794", "#EB9AB8", "#F4CCDB", "#FFFFFF"),
                                  domain = privateGreenspace$Total..Percentage.of.adresses.with.private.outdoor.space,
                                  bins = privateGreenspace_bins)

publicGreenspace <- read.csv("data/publicGreenspace.csv")
publicGreenspace <- left_join(lsoa, publicGreenspace, by = c("lsoa11cd" = "LSOA.code"))

publicGreenspace_bins <- c(0,2,4,6,8,10,12,14)
publicGreenspace_pal <- colorBin(c("#FFFFFF", "#F0B3CA", "#E16795", "#D21C60", "#AA216C", "#822779", "#5B2D86"),
                                 domain = publicGreenspace$Average.number.of..Parks..Public.Gardens..or.Playing.Fields.within.1.000.m.radius,
                                 bins = publicGreenspace_bins)

sat_clubs <- read.csv("data/sat_clubs.csv")


## Schools Database ----

postcodes <- read.csv("data/ukpostcodes.csv")
db <- read_csv("data/SchoolsDb310821.csv")
schools <- read_csv("data/Schools.csv")
schoolslatlong <- left_join(schools, postcodes, by = c("Postcode" ="postcode"))
schoolsdb <- left_join(db, schoolslatlong, by = c("EntityID" = "GroupID"))
schoolsdb <- schoolsdb[-c(1:2,4:9, 67:70)]

schoolsdbFiltered <- schoolsdb[c(4:6,12,15,16,18,20,22,23,25,35,39,41,45,50,51:55)]

print(names(schoolsdbFiltered))

# Theme -------------------------------------------------------------------



GMMTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Helvetica"
  ,appFontColor = "#63666A"
  ,primaryFontColor = "#63666A"
  ,infoFontColor = "#63666A"
  ,successFontColor = "#63666A"
  ,warningFontColor = "#63666A"
  ,dangerFontColor = "#63666A"
  ,bodyBackColor = "#FFFFFF"
  
  ### header
  ,logoBackColor = "#FFFFFF"
  
  ,headerButtonBackColor = "#FFFFFF"
  ,headerButtonIconColor = "#e6007e"
  ,headerButtonBackColorHover = "#e6007e"
  ,headerButtonIconColorHover = "#FFFFFF"
  
  ,headerBackColor = "#FFFFFF"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#FFFFFF"
  ,sidebarPadding = "0"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#63666A"
  
  ,sidebarSearchBackColor = "#FFFFFF"
  ,sidebarSearchIconColor = "#63666A"
  ,sidebarSearchBorderColor = "#63666A"
  
  ,sidebarTabTextColor = "#63666A"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"
  
  ,sidebarTabBackColorSelected = "#e6007e"
  ,sidebarTabTextColorSelected = "#FFFFFF"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#e6007e"
  ,sidebarTabTextColorHover = "#FFFFFF"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#e6007e"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#e6007e"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#E1E1E1"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"
  
  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"
  
  ### inputs
  ,buttonBackColor = "#e6007e"
  ,buttonTextColor = "#FFFFFF"
  ,buttonBorderColor = "##e6007e"
  ,buttonBorderRadius = "5"
  
  ,buttonBackColorHover = "#e6007e"
  ,buttonTextColorHover = "#FFFFFF"
  ,buttonBorderColorHover = "#BEBEBE"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#63666A"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#FFFFFF"
  ,textboxBorderColorSelect = "#63666A"
  
  ### tables
  ,tableBackColor = "#F8F8F8"
  ,tableBorderColor = "#EEEEEE"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)

# Other -------------------------------------------------------------------



DM <- length(which(schoolsdb$`Is this School signed up to the Daily Mile?` == "Yes"))
SGL2 <- length(which(schoolsdb$`Does this school get involved with the School Games at Level 2?` == "Yes"))
schools <- length(unique(schoolsdb$EntityID))
schools <- prettyNum(schools, big.mark = ",", scientific = FALSE)
SGL3 <- length(which(schoolsdb$`Has this school ever attended Level 3 School Games (Post Sept 2018)?` == "Yes"))
ALC <- length(which(schoolsdb$`Have they completed the survey` == "Yes"))
no_U25 <- prettyNum(no_U25, big.mark = ",", scientific = FALSE)
trusts <- length(unique(schoolsdb$`Which academy/trust are they part of`))

col_6 <- c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#BA1084", "#8C1D82")
col_7 <- c("#FFFFFF", "#FFBED2", "#F679A7", "#E5007E", "#C90983", "#AB1584", "#8C1D82")
col_9 <- c("#FFFFFF", "#FFCEDD", "#FB9CBC", "#F2669C", "#E5007E",
           "#D00682", "#BA1084", "#A31884", "#8C1D82")
col_11 <- c("#FFFFFF", "#FFD8E4", "#FEB0C9", "#F887AF", "#F05996",
            "#E5007E", "#d40481", "#C30C83", "#B11384", "#9E1984", "#8C1D82")
col_10 <- c("#FFFFFF", "#FFD8E4", "#FEB0C9", "#F887AF", "#F05996",
            "#E5007E", "#D00682", "#BA1084", "#A31884", "#8C1D82")


ss19 <- read.csv("data/ss19.csv")

ss192 <- ss19 %>%
  filter(!is.na(lng)) %>%
  filter (!is.na(lat))

ss192 <- ss192 %>%
  mutate(longitude = as.numeric(longitude)) %>% mutate(latitude = as.numeric(latitude))

imd <- imd %>%
  mutate(year = as.numeric(year)) %>% filter(year == 2019)

imd_pal <- colorBin(col_10 , domain = imd$decile, bins =10)


z <- nomis_get_data(id = "NM_2002_1", time = "latest", geography = "TYPE431")

u25 <- z %>%
  filter(C_AGE %in% c("201", "250")) %>%
  filter(GEOGRAPHY_NAME %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(GENDER == "0") %>%
  filter(MEASURES_NAME == "Percent")

u25 %>% write.csv("data/u25.csv")

u25 <- left_join(u25, lads, by = c("GEOGRAPHY_NAME" = "lad17nm"))

u16 <- z %>%
  filter(C_AGE == "201") %>%
  filter(GEOGRAPHY_NAME %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(GENDER == "0") %>%
  filter(MEASURES_NAME == "Percent")

u16 %>% write.csv("data/u16.csv")

u16 <- left_join(gm_la, u16, by = c("lad17nm" = "GEOGRAPHY_NAME")) 

yp <- z %>%
  filter(C_AGE == "250") %>%
  filter(GEOGRAPHY_NAME %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(GENDER == "0") %>%
  filter(MEASURES_NAME == "Percent")

yp %>% write.csv("data/yp.csv")

yp <- left_join(gm_la, yp, by = c("lad17nm" = "GEOGRAPHY_NAME"))

u16_bins <- c(18,19,20,21,22,23)

u16_pal <- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"),
                    domain = u16$OBS_VALUE, bins = u16_bins)

yp_bins <- c(8,10,12,14,16,18)

yp_pal<- colorBin(c("#FFFFFF", "#EDA4BF", "#DB497F", "#BA1F67", "#8A2676", "#5B2D86"),
                  domain = yp$OBS_VALUE, bins = yp_bins)

no_U25 <- nomis_get_data(id = "NM_2002_1", time = "latest", geography = "TYPE431") %>%
  filter(C_AGE %in% c("201", "250")) %>%
  filter(GEOGRAPHY_NAME %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(GENDER == "0") %>%
  filter(MEASURES_NAME == "Value")

no_U25 %>% write.csv("data/no_U25.csv")

no_U25 <- sum(yp$OBS_VALUE)

branded_colors <- list(
  "orange" = "#ED8B00",
  "green" = "#84BD00",
  "blue" = "#009CDD",
  "pink" = "#E5007E",
  "grey" = "#63666A",
  "pale_orange" = "#FFC777",
  "pale_green" = "#CFFF5F",
  "pale_blue"  ="#6FD5FF",
  "pale_pink" = "#FF73C0"
)

branded_pal <- function(
  primary = "blue", 
  other = "grey", 
  direction = 1
) {
  stopifnot(primary %in% names(branded_colors))
  
  function(n) {
    if (n > 6) warning("Branded Color Palette only has 6 colors.")
    
    if (n == 2) {
      other <- if (!other %in% names(branded_colors)) {
        other
      } else {
        branded_colors[other]
      }
      color_list <- c(other, branded_colors[primary])
    } else {
      color_list <- branded_colors[1:n]
    }
    
    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}

scale_colour_branded <- function(
  primary = "blue", 
  other = "grey", 
  direction = 1, 
  ...
) {
  ggplot2::discrete_scale(
    "colour", "branded", 
    branded_pal(primary, other, direction), 
    ...
  )
}

scale_color_branded <- scale_colour_branded

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}


GS_logo <- get_png("data/GS supporting GM Moving.png")

indicator_metadata(IndicatorID = 90319) %>% View()
indicator_areatypes(IndicatorID = 90319)

NCMPRec_time_Eng <- fingertips_data(IndicatorID = 90319, AreaTypeID = 6) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName == "England") %>% filter(Sex == "Persons")

NCMPRec_time <- fingertips_data(IndicatorID = 90319, AreaTypeID = 101) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))

NCMPRec_time <- fingertips_data(IndicatorID = 90319, AreaTypeID = 101) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value) %>%
  filter(AreaName %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))%>%
  filter(Timeperiod %in% c("2006/07", "2007/08", "2008/09", "2009/10", "2010/11",
                           "2011/12", "2012/13", "2013/14", "2014/15", "2015/16",
                           "2016/17", "2017/18", "2018/19"))


NCMPRec_time <- fingertips_data(IndicatorID = 90319, AreaTypeID = 202) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("England", "Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons")

NCMP6_time <- fingertips_data(IndicatorID = 90323, AreaTypeID = 202) %>% 
  select(IndicatorID, IndicatorName, AreaCode, AreaName, AreaType, Timeperiod, Value, Sex) %>%
  filter(AreaName %in% c("England", "Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  filter(Sex == "Persons")

theme_GMM <- function() {
  
  font <- "Helvetica"
  fontColour <- "#63666A"
  
  theme(
    #grid elements
    panel.grid.major.y = element_line(color = "#a3a3a2"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),   
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    
    #key text
    plot.title = element_text(
      family = font,
      colour = "#E5007E",
      size = 16,
      face = 'bold',          
      hjust = 0,              
      vjust = 2),         
    
    plot.subtitle = element_text(         
      family = font,
      colour = fontColour,
      size = 14),               
    
    plot.caption = element_text(          
      family = font,  
      colour = fontColour,
      size = 14,                 
      hjust = 0),               
    
    #general text
    axis.title = element_text(             
      family = font,    
      colour = fontColour,
      size = 14),             
    
    axis.text = element_text(            
      family = font, 
      colour = fontColour,
      size = 14),              
    
    axis.text.x = element_text(          
      margin=margin(5, b = 10)),
    
    #legend
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family = font,
                               size = 14,
                               color  = fontColour),
    
    #general
    plot.title.position = "plot"
  )
}


library(devtools)

GSColours <- c(
  `purple` = "#5B2D86",
  `pink` = "#D21C60",
  `pale purple` = "#8A6EAD",
  `pale pink` = "#D7697A",
  `blue` = "#5197D1",
  `yellow` = "#FCCA58",
  `grey` = "#3C3C3B",
  `white` = "#FFFFFF"
)

GSCols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (GSColours)
  
  GSColours[cols]
}


GSPalette <- list(
  `primary` = GSCols("purple", "pink"),
  `secondary` = GSCols("pale purple", "pale pink"),
  `gradient` = GSCols("white" , "pink", "purple"),
  `mixed` = GSCols("purple", "pink", "pale purple", "pale pink", "blue", "yellow", "grey")
)

GSPal <- function(palette = "primary", reverse = FALSE, ...) {
  pal <- GSPalette[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scaleColGS <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- GSPal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("GS", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scaleFillGS <- function(palette = "gradient", discrete = TRUE, reverse = FALSE, ...) {
  pal <- GSPal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("GS", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



theme_GS <- function() {
  
  font <- "PT Sans"
  fontColour <- "#3c3c3b"
  
  theme(
    #grid elements
    panel.grid.major.y = element_line(color = "#a3a3a2"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),   
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    
    #key text
    plot.title = element_text(
      family = font,
      colour = "#5B2D86",
      size = 16,
      face = 'bold',          
      hjust = 0,              
      vjust = 2),         
    
    plot.subtitle = element_text(         
      family = font,
      colour = fontColour,
      size = 14),               
    
    plot.caption = element_text(          
      family = font,  
      colour = fontColour,
      size = 12,                 
      hjust = 0),               
    
    #general text
    axis.title = element_text(             
      family = font,    
      colour = fontColour,
      size = 12),             
    
    axis.text = element_text(            
      family = font, 
      colour = fontColour,
      size = 12),              
    
    axis.text.x = element_text(          
      margin=margin(5, b = 10)),
    
    #legend
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family = font,
                               size = 12,
                               color  = fontColour)
  )
}

themeGMM <- function(
  base_size = 12,
  base_colour = "#63666A"
)
  ##Axis
  x_col = "white"
y_col = "white"

theme(
  legend.position = "none",
  
  axis.ticks = element_blank(),
  axis.line = element_line(colour = base_colour),
  axis.line.x = element_line(colour = x_col),
  axis.line.y = element_line(colour = y_col),
  
  ##Text
  text = element_text(
    colour = base_colour, size = base_size,
    hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.8
  ),
  axis.text = element_text(colour = base_colour, size = base_size),
  plot.title = element_text(face = "bold", 
                            hjust = 1, colour = "black", vjust = -2.5),
  
  ## Axis title attributes. Adjustments of
  
  axis.title.y = element_text(hjust = 1, vjust = 1),
  axis.title.x = element_text(hjust = 1, vjust = 0),
  
  ## Background attributes (currently all blank)
  
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  ## Strip attributes for facet grid and facet wrap
  
  strip.background =   element_blank(),
  strip.text =         element_text(color = "black", face = "bold", size = base_size + 1),
  strip.text.x =       element_text(),
  strip.text.y =       element_text(angle = -90)
)

GMM_cols <- c(
  `pink` = "#E5007E",
  `blue` = "#009CDD",
  `green` = "#84BD00",
  `orange` = "#ED8B00",
  `purple` = "#5B2D86",
  `yellow` = "#FCCA58",
  `turquoise` = "#009E83",
  `pale purple` = "#8A6EAD",
  `pale pink` = "#D7697A",
  `red` = "#e30333",
  `pale blue` = "#99d6f0",
  `pale orange` = "#f7c799" 
)

scale_GMM <- function(type = "fill", ...) {
  
  type = match.arg(type, c("colour", "fill"))
  
  cols <- unname(GMM_cols)
  
  if (type == "fill") {
    s <- scale_fill_manual(values = cols)
  } else if (type == "colour") {
    s <- scale_colour_manual(values = cols)
  }
  
  return(s)
  
}

check_pal <- function(
  x = GMM_cols
) {
  
  if (is.numeric(x)) {
    
    if (length(x) > 1) {
      
      x <- GMM_cols[x]
      
    } else
      x <- GMM_cols[1:x]
  }
  
  graphics::pie(
    rep(1, length(x)),
    col = x,
    labels = names(x))
  
}

check_pal(5)




