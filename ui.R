library(classInt); library(viridis);library(leaflet); library (dplyr); library(tidyverse) ; library(sf) ; 
library(shiny); library(htmlwidgets); library(readxl); library(fingertipsR); library(leaflet.extras); library(httr);
library(jsonlite); library(rjson); library(rgdal);library(geojsonio); library(mapview); library(leafem); library(ukpolice);
library(nomisr); library(shinydashboard);library(shinyWidgets);library(tidyverse);library(sf);library(ggiraph);
library(scales); library(htmltools);library(htmlwidgets);library(nomisr);library(readxl);
library(mapview);library(fresh); library(leaflet.extras); library(rsconnect); library(shinyscreenshot);
library(dashboardthemes); 


dashboardPage(
    dashboardHeader(title = span("Schools Dashboard", style = "color:#E6007E"),
                    tags$li(a(href = 'https://www.gmmoving.co.uk/',
                              img(src = 'https://www.greatersport.co.uk/media/2587/gmm_logo_rgb.png',
                                  title = "GM Moving", 
                                  height = "30px"),
                              style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown")
    ),
    dashboardSidebar(width = 300,
        sidebarMenu(id = "sidebarid", 
                    menuItem("Home", tabName = "home", icon = icon("home")),
                    menuItem("Mapping", tabName = "mapping", icon = icon("globe")),
                    conditionalPanel(
                        'input.sidebarid == "mapping"',
                        selectInput("domain", "Background Data", list(
                            "Physical Activity" = c("Less Active (2017-18)" = "cypal1718",
                                                    "Less Active (2018-19)" = "cypal1819",
                                                    "Less Active (2019-20)" = "cypal1920"),
                            "Health" = c("Prevalence of obesity (including severe obesity): Reception" = "ncmprec",
                                         "Prevalence of obesity (including severe obesity): Year 6" = "ncmp6",
                                         "Prevalence of obesity (including severe obesity): Reception (small area)" = "ncmprecMSOA",
                                         "Prevalence of obesity (including severe obesity): Year 6 (small area)" = "ncmp6MSOA",
                                         "School readiness: percentage of children achieving a good level of development at the end of Reception" = "readiness",
                                         "School Readiness: percentage of children with free school meal status achieving a good level of development at the end of Reception" = "readiness_fsm"),
                            "Deprivation" = c("Barriers to Housing and Services" = "housing",
                                              "Crime" = "crime",
                                              "Education, Skills and Training" = "education",
                                              "Employment" = "employment",
                                              "Income Deprivation Affecting Children" = "idaci",
                                              "Index of Multiple Deprivation" = "imd"),            
                            "Other" = c("Children in Care (per 10,000)" = "care",
                                        "16-17 Year Olds NEET" = "neet",
                                        "Rate of youth homelessness (16-24)" = "homeless",
                                        "Childhood Poverty (After Housing Costs)" = "poverty",
                                        "Access to Private Greenspace" = "pvtGreenspace",
                                        "Access to Public Greenspace" = "pubGreenspace")
                        )
                        ),
                        
                        selectizeInput("question", "School Selection", choices = names(schoolsdbFiltered), selected = "Borough"
                        ),
                        
                        selectInput("answer", "School Specifics", choices = NULL, selected = NULL, multiple = T),
                        
                        checkboxGroupInput("crime", "Crime Greater Manchester Police (June 2019-2020)",
                                           choices = sort(unique(ss192$object_of_search)))
                    ),
                    menuItem("Graphs", tabName = "graphs", icon = icon("chart-line")),
                    conditionalPanel(
                        'input.sidebarid == "graphs"',
                        selectInput("topic", "Background Data", list(
                            "Physical Activity" = c("Active Lives Children and Young People" = "cypal_time",
                                                    "Active Lives Adult" = "al_time"),
                            "Health" = c("Prevalence of obesity (including severe obesity): Reception" = "ncmprec_time",
                                         "Prevalence of obesity (including severe obesity): Year 6" = "ncmp6_time",
                                         "School readiness: percentage of children achieving a good level of development at the end of Reception" = "readiness_time"#,
                                         #"School Readiness: percentage of children with free school meal status achieving a good level of development at the end of Reception" = "readiness_fsm_time"
                                         ),
                            "Other" = c("16-17 Year Olds NEET" = "neet_time",
                                        "Children in Care (per 10,000)" = "care_time",
                                        "Childhood Poverty (After Housing Costs)" = "poverty_time")
                        )
                        ),
                        selectInput("area", "Area",
                                    c("England", "Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"),
                                    multiple = T, selected = "Manchester")
                    )
        )
    ),
    dashboardBody(
        GMMTheme,
        ## Maps Tab, core dashboard
        tabItems(
            tabItem(
                tabName = "home",
                fluidRow(HTML("<h2>", "Welcome to the GreaterSport Schools Dashboard.", "</h2>")),
                fluidRow("This dashboard has been created in conjunction with our Schools Database with the purpose
                         of making information relating to schools and children and young people across Greater Manchester 
                         more accesible.We hope that this tool proves useful in helping improve the health and wellbeing 
                         of children and young people across the county."),
                fluidRow(HTML("We would love to hear how you are using this dashboard, what decisions it may have informed or 
                         what data and information is missing. Please contact<a href='mailto:phoebe@greatersport.co.uk'> Phoebe Fryer ")),
                tags$footer(
                    fluidRow(
                        "Developed in ",
                        a(href = "https://cran.r-project.org/", target = "_blank", "R"),
                        " by ",
                        a(href = "https://www.greatersport.co.uk/", target = "_blank", "GreaterSport"),
                        "."),
                    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #63666A; padding: 5px 20px; background-color: #FFFFFF"
                )
            ),
            tabItem(
                tabName = "mapping",
                fluidRow(
                    width = '100%',
                    shinycssloaders::withSpinner(
                    leafletOutput("map", width = "99%"), type = 4, color = "#E6007E"), 

                    tags$head(
                        tags$style(HTML('#go{background-color:#E6007E}'))
                    ),
                    actionButton("go", "Download screenshot of the map", icon("download")),

                    fluidRow(
                        tags$h1("I Greater Manchester Overview"),
                        tags$style(".small-box.bg-purple { background-color: #E6007E !important; color: #FFFFFF !important; }"),
                        tags$style(".small-box.bg-fuchsia { background-color: #009fe3 !important; color: #FFFFFF !important; }"),
                        tags$style(".fa-graduation-cap { color: #FF56B3}"),
                        tags$style(".fa-award {color: #FF56B3}"),
                        tags$style(".fa-school {color: #0069b4}"),
                        tags$style(".fa-clipboard {color: #FF56B3}"),
                        tags$style(".fa-users {color: #0069b4}"),
                        tags$style(".fa-walking {color: #0069b4}"),
                        tags$style(".fa-medal {color: #0069b4}"),
                        tags$style(".fa-download {color: #ffffff}"),
                        valueBoxOutput("noU25"),
                        valueBoxOutput("Schools"),
                        valueBoxOutput("Trusts")
                    ),
                    fluidRow(
                        tags$h2("I Project & Programme Overview: 2020/21"),
                        valueBoxOutput("DM", width = 3),
                        valueBoxOutput("SGL2", width = 3),
                        valueBoxOutput("SGL3", width = 3),
                        valueBoxOutput("ALC", width = 3)
                    )
                ),
                tags$footer(
                    fluidRow(
                        "Developed in ",
                        a(href = "https://cran.r-project.org/", target = "_blank", "R"),
                        " by ",
                        a(href = "https://www.greatersport.co.uk/", target = "_blank", "GreaterSport"),
                        " under the ",
                        a(href = "https://www.trafforddatalab.io/LICENSE.txt", target = "_blank", "MIT"),
                        " licence"),
                    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #63666A; padding: 5px 20px; background-color: #FFFFFF"
                )
            ),
            tabItem(
                tabName = "graphs",
                fluidRow(
                    width = "100%",
                    plotOutput("chart"),
                ),
                tags$head(
                    tags$style(HTML('#run{background-color:#E6007E}'))
                ),
                actionButton("run", "Download screenshot of the graph", icon("download")),
                
                fluidRow(
                    textOutput("change"),
                    tags$head(tags$style("#change {colour: #63666A;
                                         font-size: 14px}"))
                ),
                tags$footer(
                    fluidRow(
                        "Developed in ",
                        a(href = "https://cran.r-project.org/", target = "_blank", "R"),
                        " by ",
                        a(href = "https://www.greatersport.co.uk/", target = "_blank", "GreaterSport"),
                        "."),
                    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #63666A; padding: 5px 20px; background-color: #FFFFFF"
                )
            )
        )
    )
)

