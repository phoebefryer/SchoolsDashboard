
ui <- dashboardPage(
    title = "Schools Dashboard",

# Header ------------------------------------------------------------------
## Specifies size of header nav bar and adds in logo which directs to GMM site

    dashboardHeader(title = span("Schools Dashboard", style = "color:#E6007E; font-size:22px"),
                    tags$li(a(href = 'https://www.gmmoving.co.uk/',
                              img(src = 'GMM_Logo.png',
                                  title = "GM Moving", 
                                  height = "50px"),
                              style = "padding-top:0px; padding-bottom:0px;"),
                            class = "dropdown",
                            tags$style(".main-header {max-height: 50px}"),
                            tags$style(".main-header .logo {height: 50px;}"),
                            tags$style(".sidebar-toggle {height: 50px!important;}"),
                            tags$style(".navbar {min-height:20px !important}"))
    ),

# Sidebar -----------------------------------------------------------------


    dashboardSidebar(width = 300,
        sidebarMenu(id = "sidebarid", 
                    menuItem("Home", tabName = "home", icon = icon("home")),
                    menuItem("Mapping", tabName = "mapping", icon = icon("globe")),

## Maps tab and options ----------------------------------------------------

                    conditionalPanel(
                        'input.sidebarid == "mapping"',

### Inputs for map backdrop, specifies the dataset to pull on ------------

                        selectInput("domain", "Background Data", list(
                            "Physical Activity" = c("Less Active (2019-20)" = "cypal1920",
                                                    "Less Active (2018-19)" = "cypal1819",
                                                    "Less Active (2017-18)" = "cypal1718"),
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
                        ), selected = "cypal1920"
                        ),

### Schools database question selection -------------------------------------
### From the questions asked on the schools db a filtered set of questions are pulled
### users then can select the topic of interest

                        selectizeInput("question", "School Selection", choices = names(schoolsdbFiltered), selected = "Borough"
                        ),

### Schools database answer selection ---------------------------------------
### Based on the question previously selected all values are pulled through via a reactive filter
### By default nothing is selected and multiple options can be chosen

                        selectInput("answer", "School Specifics", choices = NULL, selected = NULL, multiple = T),

### Checkboxes to add crime data --------------------------------------------
### YP related crime
                        
                        checkboxGroupInput("crime", "Crime Greater Manchester Police (June 2019-2020)",
                                           choices = sort(unique(ss192$object_of_search)))
                    ),

## Graphs tab and options --------------------------------------------------

                    menuItem("Graphs", tabName = "graphs", icon = icon("chart-line")),
                    conditionalPanel(
                        'input.sidebarid == "graphs"',

### Inputs for graphs, selects the dataset ----------------------------------

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
### Select area(s) of interest, Manchester is selected by default -----------

                        selectInput("area", "Area",
                                    c("England", "Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"),
                                    multiple = T, selected = "Manchester")
                    )
        )
    ),

# Body --------------------------------------------------------------------


    dashboardBody(
## Sets the theme for the whole app ----------------------------------------

        GMMTheme,

## Home Tab ------------------------------------------------
## Intros the dashboard and provides contact details
        tabItems(
            tabItem(
                tabName = "home",
                fluidRow(style = "margin-left:5px", HTML("<h2>", "Welcome to the GreaterSport Schools Dashboard.", "</h2>")),
                fluidRow(style = "font-size:20px; margin-left:5px", "This dashboard has been created in conjunction with our Schools Database with the purpose
                         of making information relating to schools and children and young people across Greater Manchester 
                         more accesible.We hope that this tool proves useful in helping improve the health and wellbeing 
                         of children and young people across the county."),
                fluidRow(style = "font-size:20px; margin-left:5px",HTML("We would love to hear how you are using this dashboard, what decisions it may have informed or 
                         what data and information is missing. Please contact<a href='mailto:phoebe@greatersport.co.uk'> Phoebe Fryer.</a>")),
                tags$footer(
                    fluidRow(
                        "Developed in ",
                        a(href = "https://cran.r-project.org/", target = "_blank", "R"),
                        " by ",
                        a(href = "https://www.greatersport.co.uk/", target = "_blank", "GreaterSport"),
                        " under the ",
                        a(href = "https://creativecommons.org/licenses/by-nc/4.0/legalcode", target = "_blank", "CC BY-NC"),
                        " licence"),
                    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #63666A; padding: 5px 20px; background-color: #FFFFFF"
                )
            ),

## Maps Tab, core dashboard ------------------------------------------------

            tabItem(
                tabName = "mapping",
                fluidRow(
                    style = "margin-left:5px; margin-right:5px",
                    width = '100%',
## Adds the map and spinner when map is loading
                    shinycssloaders::withSpinner(
                    leafletOutput("map", width = "100%"), type = 4, color = "#E6007E"), 

                    tags$head(
                        tags$style(HTML('#go{background-color:#E6007E}'))
                    ),
## Allows download of the map, exports as jpg
                    actionButton("go", "Download screenshot of the map", icon("download")),

## Key stats boxes ---------------------------------------------------------
## Sets colours to match branding and sets first row of valu boxes
                    fluidRow(
                        tags$h1(style = "margin-left:15px", "Greater Manchester Overview"),
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
## Second row of values boxes
                    fluidRow(
                        tags$h2(style = "margin-left:15px", "Project & Programme Overview: 2020/21"),
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
                        a(href = "https://creativecommons.org/licenses/by-nc/4.0/legalcode", target = "_blank", "CC BY-NC"),
                        " licence"),
                    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #63666A; padding: 5px 20px; background-color: #FFFFFF"
                )
            ),

## Graphs tab --------------------------------------------------------------
## Provides time series graphs of key LA datasets
            tabItem(
                tabName = "graphs",
                fluidRow(
                    style = "margin-left:5px; margin-right:5px",
                    width = "100%",
                    plotOutput("chart"),
                ),
                tags$head(
                    tags$style(HTML('#run{background-color:#E6007E}'))
                ),
## Allows download of graphs as image
                actionButton("run", "Download screenshot of the graph", icon("download")),
                
                fluidRow(
                    style = "margin-left:5px",
                    textOutput("change"),
                    tags$head(tags$style("#change {colour: #63666A;
                                         font-size: 20px}"))
                ),
                tags$footer(
                    fluidRow(
                        "Developed in ",
                        a(href = "https://cran.r-project.org/", target = "_blank", "R"),
                        " by ",
                        a(href = "https://www.greatersport.co.uk/", target = "_blank", "GreaterSport"),
                        " under the ",
                        a(href = "https://creativecommons.org/licenses/by-nc/4.0/legalcode", target = "_blank", "CC BY-NC"),
                        " licence"),
                    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #63666A; padding: 5px 20px; background-color: #FFFFFF"
                )
            )
        )
    )
)



# Log in page -------------------------------------------------------------
# 
# polished::secure_ui(ui,
#                     sign_in_page_ui = sign_in_ui_default(
#                         color = "#8c1d82",
#                         company_name = "GM Moving",
#                         logo_top = tags$div(style = "width: 300px; max-width: 100%; color: #FFF;", class =
#                                                 "text-center", h1("Schools Database", style = "margin-bottom: 0; margin-top: 30px;"))
#                     ))