

function(input,output, session){

# Maps --------------------------------------------------------------------
## Dataset selection ----
    domain <- reactive({
        if (input$domain == "cypal1718") {
            data <- cypal1718
        } else if (input$domain == "cypal1819") {
            data <- cypal1819
        } else if (input$domain == "cypal1920") {
            data <- cypal1920
        } else if (input$domain == "ncmprec") {
            data <- NCMPRec
        } else if (input$domain == "ncmp6") {
            data <- NCMP6
        } else if (input$domain == "ncmprecMSOA") {
            data <- obesityRec
        } else if (input$domain == "ncmp6MSOA") {
            data <- obesity6
        } else if (input$domain == "readiness") {
            data <- readiness
        } else if (input$domain == "readiness_fsm") {
            data <- readiness_fsm
        } else if (input$domain == "care") {
            data <- care
        } else if (input$domain == "neet") {
            data <- neets
        } else if (input$domain == "homeless") {
            data <- homeless
        } else if (input$domain == "housing") {
            data <- imd %>% filter(index_domain == 'Barriers to Housing and Services')
        } else if (input$domain == "crime") {
            data <- imd %>% filter(index_domain == 'Crime')
        } else if (input$domain == "education") {
            data <- imd %>% filter(index_domain == 'Education, Skills and Training')
        } else if (input$domain == "employment") {
            data <- imd %>% filter(index_domain == 'Employment')
        } else if (input$domain == "idaci") {
            data <- imd %>% filter(index_domain == 'Income Deprivation Affecting Children')
        } else if (input$domain == "imd") {
            data <- imd %>% filter(index_domain == 'Index of Multiple Deprivation')
        } else if (input$domain == "poverty"){
            data <- poverty
        } else if (input$domain == "pvtGreenspace") {
            data <- privateGreenspace
        } else if (input$domain == "pubGreenspace") {
            data <- publicGreenspace
        }
        return(data)
    })
## Palette for map ----    
    palette <-  reactive({
        if (input$domain == "cypal1718") {
            pal <- ~col(cypal1718$`Less Active`)
        } else if (input$domain == "cypal1819") {
            pal <- ~col(cypal1819$`Less Active`)
        } else if (input$domain == "cypal1920") {
            pal <- ~col(cypal1920$`Less Active`)
        } else if (input$domain == "ncmprec") {
            pal <- ncmppal(NCMPRec$Value)
        } else if (input$domain == "ncmp6") {
            pal <- ncmppal(NCMP6$Value)
        } else if (input$domain == "ncmprecMSOA") {
            pal <- obesitypal(obesityRec$Value)
        } else if (input$domain == "ncmp6MSOA") {
            pal <- obesitypal(obesity6$Value)
        } else if (input$domain == "readiness") {
            pal <- readiness_pal(readiness$Value)
        } else if (input$domain == "readiness_fsm") {
            pal <- readiness_fsm_pal(readiness_fsm$Value)
        } else if (input$domain == "care") {
            pal <- care_pal(care$Value)
        } else if (input$domain == "neet") {
            pal <- neets_pal(neets$Value)
        } else if (input$domain == "homeless") {
            pal <- homeless_pal(homeless$Rate.of.youth.homelessness..16.24.)
        } else if (input$domain == "housing") {
            pal <- ~ imd_pal(housing$decile)
        } else if (input$domain == "crime") {
            pal <- ~ imd_pal(crime$decile)
        } else if (input$domain == "education") {
            pal <- ~ imd_pal(education$decile)
        } else if (input$domain == "employment") {
            pal <- ~ imd_pal(employment$decile)
        } else if (input$domain == "idaci") {
            pal <- ~ imd_pal(idaci$decile)
        } else if (input$domain == "imd") {
            pal <- ~ imd_pal(imd$decile)
        } else if (input$domain == "poverty") {
            pal <- poverty_pal(poverty$`Percentage.2018.19`)
        } else if (input$domain == "pvtGreenspace") {
            pal <- privateGreenspace_pal(privateGreenspace$Total..Percentage.of.adresses.with.private.outdoor.space)
        } else if (input$domain == "pubGreenspace") {
            pal <- publicGreenspace_pal(publicGreenspace$Average.number.of..Parks..Public.Gardens..or.Playing.Fields.within.1.000.m.radius)
        }
        return(pal)
    })
## Colours for legend ----    
    colours <- reactive({
        if ((input$domain == "cypal1718")|(input$domain == "cypal1819")|(input$domain == "cypal1920")|(input$domain == "ncmprec")|
            (input$domain == "ncmp6")|(input$domain == "care")|
            (input$domain == "homeless")) {
            col <- col_6
        } else if ((input$domain == "readiness")|(input$domain == "readiness_fsm")|
                   (input$domain == "neet")|(input$domain == "pubGreenspace")) {
            col <- col_7
        } else if (input$domain == "poverty") {
            col <- col_9
        } else if ((input$domain == "ncmprecMSOA")|(input$domain == "ncmp6MSOA")){
            col <- col_11
        } else if ((input$domain == "housing")|(input$domain == "crime")|(input$domain == "education")|
                   (input$domain == "employment")|(input$domain == "idaci")|(input$domain == "imd")|
                   (input$domain == "pvtGreenspace")) {
            col <- col_10
        }
        return(col)
    })
## Legend labels ----    
    labels <- reactive({
        if ((input$domain == "cypal1718")|(input$domain == "cypal1819")|input$domain == "cypal1920") {
            lab <- c("25-30%", "30-35%", "35-40%", "40-45%", "45-50%", "50-55%")
        } else if ((input$domain == "ncmprec")|(input$domain == "ncmp6")) {
            lab <- c("5-10%", "10-15%", "15-20%", "20-25%", "25-30%", "25-30%")
        } else if ((input$domain == "ncmprecMSOA")|(input$domain == "ncmp6MSOA")) {
            lab <- c("0-5%", "5-10%", "10-15%", "15-20%", "20-25%", "25-30%", "25-30%", "30-35%", "35-40%", "40-45%", "45-50%")
        } else if (input$domain == "readiness") {
            lab <- c("64-66%", "66-68%", "68-70%", "70-72%", "72-74%", "74-76%", "76-78%")
        } else if (input$domain == "readiness_fsm") {
            lab <- c("44-47%", "47-50%", "50-53%", "53-56%", "56-59%", "59-62%", "62-65%")
        } else if (input$domain == "homeless") {
            lab <- c("0.5-1.0%", "1.0-1.5%", "1.5-2.0%", "2.0-2.5%", "2.5-3.0%", "3.0-3.5%")
        } else if ((input$domain == "housing")|(input$domain == "crime")|(input$domain == "education")|
                   (input$domain == "employment")|(input$domain == "idaci")|(input$domain == "imd")) {
            lab <- c("10 (least deprived)","9","8","7","6","5","4","3","2","1 (most deprived)")
        } else if (input$domain == "poverty") {
            lab <- c("10-15%", "15-20%", "20-25%", "25-30%", "25-30%", "30-35%", "35-40%", "40-45%", "45-50%")
        } else if (input$domain == "pvtGreenspace") {
            lab <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
        } else if (input$domain == "pubGreenspace") {
            lab <- c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14")
        } else if (input$domain == "care") {
            lab <- c("40-60", "60-80", "80-100", "100-120", "120-140", "140-160")
        } else if (input$domain == "neet") {
            lab <- c("3-4%", "4-5%", "5-6%", "6-7%", "7-8%", "8-9%", "9-10%")
        }
        return(lab)
    })
## Title for legend ----    
    title <-  reactive({
        if (input$domain == "cypal1718") {
            t <-  "Less Active (2017-18)"
        } else if (input$domain == "cypal1819") {
            t <- "Less Active (2018-19)"
        } else if (input$domain == "cypal1920") {
            t <- "Less Active (2019-20)"
        } else if (input$domain == "ncmprec") {
            t <- "Prevalence of obesity (including severe obesity): Reception"
        } else if (input$domain == "ncmp6") {
            t <- "Prevalence of obesity (including severe obesity): Year 6"
        } else if (input$domain == "ncmprecMSOA") {
            t <- "Prevalence of obesity (including severe obesity): Reception"
        } else if (input$domain == "ncmp6MSOA") {
            t <- "Prevalence of obesity (including severe obesity): Year 6"
        } else if (input$domain == "readiness") {
            t <- "School readiness: percentage of children achieving a good level of development at the end of Reception"
        } else if (input$domain == "readiness_fsm") {
            t <- "School Readiness: percentage of children with free school meal status achieving a good level of development at the end of Reception"
        } else if (input$domain == "care") {
            t <- "Children in Care (per 10,000)"
        } else if (input$domain == "neet") {
            t <- "16-17 Year Olds NEET"
        } else if (input$domain == "homeless") {
            t <- "Rate of youth homelessness (16-24)"
        } else if (input$domain == "housing") {
            t <- "Barriers to Housing and Services"
        } else if (input$domain == "crime") {
            t <- "Crime"
        } else if (input$domain == "education") {
            t <- "Education, Skills and Training"
        } else if (input$domain == "employment") {
            t <- "Employment"
        } else if (input$domain == "idaci") {
            t <- "Income Deprivation Affecting Children"
        } else if (input$domain == "imd") {
            t <- "Index of Multiple Deprivation"
        } else if (input$domain =="poverty") {
            t <- "Childhood Poverty (After Housing Costs)"
        } else if (input$domain == "pvtGreenspace") {
            t <- "Percentage of adresses with private outdoor space"
        } else if (input$domain == "pubGreenspace") {
            t <- "Average number of  Parks, Public Gardens, or Playing Fields within 1km"
        }
        return(t)
    })
## Source ----    
    source <-  reactive({
        if (input$domain == "cypal1819") {
            con <- "Source: Sport England Active Lives Children and Young People Survey, 2018/19"
        } else if (input$domain == "cypal1718") {
            con <- "Source: Sport England Active Lives Children and Young People Survey, 2017/18"
        } else if (input$domain == "cypal1920") {
            con <- "Source: Sport England Active Lives Children and Young People Survey, 2019/20"
        } else if ((input$domain == "ncmprec")|(input$domain == "ncmp6")) {
            con <- "Source: National Child Measurement Programme, 2019/20"
        } else if ((input$domain == "ncmprecMSOA")|(input$domain == "ncmp6MSOA")) {
            con <- "Source: National Child Measurement Programme, 2017/18 - 2019/20"
        } else if ((input$domain == "readiness")|(input$domain == "readiness_fsm")) {
            con <- "Source: Department for Education, 2018/19"
        } else if ((input$domain == "neet")) {
            con <- "Source: Department for Education, 2019"
        }else if (input$domain == "care") {
            con <- "Source: Department for Education, 2020"
        } else if (input$domain == "homeless") {
            con <- "Source: CentrePoint, 2019/20" 
        } else if ((input$domain == "housing")|(input$domain == "crime")|(input$domain == "education")|
                   (input$domain == "employment")|(input$domain == "idaci")|(input$domain == "imd")) {
            con <- "Source: English Indices of Deprivation 2019"
        } else if (input$domain == "poverty") {
            con <- "Source: End Child Poverty, 2018/19"
        } else if ((input$domain == "pvtGreenspace") | (input$domain == "pubGreenspace")) {
            con <- "Source: Office for National Statistics, 2020"
        }
        return(con)
    })
## Reactive filtering ----    
    observe({
        updateSelectInput(session, "answer",
                          choices = 
                              if (input$question == 'Is this school involved in any other projects?') {
                                  unique(scan(text=schoolsdb$`Is this school involved in any other projects?`, what='', sep='|'))
                              }
                          else {unique(schoolsdbFiltered[input$question])}
        )
        
    })
    
    df <- reactive({
        if (input$question == 'Is this school involved in any other projects?') {
            df <-  filter(schoolsdb, grepl(input$answer, `Is this school involved in any other projects?`))
        }
        else {
            df <- schoolsdb[schoolsdbFiltered[[input$question]] %in% input$answer,]
        }
        return(df)
    })
    
    df2 <- reactive({
        ss192 %>% dplyr::filter(object_of_search %in% input$crime)
    })
    

## Map Generation ----   
    
    output$map <- renderLeaflet({
        shiny::validate(need(nrow(domain()) != 0, message = FALSE))
        
        bbox <- st_bbox(lsoa) %>% as.vector()
        
        
        leaflet() %>%
            setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
            addTiles(
                options = tileOptions(minZoom = 9, maxZoom = 25)
            ) %>% 
            addLogo("https://www.greatersport.co.uk/media/3405/gs-supporting-gm-moving.png",
                    url = "https://www.greatersport.co.uk/",
                    alpha = 1,
                    width = 179,
                    position = "topleft") %>%
            addSearchOSM() %>%
            addPolygons(
                data = gm_la,
                weight = 2,
                opacity = 1,  
                color = 'black', 
                fillOpacity = 0
            )%>%
            addPolygons(
                data = domain(),
                fillColor = palette(),
                weight = 1,
                opacity = 1,
                color = "#FFF",
                dashArray = "1",
                fillOpacity = 0.5,
                highlight = highlightOptions(
                    weight = 3,
                    color = "#FFF",
                    fillOpacity = 1,
                    bringToFront = TRUE
                )
            ) %>%
            addPolygons(
                data = lads,
                weight = 2,
                opacity = 1,  
                color = 'black', 
                fillOpacity = 0
            ) %>%  
            addCircleMarkers(data = df2(),
                             lng = ~longitude, lat = ~latitude,
                             popup = ~object_of_search,
                             radius = 0.5,
                             color = "#84bd00") %>%

            addMarkers(data = df(),
                       popup = paste0(
                           "<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:200px'>",
                           "<br>",'<strong>' ,'<h3>',schoolsdb$EntityTitle,'</strong>','</h3>',
                           '<i>','<u>',"Overview",'</i>','</u>',
                           "<br>URN:" ,schoolsdb$`URN.x`,
                           "<br>Borough: " ,schoolsdb$Borough,
                           "<br>Phase: " ,schoolsdb$`School Type - Education Phase`,
                           "<br>School Type: " ,schoolsdb$`Type of School`,
                           "<br>Academy/Trust: " ,schoolsdb$`Are they part of an academy (single/multi) or trust`,
                           "<br>Academy/Trust Name: " ,schoolsdb$`Which academy/trust are they part of`,
                           "<br>Cluster: " ,schoolsdb$`Are they part of a cluster?`,
                           "<br>Nursery Attached: " ,schoolsdb$`Does this School have a Nursery attached?`,
                           "<br>Pupils Eligible for Free School Meals: " ,schoolsdb$`Percentage of pupils eligible for Free School Meals`,
                           "<br>Pupils Eligible for Pupil Premium: " ,schoolsdb$`Percentage of pupils eligible for pupil premium`,
                           "<br>IMD Decile (1=most deprived): " ,schoolsdb$`IMD Decile`,
                           "<br>Ofsted: " ,schoolsdb$`Ofsted rating`,    
                           "<br>Primary Premium Funding: ?" ,schoolsdb$`How much primary premium do they receive?`,
                           "<br>PESSPA Funding: ?" ,schoolsdb$`How much PESSPA do they receive?`,
                           "<br>",'<i>','<u>',"Daily Mile",'</i>','</u>',
                           "<br>Signed up to The Daily Mile: ", schoolsdb$`Is this School signed up to the Daily Mile?`,
                           "<br>Signed up the The Daily Mile Destinations: ", schoolsdb$`Have they signed up for The Daily Mile Destinations?`,
                           "<br>",'<i>','<u>', "School Games",'</i>','</u>',
                           "<br>Involved in L2 School Games: ", schoolsdb$`Does this school get involved with the School Games at Level 2?`,
                           "<br>School Games Mark: ", schoolsdb$`What level of School Games mark has this school achieved?`,
                           "<br>",'<i>','<u>', "Active Lives",'</i>','</u>',
                           "<br>Selected: ", schoolsdb$`Have they been selected (term)?`,
                           "<br>Opted In: ", schoolsdb$`Has this school opted into the Active Lives CYP Survey?`,
                           "<br>Completed: ", schoolsdb$`Have they completed the survey`,
                           "<br>",'<i>','<u>', "Open Facilities",'</i>','</u>',
                           "<br>Facilities Open: ", schoolsdb$`Are their facilities open?`,
                           "<br>Facilities Quality: ", schoolsdb$`Overall facilities quality`)
            ) %>%
            addLegend(
                position = "bottomright",
                title = title(),
                opacity = 1,
                colors = colours(),
                labels = labels()
            ) %>%
            addControl(
                tags$em(source()), position = "bottomleft"
            ) %>%
            
            onRender(
                " function(el, t) {
        var myMap = this;
        myMap._container.style['background'] = '#ffffff';}"
            )
        
    })
    

    
    observeEvent(input$go, {
        screenshot(selector = "#map", filename = "Dashboard Map")
    })

# Value Boxes -------------------------------------------------------------

    
    output$noU25 <- renderValueBox({
        valueBox(
            paste0(no_U25), HTML("<br/>U25's live in Greater Manchester"), 
            icon = icon("users"),
            color = "fuchsia"
        )
    })
    
    output$Schools <- renderValueBox({
        valueBox(
            paste0(schools), HTML("<br/>Schools in Greater Manchester"), 
            icon = icon("graduation-cap"),
            color = "purple"
        )
    })
    
    output$Trusts <- renderValueBox({
        valueBox(
            paste0(trusts), HTML("Different trusts and academies<br/>operating in Greater Manchester"),
            icon = icon("school"),
            color = "fuchsia"
        )
    })
    
    output$DM <- renderValueBox({
        valueBox(
            icon = icon("walking"),
            paste0(DM), HTML("<br/>Schools signed up to The Daily Mile<sup>TM</sup> <br/>"),
            color = "fuchsia"
        )
    })
    
    output$SGL2 <- renderValueBox({
        valueBox(
            paste0(SGL2), HTML("Schools who have taken part in a borough <br/> level School Games event"),
            icon = icon("award"),
            color = "purple"
        )
    })
    
    output$SGL3 <- renderValueBox({
        valueBox(
            paste0(SGL3), HTML("Schools who have taken part in a county <br/> level School Games event"),
            icon = icon("medal"),
            color = "fuchsia"
        )
    })
    
    output$ALC <- renderValueBox({
        valueBox(
            paste0(ALC), HTML("Schools who have have completed the <br/> Active Lives Children & Young People survey"),
            icon = icon("clipboard"),
            color = "purple"
        )
    })
    

# Graphs ------------------------------------------------------------------

## Dataset ----    
    topic <- reactive({
        if (input$topic == "ncmprec_time") {
            data <- NCMPRec_time
        } else if (input$topic == "ncmp6_time") {
            data <- NCMP6_time
        } else if (input$topic == "readiness_time") {
            data <- readiness_time
        } else if (input$topic == "readiness_fsm_time") {
            data <- readiness_fsm_time
        } else if (input$topic == "neet_time") {
            data <- neets_time
        } else if (input$topic == "cypal_time") {
            data <- cypal_time
        } else if (input$topic == "al_time") {
            data <- al_time
        } else if (input$topic == "care_time") {
            data <- care_time
        } else if (input$topic == "poverty_time") {
            data <- poverty_time
        }
        return(data)
    })
## Graph Title ----    
    graphTitle <- reactive({
        if (input$topic == "ncmprec_time") {
            t <- "Proportion of Children Classified as Overweight in Reception"
        } else if (input$topic == "ncmp6_time") {
            t <- "Proportion of Children Classified as Overweight in Year 6"
        } else if (input$topic == "readiness_time") {
            t <- "Proportion of Children Classified as Achieving a Good Level of School Readiness"
        } else if (input$topic == "readiness_fsm_time") {
            t <- "Proportion of Children on Free School Meals Classified as Achieving a Good Level of School Readiness"
        } else if (input$topic == "neet_time") {
            t <- "Proportion of 16 and 17 Year Olds Not in Education, Employment or Training"
        } else if (input$topic == "cypal_time") {
            t <- "Proportion of Children and Young People Classified as Inactive"
        } else if (input$topic == "al_time") {
            t <- "Proportion of Adults Classified as Inactive"
        } else if (input$topic == "care_time") {
            t <- "Number of Children in Care"
        } else if (input$topic == "poverty_time") {
            t <- "Proportion of Children and Young People Living in Poverty After Housing Costs"
        }
        return(t)
    })
## Source ----    
    source <-  reactive({
        if (input$topic == "al_time") {
            s <- "Source: Sport England Active Lives Survey"
        } else if (input$topic == "cypal_time") {
            s <- "Source: Sport England Active Lives Children and Young People Survey"
        } else if ((input$topic == "ncmprec_time")|(input$topic == "ncmp6_time")) {
            s <- "Source: National Child Measurement Programme"
        } else if ((input$topic == "readiness_time")|(input$topic == "readiness_fsm_time")|
                   (input$topic == "care_time")|(input$topic == "neet_time")) {
            s <- "Source: Department for Education"
        } else if (input$topic == "poverty_time") {
            s <- "Source: End Child Poverty"
        }
        return(s)
    })
## Area selection ----    
    area <- reactive({
        topic() %>% filter(AreaName %in% input$area)
    })
## Y axis ----    
    ylab <- reactive({
        if ((input$topic == "cypal_time")|(input$topic == "al_time")|(input$topic == "ncmprec_time")|
            (input$topic == "ncmp6_time")|(input$topic == "readiness_time")|(input$topic == "readiness_fsm_time")|
            (input$topic == "neet_time")|(input$topic == "poverty_time")) {
            y <- "Percentage (%)"
        } else if (input$topic == "care_time") {
            y <- "Rate per 10,00-"
        }
        return(y)
    })
## Graph plotting ----    
    output$chart <- renderPlot({
        if (input$area %in% "England") {
            line_size <- 2
        } else {
            line_size <- 1
        }
        
        ggplot(area(), aes(x=Timeperiod, y=Value, group = AreaName)) +
            geom_line(aes(color = AreaName), size = line_size) +
            geom_point(aes(color = AreaName)) +
            labs(x = "Time", y = "Rate (%)", col="Area") +
            ggtitle(graphTitle()) +
            theme_GMM() +
            scale_colour_manual(values = GMM_cols %>% unname)

    })
## Text to sit under graph ----
### Earliest release ----
    early <- reactive({
        if (input$topic == "ncmprec_time") {
            min <- NCMPRec_time %>% filter(Timeperiod == "2006/07")
        } else if (input$topic == "ncmp6_time") {
            min <- NCMP6_time %>% filter(Timeperiod == "2006/07")
        } else if (input$topic == "readiness_time") {
            min <- readiness_time %>% filter(Timeperiod == "2012/13")
        } else if (input$topic == "readiness_fsm_time") {
            min <- readiness_fsm_time %>% filter(Timeperiod == "2012/13")
        } else if (input$topic == "neet_time") {
            min <- neets_time %>% filter(Timeperiod == "2016")
        } else if (input$topic == "cypal_time") {
            min <- cypal_time %>% filter(Timeperiod == "2017-18")
        } else if (input$topic == "al_time") {
            min <- al_time %>% filter(Timeperiod == "Nov 15-16")
        } else if (input$topic == "care_time") {
            min <- care_time %>% filter(Timeperiod == "2011")
        } else if (input$topic == "poverty_time") {
            min <- poverty_time %>% filter(Timeperiod == "2014/15")
        }
        return(min)
    })
### Latest release ----    
    latest <- reactive({
        if (input$topic == "ncmprec_time") {
            max <- NCMPRec_time %>% filter(Timeperiod == "2019/20")
        } else if (input$topic == "ncmp6_time") {
            max <- NCMP6_time %>% filter(Timeperiod == "2019/20")
        } else if (input$topic == "readiness_time") {
            max <- readiness_time %>% filter(Timeperiod == "2018/19")
        } else if (input$topic == "readiness_fsm_time") {
            max <- readiness_fsm_time %>% filter(Timeperiod == "2018/19")
        } else if (input$topic == "neet_time") {
            max <- neets_time %>% filter(Timeperiod == "2019")
        } else if (input$topic == "cypal_time") {
            max <- cypal_time %>% filter(Timeperiod == "2019-20")
        } else if (input$topic == "al_time") {
            max <- al_time %>% filter(Timeperiod == "Nov 18-19")
        } else if (input$topic == "care_time") {
            max <- care_time %>% filter(Timeperiod == "2019")
        } else if (input$topic == "poverty_time") {
            max <- poverty_time %>% filter(Timeperiod == "2018/19")
        }
        return(max)
    })
### Graph title ----    
    graph_title <- reactive({
        if (input$topic == "ncmprec_time") {
            t <- "prevalence of obesity in reception age children"
        } else if (input$topic == "ncmp6_time") {
            t <- "prevalence of obesity in year 6 children"
        } else if (input$topic == "readiness_time") {
            t <- "proportion of children achieving a good level of development at the end of reception"
        } else if (input$topic == "readiness_fsm_time") {
            t <- "percentage of children with free school meal status achieving a good level of development at the end of reception"
        } else if (input$topic == "neet_time") {
            t <- "proportion of 16-17 Year Olds NEET"
        } else if (input$topic == "cypal_time") {
            t <- "inactivity rate amongst children and young people"
        } else if (input$topic == "al_time") {
            t <- "inactivity rate amongst adults"
        } else if (input$topic == "care_time") {
            t <- "proportion of children in care (per 100,000)"
        } else if (input$topic == "poverty_time") {
            t <- "proportion of children in poverty, after housing costs,"
        }
        return(t)
    })
### Reactive area and value ----    
    x <- reactive({
        early() %>% filter(AreaName %in% input$area) 
    })
    
    y <- reactive({
        latest() %>% filter(AreaName %in% input$area) 
    })
    
    diff <- reactive ({
        y()$Value-x()$Value
    })
    
### Text ----    
    output$change <- renderText({
        paste("Since baseline the ", graph_title(), "in ", input$area, " has ", 
              (if ((y()$Value-x()$Value)>0){
                  print("increased")
              } else {
                  print("decreased")
              }),
              " by ", format(round(abs(diff()), 2), nsmall = 2), "%." )
    })
## Screenshot ----
    observeEvent(input$go, {
        screenshot(selector = "#chart", filename = "chart")
    })
    
    
}
