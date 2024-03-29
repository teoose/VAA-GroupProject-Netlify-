#
# https://isss608-vaa-suanern.shinyapps.io/ShinyProjectAspatialPrototype/
# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, tidyverse, dplyr, leaflet, leaflet.extras, plotly, highcharter, 
               ggthemes, fresh, sf, spdep, tmap, tm, DT,
               ggridges, ggdist)

# read data files
final <- readRDS("data/final.rds")
joined <- readRDS("data/joined_data.rds")
mapping_rates <- readRDS("data/mapping_rates.rds")

joined2 <- readRDS("data/joined_data2.rds")
mapping_rates2 <- readRDS("data/mapping_rates2.rds")

maplayer <- sf::st_read(dsn = "data/geospatial", layer = "geoBoundaries-MMR-ADM1")


###############################   
########### MAIN BODY - START
###############################

#==========================================================  
# main header ---
header <- dashboardHeader(title = "Decoding Chaos")


# main sidebar ---
sidebar <- dashboardSidebar(
  tags$style(HTML("
      .main-sidebar{
        width: 250px;
      }
      .main-header > .navbar {
        margin-left: 250px;      
      }
      
      
      .box.box-solid.box-info>.box-header {
      color:#000000;
      background:#F8F8F8
                    }
      .box.box-solid.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      }
      .box.box-info>.box-header {
      color:black; 
      background:#F8F8F8
      }
      .box.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      background: #F8F8F8;
      }
                  ")),
  
  sidebarMenu(
    width = 100,
    menuItem("Aspatial Analysis", tabName = "Aspatial", icon = icon("globe")),
    menuItem("Geospatial Analysis", tabName = "Geospatial", icon = icon("circle-nodes")),
    menuItem("Confirmatory Analysis", tabName = "Confirmatory", icon = icon("clipboard-check")),
    menuItem("Visit ACLED data", icon = icon("send",lib='glyphicon'), 
             href = "https://acleddata.com/data-export-tool/")))


# customise theme ---
mytheme <- create_theme(
  adminlte_color(
    red = "#9A3E41"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#F8F8F8",
    dark_hover_bg = "#9A3E41",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF", 
    info_box_bg = "#FFF"
  )
)

# =============================    
########### EXPLORATORY - START
# =============================


#==========================================================  
# AspatialOverviewrow1 ---
#==========================================================  
AspatialOverviewrow1 <-  fluidRow(
  box(title = "Armed Conflict Incidents in Myanmar (2010 to 2023)",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      column(4,
             box(title = "Map Panel",
                 status = "info",
                 solidHeader = FALSE, 
                 collapsible = TRUE,
                 width = NULL,
                 sliderInput(inputId = "YearSlider_eo1", 
                             label = "Years:", 
                             min = 2010, 
                             max = 2023,
                             value = c(2010, 2023),
                             step = 1),
                 selectizeInput(inputId = "EventSelect_eo1",
                                label = "Select Event(s)",
                                choices = unique(as.character(final$event_type)),
                                # selected = c("Battles", "Violence against civilians"),
                                # options = list(maxItems=3),
                                multiple = TRUE),
                 selectizeInput(inputId = "AdminSelect_eo1",
                                label = "Select Administrative Region 1(s)",
                                choices = unique(as.character(final$admin1)),
                                # selected = c("Bago-West", "Bago-East", "Shan-South", "Yangon", "Rakhine", "Kachin", "Sagaing", "Mandalay", "Magway"),
                                multiple = TRUE),
                 actionButton("emapButton", "Update"),
                 # actionButton("emapresetButton", "Reset"),
                 hr(),
                 checkboxInput(inputId="ShowLine", label="Show Trend Line", value=FALSE),
                 conditionalPanel(condition="input.ShowLine",
                                  withSpinner(highchartOutput("line_eo1", height = "250px"))
                 ),
                 checkboxInput(inputId="MoreInfo", label="About Map", value=FALSE),
                 conditionalPanel(condition="input.MoreInfo",
                                  textOutput("abouttext"))
             )
      ),
      column(8,
        box(width = 12,  # Automatically adjust to full width
          align = "left",
          withSpinner(leafletOutput("emap_eo1", height = "650px", width = "100%"))
          )
        )
          )
          #),
          # absolutePanel(id = "controls", class = "panel panel-default",
          #              bottom = -40, left = 300, width = 250, fixed=TRUE,
          #              draggable = TRUE, height = "auto",
          #              box(title = "About",
          #                  status = "danger",
          #                  solidHeader = TRUE,
          #                  collapsible = TRUE,
          #                  width = NULL,
          #                  align = "justify",
          #                  textOutput("abouttext") 
          #              )
          # )      
      )



#==========================================================  
# AspatialDistributionrow1 ---
#==========================================================  
AspatialDistributionrow1 <-  fluidRow(
  column(2,
         box(title = "Desired Characteristics",
             status = "info",
             solidHeader = FALSE, 
             width = NULL,
             height = "900px",
             helpText("Filter options are applicable for both distribution"),
             sliderInput(inputId = "YearSelect_eg2", 
                         label = "Year:", 
                         min = 2010, max = 2023,
                         value = 2023),
             radioButtons(inputId = "Admin_eg2",
                          label = "Administrative Region",
                          choices = c("Level 1", "Level 2"),
                          selected = "Level 1",
                          inline = TRUE),
             radioButtons(inputId = "InciFata_eg2",
                          label = "Display (Rates)",
                          choices = c("Incidents", "Fatalities"),
                          selected = "Fatalities",
                          inline = TRUE),
             selectInput(inputId = "ColourSelect_eg2", 
                         label = "Colour Palette:", 
                         choices = c("Blues", "Greens", "Greys", "Oranges", "Reds"),
                         selected = "Reds"
             ),
             hr(),
             checkboxInput(inputId="ShowSDMStyle", label="Customise Spatial Map", value=FALSE),
             conditionalPanel(condition="input.ShowSDMStyle",
                              helpText("Filter options are applicable for spatial map distribution only"),
                              selectInput(inputId = "ClassificationSelect_eg2", 
                                          label = "Spatial Map Classification Type:",
                                          choices = c("boxmap", "equal", "kmeans", "pretty", "quantile"),
                                          selected = "kmeans"
                              ),
                              sliderInput(inputId = "ClassSelect_eg2", 
                                          label = "Number of Classes:",
                                          min = 3, max = 10,
                                          value = 5)
             ),
             hr(),
             checkboxInput(inputId="ShowRidgeStyle", label="Customise Density Ridge Plot", value=FALSE),
             conditionalPanel(condition="input.ShowRidgeStyle",
                              helpText("Filter option is applicable for density distribution only"),
                              selectInput(inputId = "RidgeStyle_eg2", 
                                          label = "Density Ridge Style:", 
                                          choices = c("Default", "Quantile", "Probability", "Tail Probability"),
                                          selected = "Default"
                              )
             )
  )),
  column(5,
         box(title = "Spatial Map Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,
             align = "left",
             withSpinner(tmapOutput("emap_eg2", height = "850px", width = "100%"))
         )   
  ),
  column(5,
         box(title = "Density Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,  # Automatically adjust to full width
             align = "left",
             withSpinner(plotOutput("box_ed1", height = "850px", width = "100%"))
             # withSpinner(plotOutput("hist_eg2", height = "850px", width = "100%"))
         )   
  )
)

AspatialDistributionrow2 <-  fluidRow(
  box(title = "Chart Interpretation",
      status = "danger",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = NULL,
      align = "justify",
      textOutput("spatialtext") 
  )
)

#==========================================================  
# AspatialSubTabs
#==========================================================  
AspatialSubTabs <- tabsetPanel(
  tabPanel("Overview", 
           AspatialOverviewrow1
  ),
  tabPanel("Aspatial Distribution Analysis", 
           AspatialDistributionrow1,
           AspatialDistributionrow2
   )
)



# =============================    
########### EXPLORATORY - END
# =============================

# main body ---
body <- dashboardBody(
  
  # use theme
  use_theme(mytheme),
  
  
  # =============================
  # tabItems - All Pages
  # =============================
  tabItems(
    # 1st tab content ---
    tabItem(tabName = "Aspatial",
            AspatialSubTabs
    ),
    # 2nd tab content ---
    tabItem(tabName = "Geospatial"
            #/// replace with fluidRow Names
    ),
    # 3rd tab content ---
    tabItem(tabName = "Confirmatory"
            #/// replace with fluidRow Names
    )
  )
)  



###############################   
########### MAIN BODY - END
###############################


# =============================    
########### UI - START
# =============================

# UI dashboard ---
ui <- dashboardPage(title = 'Armed Conflicts in Myanmar (2010 to 2023)', 
                    header, sidebar, body, skin='red')    

# =============================    
########### UI - END
# =============================


# =============================    
########### SERVER - START
# =============================
server <- function(input, output) {
  
  # =============================    
  # ASPATIAL ANALYSIS
  # =============================
  
  #==========================================================
  # Aspatial Map ---
  #==========================================================   
  final_explorespatial <- eventReactive(input$emapButton, {
    # req(input$YearSlider_eo1)
    # req(input$EventSelect_eo1)
    # req(input$AdminSelect_eo1)
    filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
      filter(event_type %in% input$EventSelect_eo1) %>%
      filter(admin1 %in% input$AdminSelect_eo1)
  })
  
  # Render Point Map --- 
  output$emap_eo1 <- renderLeaflet({
    
    incident_pts <- final_explorespatial()
    
    incident_pts %>%
      filter(fatalities > 0)
    
    cof <- colorFactor(c("#ff7e8a", "#394938", "#ffa500", 
                         "#0092ff", "#741b47", "#60dcb5"), 
                       domain=c("Battles", "Explosions/ Remote violence", 
                                "Protests", "Riots",
                                "Strategic developments", 
                                "Violence against civilians"))
    
    leaflet(incident_pts, options = leafletOptions(minZoom = 6, maxZoom = 8
                                                   # ,maxBounds = list(list(94, 19), list(102, 23))
                                                   )) %>% 
      addTiles('https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
      addPolygons(data = maplayer, color = "lightgrey", weight = 1, fillOpacity = 0.5) %>%
      setView(98, 21, zoom = 6) %>%
      addCircleMarkers(lat= ~latitude, lng = ~longitude, radius = ~(fatalities)^(1/2),
                       color = ~cof(event_type), weight = 1, fillOpacity = 0.1,
                       popup = paste( "<strong>", incident_pts$event_date, "</strong>", 
                                      "<br><strong>Country: </strong>",
                                      incident_pts$country, 
                                      "<br><strong>Sub-national Admin Region: </strong>",
                                      incident_pts$admin1, "/", 
                                      incident_pts$admin2, "/", 
                                      incident_pts$admin3, 
                                      "<br><strong>Event type: </strong>",
                                      incident_pts$event_type, 
                                      "<br><strong>Sub-event type: </strong>", 
                                      incident_pts$sub_event_type, 
                                      "<br><strong>Summary: </strong>", 
                                      incident_pts$notes,
                                      "<br><strong>Total Fatalities: </strong>",
                                      incident_pts$fatalities)) %>% 
      addLegend("bottomright", pal = cof, values = incident_pts$event_type, title = "Event Type")
  })
  
  output$abouttext <- renderText({ 
    "Armed conflicts due to political violence and coordinated attacks targeting innocent civilians, have been on the rise globally. 
      This threatens the public at both physical and psychological levels. 
      This visualisation UI helps: 
      (1) discover armed conflicts trends and 
      (2) conceptualise armed conflict spaces in Myanmar." 
  })
  
  
  
  #==========================================================
  # Aspatial Line Chart ---
  #========================================================== 
  final_exploreline <- eventReactive(input$emapButton, {
    # req(input$YearSlider_eo1)
    # req(input$EventSelect_eo1)
    # req(input$AdminSelect_eo1)
    filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
      filter(event_type %in% input$EventSelect_eo1) %>%
      filter(admin1 %in% input$AdminSelect_eo1)
    
  })
  
  output$line_eo1 <- renderHighchart({
    linedata <- final_exploreline()
    
    year_fata <- linedata %>%
      filter(fatalities > 0) %>%
      group_by(year) %>%
      select(year, fatalities, event_type, admin1) %>%
      summarise(total_fata = sum(fatalities),
                total_inci = n()) %>%
      ungroup()
    
    hc_plot1 <-  highchart() %>% 
      hc_add_series(year_fata, hcaes(x = as.factor(year), y = total_fata), type = "line", 
                    name = "Total Fatalities", color = "lightcoral") %>%
      hc_add_series(year_fata, hcaes(x = as.factor(year), y = total_inci), type = "line", 
                    name = "Total Incidents", color = "black") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
                 backgroundColor = "#FCFFC5",
                 borderWidth = 5,
                 pointFormat = "<b>{point.year}</b> 
                                 <br> Fatalities: <b>{point.total_fata}</b>
                                 <br> Incidents: <b>{point.total_inci}</b>"
      ) %>%
      # hc_title(text = "Armed Conflict Over The Years") %>% 
      hc_subtitle(text = paste("Year ",input$YearSlider_eo1[1], "-", input$YearSlider_eo1[2])) %>%
      hc_xAxis(title = list(text = paste(input$YearSlider_eo1[1], "-", input$YearSlider_eo1[2])), labels = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Frequency"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "lightcoral", width = 1, dashStyle = "Dash",
                 value = mean(year_fata$total_fata),
                 label = list(text = paste("Average fatalities:", round(mean(year_fata$total_fata))),
                              style = list(color = 'lightcoral', fontSize = 8)))))
    hc_plot1
  })
  #==========================================================
  # Aspatial Spatial Distribution Map ---
  #==========================================================  
  mapping_rates_exploredist <- reactive({
    req(input$InciFata_eg2)
    req(input$YearSelect_eg2)
    req(input$ClassSelect_eg2)
    req(input$ClassificationSelect_eg2)
    req(input$ColourSelect_eg2)
    req(input$Admin_eg2)
  })
  
  # Render Spatial Distribution Map --- 
  output$emap_eg2 <- renderTmap({
    
    if(input$InciFata_eg2 == "Incidents"){
      var <- paste("pct_inci_", input$YearSelect_eg2, sep = "")
    }
    else{
      var <- paste("pct_fata_", input$YearSelect_eg2, sep = "")
    }
    
    # Render boxmap classification
    if(input$ClassificationSelect_eg2 == "boxmap"){
      # create boxbreak function
      boxbreaks <- function(v,mult=1.5) {
        qv <- unname(quantile(v))
        iqr <- qv[4] - qv[2]
        upfence <- qv[4] + mult * iqr
        lofence <- qv[2] - mult * iqr
        # initialize break points vector
        bb <- vector(mode="numeric",length=7)
        # logic for lower and upper fences
        if (lofence < qv[1]) {  # no lower outliers
          bb[1] <- lofence
          bb[2] <- floor(qv[1])
        } else {
          bb[2] <- lofence
          bb[1] <- qv[1]
        }
        if (upfence > qv[5]) { # no upper outliers
          bb[7] <- upfence
          bb[6] <- ceiling(qv[5])
        } else {
          bb[6] <- upfence
          bb[7] <- qv[5]
        }
        bb[3:5] <- qv[2:4]
        return(bb)
      }
      
      # create get.var function
      get.var <- function(vname,df) {
        v <- df[vname] %>% st_set_geometry(NULL)
        v <- unname(v[,1])
        return(v)
      }
      
      # create boxmap function
      boxmap <- function(vnam, df, 
                         legtitle=NA,
                         mtitle= paste0("Box Map of ", vnam),
                         mult=1.5){
        var <- get.var(vnam,df)
        bb <- boxbreaks(var)
        tm_shape(df) +
          tm_polygons() +
          tm_shape(df) +
          tm_fill(vnam,title=legtitle,
                  breaks=bb,
                  palette=input$ColourSelect_eg2,
                  labels = c("lower outlier", 
                             "< 25%", 
                             "25% - 50%", 
                             "50% - 75%",
                             "> 75%", 
                             "upper outlier"))  +
          tm_layout(main.title = mtitle, 
                    title.position = c("left",
                                       "top")) +
          tm_view(set.zoom.limits = c(5,7))
      }
      
      if(input$Admin_eg2 == "Level 1"){
        box_inci <- boxmap(var, mapping_rates)
      }
      else{
        box_inci <- boxmap(var, mapping_rates2)
      }
      
    }
    # Render other classification types
    else{
      if(input$Admin_eg2 == "Level 1"){
        tmap_obj2 <- tm_shape(mapping_rates) +
          tm_fill(var, 
                  n = input$ClassSelect_eg2,
                  style = input$ClassificationSelect_eg2,
                  palette = input$ColourSelect_eg2, 
                  legend.hist = TRUE, 
                  legend.is.portrait = TRUE, 
                  legend.hist.z = 0.1) +
          tm_borders(lwd = 0.1, alpha = 1) +
          tm_layout(main.title = "Distribution of Armed Conflict Incidents in Myanmar \nKmeans classification",
                    title = "",
                    main.title.size = 1,
                    legend.height = 0.60,
                    legend.width = 5.0,
                    legend.outside = FALSE,
                    legend.position = c("left", "bottom")) +
          tm_view(set.zoom.limits = c(5,7))
      }
      else{
        tmap_obj2 <- tm_shape(mapping_rates2) +
          tm_fill(var, 
                  n = input$ClassSelect_eg2,
                  style = input$ClassificationSelect_eg2,
                  palette = input$ColourSelect_eg2, 
                  legend.hist = TRUE, 
                  legend.is.portrait = TRUE, 
                  legend.hist.z = 0.1) +
          tm_borders(lwd = 0.1, alpha = 1) +
          tm_layout(main.title = "Distribution of Armed Conflict Incidents in Myanmar \nKmeans classification",
                    title = "",
                    main.title.size = 1,
                    legend.height = 0.60,
                    legend.width = 5.0,
                    legend.outside = FALSE,
                    legend.position = c("left", "bottom")) +
          tm_view(set.zoom.limits = c(5,7))
      }
    }
  })
  
  ########################################  
  output$spatialtext <- renderText({ 
    paste(
      "A density ridge plot (left) provides insights to the distribution across Myanmar's
    different geographical subnational administrative region 1 or 2 based on the selected density style 
    (i.e. default, quantile, probability, tail probability).",
      "A choropleth map (right) visualises spatial pattern across Myanmar's
    different geographical subnational administrative region 1 or 2, which can be further customised 
    by adjusting the desired data classification type and number of classes (or data ranges)." 
    )
    
  })
  
  #==========================================================  
  # Aspatial Density Distributions ---
  #==========================================================  
  mapping_rates_exploredens <- reactive({
    req(input$Admin_eg2)
    req(input$InciFata_eg2)
    req(input$YearSelect_eg2)
    req(input$RidgeStyle_eg2)
    req(input$ColourSelect_eg2)
  })
  
  # Render density --- 
  output$box_ed1 <- renderPlot({
    years <- input$YearSelect_eg2
    
    if(input$Admin_eg2 == "Level 1"){
      datatable <- joined
    }
    else{
      datatable <- joined2
    }
    
    dens <- datatable %>%
      group_by(event_date, year, shapeName) %>%
      filter(year == years & !is.na(shapeName)) %>%
      mutate(total_inci = n(),
             total_fata = sum(fatalities),
             total_political = sum(political_count),
             total_violenceagainstcivilian = sum(civilian_count)) %>%
      ungroup()
    
    if(input$InciFata_eg2 == "Incidents"){
      var <- dens$total_inci
    }
    else{
      var <- dens$total_fata
    }
    
    # set colour for density ridge plots
    if(input$RidgeStyle_eg2 == "Default"){
      
      if(input$ColourSelect_eg2 == "Reds"){
        col <- "#F8B3AE"
      }
      else if(input$ColourSelect_eg2 == "Blues"){
        col <- "#7FB9DA"
      }
      else if(input$ColourSelect_eg2 == "Greens"){
        col <- "#85CC84"
      }
      else if(input$ColourSelect_eg2 == "Greys"){
        col <- "#A5A5A5"
      }
      else if(input$ColourSelect_eg2 == "Oranges"){
        col <- "#FD9A4E"
      }
      
    # plot density ridge plots
    ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)))) +
        geom_density_ridges(
          scale = 3,
          rel_min_height = 0.01,
          bandwidth = 3.4,
          alpha = 0.5,
          color = "white",
          fill = col
        ) +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +
        theme(axis.text.x = element_blank())
    }
    else if(input$RidgeStyle_eg2 == "Quantile"){
      ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)), 
                 fill = factor(stat(quantile))
             )) +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          calc_ecdf = TRUE, 
          quantiles = 4,
          quantile_lines = TRUE) +
        scale_fill_viridis_d(name = "Quartiles") +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +         
        theme(axis.text.x = element_blank())
    }
    else if(input$RidgeStyle_eg2 == "Probability"){
      ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)), 
                 fill = factor(stat(quantile))
             )) +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          calc_ecdf = TRUE, 
          quantiles = c(0.015, 0.985)
        ) +
        scale_fill_manual(
          name = "Probability",
          values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
          labels = c("(0, 0.015]", "(0.015, 0.985]", "(0.985, 1]")
        ) +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +         
        theme(axis.text.x = element_blank())
    }
    else{
      ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)), 
                 fill = 0.5 - abs(0.5-stat(ecdf)))) +
        stat_density_ridges(geom = "density_ridges_gradient", 
                            calc_ecdf = TRUE) +
        scale_fill_viridis_c(name = "Tail probability",
                             direction = -1) +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +         
        theme(axis.text.x = element_blank())
    }
    
    
  })

  
  #==========================================================  
  # Aspatial Histogram ---
  #==========================================================
  # mapping_rates_exploredens <- reactive({
  #   req(input$Admin_eg2)
  #   req(input$InciFata_eg2)
  #   req(input$YearSelect_eg2)
  #   req(input$ColourSelect_eg2)
  # })
  # 
  # # Render density --- 
  # output$hist_eg2 <- renderPlot({
  #   years <- input$YearSelect_eg2
  #   
  #   if(input$Admin_eg2 == "Level 1"){
  #     datatable <- mapping_rates
  #   }
  #   else{
  #     datatable <- mapping_rates2
  #   }
  # 
  #   if(input$InciFata_eg2 == "Incidents"){
  #     var <- paste("pct_inci_", input$YearSelect_eg2, sep = "")
  #   }
  #   else{
  #     var <- paste("pct_fata_", input$YearSelect_eg2, sep = "")
  #   }
  # 
  #   # set colour for density ridge plots
  #     if(input$ColourSelect_eg2 == "Reds"){
  #       col <- "#F8B3AE"
  #     }
  #     else if(input$ColourSelect_eg2 == "Blues"){
  #       col <- "#7FB9DA"
  #     }
  #     else if(input$ColourSelect_eg2 == "Greens"){
  #       col <- "#85CC84"
  #     }
  #     else if(input$ColourSelect_eg2 == "Greys"){
  #       col <- "#A5A5A5"
  #     }
  #     else if(input$ColourSelect_eg2 == "Oranges"){
  #       col <- "#FD9A4E"
  #     }
  # 
  # ggplot(data=datatable, 
  #               aes(x= var)) +
  #   geom_histogram(bins=20, 
  #                  color="black", 
  #                  fill= col) +
  #   # ggtitle("Histogram") +
  #   theme_classic()
  
  # })
  
}
# =============================    
########### SERVER - END
# =============================


# =============================    
########### RUN APPLICATION
# =============================
shinyApp(ui = ui, server = server)
