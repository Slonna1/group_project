library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(tidyverse)
library(here)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(sf)
library(leaflet)
library(leaflet.extras)
library(rgdal)

####################################################################
###############
###############  Simone Famiano

dashboard_data_table <- read_csv(
  here("clean_data/dashboard_data_table.csv"))
tot_and_avg_stays <- read_csv(
  here("clean_data/national_total_stays_and_avg_length.csv"))

####################################################################
###############
###############  John Hios
# Load data
kpi_beds <- read_csv(here("clean_data/kpi_beds_clean.csv")) %>%
  mutate(across(is.numeric, round, digits=1))  

kpi_diff_beds <- read_csv(here("clean_data/kpi_diff_beds_clean.csv")) %>%
  mutate(across(is.numeric, round, digits=1)) 

# Get drop-down list menu options (and delete any occurring NAs)
hb_name_labels <- unique(kpi_beds$HBName) %>% 
  discard(is.na)


kpi_labels <- c("All Staffed Beds", 
                "Total Occupied Beds", 
                "Average Available Staffed Beds", 
                "Average Occupied Beds", 
                "Percentage Occupancy")

####################################################################
###############
###############  Grant Walker
data <- read_csv(here("clean_data/phs_admissions_data_clean.csv"))
hb_name <- unique(data$hb_name)
hb_urban <- unique(data$urban_rural)

####################################################################
###############
###############  Jack Patton
raw_data <- read_csv(
  here("raw_data/beds_by_nhs_board_of_treatment_and_specialty.csv"))

scotlandpoly <- readOGR(
  here("clean_data/HB_WGS_84_EPSG4326/reprojected_hb.shp"))

health_boards <- read_csv(here("raw_data/HB_scotland.csv"))

hospitals <- read_csv(here("raw_data/hospitals_scotland.csv")) %>%
  filter(!is.na(XCoordinate | YCoordinate))


hosptials_data <- inner_join(raw_data, hospitals, by = "Location")
HB_data <- inner_join(raw_data, health_boards, by = c("Location" = "HB"))

HB_data = select(HB_data, Quarter, PercentageOccupancy, TotalOccupiedBeds, 
                 AverageOccupiedBeds, AllStaffedBeds, Location, SpecialtyName, 
                 HB)
corrected_names = data.frame(scotlandpoly$HBName, scotlandpoly$HBCode)

HB_data <- inner_join(HB_data, corrected_names, 
                      by = c("Location" = "scotlandpoly.HBCode"))

hospitals_longlat <- hospitals %>%
  st_as_sf(coords = c("XCoordinate", "YCoordinate"), 
           crs = 27700) %>% st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()
hospitals <- cbind(hospitals, hospitals_longlat)

####################################################################

ui <- fluidPage(
  tabsetPanel(
    
    tags$head(tags$style('
    body {
      font-family: Arial; 
      font-size: 20px; 
    }'
    )),
    
    tabPanel("Intro",
             
             h1(strong("Intro"), align="center", 
                style = "font-size:100px;"),
             
             br(),
             br(),
             
    p("Our dashboard contains 4 main tabs providing insights on various aspects of secondary care in the acute hospital sector from 2020 onward."),
      
      
    p("Our topic is to consider the impact that winter may have on the health care, primarily the hospital (acute care) sector in Scotland."), 
    
            br(),
      
    p("This is achieved by answering the following questions:"),
    
            br(),
      
        
      p(HTML("- <em> Capacity – what is happening to the number of beds over the period? Perhaps think about the specialities these are if there has been specific variation? </em>")),
      
        
      p(HTML("- <em> Are there any demographic groups that are driving the activity? </em> ")),
      
        
      p(HTML("- <em> What does this mean for the number of people in intensive care and the number of beds? </em> ")),
      
        
      p(HTML("- <em>  How significant are the differences in hospital activity across the country? Does urban/rural have an impact or is it related to the size and capacity of the health board? </em> ")),
    
            br(),
            br(),
    
    p("The dashboard outlines our topic in terms of:"),
    
            br(),
        
      
      p(HTML("- <em> Hospital Capacity (in terms of patients) across the National Health Boards in Scotland. </em> ")),
      
        
      p(HTML("- <em> Demographics on total stays and average length of stays on a countrywide level.</em>")),
      
        
      p(HTML("- <em> Bed availability/occupancy per National Health Board and countrywide. </em>")),
        
        
      p(HTML("- <em> Variation per admission type and differences between rural and urban areas. </em>")),
    
    ),
   
    tabPanel("Hospital Capacity",
             
             
             # Application title
             
             h1(strong("Hospital Capacity By Health Board"), align="center", 
                                             style = "font-size:100px;"),
             
             #output print
             verticalLayout(
               wellPanel("Region Selected: ", textOutput("region_selected")),
               
               # the map
               mainPanel(
                 leafletOutput("scotlandHM"),
                 br(),
                 br(),
                 plotOutput("plotteddata")
               )
             ),
             
             br(),
             br(),
             
             
      
    ),
    
    
    tabPanel("Demographics",

             
             # Application title
             
             h1(strong("Demographics Data"), align="center", 
                                             style = "font-size:100px;"),
             
            
   
   br(),
   br(),
   
   fluidRow(  
     column(3,
            
            
            
            
            pickerInput("age_input", 
                        "Select Age Groups", 
                        choices = unique(tot_and_avg_stays$age),
                        multiple =  TRUE,
                        selected = unique(tot_and_avg_stays$age)),
     ),
     
     
     
     column(9,
            pickerInput("quarter_input", 
                        "Select Quarter", 
                        choices = unique(tot_and_avg_stays$quarter),
                        multiple =  TRUE,
                        selected = unique(tot_and_avg_stays$quarter))
            
     )
   ),
   
   
   plotOutput("tot_stays_plot"),
   
   br(),
   br(),
   
   
   plotOutput("avg_length_plot"),
   
   
   
   dataTableOutput("table_output"),
   
   
    ),
   
   
   
   tabPanel("Acute Care vs. NHS Bed Numbers",
            h1(strong("Acute Care vs. NHS Bed Numbers"), align = "center", 
                                                         style = 
                 "font-size:100px;"),
           
            tags$head(tags$style('
       body {
       font-family: Arial; 
       font-size: 16px;
       }'
            )),
       
       br(),
       br(),
       
       # ADD theme here
       # theme = shinytheme("united"),
       
       fluidRow(  
         column(6,
                selectInput("nhs_board_input",
                            "Select Board",
                            choices = hb_name_labels
                ),
         ),  
         
         column(6, 
                selectInput("kpi_input",
                            "Select Key Performance Index (KPI)",
                            choices = kpi_labels
                )
         ), 
         
       ),
       
       br(),
       br(),
       
       # Show a plot of the generated distribution
       plotOutput("trend_plot"),
       br(),
       br(),
       plotOutput("diff_plot")
       
   ), 
   
   tags$head(tags$style('
    body {
      font-family: Arial; 
      font-size: 20px; 
    }'
   )),
   
   tabPanel("National Variation",
            
            h1(strong("National Variation In Hospital Activity"), 
               align ="center", 
               style = "font-size:100px;"),
            
            fluidRow(
              
              titlePanel(h2("Health Board", style = "font-size:50px;")),
              br(),
              p("Explore the differences between (left) and within (right) the 14 Health boards of Scotland. Within a Health board the variation in admissions is shown."),
              br(),
              column(6, 
                     pickerInput(inputId = "multi_hb_input", 
                                 label = "Select Health Boards", 
                                 choices = unique(data$hb_name),
                                 multiple = TRUE,
                                 selected = c("Ayrshire and Arran", 
                                              "Borders"))),
              
              column(6,
                     selectInput(inputId = "hb_input", 
                                 label = "Select Health Board", 
                                 choices = hb_name)),
              
            ),
            
            fluidRow(
              
              # column(6,
              #   radioButtons(inputId = "urban_input", 
              #                label = "Urban or rural?",
              #                choices = hb_urban)),
              
              column(6,
                     plotOutput("hb_quarter")),
              
              column(6,
                     plotOutput("hb_activity")),
              
            ),
            
            br(),
            
            fluidRow(
              
              titlePanel(h2("Urban or Rural", style = "font-size:50px;")),
              br(),
              p("The differences between the 6 urban and 8 rural Health board admissions are presented on a per capita basis."),
              br(),
              column(6,
                     plotOutput("hb_urban")),
              
              column(6,
                     plotOutput("hb_urban_rural_time")),
              
            )           
   )
   
  ))

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  ############### Jack's
  ##map stuff
  output$scotlandHM <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addResetMapButton()%>%
      addPolygons(data = scotlandpoly, popup = ~HBName, group = "HB_regions", 
                  layerId = ~HBName) %>%
      addCircles(data = hospitals, lng = ~X, lat = ~Y, label = ~LocationName, 
                 group = "hospital_locations", color = "red")
  })
  ## trigger map stuff
  observeEvent(input$scotlandHM_shape_click, { 
    region_selected <- reactive(input$scotlandHM_shape_click)
    p <- reactive(region_selected()$id)
    output$region_selected <- renderText(p())
    
    
    ##map data stuff
    filtered_df <- reactive({
      HB_data %>%
        filter(scotlandpoly.HBName == p()) %>%
        group_by(Quarter) %>%
        summarise(PercentageOccupancy_mean = mean(
          PercentageOccupancy, na.rm = TRUE), 
                  PercentageOccupancy_lowwer = quantile(
                    PercentageOccupancy, 0.25, na.rm = TRUE), 
                  PercentageOccupancy_upper = quantile(
                    PercentageOccupancy, 0.75, na.rm = TRUE),  
                  Quarter, scotlandpoly.HBName) %>%
        arrange()
    })
    
    
    ##plotting stuff
    output$plotteddata <- renderPlot({
      ggplot(filtered_df()) +
        
        annotate('rect', xmin = -Inf, xmax = "2019Q4", 
                 ymin=-Inf, ymax=Inf, alpha=0.5, fill="white") +
        
        geom_vline(aes(xintercept = "2019Q4"), color = "red") +
        
        geom_line(aes(x = Quarter, y = PercentageOccupancy_mean), 
                  group = "mean", size = 1) +
        geom_line(aes(x = Quarter, y = PercentageOccupancy_upper), 
                  group = "75th percentile", size=0.5, alpha=0.5,
                  color = "blue", linetype="twodash") +
        geom_line(aes(x = Quarter, y = PercentageOccupancy_lowwer), 
                  group = "25th percentile", size=0.5, alpha=0.5, 
                  color = "blue", linetype="twodash") +
        labs(
          x = "Yearly Quarter", 
          y = "Avarage Percentage Hospital Capacity", 
          title = "Hospital Capacity Over Time",
          subtitle = "Historical Reference Data Plotted Before 2020"
        ) + ylim(0, 100) +
        theme(text=element_text(size = 16,  family = "Arial"),
              strip.background =element_rect(fill = "#074859"),
              strip.text = element_text(colour = 'white', size = 18,
                                        family = "Arial"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        theme(axis.title.x = element_text(margin = margin(t = 10)))
      
    })
    
  })
  
  ############### Simone's
  output$table_output <- renderDataTable({
    
    dashboard_data_table %>% 
      filter(Age %in% input$age_input)%>% 
      filter(Quarter %in% input$quarter_input) %>% 
      arrange(Location)
    
    datatable(dashboard_data_table, escape = FALSE, selection = "none") %>% 
      formatStyle("Gender",
                  target = "row",
                  backgroundColor = styleEqual(c("Female", "Male"),
                                               c("#062e3c", "#074859"))
      ) %>% 
      formatStyle(columns = names(dashboard_data_table), color = "white",
                  fontWeight = "bold", family = "Arial")
  })
  
  
  output$tot_stays_plot <- renderPlot({
    tot_and_avg_stays %>%
      filter(age %in% input$age_input) %>% 
      filter(quarter %in% input$quarter_input) %>%
      ggplot() +
      geom_col(position = "dodge", colour = "white", aes(x = age, 
                                                         y = tot_stays, 
                                                         fill = quarter)
      ) +
      ggtitle("Total Stays") +
      labs(x = "Age", y = "Total Stays") +
      scale_fill_manual("Quarter", values = c("2020Q1" = "#061a1f",
                                              "2020Q2" = "#062e3c",
                                              "2020Q3" = "#074859",
                                              "2020Q4" = "#11667f",
                                              "2021Q1" = "#008b87",
                                              "2021Q2" = "#47899b",
                                              "2021Q3" = "#659799")
      ) +
      facet_wrap(~sex) +
      theme_bw() +
      theme(text=element_text(size = 20,  family = "Arial"),
            strip.background =element_rect(fill = "#062e3c"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
  
  output$avg_length_plot <- renderPlot({
    tot_and_avg_stays %>% 
      filter(age %in% input$age_input) %>% 
      filter(quarter %in% input$quarter_input) %>%
      select(age, average_length_of_stay, quarter, sex) %>% 
      ggplot() +
      geom_col(position = "dodge",  
               colour = "white", aes(x = age, y = average_length_of_stay, 
                                     fill = quarter)
      ) +
      ggtitle("Average Length of Stay") +
      labs(x = "Age", y = "Average Length of Stay") +
      scale_fill_manual("Quarter", values = c("2020Q1" = "#061a1f",
                                              "2020Q2" = "#062e3c",
                                              "2020Q3" = "#074859",
                                              "2020Q4" = "#11667f",
                                              "2021Q1" = "#008b87",
                                              "2021Q2" = "#47899b",
                                              "2021Q3" = "#659799")
      ) +
      facet_wrap(~sex) +
      theme_bw() +
      theme(text=element_text(size = 20,  family = "Arial"),
            strip.background =element_rect(fill = "#074859"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
  
  
  ############### John's
  output$trend_plot <- renderPlot({
    
    kpi_beds %>%
      filter(HBName == input$nhs_board_input) %>%
      filter(Location == HB) %>%
      ggplot() +
      aes(x = Quarter, y = .data[[input$kpi_input]], group = HBName, 
          fill = HBName) +
      geom_col(colour = "black") +
      geom_text(aes(label = .data[[input$kpi_input]]), size = 6, 
                vjust = -0.25) +
      scale_fill_manual(guide = "none", values = "#008b87") +
      ylim(c(0, NA)) +
      theme_bw() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 20,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
  
  output$diff_plot <- renderPlot({
    
    kpi_diff_beds %>%
      filter(HBName == input$nhs_board_input) %>%
      filter(Location == HB) %>%
      ggplot() +
      aes(x = Quarter, y = .data[[input$kpi_input]], group = HBName, 
          fill = HBName) +
      geom_col(colour = "black") +
      ylab("Pre-Covid19 Percentage Difference (%)") +
      geom_text(aes(label = .data[[input$kpi_input]]), size = 6, 
                vjust = -0.25) +
      scale_fill_manual(guide = "none", values = "#008b87") +
      theme_bw() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 20,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
  
  
  ############### Grant's
  output$hb_activity <- renderPlot({
    data %>% 
      filter(hb_name == input$hb_input) %>%
      group_by(quarter, admission_type) %>%  
      summarise(episodes = sum(episodes)) %>% 
      ggplot() +
      geom_line(aes(x = quarter, y = episodes, group = admission_type, 
                    colour = admission_type), alpha = 1, size = 0.9) +
      geom_point(aes(x = quarter, y = episodes, colour = admission_type), 
                 alpha = 0.5, size = 0.9) +
      xlab("Quarter") +
      ylab("Episodes") +
      scale_colour_manual("Admission Type", values = c(
        "All Day cases" = "#061a1f",
        "All Inpatients" = "#062e3c",
        "Elective Inpatients" = "#074859",
        "Emergency Inpatients" = "#11667f",
        "Transfers" = "#008b87")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 16,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_colour_brewer(palette = "Paired") 
  })
  
  output$hb_quarter <- renderPlot({
    data %>%
      filter(hb_name == input$multi_hb_input) %>%
      group_by(quarter, hb_name) %>% 
      summarise(episodes = sum(episodes)/mean(pop)) %>% 
      ggplot() +
      geom_line(aes(x = quarter, y = episodes, group = hb_name, 
                    colour = hb_name), alpha = 0.75, size = 0.9) +
      geom_point(aes(x = quarter, y = episodes, colour = hb_name), 
                 alpha = 0.75, size = 0.9) +
      xlab("Quarter") +
      ylab("Episodes per capita") +
      scale_colour_manual("Health boards", values = c(
                                              "Ayrshire and Arran" = "#061a1f",
                                              "Borders" = "#062e3c",
                                              "Dumfries and Galloway" = 
                                                "#074859",
                                              "Fife" = "#11667f",
                                              "Forth Valley" = "#008b87",
                                              "Grampian" = "#47899b",
                                              "Greater Glasgow and Clyde" = 
                                                "#659799",
                                              "Highland" = "#012F3C",
                                              "Lanarkshire" = "#2A6B75", 
                                              "Lothian" = "#E0D7C4", 
                                              "Orkney" = "#01636A", 
                                              "Shetland" = "#98B1AF", 
                                              "Tayside" = "#054C59", 
                                              "Western Isles" = "#022129")) +
      theme_bw() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 16,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  })
  
  output$hb_urban <- renderPlot({
    data %>%
      filter(hb_code != "S27000001") %>% # urban_rural status unclear
      group_by(urban_rural, admission_type) %>%
      summarise(episodes = sum(episodes)/mean(pop)) %>% 
      ggplot() +
      aes(x = admission_type, y = episodes) +
      geom_bar(stat = "identity") +
      facet_grid(~urban_rural) +
      xlab("Admission type") +
      ylab("Episodes per capita") +
      theme_bw() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 16,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$hb_urban_rural_time <- renderPlot({
    
    data %>%
      group_by(quarter, urban_rural) %>% 
      summarise(episodes = sum(episodes)/mean(pop)) %>% 
      ggplot() +
      aes(x = quarter, y = episodes, group = urban_rural, 
          colour = urban_rural) +
      geom_line(size = 0.9) +
      geom_point(aes(x = quarter, y = episodes, colour = urban_rural), 
                 alpha = 0.5, size = 0.9) +
      xlab("Quarter") +
      ylab("Episodes per capita") +
      scale_colour_manual("Urban or rural", 
                          values = c("Urban" = "#061a1f", 
                                     "Rural" = "#659799")) +
      ylim(0,NA) +
      theme_bw() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 16,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(legend.title=element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
