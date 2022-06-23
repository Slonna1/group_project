library(shiny)
library(tidyverse)
library(shinyWidgets)

data <- read_csv("../shiny_dashboard_gw/data/phs_admissions_data_clean.csv")
hb_name <- unique(data$hb_name)
hb_urban <- unique(data$urban_rural)

#hb_urban <- unique(data$urban_rural)

ui <- fluidPage(
  
  titlePanel(h1("National variation in hospital activity", align = "center")),
  
  fluidRow(
    
    titlePanel(h2("Health Board")),
    br(),
    p("Explore the differences between (left) and within (right) the 14 Health boards of Scotland. Within a Health board the variation in admissions is shown."),
    br(),
    column(6, 
            pickerInput(inputId = "multi_hb_input", 
                       label = "Select Health Boards", 
                       choices = unique(data$hb_name),
                       multiple = TRUE,
                       selected = unique(data$hb_name))),
           
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
    
    titlePanel(h2("Urban or rural")),
    br(),
    p("The differences between urban and rural Health board admissions are presented on a per capita basis."),
    br(),
    column(6,
           plotOutput("hb_urban")),
    
    column(6,
           plotOutput("hb_urban_rural_time")),
    
  )
)

server <- function(input, output) {
  
  output$hb_activity <- renderPlot({
    data %>% 
      filter(hb_name == input$hb_input) %>%
      group_by(quarter, admission_type) %>%  
      summarise(episodes = sum(episodes)) %>% 
      ggplot() +
      geom_line(aes(x = quarter, y = episodes, group = admission_type, colour = admission_type), alpha = 1) +
      geom_point(aes(x = quarter, y = episodes, colour = admission_type), alpha = 0.5, size = 0.75) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab("Admission type") +
      ylab("Episodes")
      })
  
  output$hb_quarter <- renderPlot({
    data %>%
      filter(hb_name == input$multi_hb_input) %>%
      group_by(quarter, hb_name) %>% 
      summarise(episodes = sum(episodes)/mean(pop)) %>% 
      ggplot() +
      geom_line(aes(x = quarter, y = episodes, group = hb_name, colour = hb_name), alpha = 0.75) +
      geom_point(aes(x = quarter, y = episodes, colour = hb_name), alpha = 0.75) +
      xlab("Quarter") +
      ylab("Episodes per capita") +
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
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
  
  output$hb_urban_rural_time <- renderPlot({
    
    data %>%
      group_by(quarter, urban_rural) %>% 
      summarise(episodes = sum(episodes)/mean(pop)) %>% 
      ggplot() +
      aes(x = quarter, y = episodes, group = urban_rural, colour = urban_rural) +
      geom_line() +
      xlab("Quarter") +
      ylab("Episodes per capita") +
      ylim(0,NA)
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)