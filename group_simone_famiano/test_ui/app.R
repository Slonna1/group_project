
library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)


ui <- fluidPage(

  

  # Application title

h1(strong("Demographics Data"), align="center", style = "font-size:100px;"),

tags$head(tags$style('
   body {
      font-family: Arial; 
      font-size: 20px; 
   }'
)),


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
          
 
 
         dataTableOutput("table_output")
    
)   

 

# Define server logic required to draw a histogram

server <- function(input, output) {

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
}

# Run the application 
shinyApp(ui = ui, server = server)
