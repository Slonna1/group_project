library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

  fluidRow(  
    column(3,
           selectInput("hb_input", 
                       "Select Age Groups", 
                       choices = unique(phs_data_comparison_weekending$hb)),
    ),
    

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hbPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
  output$hbPlot <- renderPlot({
      phs_data_comparison_weekending %>%
        filter(hb %in% input$hb_input) %>% 
        ggplot() +
        aes(x = reorder(week_ending, hb), y = difference, 
            fill = difference > 0) +
        geom_col() +
        ylim(-min_max, min_max) +
        
        labs(y = "Difference in Hospital Stays", x = "Health Board", 
             title = "Hospital Stays differences between 2020/21 and 2018/19") + 
        scale_fill_manual(values = c("#061a1f", "#062e3c"), name = "", 
                          labels = c("decrease", "increase")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
