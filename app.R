#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(data.table)
library(dplyr)
library(ggplot2)


# Import datasets ----

skaters <- fread("nhl-stats_skaters.csv")

levels.team <- sort(unique(skaters$Team))

goalies <- fread("nhl-stats_goalies.csv")
goalies$`Minutes Played` <- as.numeric(gsub(",", "", goalies$`Minutes Played`))


# Start ui ----


ui <- fluidPage(
  
  # Application title
  titlePanel("NHL Player Statistics"), 
  
  
  
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Select a dataset ----
    selectInput("playerType", "Choose a player type:",
                choices = c("Skater", "Goalie"), selected = "Skater", multiple = FALSE),
    
    # Panel for playerType == Skater ----  
    conditionalPanel(
      
      condition = "input.playerType == 'Skater'",
      
      column(12,
             hr(),
             
             verbatimTextOutput('teamout1'),
             pickerInput('teamin1', 'Teams', levels.team, 
                         options = list(`actions-box` = TRUE),
                         selected = levels.team,
                         multiple = TRUE)
      ),
      
      column(12,
             hr(),
             
             verbatimTextOutput('positionout1'),
             pickerInput('positionin1', 'Position',
                         choices = c("D", "C", "LW", "RW"),
                         options = list(`actions-box` = TRUE),
                         selected = c("D", "C", "LW", "RW"),
                         multiple = TRUE)
      ),
      
      
      column(12,
             
             sliderInput("slider1_sk", label = h3("Games Played"), min = min(skaters$Games),
                         max = max(skaters$Games), value = c( min(skaters$Games), max(skaters$Games) ) )
      ),
      
      
      varSelectInput("metricin1", "Metric", skaters[,c(4:17)], selected = "Games", multiple=FALSE)
    ),
    
    
    
    # Panel for playerType == Goalie ----
    conditionalPanel(
      
      condition = "input.playerType == 'Goalie'",
      
      column(12,
             hr(),
             
             verbatimTextOutput('teamout2'),
             pickerInput('teamin2', 'Teams', levels.team, 
                         options = list(`actions-box` = TRUE),
                         selected = levels.team,
                         multiple = TRUE)
      ),
      
      column(12,
             
             sliderInput("slider1_g", label = h3("Games Played"), min = 0,
                         max = max(goalies$Games), value = c(0, max(goalies$Games)))
      ),
      
      column(12,
             
             sliderInput("slider2_g", label = h3("Wins"), min = min(goalies$Wins),
                         max = max(goalies$Wins), value = c( min(goalies$Wins), max(goalies$Wins) ) )
      ),
      
      varSelectInput("metricin2", "Metric", goalies[,c(3:13)], selected = "Games", multiple=FALSE)    
      
      )
  ),  
  
  # Main panel for output, on right ----
  mainPanel(
    
    h4("Players:"),
    div( tableOutput("table1"),
         style = 'height: 400px; overflow-y:scroll'
    ),
    
    h4("Selected Metric Histogram"),
    div( plotOutput("hist1"),
         style = 'height:400px'
    )
  )
  
)






# Server ----

server <- function(input, output) {
  
  # Get dataset input ----
  
  datasetInput <- reactive({
    
    switch(input$playerType,
           "Skater" = skaters,
           "Goalie" = goalies)
    
  })
  
  # Format data table ----
  
  df_subset <- reactive({
    
    if (input$playerType == "Skater"){
      
      b1 <- input$slider1_sk
      
      a <- subset(datasetInput(), Team %in% input$teamin1) 
      
      a <- subset(a, Pos %in% input$positionin1)
      
      a <- subset(a, Games >= b1[1] & Games <= b1[2])
      
      a <- a %>% dplyr::arrange(-!!input$metricin1, -Games)
      
      return(a)
      
    }
    
    if (input$playerType == "Goalie"){
      
      b1 <- input$slider1_g
      
      b2 <- input$slider2_g
      
      a <- subset(datasetInput(), Team %in% input$teamin2)
      
      a <- subset(a, Games >= b1[1] & Games <= b1[2])
      
      a <- subset(a, Wins >= b2[1] & Wins <= b2[2])
      
      a <- a %>% dplyr::arrange(!!input$metricin2, -Games)
      
      return(a)
    }
  })
  
  # Data for histogram ----
  
  hist_metric <- reactive({
    
    if (input$playerType == "Skater"){
      
      b1 <- input$slider1_sk
      
      a <- subset(datasetInput(), Team %in% input$teamin1) 
      
      a <- subset(a, Pos %in% input$positionin1)
      
      a <- subset(a, Games >= b1[1] & Games <= b1[2])
      
      a1 <- a %>% dplyr::select(!!input$metricin1) %>% unlist() %>% as.numeric()
      
      a2 <- a %>% dplyr::select(Team) %>% unlist() %>% as.character()
      
      a <- cbind(a1, a2)
      
      return(a)
      
    }
    
    if (input$playerType == "Goalie"){
      
      b1 <- input$slider1_g
      
      b2 <- input$slider2_g
      
      a <- subset(datasetInput(), Team %in% input$teamin2)
      
      a <- subset(a, Games >= b1[1] & Games <= b1[2])
      
      a <- subset(a, Wins >= b2[1] & Wins <= b2[2])
      
      a1 <- a %>% dplyr::select(!!input$metricin2) %>% unlist() %>% as.numeric()
      
      a2 <- a %>% dplyr::select(Team) %>% unlist() %>% as.character()
      
      a <- cbind(a1, a2)
      
      
      return(a)
    }
  })
  
  
  # Histogram axis label ----
    
    hist.xlab <- reactive({
      
      if (input$playerType == "Skater"){
        
        return(input$metricin1)
        
      }
      
      if (input$playerType == "Goalie"){
        
        return(input$metricin2)
        
      }
    })
  
    
  # Output table ----
    
  output$table1 <- renderTable(df_subset() )
  
  
  # Output histogram ----
  
  output$hist1 <- renderPlot(
    
    ggplot(data = data.frame(hist_metric()), aes( x = as.numeric(hist_metric()[,1]),
                                                  
                                                  stat(count),
                                                  
                                                  fill = as.factor(hist_metric()[,2] ) ) ) + 
      
      geom_density(size = 1, alpha = 0.6)+
      
      labs(x = hist.xlab(), y = "Player Frequency", fill = "Team") + 
      
      theme_bw()
    
  )
  
  
  
  
}

# Run the application  ----
shinyApp(ui = ui, server = server)
