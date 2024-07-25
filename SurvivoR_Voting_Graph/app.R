#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(survivoR)
library(igraph)
library(dplyr)

boot_mapping <- boot_mapping
vote_history <- vote_history

Seasons <- as.vector(unique(vote_history$season_name))

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Season","Choose a season", Seasons),
      uiOutput("Tribe")), 
    mainPanel(
      plotOutput("Graph")
    )
  ))

server <- function(input, output) {

    output$Tribe <- renderUI({
        temp_season <- vote_history[vote_history$season_name == input$Season,]
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
    })
    
    data_for_graph <- reactive({
      season <- input$Season
      tribe <- input$Tribe
      tribe_status <- "Original"
      boot_mapping_original <- dplyr::filter(boot_mapping)
      vote_history_boot_map_og <- inner_join(vote_history, boot_mapping_original, by = c('season' = 'season', 'episode' = 'episode', 'castaway' = 'castaway', 'version' = 'version'))
      vote_history_boot_map_og_tribe <- dplyr::filter(vote_history_boot_map_og, season_name.x == !!season, tribe.x == !!tribe, tribe_status.x == !!tribe_status)
      season_before_graph <- vote_history_boot_map_og_tribe %>% select(castaway, vote)
      season_before_graph <- season_before_graph[complete.cases(season_before_graph), ]
      voters_s <- season_before_graph[['castaway']]
      voted_s <- season_before_graph[['vote']]
      c(rbind(voters_s, voted_s))
    })
    
    output$Graph <- renderPlot(
      plot(graph(data_for_graph()))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
