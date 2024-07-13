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

generate_voting_graph <- function(season, tribe){
  boot_mapping <- boot_mapping
  vote_history <- vote_history
  season <- season
  tribe <- tribe
  tribe_status <- "Original"
  boot_mapping_original <- dplyr::filter(boot_mapping)
  vote_history_boot_map_og <- inner_join(vote_history, boot_mapping_original, by = c('season' = 'season', 'episode' = 'episode', 'castaway' = 'castaway'))
  vote_history_boot_map_og_tribe <- dplyr::filter(vote_history_boot_map_og, season_name.x %in% season, tribe.x %in% tribe, tribe_status.x %in% tribe_status)
  season_before_graph <- vote_history_boot_map_og_tribe %>% select(castaway, vote)
  season_before_graph <- season_before_graph[complete.cases(season_before_graph), ]
  voters_s = season_before_graph[['castaway']]
  voted_s = season_before_graph[['vote']]
  season_ready_for_graph = c(rbind(voters_s, voted_s))
  return(graph(season_ready_for_graph))
}

Seasons <- as.vector(unique(vote_history$season_name))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Season","Choose a season", Seasons),
      uiOutput("Tribe")), 
    mainPanel(
      plotOutput("Graph")
    )
  ))

    # Application title
    #titlePanel("Old Faithful Geyser Data"),
    
  #  selectInput("Season","Choose a season", Seasons),
   # uiOutput("Tribe")
    #)
    #mainPanel(
     # plotOutput("Graph")
    #)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Tribe <- renderUI({
      if(input$Season == "Survivor: Borneo"){
        temp_season <- vote_history %>% dplyr::filter(season_name == "Survivor: Borneo")
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
      }else if(input$Season == "Survivor: The Australian Outback"){
        temp_season <- vote_history %>% dplyr::filter(season_name == "Survivor: The Australian Outback")
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
      }else if(input$Season == "Survivor: Africa"){
        temp_season <- vote_history %>% dplyr::filter(season_name == "Survivor: Africa")
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
      }else if(input$Season == "Survivor: Marquesas"){
        temp_season <- vote_history %>% dplyr::filter(season_name == "Survivor: Marquesas")
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
      }else if(input$Season == "Survivor: Thailand"){
        temp_season <- vote_history %>% dplyr::filter(season_name == "Survivor: Thailand")
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
      }else if(input$Season == "Survivor: The Amazon"){
        temp_season <- vote_history %>% dplyr::filter(season_name == "Survivor: The Amazon")
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
      }else if(input$Season == "Survivor: Pearl Islands"){
        temp_season <- vote_history %>% dplyr::filter(season_name == "Survivor: Pearl Islands")
        temp_tribe <- as.vector(unique(temp_season$tribe))
        selectInput("Tribe", "Select a tribe", temp_tribe)
      }
    })
    voting_graph <- reactive(generate_voting_graph(input$Season, renderText(output$Tribe)))
    output$Graph <- renderPlot({
      print(plot(voting_graph()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
