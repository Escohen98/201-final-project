library(shiny)
library(dplyr)
NFL_data <- read.csv("data/spreadspoke_scores.csv", stringsAsFactors = FALSE) %>%
  select(schedule_season, schedule_week, team_home, team_away) %>%
  filter(schedule_season == substr(date(), 21, 24)) %>%
  mutate(game_title = paste(team_away, "@ ", team_home))

shinyUI(fluidPage(
  
  titlePanel("NFL Game Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("schedule", label = h3("Week Select"),
                  choices = c(1:17), selected = 1),
      
      selectInput("game", label = h3("Game Select"),
                  choices = NFL_data %>%
                    filter(schedule_week == textOutput(selected_week)) %>%
                    select(game_title))
    ),
    
    mainPanel(
      plotOutput("home_and_away_chart")
      ## plotOutput("PLOT CALL") Requires plot input
    )
  )
)
)