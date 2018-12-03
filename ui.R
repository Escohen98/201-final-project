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
                  choices = list("1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5",
                                 "6" = "6",
                                 "7" = "7",
                                 "8" = "8",
                                 "9" = "9",
                                 "10" = "10",
                                 "11" = "11",
                                 "12" = "12",
                                 "13" = "13",
                                 "14" = "14",
                                 "15" = "15",
                                 "16" = "16",
                                 "17" = "17"),
                                 selected = "1"),
      
      selectInput("game", label = h3("Game Select"),
                  choices = NFL_data %>%
                    filter(schedule_week == textOutput("schedule")) %>%
                    select(game_title))
    ),
  
  mainPanel(
    plotOutput("home_and_away_chart")
    ## plotOutput("PLOT CALL") Requires plot input
  )
  )
)
)