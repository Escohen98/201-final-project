library(shiny)
library(dplyr)
NFL_data <- read.csv("data/spreadspoke_scores.csv") %>%
  select(schedule_season, schedule_week, team_home, team_away) %>%
  filter(schedule_season == substr(date(), 21, 24)) %>%
  mutate(game_title = paste(team_away, "@ ", team_home))

shinyUI(fluidPage(
  
  titlePanel("NFL Game Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("schedule", label = h3("Week Select"),
                  choices = list("Week 1" = "Week 1",
                                 "Week 2" = "Week 2",
                                 "Week 3" = "Week 3",
                                 "Week 4" = "Week 4",
                                 "Week 5" = "Week 5",
                                 "Week 6" = "Week 6",
                                 "Week 7" = "Week 7",
                                 "Week 8" = "Week 8",
                                 "Week 9" = "Week 9",
                                 "Week 10" = "Week 10",
                                 "Week 11" = "Week 11",
                                 "Week 12" = "Week 12",
                                 "Week 13" = "Week 13",
                                 "Week 14" = "Week 14",
                                 "Week 15" = "Week 15",
                                 "Week 16" = "Week 16",
                                 "Week 17" = "Week 17"),
                                 selected = "Week 1"),
      
      selectInput("game", label = h3("Game Select"),
                  choices = NFL_data %>%
                    filter(schedule_week == substr(input$schedule, 5, 7)) %>%
                    select(game_title)
    )
  ),
  mainPanel(
    plotOutput("home_and_away_chart"), ## Requires plot input
    plotOutput("PLOT CALL") ## Requires plot input
  )
  )
)