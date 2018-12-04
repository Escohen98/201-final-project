library(shiny)
library(dplyr)

current_date <- strtoi(substr(date(), nchar(date()) - 3, nchar(date())))

game_data <- read.csv("data/spreadspoke_scores.csv") %>% 
  filter(schedule_season == current_date)
game_data$schedule_week <- as.numeric(game_data$schedule_week)
game_data <- arrange(game_data, desc(schedule_week))

list_of_games <- c()

for (i in 1:nrow(game_data)) {
  list_of_games <- c(list_of_games, paste0("Week ", game_data[i, "schedule_week"],
                                           ": ", game_data[i, "team_away"],
                                           " @ ", game_data[i, "team_home"]))
}

shinyUI(fluidPage(
  
  titlePanel("NFL Game Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("game", label = h3("Game Select"),
                  #choices = textOutput("selected_week"))

                  choices = list_of_games)
    ),
  
  mainPanel(
    plotOutput("home_versus_away_chart"),
    plotOutput("home_and_away_chart"),
    plotOutput("point_differential_chart"),
    textOutput("nine_game_mention")
  )
  )
)
)