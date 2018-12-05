library(shiny)
library(dplyr)

current_date <- strtoi(substr(date(), nchar(date()) - 3, nchar(date())))
buttons <- c("Not Very Important", "Fairly Important", "Very Important")

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
                  choices = list_of_games),
      
      radioButtons("win_rate_importance", label =
                     ("How Important is Overall Win Rate of the Teams to You?"),
                   choices = buttons, selected = "Very Important"),
      
      radioButtons("win_rate_home_away_importance", label =
                     ("How Important is the Win Rate of the Teams Based on Home Field advantage to You?"),
                   choices = buttons, selected = "Fairly Important"),
      radioButtons("head_to_head_importance", label =
                     ("How Important is the Outcome of Head-to-Head Games Between the Two Teams to You?"),
                   choices = buttons, selected = "Fairly Important"),
      radioButtons("point_differential_importance", label =
                     ("How Important is the Point Differential of the Teams to You?"),
                   choices = buttons, selected = "Very Important"),
      radioButtons("weather_importance", label =
                     ("How Important is the Play of Teams in Weather Similar to the Upcoming Game to You?"),
                   choices = buttons, selected = "Fairly Important")
    ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Prediction", textOutput("winning_team")),
                tabPanel("Team Win Rates", plotlyOutput("home_versus_away_chart")),
                tabPanel("Rates: Home vs Away", plotlyOutput("home_and_away_chart")),
                tabPanel("Point Differential", plotOutput("point_differential_chart")),
                tabPanel("About The Site", textOutput("nine_game_mention"))
    )
  )
  )
)
)