library(shiny)
library(dplyr)
library(ggplot2)
source("summary.R")

server <- function(input, output) {
  
  current_date <- strtoi(substr(date(), nchar(date()) - 3, nchar(date())))
  
  stadiums <- read.csv("../data/nfl_stadiums.csv", stringsAsFactors = FALSE)
  teams <- read.csv("../data/nfl_teams.csv", stringsAsFactors = FALSE)
  
  game_data <- read.csv("../data/spreadspoke_scores.csv") %>% 
    filter(schedule_season == current_date | schedule_season == current_date - 1 |
             schedule_season == current_date - 2)
  game_data$schedule_week <- as.numeric(game_data$schedule_week)
  game_data <- arrange(game_data, desc(schedule_season), desc(schedule_week))
  
  ## Records the week the game is being played
  week <- reactive({
    gameTitle <- input$game
    week <- 1
    if (substr(gameTitle, 7, 7) == ":")
      week <- as.numeric(substr(gameTitle, 6, 6))
    else
      week <- as.numeric(substr(gameTitle, 6, 7))
    week
  })
  
  ## Creats a vector where
  ## first data point is the away team of the inputted game,
  ## second point is the home team
  home_and_away_teams <- reactive({
    gameTitle <- input$game
    week <- week()
    homeTeam <- ""
    awayTeam <- ""
    temp <- filter(game_data, schedule_week == week & schedule_season == current_date)
    for (i in 1:nrow(temp)) {
      if(grepl(temp[i, "team_home"], gameTitle)) {
        homeTeam <- temp[i, "team_home"]
        awayTeam <- temp[i, "team_away"]
      }
    }
    home_and_away <- c(paste(awayTeam), paste(homeTeam))
    home_and_away
  })
  
  head_to_head <- reactive({
    temp <- head_to_head_data()
    teamNames <- home_and_away_teams()
    homeTeamWins <- 0
    awayTeamWins <- 0
    
    ## counts how many head-to-head games each team won
    if (nrow(temp) > 0) {
      for (i in 1:nrow(temp)) {
        if (teamNames[2] == paste(temp[i, "team_home"])) {
          if (temp[i, "score_home"] > temp[i, "score_away"]) {
            homeTeamWins <- homeTeamWins + 1
          } else if (temp[i, "score_home"] < temp[i, "score_away"]) {
            awayTeamWins <- awayTeamWins + 1
          } else {
            homeTeamWins <- homeTeamWins + 0.5
            awayTeamWins <- awayTeamWins + 0.5
          }
        } else {
          if (temp[i, "score_home"] < temp[i, "score_away"]) {
            homeTeamWins <- homeTeamWins + 1
          } else if (temp[i, "score_home"] > temp[i, "score_away"]) {
            awayTeamWins <- awayTeamWins + 1
          } else {
            homeTeamWins <- homeTeamWins + 0.5
            awayTeamWins <- awayTeamWins + 0.5
          }
        }
      }    
      homeTeamWins <- homeTeamWins / nrow(temp)
      awayTeamWins <- awayTeamWins / nrow(temp)
    }
    
    
    
    data <- data_frame("Team_Name" = teamNames,
                       "Head_to_Head_Win_Rate" = c(awayTeamWins, homeTeamWins))
  })
  
  output$head_to_head_plot <- renderPlotly({
    teamNames <- home_and_away_teams()
    data <- head_to_head()
    
    if (data[1, 2] == data[2, 2]) {
      data[1, 2] <- .5
      data[2, 2] <- .5
    }
    
    plot_ly(data, labels = ~Team_Names, values = ~Head_to_Head_Win_Rate, type = "pie") %>% 
      layout(title = "Head-to-Head Matchup Record",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
}