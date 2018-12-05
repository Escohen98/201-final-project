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
  
  home_team_weather <- reactive({
    teamNames <- home_and_away_teams()
    homeTeam <- teamNames[2]
    data <- filter(game_data, score_home != "NA",
                   schedule_week <= week | schedule_season < current_date,
                   team_home == homeTeam | team_away == homeTeam,
                   weather_temperature != "NA")
    
    numWarmGames <- 0
    numColdGames <- 0
    warmRecord <- 0
    coldRecord <- 0
    for (i in 1:nrow(data)) {
      if (homeTeam == paste(data[i, "team_home"])) {
        if (data[i, "weather_temperature"] >= 50) {
          if (data[i, "score_home"] > data[i, "score_away"]) {
            warmRecord <- warmRecord + 1
          }
          numWarmGames <- numWarmGames + 1
        } else {
          if (data[i, "score_home"] > data[i, "score_away"]) {
            coldRecord <- coldRecord + 1
          }
          numColdGames <- numColdGames + 1
        }
      } else {
        if (data[i, "weather_temperature"] >= 50) {
          if (data[i, "score_home"] < data[i, "score_away"]) {
            warmRecord <- warmRecord + 1
          }
          numWarmGames <- numWarmGames + 1
        } else {
          if (data[i, "score_home"] < data[i, "score_away"]) {
            coldRecord <- coldRecord + 1
          }
          numColdGames <- numColdGames + 1 
        }
      }
    }
    
    if (numWarmGames == 0)
      numWarmGames <- 1
    if (numColdGames == 0)
      numColdGames <- 1
    
    warmRecord <- warmRecord / numWarmGames
    coldRecord <- coldRecord / numColdGames
    
    home_team_weather <- data_frame("Team_Name" = c("Cold Weather", "Warm Weather"),
                                    "Temp_Based_Record" = c(coldRecord, warmRecord))
  })
  
  away_team_weather <- reactive({
    teamNames <- home_and_away_teams()
    awayTeam <- teamNames[1]
    data <- filter(game_data, score_home != "NA",
                   schedule_week <= week | schedule_season < current_date,
                   team_home == awayTeam | team_away == awayTeam,
                   weather_temperature != "NA")
    
    numWarmGames <- 0
    numColdGames <- 0
    warmRecord <- 0
    coldRecord <- 0
    for (i in 1:nrow(data)) {
      if (awayTeam == paste(data[i, "team_home"])) {
        if (data[i, "weather_temperature"] >= 50) {
          if (data[i, "score_home"] > data[i, "score_away"]) {
            warmRecord <- warmRecord + 1
          }
          numWarmGames <- numWarmGames + 1
        } else {
          if (data[i, "score_home"] > data[i, "score_away"]) {
            coldRecord <- coldRecord + 1
          }
          numColdGames <- numColdGames + 1
        }
      } else {
        if (data[i, "weather_temperature"] >= 50) {
          if (data[i, "score_home"] < data[i, "score_away"]) {
            warmRecord <- warmRecord + 1
          }
          numWarmGames <- numWarmGames + 1
        } else {
          if (data[i, "score_home"] < data[i, "score_away"]) {
            coldRecord <- coldRecord + 1
          }
          numColdGames <- numColdGames + 1 
        }
      }
    }
    
    if (numWarmGames == 0)
      numWarmGames <- 1
    if (numColdGames == 0)
      numColdGames <- 1
    
    warmRecord <- warmRecord / numWarmGames
    coldRecord <- coldRecord / numColdGames
    
    away_team_weather <- data_frame("Team_Name" = c("Cold Weather", "Warm Weather"),
                                    "Temp_Based_Record" = c(coldRecord, warmRecord))
  })
  
  weather_chart <- reactive({
    temperature <- input$temp
    teamNames <- home_and_away_teams()
    home_team_weather <- home_team_weather()
    away_team_weather <- away_team_weather()
    
    data <- data_frame("Team_Name" = teamNames,
                       "Win_Rate" = c(0, 0))
    if (temperature == paste("Below 50 Degrees"))
      data$Win_Rate <- c(away_team_weather[1, 2], home_team_weather[1, 2])
    else
      data$Win_Rate <- c(away_team_weather[2, 2], home_team_weather[2, 2])
    
    data
  })
  
}