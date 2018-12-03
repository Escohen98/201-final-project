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
  
  
  home_and_away_teams <- reactive({
    week <- input$schedule_week
    gameTitle <- input$game_name
    homeTeam <- ""
    awayTeam <- ""
    temp <- filter(game_data, schedule_week == week & schedule_season == current_date)
    for (i in 1:nrow(temp)) {
      if(grepl(temp[i, "team_home"], gameTitle))
        homeTeam <- temp[i, "team_home"]
      if(grepl(temp[i, "team_away"], gameTitle))
        awayTeam <- temp[i, "team_away"]
    }
    home_and_away <- c(awayTeam, homeTeam)
    home_and_away
  })
  
  home_team_data <- reactive({
    temp <- home_and_away_teams()
    homeTeam <- temp[2]
    homeData <- filter(game_data, team_home == homeTeam)
    homeData <- head(homeData, 9)
    homeData
  })
  
  away_team_data <- reactive({
    temp <- home_and_away_teams()
    awayTeam <- tempA[1]
    awayData <- filter(game_data, team_away == awayTeam)
    awayData <- head(awayData, 9)
    awayData
  })
  
  output$home_and_away_chart <- renderPlot({
    homeData <- home_team_data()
    awayData <- away_team_data()
    team_names <- home_and_away_teams()
    homeWins <- 0
    awayWins <- 0
    for(i in 1:9) {
      if(homeData[i, "score_home"] > homeData[i, "score_away"]) {
        homeWins <- homeWins + 1 
      }
      if(awayData[i, "score_home"] < awayData[i, "score_away"]) {
        awayWins <- awayWins + 1 
      }
    }
    homeWinRate <- homeWins / 9
    awayWinRate <- awayWins / 9
    
    team_names_detailed <- c(paste(team_names[1], "in away games"),
                             paste(team_names[2], "in home games"))
    winRateChart <- data_frame("Team_Name" = team_names,
                               "Win_Rate" = c(awayWinRate, homeWinRate))

    
    ggplot(data = winRateChart, aes(x = Team_Name, y = Win_Rate)) + geom_bar() +
      ggtitle(paste("Win Rates for", team_names_detailed[1],
                    "and", team_names_detailed[2]))
  })
}