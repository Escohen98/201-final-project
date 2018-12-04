library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
  
  ################## Initial constants  #################
  
  ## Gets current date
  current_date <- strtoi(substr(date(), nchar(date()) - 3, nchar(date())))
  
  ## Data frames used for functions
  stadiums <- read.csv("data/nfl_stadiums.csv", stringsAsFactors = FALSE)
  teams <- read.csv("data/nfl_teams.csv", stringsAsFactors = FALSE)
  
  ## Game data frame
  game_data <- read.csv("data/spreadspoke_scores.csv") %>% 
    filter(schedule_season == current_date | schedule_season == current_date - 1 |
             schedule_season == current_date - 2)
  game_data$schedule_week <- as.numeric(game_data$schedule_week)
  game_data <- arrange(game_data, desc(schedule_season), desc(schedule_week))
  
  ############### Reactive Variables  ######################
  
  ## Creats a vector where
  ## first data point is the away team of the inputted game,
  ## second point is the home team
  home_and_away_teams <- reactive({
    gameTitle <- input$game
    week <- 0
    if (substr(gameTitle, 7, 7) == ":")
      week <- as.numeric(substr(gameTitle, 6, 6))
    else
      week <- as.numeric(substr(gameTitle, 6, 7))
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
  
  ## Creates a data frame of the last nine GAMES of the home team
  home_team_data <- reactive({
    temp <- home_and_away_teams()
    homeTeam <- temp[2]
    data <- filter(game_data, score_home != "NA", team_home == homeTeam | team_away == homeTeam)
    data <- head(data, 9)
    data
  })
  
  ## Creates a data frame of the last nine HOME GAMES of the home team
  home_team_data_at_home <- reactive({
    temp <- home_and_away_teams()
    homeTeam <- temp[2]
    homeData <- filter(game_data, score_home != "NA", team_home == homeTeam)
    homeData <- head(homeData, 9)
    homeData
  })
  
  ## Creates a data frame of the last nine GAMES of the away team
  away_team_data <- reactive({
    temp <- home_and_away_teams()
    awayTeam <- temp[1]
    data <- filter(game_data, score_home != "NA", team_home == awayTeam | team_away == awayTeam)
    data <- head(data, 9)
    data
  })
  
  ## Creates a data frame of the last nine AWAY GAMES of the away team
  away_team_data_at_away <- reactive({
    temp <- home_and_away_teams()
    awayTeam <- temp[1]
    awayData <- filter(game_data, score_home != "NA", team_away == awayTeam)
    awayData <- head(awayData, 9)
    awayData
  })
  
  ## Creates a chart that holds the win rate for the home team and the away team 
  win_rate_chart <- reactive({
    team_names <- home_and_away_teams()
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    home_data <- home_team_data()
    away_data <- away_team_data()
    
    ## Records the number of home and away team wins
    ## Calculates win rates of home and away teams
    home_wins <- 0
    away_wins <- 0
    
    for(i in 1:9) {
      if(home_team == paste(home_data[i, "team_home"])) {
        if(home_data[i, "score_home"] > home_data[i, "score_away"]) {
          home_wins <- home_wins + 1
        }
      } else {
        if(home_data[i, "score_away"] > home_data[i, "score_home"]) {
          home_wins <- home_wins + 1
        }
      }
      if(away_team == paste(away_data[i, "team_away"])) {
        if(away_data[i, "score_home"] < away_data[i, "score_away"]) {
          away_wins <- away_wins + 1
        }
      } else {
        if(away_data[i, "score_away"] < away_data[i, "score_home"]) {
          away_wins <- away_wins + 1
        }
      }
    }
    
    home_win_rate <- home_wins / 9
    away_win_rate <- away_wins / 9
    
    win_rate_chart <- data_frame("Team_Name" = team_names,
                                 "Win_Rate" = c(away_win_rate, home_win_rate))
    win_rate_chart
  })
  
  ## Creates a chart that holds the home win rate for the home team and the away win rate for the away team
  home_away_win_rate_chart <- reactive({
    homeData <- home_team_data_at_home()
    awayData <- away_team_data_at_away()
    team_names <- home_and_away_teams()
    
    ## Records the number of home and away wins of gthe home and away teams
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
    
    winRateChart <- data_frame("Team_Name" = team_names,
                               "Win_Rate" = c(awayWinRate, homeWinRate))
    
    winRateChart
  })
  
  #################    This calculates who is supposed to win the game   ##############
  who_wins_the_game_calculator <- reactive({
    home_away_win_rate_chart <- home_away_win_rate_chart()
  })
  
  
  ################# Outputs begin here   #########################
  
  
  ## Returns a chart displaying the home win rate of the home team and the away win rate of the away team
  output$home_and_away_chart <- renderPlot({
    team_names <- home_and_away_teams()
    winRateChart <- home_away_win_rate_chart()
    
    ## Formats strings for the title
    team_names_detailed <- c(paste(team_names[1], "in away games"),
                             paste(team_names[2], "in home games"))
    
    ## Chart that is being returned
    ggplot(data = winRateChart, aes(x = Team_Name, y = Win_Rate, fill = Team_Name)) + geom_bar(stat = "identity") +
      ggtitle(paste("Win Rates for", team_names_detailed[1],
                    "and", team_names_detailed[2])) + theme(legend.position = "none")
  })
  
  ## Returns a chart displaying the past nine records for both home and away teams
  output$home_versus_away_chart <- renderPlot({
    win_rate_chart <- win_rate_chart()
    team_names <- home_and_away_teams()
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    ## Formats strings for the title
    team_names_detailed <- c(team_names[1],
                             paste(team_names[2], "in past 9 games"))
    
    ## plots win rates for home and way teams
    ggplot(data = win_rate_chart, aes(x = Team_Name, y = Win_Rate, fill = Team_Name)) + geom_bar(stat = "identity") +
      labs(title = paste("Win Rates for", team_names_detailed[1], "and", team_names_detailed[2], sep=" ")) +
      theme(legend.position = "none")
  })
}