library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

convert_importance <- function(string) {
  multiplyer <- 0
  if (string == "Not Very Important") {
    multiplyer <- 1
  } else if (string == "Fairly Important") {
    multiplyer <-  2
  } else {
    multiplyer <-  3
  }
  return(multiplyer)
}

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
  
  ## Creates a data frame of the last nine GAMES of the home team
  home_team_data <- reactive({
    temp <- home_and_away_teams()
    week <- week()
    homeTeam <- temp[2]
    data <- filter(game_data, score_home != "NA",
                   schedule_week <= week | schedule_season < current_date,
                   team_home == homeTeam | team_away == homeTeam)
    data <- head(data, 9)
    data
  })
  
  ## Creates a data frame of the last nine HOME GAMES of the home team
  home_team_data_at_home <- reactive({
    temp <- home_and_away_teams()
    week <- week()
    homeTeam <- temp[2]
    homeData <- filter(game_data, score_home != "NA",
                       schedule_week <= week | schedule_season < current_date,
                       team_home == homeTeam)
    homeData <- head(homeData, 9)
    homeData
  })
  
  ## Creates a data frame of the last nine GAMES of the away team
  away_team_data <- reactive({
    temp <- home_and_away_teams()
    week <- week()
    awayTeam <- temp[1]
    data <- filter(game_data, score_home != "NA",
                   schedule_week <= week | schedule_season < current_date,
                   team_home == awayTeam | team_away == awayTeam)
    data <- head(data, 9)
    data
  })
  
  ## Creates a data frame of the last nine AWAY GAMES of the away team
  away_team_data_at_away <- reactive({
    temp <- home_and_away_teams()
    week <- week()
    awayTeam <- temp[1]
    awayData <- filter(game_data, score_home != "NA",
                       schedule_week <= week | schedule_season < current_date,
                       team_away == awayTeam)
    awayData <- head(awayData, 9)
    awayData
  })
  
  ## Creates a data frame holding all the games played between the teams over the last three years
  head_to_head_data <- reactive({
    temp <- home_and_away_teams()
    week <- week()
    homeTeam <- temp[2]
    awayTeam <- temp[1]
    data <- filter(game_data, score_home != "NA",
                   schedule_week <= week | schedule_season < current_date,
                   team_home == homeTeam | team_away == homeTeam)
    data <- filter(data, team_home == awayTeam | team_away == awayTeam)
    data
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
  
  ## Creates a chart that holds the HOME WIN RATE FOR THE HOME TEAM 
  ## and the AWAY WIN RATE FOR THE AWAY TEAM
  home_away_win_rate_chart <- reactive({
    homeData <- home_team_data_at_home()
    awayData <- away_team_data_at_away()
    team_names <- home_and_away_teams()
    
    ## Records the number of home and away wins of the home and away teams
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
  
  ## Creates a chart that holds the point differentials for the two teams
  point_differential <- reactive({
    home_team_data <-  home_team_data()
    away_team_data <- away_team_data()
    team_names <- home_and_away_teams()
    homeTeam <- team_names[2]
    awayTeam <- team_names[1]
    home_point_differential <- 0
    away_point_differential <- 0
    
    ## calculates the point differential
    for (i in 1:9) {
      if (homeTeam == paste(home_team_data[i, "team_home"])) {
        home_point_differential <- home_point_differential +
          home_team_data[i, "score_home"] -
          home_team_data[i, "score_away"]
      } else {
        home_point_differential <- home_point_differential +
          home_team_data[i, "score_away"] -
          home_team_data[i, "score_home"]
      }
      
      
      if (awayTeam == paste(away_team_data[i, "team_home"])) {
        away_point_differential <- away_point_differential +
          away_team_data[i, "score_home"] -
          away_team_data[i, "score_away"]
      } else {
        away_point_differential <- away_point_differential +
          away_team_data[i, "score_away"] -
          away_team_data[i, "score_home"]
      }
    }
    
    ## putting differentials in a data table
    data <- data_frame("Team_Name" = team_names,
                       "Point_Differential" =
                         c(away_point_differential, home_point_differential))
    
    data
  })
  
  ## This creates a chart that compares the head-to-head win rates of the two teams
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
    

    
    data <- data_frame("Team_Names" = teamNames,
                       "Head_to_Head_Win_Rate" = c(awayTeamWins, homeTeamWins))
  })
  
  #################    This calculates who is supposed to win the game   ##############
  who_wins_calculator <- reactive({
    home_away_win_rate_chart <- home_away_win_rate_chart()
    win_rate_chart <- win_rate_chart()
    point_differential <- point_differential()
    #head_to_head <- head_to_head()
    #weather_chart <- weather_chart()
    home_team_score <- 0
    away_team_score <- 0
    if (win_rate_chart[2,2] > win_rate_chart[1,2]) {
      home_team_score <- (home_team_score + convert_importance(paste(input$win_rate_importance)))
    } else if (win_rate_chart[2,2] < win_rate_chart[1,2]){
      away_team_score <- (away_team_score + convert_importance(paste(input$win_rate_importance)))
    }
    if (home_away_win_rate_chart[2,2] > home_away_win_rate_chart[1,2]) {
      home_team_score <- (home_team_score + convert_importance(paste(input$win_rate_home_away_importance)))
    } else if (home_away_win_rate_chart[2,2] < home_away_win_rate_chart[1,2]){
      away_team_score <- (away_team_score + convert_importance(paste(input$win_rate_home_away_importance)))
    }
    if (point_differential[2,2] > point_differential[1,2]) {
      home_team_score <- (home_team_score + convert_importance(paste(input$point_differential_importance)))
    } else if (point_differential[2,2] < point_differential[1,2]){
      away_team_score <- (away_team_score + convert_importance(paste(input$point_differential_importance)))
    }
    #if (head_to_head[2,2] > head_to_head[1,2]) {
    #  home_team_score <- (home_team_score + convert_importance(paste(input$head_to_head_importance)))
    #} else if (head_to_head[2,2] < head_to_head[1,2]) {
    #  away_team_score <- (away_team_score + convert_importance(paste(input$head_to_head_importance)))
    #}
    #if (weather_chart[2,2] > weather_chart[1,2]) {
    #  home_team_score <- (home_team_score + convert_importance(paste(input$weather_importance)))
    #} else if (weather_chart[2,2] < weather_chart[1,2]){
    #  away_team_score <- (away_team_score + convert_importance(paste(input$weather_importance)))
    #}
    this_team_wins <- ""
    if (home_team_score > away_team_score) {
      this_team_wins <- paste(win_rate_chart[2,1])
    } else if (home_team_score < away_team_score) {
      this_team_wins <- paste(win_rate_chart[1,1])
    } else {
      this_team_wins <- "Based on our data, it's a tossup! Select tabs above for more information."
    }
    return(this_team_wins)
  })
  
  ################# Outputs begin here   #########################
  
  
  ## Returns a chart displaying the home win rate of the home team and the away win rate of the away team
  output$home_and_away_chart <- renderPlotly({
    team_names <- home_and_away_teams()
    winRateChart <- home_away_win_rate_chart()
    
    ## Formats strings for the title
    team_names_detailed <- c(paste(team_names[1], "in away games"),
                             paste(team_names[2], "in home games"))
    
    # Chart that is being returned
    plot_ly(winRateChart, x = ~Team_Name, y = ~Win_Rate, type = "bar", 
            marker = list(color = c("rgba(155,158,206,1)", "rgba(206,231,230,1)"),
                          line = list(color = "rgb(8,48,107)", width = 1.5))) %>%
      layout(title = paste("Win Rates for", team_names_detailed[1], "and", team_names_detailed[2]),
             xaxis = list(title = "Team Name"),
             yaxis = list(title = "Win Rate"))
  })
  
  ## Returns a chart displaying the past nine records for both home and away teams
  output$home_versus_away_chart <- renderPlotly({
    win_rate_chart <- win_rate_chart()
    team_names <- home_and_away_teams()
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    ## plots win rates for home and way teams
    plot_ly(win_rate_chart, x = ~Team_Name, y = ~Win_Rate, type = "bar", 
            marker = list(color = c("rgba(237,231,217,1)", "rgba(164,150,148,1)"),
                          line = list(color = "rgb(8,48,107)", width = 1.5))) %>%
      layout(title = paste("Win Rates for", team_names[1], "and", team_names[2]),
             xaxis = list(title = "Team Name"),
             yaxis = list(title = "Win Rate"))
  })
  
  ## plots the point differentials of the two teams
  output$point_differential_chart <- renderPlot({
    point_differential <- point_differential()
    
    ## formatting title
    chartTitle <- paste("Point differentials (points scored - points allowed) for the",
                        point_differential[1, "Team_Name"], "and the",
                        point_differential[2, "Team_Name"])
    
    ## plotting data
    ggplot(data = point_differential,
           aes(x = Team_Name, y = Point_Differential, fill = Team_Name)) +
      geom_bar(stat = "identity") + labs(title = chartTitle) + theme(legend.position = "none")
  })
  
  
  ## Explaines the data used for each chart
  output$nine_game_mention <- renderText({
    paste("** NOTE: All data used is over the nine pertinant games prior to",
          "the game being predicted (ex: the chart comparing records looks at the",
          "teams' last nine games, while the chart comparing the home team's",
          "record at home to the away team's record away looks at the teams' last",
          "nine home and away games, respectively). This is to ensure that the",
          "statistics presented are most representative of how the teams are",
          "currently performing **")
  })
  
  josh_data <- reactive({
    temp <- home_and_away_teams()
    week <- week()
    homeTeam <- temp[2]
    awayTeam <- temp[1]
    data <- filter(game_data, score_home != "NA",
                   schedule_week <= week | schedule_season < current_date,
                   team_home == homeTeam | team_away == homeTeam,
                   team_home == awayTeam | team_away == awayTeam)
    data
  })
  
  output$winning_team <- renderText ({
    if (who_wins_calculator() != "Based on our calculations, it's a tossup! Select tabs above for more information.") {
      paste("Based on our calculations, it is projected that the", who_wins_calculator(), "will win. Select tabs above for more information.")
    } else {
      who_wins_calculator()
    }
  })
  
}