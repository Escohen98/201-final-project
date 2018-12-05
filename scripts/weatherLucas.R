####################### This is Lucas's attempt at gathering weather data  #####################


## Creates a data frame containing the win rate of the home team during cold and warm games
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
  
  ## Counting the number of cold and warm games played, and the results of the games
  for (i in 1:nrow(data)) {
    if (homeTeam == paste(data[i, "team_home"])) {
      if (as.numeric(data[i, "weather_temperature"]) >= 50) {
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
      if (as.numeric(data[i, "weather_temperature"]) >= 50) {
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
  
  home_team_weather <- data_frame("Temperature" = c("Cold Weather", "Warm Weather"),
                                  "Temp_Based_Record" = c(coldRecord, warmRecord))
  home_team_weather
})

## Creates a data frame containing the win rate of the away team during cold and warm games
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
  
  ## Counting the number of cold and warm games played, and the results of the games
  for (i in 1:nrow(data)) {
    if (awayTeam == paste(data[i, "team_home"])) {
      if (as.numeric(data[i, "weather_temperature"]) >= 50) {
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
      if (as.numeric(data[i, "weather_temperature"]) >= 50) {
        if (data[i, "score_home"] <data[i, "score_away"]) {
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
  
  away_team_weather <- data_frame("Temperature" = c("Cold Weather", "Warm Weather"),
                                  "Temp_Based_Record" = c(coldRecord, warmRecord))
  away_team_weather
})

## Creates a data frame containing the win rate of the home and away team during the given weather
weather_chart <- reactive({
  temperature <- input$temp
  teamNames <- home_and_away_teams()
  home_team_weather <- home_team_weather()
  away_team_weather <- away_team_weather()
  
  data <- data_frame("Team_Name" = teamNames,
                     "Win_Rate" = c(0, 0))
  if (temperature == "Below 50 Degrees") {
    data$Win_Rate <- c(as.numeric(away_team_weather[1, 2]),
                       as.numeric(home_team_weather[1, 2]))
  } else {
    data$Win_Rate <- c(as.numeric(away_team_weather[2, 2]),
                       as.numeric(home_team_weather[2, 2]))
  }
  
  data
})












## Creates chart of win rates based on Weather
output$weather_chart <- renderPlotly ({
  home_team_weather <- home_team_weather()
  away_team_weather <- away_team_weather()
  teamNames <- home_and_away_teams()
  homeTeam <- teamNames[2]
  awayTeam <- teamNames[1]
  
  data <- data_frame("Temperature" = c("Cold Weather", "Warm Weather"),
                     "Home_Team" = home_team_weather$Temp_Based_Record,
                     "Away_Team" = away_team_weather$Temp_Based_Record)
  
  chartTitle <- paste("Win Rate of the Teams in Cold and Warm Weather")
  
  plot_ly(data, x = ~Temperature, y = ~Home_Team, type = 'bar', name = homeTeam,
          marker = list(color = 'rgb(58,200,225)',
                        line = list(color = 'rgb(8,48,107)'))) %>% 
    add_trace(y = ~Away_Team, name = awayTeam,
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)'))) %>%
    layout(title = chartTitle, yaxis = list(title = 'Win Rate'), barmode = 'group')
})