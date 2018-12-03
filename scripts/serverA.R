library("dplyr")
library("shiny")
library("ggplot2")
library("plotly")

server(function(input, output) {
  current_date <- strtoi(substr(date(), nchar(date()) - 3, nchar(date())))
  
  stadiums <- read.csv("../data/nfl_stadiums.csv", stringsAsFactors = FALSE)
  teams <- read.csv("../data/nfl_teams.csv", stringsAsFactors = FALSE)
  
  game_data <- read.csv("data/spreadspoke_scores.csv") %>% 
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
    home_and_away <- c(paste(awayTeam), paste(homeTeam))
    home_and_away
  })
  
  home_team_data <- reactive({
    temp <- home_and_away_teams()
    homeTeam <- temp[2]
    data <- filter(game_data, score_home != "NA", team_home == homeTeam | team_away == homeTeam)
    data <- head(data, 9)
    data
  })
  
  away_team_data <- reactive({
    temp <- home_and_away_teams()
    awayTeam <- temp[1]
    data <- filter(game_data, score_home != "NA", team_home == awayTeam | team_away == awayTeam)
    data <- head(data, 9)
    data
  })
  
  output$home_versus_away_chart <- renderPlot({
    temp <- home_and_away_teams()
    home_team <- temp[2]
    away_team <- temp[1]
    
    home_data <- home_team_data()
    away_data <- away_team_data()
    team_names <- home_and_away_teams()
    
    home_wins <- 0
    away_wins <- 0
    
    for(i in 9) {
      if(home_team == home_data[i, "team_home"]) {
        if(home_data[i, "score_home"] > home_data[i, "score_away"]) {
          home_wins <- home_wins + 1
        }
      } else {
        if(home_data[i, "score_away"] > home_data[i, "score_home"]) {
          home_wins <- home_wins + 1
        }
      }
      if(away_team == away_data[i, "team_away"]) {
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
    
    team_names_detailed <- c(paste(team_names[1], "in past 9 games"),
                             paste(team_names[2], "in past 9 games"))
    win_rate_chart <- data_frame("Team_Name" = team_names,
                               "Win_Rate" = c(away_win_rate, home_win_rate))
    
    ggplot(data = win_rate_chart, aes(x = Team_Name, y = Win_Rate)) + geom_bar(stat = "identity") +
      labs(title = paste("Win Rates for", team_names_detailed[1], "and", team_names_detailed[2], sep=" "))
  })
})

