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
  
  point_differential <- reactive({
    home_team_data <-  home_team_data()
    away_team_data <- away_team_data()
    team_names <- home_and_away_teams()
    homeTeam <- team_names[2]
    awayTeam <- team_names[1]
    home_point_differential <- 0
    away_point_differential <- 0
    
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
    
    data <- data_frame("Team_Name" = team_names,
                       "Point_Differential" =
                         c(away_point_differential, home_point_differential))
    
    data
  })
  
  output$point_differential_chart <- renderPlot({
    point_differential <- point_differential()
    
    chartTitle <- paste("Point differentials (points for - points against) for the",
                        point_differential[1, "Team_Name"], "and the",
                        point_differential[2, "Team_Name"])
    
    ggplot(data = point_differential,
           aes(x = Team_Name, y = Point_Differential, fill = Team_Name)) +
      geom_bar(stat = "identity") + labs(title = chartTitle) + theme(legend.position = "none")
  })
  
  
  output$site_description <- renderText({
    paste("")
  })
}