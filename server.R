library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
  
  output$selected_week <- renderText ({ input$schedule })
  
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
  
  ## Creats a vector where
  ## first data point is the away team of the inputted game,
  ## second point is the home team
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
  
  ## Creates a data frame of the last nine GAMES of the home team
  home_team_data <- reactive({
    temp <- home_and_away_teams()
    homeTeam <- temp[2]
    data <- filter(game_data, team_home == homeTeam | team_away == homeTeam)
    data <- head(data, 9)
    data
  })
  
  ## Creates a data frame of the last nine HOME GAMES of the home team
  home_team_data_at_home <- reactive({
    temp <- home_and_away_teams()
    homeTeam <- temp[2]
    homeData <- filter(game_data, team_home == homeTeam)
    homeData <- head(homeData, 9)
    homeData
  })
  
  ## Creates a data frame of the last nine GAMES of the away team
  away_team_data <- reactive({
    temp <- home_and_away_teams()
    awayTeam <- temp[1]
    data <- filter(game_data, team_home == awayTeam | team_away == awayTeam)
    data <- head(data, 9)
    data
  })
  
  ## Creates a data frame of the last nine AWAY GAMES of the away team
  away_team_data_at_away <- reactive({
    temp <- home_and_away_teams()
    awayTeam <- tempA[1]
    awayData <- filter(game_data, team_away == awayTeam)
    awayData <- head(awayData, 9)
    awayData
  })
  
  ## Returns a chart displaying the home win rate of the home team and the away win rate of the away team
  output$home_and_away_chart <- renderPlot({
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
    
    ## Formats strings for the title
    team_names_detailed <- c(paste(team_names[1], "in away games"),
                             paste(team_names[2], "in home games"))
    winRateChart <- data_frame("Team_Name" = team_names,
                               "Win_Rate" = c(awayWinRate, homeWinRate))
    
    ## Chart that is being returned
    ggplot(data = winRateChart, aes(x = Team_Name, y = Win_Rate)) + geom_bar() +
      ggtitle(paste("Win Rates for", team_names_detailed[1],
                    "and", team_names_detailed[2]))
  })
}