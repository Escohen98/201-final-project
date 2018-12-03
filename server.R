library(shiny)

server <- function(input, output) {
  ## Gets current date
  current_date <- strtoi(substr(date(), nchar(date()) - 3, nchar(date())))
  
  ## Data frames used for functions
  stadiums <- read.csv("../data/nfl_stadiums.csv", stringsAsFactors = FALSE)
  teams <- read.csv("../data/nfl_teams.csv", stringsAsFactors = FALSE)
  
  ## Game data frame
  game_data <- read.csv("data/spreadspoke_scores.csv") %>% 
    filter(schedule_season == current_date | schedule_season == current_date - 1 |
             schedule_season == current_date - 2)
  game_data$schedule_week <- as.numeric(game_data$schedule_week)
  game_data <- arrange(game_data, desc(schedule_season), desc(schedule_week))
  
  ## Creats a vector where
  ## first data point is the away team of the inputted game, second point is the home team
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
  
  
}