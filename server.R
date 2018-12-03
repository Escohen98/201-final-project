library(shiny)

server <- function(input, output) {
  current_date <- strtoi(substr(date(), nchar(date()) - 3, nchar(date())))
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
    home_and_away <- c(awayTeam, homeTeam)
    home_and_away
  })
}