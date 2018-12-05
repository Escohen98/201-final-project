library(dplyr)

##Enables scripts to run in either main dir or scripts folder.
file_path <- "data/"
if(substr(getwd(),nchar(getwd())-6, nchar(getwd())) == "scripts" ) { 
  file_path <- paste0('../', file_path)
}
stadiums <- read.csv(paste0(file_path,"nfl_stadiums.csv"), stringsAsFactors = FALSE)
teams <- read.csv(paste0(file_path,"nfl_teams.csv"), stringsAsFactors = FALSE)
NFL <- read.csv(paste0(file_path,"nfl.csv"), stringsAsFactors = FALSE)
#spreadspoke <- read.csv(file_path, stringsAsFactors = FALSE)

#Gets and returns all rows with given team_name or team_id from data. 
get_team_data <- function(team, data) {
  team_data <- ""
  if(nchar(team) != 3) {
    name_to_id(team)[1]
  }
  team_data <- filter(data, (home_id == team) | (away_id == team))
  team_data
}

#Given the team id and data of a single game,
#function determines if the given team won 
#returns the team_id of the winning team. 
get_team_result <- function(data) {
  winner <- ""
  if(data$score_away > data$score_home) {
    winner <- name_to_id(data$team_away)[1]
  } else {
    winner <- name_to_id(data$team_home)[1]
  }
  
  winner
}

stadium_to_rank <- function(team) {
  
}

#Appends and returns a dataframe containing the given data and a winner_id column 
#that has the team_id of the team that won each game. 
#Sets value to NA if game has yet to be played. 
categorize_weather <- function(this_data, max=0, min=1) {
  data <- filter(this_data,!is.na(ave_weather))
  if(max == 0) {
    max <- nrow(data)
  }
  data <- data[min:max,] 
  for(row in 1:(max-min+1)) {
    if((data[row, "ave_weather"] >= 0) & (data[row, "ave_weather"] < 116.0)) {
      data[row, "ave_weather"] <- 4
    } else if((data[row, "ave_weather"] >= 116.0) & (data[row, "ave_weather"] < 134.0)) {
      data[row, "ave_weather"] <- 3
    } else if((data[row, "ave_weather"] >= 134.0) & (data[row, "ave_weather"] < 149.0)) {
      data[row, "ave_weather"] <- 2
    } else if((data[row, "ave_weather"] >= 149.0) & (data[row, "ave_weather"] < 193.0)) {
      data[row, "ave_weather"] <- 1
    } else {
      data[row, "ave_weather"] <- NULL
    }
  }
  data
}

#Appends home_id and away_id to table.
append_ids <- function(data) {
  home_ids <- c()
  away_ids <- c()
  for(row in 1:nrow(data)) {
    if(is.na(data[row,]$team_home)) {
      home_ids <- c(home_ids, data[row,]$team_home)
    } else {
      home_ids <- c(home_ids, name_to_id(data[row,]$team_home)[1])
    }
    
    if(is.na(data[row,]$team_away)) {
      away_ids <- c(away_ids, data[row,]$team_away)
    } else {
      away_ids <- c(away_ids, name_to_id(data[row,]$team_away)[1])
    }
  }
  something <- mutate(data, home_id = home_ids, away_id = away_ids)
}

#Adds columns to CSV
#test_table <- append_ids(spreadspoke)
#View(test_table)
#n^2. Only run once. 
#write.csv(append_winner(append_ids(spreadspoke)), file = "../data/spreadspoke_scores.csv", row.names=FALSE)

#Takes a team ID and returns the given team name (does not include location).
#Works for team_id or team_id_pfr.
id_to_name <- function(id) {
  team <- select(teams, team_name_short, team_id, team_id_pfr) %>%
    filter((as.character(team_id) == as.character(id)) || (as.character(team_id_pfr) == as.character(id)))
  team$team_name_short
}

#Takes a team name and returns the given team id (not the team_id_pfr).
#Make sure not to include the team's location (team_name) and only the name (team_name_short)
name_to_id <- function(name) {
  team <- select(teams, team_name_short, team_id) %>%
    filter(as.character(team_name_short) == as.character(shorten_name(name)))
  team$team_id
}

#Returns the number of wins a team has in the given year up until the given week. Returns a numeric result.
team_record <- function(team, week, year, data) {
  new_data <- get_year_data(year, get_team_data(team, data)) %>%
    filter(!is.na(schedule_week)) %>%
    filter((is.numeric(schedule_week) <= week) & (winner_id == team))
  result <- nrow(new_data)
  if(week > 16) {
    result <- "bad." 
  }
  as.numeric(result)
}

#Helper function to filter only the requested year from the data and return the new table.
get_year_data <- function(year, data) {
  year_data <- filter(data, as.numeric(schedule_season) == as.numeric(year))
}

#Takes in a String and returns the last word in the String.
shorten_name <- function(full_name) {
  tail(strsplit(full_name, " ")[[1]], 1)
}