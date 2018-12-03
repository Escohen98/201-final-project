library(dplyr)

#stadiums <- read.csv("../data/nfl_stadiums.csv", stringsAsFactors = FALSE)
#teams <- read.csv("../data/nfl_teams.csv", stringsAsFactors = FALSE)
#spreadspoke <- read.csv("../data/spreadspoke_scores.csv", stringsAsFactors = FALSE)

weather_effect_model <- function(team) {
 
}

#Gets and returns all rows with given team_name or team_id from data. 
get_team_data <- function(team, data) {
  team_data <- ""
  if(nchar(team) == 3) {
    team_data <- filter(data, (home_id == team) || (away_id == team))
  } else {
    team_data <- filter(data, (team_home == team) || (team_away == team))
  }
  
  team_data
}

#Given the team id and data of a single game,
#function determines if the given team won 
#returns true if the team won, false otherwise. 
get_team_result <- function(team_id, data) {
  team <- team_id
  if(nchar(team) != 3) {
    team <- name_to_id()
  }
  winner <- ""
  if(data$away_id[1] == team_id) {
    winner <- data$score_away[1] > data$score_home[1]
  } else if(data$home_id[1] == team_id) {
    winner <- data$score_home[1] > data$score_away[1]
  } else {
    winner <- NULL
  }
  
  winner
}

append_winner <- function(data) {
  
}

#Takes a team ID and returns the given team name (does not include location).
#Works for team_id or team_id_pfr.
id_to_name <- function(id, data) {
  team <- select(data, team_name_short, team_id, team_id_pfr) %>%
    filter((team_id == id) || (team_id_pfr == id))
  team$team_name_short[1]
}

#Takes a team name and returns the given team id (not the team_id_pfr).
#Make sure not to include the team's location (team_name) and only the name (team_name_short)
name_to_id <- function(name, data) {
  team <- select(data, team_name_short, team_id) %>%
    filter(team_name_short == name)
  team$team_id[1]
}

#Takes in a String and returns the last word in the String.
shorten_name <- function(full_name) {
  tail(strsplit(full_name, " ")[[1]], 1)
}

##Added Columns to CSV. 
#something <- mutate(spreadspoke, home_id = name_to_id(shorten_name(team_home)), away_id = name_to_id(shorten_name(team_away)))
#write.csv(something, file = "../data/spreadspoke_scores.csv", row.names=FALSE)
