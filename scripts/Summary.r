library(dplyr)

stadiums <- read.csv("../data/nfl_stadiums.csv", stringsAsFactors = FALSE)
teams <- read.csv("../data/nfl_teams.csv", stringsAsFactors = FALSE)
spreadspoke <- read.csv("../data/spreadspoke_scores.csv", stringsAsFactors = FALSE)

  #weather_effect <- function(team) {
 
#}

get_team_data <- function(team, data) {
  team_data <- select(data, schedule_season, )
}

#Takes a team ID and returns the given team name (does not include location).
#Works for team_id or team_id_pfr.
id_to_name <- function(id) {
  team <- select(teams, team_name_short, team_id, team_id_pfr) %>%
    filter((team_id == id) || (team_id_pfr == id))
  team$team_name_short[1]
}

#Takes a team name and returns the given team id (not the team_id_pfr).
#Make sure not to include the team's location (team_name) and only the name (team_name_short)
name_to_id <- function(name) {
  team <- select(teams, team_name_short, team_id) %>%
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

