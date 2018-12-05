#File consists of helper function to reduce redundancy throughout project. 
#Similar to abstract class in Object-Oriented programming.
#Primary implementation in ml_predictions.r
#Created By Eric Cohen

library(dplyr)

#Gets and returns all rows with given team_name or team_id from data.
#Returns rows of only home if stadium == home, only away if stadium == away, 
#otherwise returns both.
#Switches home and away teams if stadium = 1 (home team). 
get_team_data <- function(team_name, df, stadium="") {
  team_data <- df
  team <- is_id(team_name)
  if(stadium == 1) {
      team_data <- mutate(team_data, team_home = df$team_away, team_away = df$team_home, home_id = df$away_id, away_id = df$home_id, 
                          score_home = df$score_away, score_away = df$score_home) %>%
        filter(home_id == team)
  } else if(stadium == 0) {
    team_data <- filter(df, away_id == team)
  } else {
  team_data <- filter(df, (home_id == team) | (away_id == team))
  }
  team_data
}

#Given the team id and data of a single game,
#function determines if the given team won 
#returns the team_id of the winning team. 
get_team_result <- function(df) {
  winner <- ""
  if(df$score_away > df$score_home) {
    winner <- name_to_id(df$team_away)[1]
  } else {
    winner <- name_to_id(df$team_home)[1]
  }
  
  winner
}

#Given the home team id, computes rank of weather.
#Key: 
## Rank | Stadium
##  1   |   warm
##  2   | moderate
##  3   |   dome
##  4   |   cold
stadium_to_rank <- function(team) {
  NFL <- get_NFL()
  stadiums <- select(get_stadiums(), stadium=stadium_name, 
                     weather=stadium_weather_type)
  #Safest bet. Most likely will not affect data.
  #stadiums$stadium_weather_type[is.na(stadium$stadium_weather_type)] <- paste("dome") 
  for(t in NFL$Team) {
    t <- shorten_name(t)
  }
  #team_name <- id_to_name(team)[1]
  names(stadiums)[1] <- paste("Stadium")
  for(row in 1:nrow(NFL)) {
    NFL$Team[row] <- shorten_name(NFL$Team[row])
  }
  field <- NFL[NFL$Team == team, "Stadium"]
  temp <- filter(stadiums, Stadium == field)["weather"]
  if(temp == "cold") {
    temp <- 4
  } else if(temp == "moderate") {
    temp <- 2
  } else if(temp == "warm") {
    temp <- 1
  } else {
    temp <- 3
  }
  
  temp
}

#Appends and returns a dataframe containing the given data and a winner_id column 
#that has the team_id of the team that won each game. 
#Sets value to NA if game has yet to be played. 
#Very slow.
append_winner <- function(df) {
  winner <- c()
  for(row in 1:nrow(df)) {
    if(is.na(df[row,]$score_home)) {
      winner <- c(winner, df[row,]$score_home) 
    } else {
      winner <- c(winner, get_team_result(df[row,]))
    }
  }
  new_data <- mutate(df, winner_id=winner)
}

#Creates and returns a new dataframe containing the columns away_win and ave_weather.
#@param info_data - the dataframe containing the necessary data
#@param weights - A list of values to be multiplied for each weather feature 
#                 format: (temperature, wind, humidity).
#@return df - dataframe containing the columns home_win and ave_weather
#@column home_win - Contains a 1 if the away team won, otherwise 0. Ties are omitted.
#@column ave_weather - Total value of all weather components multipled by their weights. 
prepare_for_model <- function(info_data, weights=c(1,1,1)) {
  info_data$weather_temperature <- as.numeric(info_data$weather_temperature)
  df <- filter(info_data, !(is.na(weather_temperature) | is.na(weather_wind_mph) | is.na(weather_humidity) | (score_home == score_away))) %>%
    mutate(away_win = ((score_away-score_home)/abs(score_home-score_away))) %>%
    mutate(ave_weather = ((weather_temperature*weights[1] + weather_wind_mph * weights[2] + as.numeric(weather_humidity) * weights[3]))) %>%
    select(away_win, ave_weather, weather_temperature, weather_wind_mph, weather_humidity) %>%
    filter(!is.na(away_win))
  condition <- df$away_win == -1
  df[condition,1] <-  0
  df
}

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#54.0   116.0   134.0   132.2   149.0   192.0    3284
#Converts data to categorical variable from 1 to 4 based on spread of weather data. 
#1 is the highest rank (greatest temp + humidity + wind) and 4 is the least. 
#@param min - The min row to filter from the dataframe (default=1)
#@param max - The max row to filter from the dataframe (default=length of filtered dataframe)
categorize_weather <- function(this_data, max=0, min=1) {
  df <- filter(this_data,!is.na(ave_weather))
  if(max == 0) {
    max <- nrow(df)
  }
  
  df <- df[min:max,] 
  for(row in 1:(max-min+1)) {
    if((df[row, "ave_weather"] >= 0) & (df[row, "ave_weather"] < 116.0)) {
      df[row, "ave_weather"] <- 4
    } else if((df[row, "ave_weather"] >= 116.0) & (df[row, "ave_weather"] < 134.0)) {
      df[row, "ave_weather"] <- 3
    } else if((df[row, "ave_weather"] >= 134.0) & (df[row, "ave_weather"] < 149.0)) {
      df[row, "ave_weather"] <- 2
    } else if((df[row, "ave_weather"] >= 149.0) & (df[row, "ave_weather"] < 193.0)) {
      df[row, "ave_weather"] <- 1
    } else {
      df[row, "ave_weather"] <- NULL
    }
  }
  df
}

#Appends home_id and away_id to table.
#Very slow.
append_ids <- function(df) {
  home_ids <- c()
  away_ids <- c()
  for(row in 1:nrow(data)) {
    if(is.na(df[row,]$team_home)) {
      home_ids <- c(home_ids, df[row,]$team_home)
    } else {
      home_ids <- c(home_ids, name_to_id(df[row,]$team_home)[1])
    }
    
    if(is.na(df[row,]$team_away)) {
      away_ids <- c(away_ids, data[row,]$team_away)
    } else {
      away_ids <- c(away_ids, name_to_id(df[row,]$team_away)[1])
    }
  }
  something <- mutate(df, home_id = home_ids, away_id = away_ids)
}

#Takes a team ID and returns the given team name (does not include location).
#Works for team_id or team_id_pfr.
id_to_name <- function(id) {
  team <- select(get_teams(), team_name_short, team_id, team_id_pfr) %>%
    filter((as.character(team_id) == as.character(id)) || (as.character(team_id_pfr) == as.character(id)))
  team$team_name_short
}

#Takes a team name and returns the given team id (not the team_id_pfr).
#Make sure not to include the team's location (team_name) and only the name (team_name_short)
name_to_id <- function(name) {
  team <- select(get_teams(), team_name_short, team_id) %>%
    filter(as.character(team_name_short) == as.character(shorten_name(name)))
  team$team_id
}

#Checks if the given character is of length 3 (team id). If true, returns team, otherwise converts team to 3 characters and returns result.
is_id <- function(team_var) {
  team <- team_var
  if(nchar(team) != 3) {
    team <- name_to_id(team)[1]
  }
  
  team
}

#Returns the number of wins a team has in the given year up until the given week. Returns a numeric result.
team_record <- function(team, week, year, df) {
  new_data <- get_year_data(year, get_team_data(team, df)) %>%
    filter(!is.na(schedule_week)) %>%
    filter((is.numeric(schedule_week) <= week) & (winner_id == team))
  result <- nrow(new_data)
  if(week > 16) {
    result <- "bad." 
  }
  as.numeric(result)
}

#Helper function to filter only the requested year from the data and return the new table.
get_year_data <- function(year, df) {
  year_data <- filter(df, as.numeric(schedule_season) == as.numeric(year))
}

#Takes in a String and returns the last word in the String.
shorten_name <- function(full_name) {
  tail(strsplit(full_name, " ")[[1]], 1)
}

#Returns the relative file path of the file.
#Path is dependent on working directory. 
#Can be in either scripts or main directory.
get_file_path <- function(file) {
  file_path <- "data/"
  if(substr(getwd(),nchar(getwd())-6, nchar(getwd())) == "scripts" ) { 
    file_path <- paste0('../', file_path, file)
  } else {
    file_path <- paste0(file_path, file)
  }
  
  return(file_path)
}

#Returns a dataframe containing stadium information
#Also fixed some typos.
get_stadiums <- function() {
  stadiums <- read.csv(get_file_path("nfl_stadiums.csv"), stringsAsFactors = FALSE)
}

#Returns a dataframe containing team information
get_teams <- function() {
  teams <- read.csv(get_file_path("nfl_teams.csv"), stringsAsFactors = FALSE)
}

get_scores <- function() {
  spreadspoke <- read.csv(get_file_path("spreadspoke_scores.csv"), stringsAsFactors = FALSE)
} 

#Returns a dataframe containing information on which team plays at which stadium.
#Also updates Chargers and Rams, and any other stadium that does not match with get_stadiums.
get_NFL <- function() {
  NFL <- read.csv(get_file_path("nfl.csv"), stringsAsFactors = FALSE)
}

#Adds columns to CSV
#n^2. Only run once. 
#write.csv(append_winner(append_ids(get_scores())), file = "../data/spreadspoke_scores.csv", 
#                                                                          row.names=FALSE)
