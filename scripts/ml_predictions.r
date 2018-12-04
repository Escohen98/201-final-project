library(bootstrap)
library(leaps)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(GGally)
library(ggplot2)

source("Summary.r")
#Train a linear regression prediction model on weather. 
#Coming soon
weather_effect_model <- function(team, info_data) {
  attach(info_data)
  leaps <- regsubsets(((score_home-score_away)/abs(score_home-score_away)) ~ weather_temperature + weather_wind_mph + as.numeric(weather_humidity))
  fit <- lm(leaps, data=get_team_data(team,info_data))
  summary(fit)
}

#weather_validator <- function()
#  weather_effect_model("DEN", spreadspoke)

#Graphs the model of home_win vs. weather. Dplyr is masked by GGally.
ggpairs_model_checker <- function(info_data, weights) {
  df <- filter(info_data, !(is.na(weather_temperature) | is.na(weather_wind_mph) | is.na(weather_humidity))) %>%
   dplyr::mutate(home_win = ((score_home-score_away)/abs(score_home-score_away))) %>%
    dplyr::mutate(ave_weather = ((weather_temperature*weights[1] + weather_wind_mph * weights[2] + as.numeric(weather_humidity) * weights[3]))) %>%
    dplyr::select(home_win, ave_weather, weather_temperature, weather_wind_mph, weather_humidity)
  df[is.na(df)] <- 0
  #View(df)
  #df <- fortify(c(((info_data$score_home-info_data$score_away)/abs(info_data$score_home-info_data$score_away)), (info_data$weather_temperature + info_data$weather_wind_mph + as.numeric(info_data$weather_humidity))/3))
  #df <- as.data.frame(df)
  ggpairs(data=df, upper=list(df$home_win), lower=list(df$ave_weather, df$weather_temperature*weights[1], df$weather_wind_mph*weights[2], as.numeric(df$weather_humidity)*weights[3]), title="Home Win vs. Weather", cardinality_threshold = 100)
}

ggpairs_model_checker(spreadspoke, c(3, 2, 0.5))

individual_model_checker <- function(feature, info_data) {
  suppressWarnings(info_data$weather_temperature <- as.numeric(info_data$weather_temperature))
  df <- filter(info_data, !is.na(weather_temperature)) %>%
    dplyr::mutate(home_win = ((score_home-score_away)/abs(score_home-score_away))) %>%
    dplyr::mutate(ave_weather = (weather_temperature)) %>%
    dplyr::select(home_win, ave_weather)
  df[is.na(df)] <- 0
  #View(df)
  #df <- fortify(c(((info_data$score_home-info_data$score_away)/abs(info_data$score_home-info_data$score_away)), (info_data$weather_temperature + info_data$weather_wind_mph + as.numeric(info_data$weather_temperature))/3))
  #df <- as.data.frame(df)
  ggpairs(data=df, columns=1:2, title="Home Win vs. Weather")
}

#humidity corr: -0.00712
#temperature corr: -0.0344
#wind_mph corr: .0247
individual_model_checker("weather_temperature", spreadspoke)
individual_model_checker("spreadspoke$weather_humidity", spreadspoke)
individual_model_checker("spreadspoke$weather_wind_mph", spreadspoke)

#weather_effect_model("DEN", spreadspoke)