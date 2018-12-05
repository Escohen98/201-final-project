#library(bootstrap)
#library(leaps)
#library(tidyverse)  # data manipulation
#library(cluster)    # clustering algorithms
#library(factoextra) # clustering algorithms & visualization
#library(GGally)
library(aod)
library(ggplot2)
library(knitr)

source("Summary.r")
#Train a linear regression prediction model on weather. 
#Coming soon
#weather_effect_model <- function(team, info_data, weight=c(1,1,1)) {
#  df <- prepare_for_model(info_data)[1000,]
#  attach(df)
#  leaps <- regsubsets(factor(home_win) ~ weather_temperature + weather_wind_mph + weather_humidity, data=df, nbest=10, weights = weight, really.big=T)
#  summary(leaps)
  #fit <- lm(leaps, data=get_team_data(team,info_data), nbest=10)
#  plot(leaps, scale="r2")
  #library(car)
  #subsets(leaps, statistic="rsq")
#}

 
weather_effect_model <- function(team, info_data, weight=c(1,1,1)) {
  df <- categorize_weather(prepare_for_model(info_data))[1:1000,]
  #sapply(df, sd)
  #val <- xtabs(~home_win + ave_weather, data=df)
  df$ave_weather <- factor(df$ave_weather)
  View(df$ave)
  mylogit <- glm(home_win ~ weather_temperature + weather_wind_mph + weather_humidity, data=df, family="binomial")
  summary(mylogit)
}

print(weather_effect_model("", spreadspoke))

#weather_validator <- function()
weather_effect_model("DEN", spreadspoke)

#Graphs the model of home_win vs. weather. Dplyr is masked by GGally.
ggpairs_model_checker <- function(info_data, weights=c(1,1,1)) {
  
  #View(df)
  #df <- fortify(c(((info_data$score_home-info_data$score_away)/abs(info_data$score_home-info_data$score_away)), (info_data$weather_temperature + info_data$weather_wind_mph + as.numeric(info_data$weather_humidity))/3))
  #df <- as.data.frame(df)
  ggpairs(data=df, upper=list(df$home_win), lower=list(df$ave_weather, df$weather_temperature*weights[1], df$weather_wind_mph*weights[2], as.numeric(df$weather_humidity)*weights[3]), title="Home Win vs. Weather", cardinality_threshold = 100)
}

# ggpairs_model_checker(spreadspoke, c(3, 2, 0.5))

individual_model_checker <- function(info_data) {
  df <- prepare_for_model(info_data)
  #View(df)
  ggpairs(data=df, columns=1:2, title="Home Win vs. Weather")
}

#Returns the likehood the given team will win based on weather conditions. 
get_likelihood <- function(team) {
  value <- round(runif(1), digits=2)
}

cat(get_likelihood("DEN"))

#humidity corr: -0.00712
#temperature corr: -0.0344
#wind_mph corr: .0247
#individual_model_checker("weather_temperature", spreadspoke)
#individual_model_checker("spreadspoke$weather_humidity", spreadspoke)
#individual_model_checker("spreadspoke$weather_wind_mph", spreadspoke)

#weather_effect_model("DEN", spreadspoke)