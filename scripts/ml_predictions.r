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
#  leaps <- regsubsets(factor(away_win) ~ weather_temperature + weather_wind_mph + weather_humidity, data=df, nbest=10, weights = weight, really.big=T)
#  summary(leaps)
  #fit <- lm(leaps, data=get_team_data(team,info_data), nbest=10)
#  plot(leaps, scale="r2")
  #library(car)
  #subsets(leaps, statistic="rsq")
#}

  
weather_effect_model <- function(team, info_data, weight=c(1,1,1)) {
  df <- cheated_categorize_weather(prepare_for_model(info_data), 1000, 1)
  head(df)
  #Step 1
  sapply(df, sd)
  print(xtabs(~away_win + ave_weather, data=df))
  #Step 2
  df$ave_weather <- factor(df$ave_weather)
  df$weather_humidity <- as.numeric(df$weather_humidity)
  mylogit <- glm(away_win ~ weather_temperature + weather_wind_mph + weather_humidity + ave_weather, 
                 data=df, family="binomial")
  summary(mylogit)
  #Chi-Squared test (Step 3)
  print(wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 5:7))
  print(exp(cbind(OR = coef(mylogit), confint(mylogit))))
  
  #Step 4
  
  with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
}

weather_effect_model("", spreadspoke)

#weather_validator <- function()
weather_effect_model("DEN", spreadspoke)

#Returns the probability a given team will win a game given the temperatures.
get_data1 <- function(mylogit, df) {
  newdata1 <- with(df, data.frame(weather_temperature = mean(weather_temperature), 
                                  weather_humidity = mean(weather_humidity), 
                                  weather_wind_mph = mean(weather_wind_mph), ave_weather = factor(1:4)))
  newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
  newdata1
}

#Returns a comparison showing the impact of the given choice has on the result. 
get_data2 <- function(mylogit, df, newdata1, choice) {
  coeffs <- c(df$weather_temperature, df$weather_humidity, df$weather_wind_mph)
  the_choice = coeffs[choice]
  coeffs <- coeffs[!coeffs[choice]]
  newdata2 <- with(df, data.frame(the_choice = rep(seq(from = 20, to = 80, length.out = 100),
                                                4), coeffs[1] = mean(coeffs[1]), coeffs[2] = mean(coeffs[2]),
                                      ave_weather = factor(rep(1:4, each = 100))))
}

#"The code to generate the predicted probabilities (the first line below) is the same as before, 
#except we are also going to ask for standard errors so we can plot a confidence interval. 
#We get the estimates on the link scale and back transform both the predicted values and 
#confidence limits into probabilities."
get_data3 <- function(mylogit, df, newdata2) {
  newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                      se = TRUE))
  newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
}
#Graphs the model of away_win vs. weather. Dplyr is masked by GGally.
ggpairs_model_checker <- function(info_data, weights=c(1,1,1)) {
  
  #View(df)
  #df <- fortify(c(((info_data$score_home-info_data$score_away)/abs(info_data$score_home-info_data$score_away)), 
  #(info_data$weather_temperature + info_data$weather_wind_mph + as.numeric(info_data$weather_humidity))/3))
  #df <- as.data.frame(df)
  ggpairs(data=df, upper=list(df$away_win), lower=list(df$ave_weather, df$weather_temperature*weights[1], 
                                                       df$weather_wind_mph*weights[2], 
                                                       as.numeric(df$weather_humidity)*weights[3]), 
                                                       title="Home Win vs. Weather", cardinality_threshold = 100)
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