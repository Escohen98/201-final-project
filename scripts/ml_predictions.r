#All methods in this file are used to train, validate, test, and visualize 
#a Logistical Regression Machine Learning Model to determine NFL game outcomes 
#based on weather data. 
#The majority of the code was taken, implemented, and modified from 
#https://stats.idre.ucla.edu/r/dae/logit-regression/

library(aod)
library(ggplot2)
library(knitr)
library(caret)

source("Summary.r")
  
weather_effect_model <- function(team, info_data) {
  df <- categorize_weather(prepare_for_model(info_data))
  #Step 1
  sapply(df, sd)
  #Step 2
  df$ave_weather <- factor(df$ave_weather)
  df$weather_humidity <- as.numeric(df$weather_humidity)
  mylogit <- glm(away_win ~ weather_temperature + weather_wind_mph + weather_humidity + ave_weather, 
                 data=df, family="binomial")
  list(mylogit, df)
}

visualize <- function(mylogit) {
  summary(mylogit)
  sapple(df, sd)
  print(xtabs(~away_win + ave_weather, data=df))
  #Chi-Squared test (Step 3)
  print(wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 5:7))
  print(exp(cbind(OR = coef(mylogit), confint(mylogit))))
  with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
}

#weather_effect_model("", spreadspoke)

#Returns the probability a given team will win a game given the temperatures.
get_data1 <- function(team, data) {
  model <- weather_effect_model(team, data)
  mylogit <- model[1]
  df <- model[2]
  newdata1 <- with(df, data.frame(weather_temperature = mean(weather_temperature), 
                                  weather_humidity = mean(weather_humidity), 
                                  weather_wind_mph = mean(weather_wind_mph), ave_weather = factor(1:4)))
  newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
  c(model, newdata1)
}

#Returns a comparison showing the impact of the given choice has on the result. 
#choice must be the following:
## Key | Value
##  1  | weather_temperature
##  2  | weather_humidity
##  3  | weather_wind_mph
get_data2 <- function(model) {
  mylogit <- model[[1]]
  df <- model[[2]]
  #coeffs <- c(df$weather_temperature, df$weather_humidity, df$weather_wind_mph)
  #coeffs <- coeffs[!coeffs[choice]]
  newdata2 <- with(df, data.frame(weather_wind_mph = rep(seq(from = 0, to = 40, length.out = 100),
              4), weather_temperature = mean(weather_temperature), weather_humidity = mean(weather_humidity), 
              ave_weather = factor(rep(1:4, each = 100))))
  newdata2
}

#"The code to generate the predicted probabilities (the first line below) is the same as before, 
#except we are also going to ask for standard errors so we can plot a confidence interval. 
#We get the estimates on the link scale and back transform both the predicted values and 
#confidence limits into probabilities."
#choice must be the following:
## Key | Value
##  1  | weather_temperature
##  2  | weather_humidity
##  3  | weather_wind_mph
get_data3 <- function(model) {
  model2 <- get_data2(model)
  mylogit <- model[[1]]
  df <- model[[2]]
  newdata2 <- model2
  
  newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                      se = TRUE))
  newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
}

plot_data3 <- function(newdata3) {
  ggplot(newdata3, aes(x = weather_wind_mph, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                      ymax = UL, fill = ave_weather), alpha = 0.2) + geom_line(aes(colour = ave_weather),
                                      size = 1)
}

demo <- weather_effect_model("", spreadspoke)
plot_data3(get_data3(demo))
