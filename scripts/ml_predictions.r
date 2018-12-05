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
  
weather_effect_model <- function(team, info_data, weight=c(1,1,1)) {
  df <- categorize_weather(prepare_for_model(info_data), 1000, 1)
  head(df)
  #Step 1
  sapply(df, sd)
  print(xtabs(~away_win + ave_weather, data=df))
  #Step 2
  df$ave_weather <- factor(df$ave_weather)
  df$weather_humidity <- as.numeric(df$weather_humidity)
  mylogit <- glm(away_win ~ weather_temperature + weather_wind_mph + weather_humidity + ave_weather, 
                 data=df, family="binomial")
  c(mylogit, df)
}

visualize <- function(mylogit) {
  summary(mylogit)
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
get_data2 <- function(model, choice) {
  mylogit <- model[1]
  df <- model[2]
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
get_data3 <- function(model, choice) {
  model2 <- get_data2(model, choice)
  mylogit <- model[1]
  df <- model[2]
  newdata2 <- model2[3]
  
  newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                      se = TRUE))
  newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
}

plot_data3 <- function(newdata3) {
  ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                      ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                      size = 1)
}