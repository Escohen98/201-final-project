source("summary.r")

##Added Columns to CSV. 
something <- mutate(spreadspoke, home_id = name_to_id(shorten_name(team_home)), away_id = name_to_id(shorten_name(team_away)))
write.csv(something, file = "../data/spreadspoke_scores.csv", row.names=FALSE)

##Added who won to CSV