#import data
esportsearningsplayers <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/E-Sport-Final/highest_earning_players.csv')
esportsearningsteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/E-Sport-Final/highest_earning_teams.csv')
esportscountry <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/E-Sport-Final/country-and-continent-codes-list.csv')
#showcase what game are currently competitive esports games
unique(esportsearningsteams$Game)
# [1] "Overwatch"                        "Starcraft II"                     "League of Legends"                "Fortnite"                        
#[5] "Counter-Strike: Global Offensive"    "Dota 2"                           "PUBG"                             "Heroes of the Storm"             
#[9] "Hearthstone" 

#showcase the teams 
unique(esportsearningsteams$TeamName)
