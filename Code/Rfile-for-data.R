#import data
esportsearningsplayers <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/E-Sport-Final/highest_earning_players.csv')
esportsearningsteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/E-Sport-Final/highest_earning_teams.csv')
esportscountry <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/E-Sport-Final/country-and-continent-codes-list.csv')
#showcase what game are currently competitive esports games
unique(esportsearningsteams$Game)
# [1] "Overwatch"                        "Starcraft II"                     "League of Legends"                "Fortnite"                        
#[5] "Counter-Strike: Global Offensive"    "Dota 2"                           "PUBG"                             "Heroes of the Storm"             
#[9] "Hearthstone" 

#Data Wrangling

#showcase the teams 
unique(esportsearningsteams$TeamName)

#Questions to answer: How influential are League of Legends teams compared to the other games in E-Sports using TotalUSDPrize. 
#Use ANOVA testing with t-test? Maybe compare to another game to see the difference in influence in the industry.
#

#independent variable : tournaments.
#dependent variable : teams/TotalUSDPrize (2 dependent variables)

#MAKE A HISTOGRAM
library(ggplot2)
ggplot(esportsearningsteams, aes(x = Game)) + geom_bar()

#import data with no national teams. Used SQL to remove national teams from the mix of teams
Nonationalteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Code/NoNational.csv')

