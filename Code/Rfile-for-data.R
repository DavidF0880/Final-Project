#import data
esportsearningsplayers <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/highest_earning_players.csv')
esportsearningsteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/highest_earning_teams.csv')
esportscountry <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/country-and-continent-codes-list.csv')
#showcase what game are currently competitive esports games
unique(esportsearningsteams$Game)
# [1] "Overwatch"                        "Starcraft II"                     "League of Legends"                "Fortnite"                        
#[5] "Counter-Strike: Global Offensive"    "Dota 2"                           "PUBG"                             "Heroes of the Storm"             
#[9] "Hearthstone" 

#Data Wrangling

#showcase the teams 
unique(esportsearningsteams$TeamName)

#Questions to answer: It is believed that League of legends holds the most influence in E-Sports. Believed that it holds 19% of influence in the industry. Is this true?
#Use Chi_square testing with t-test? Maybe compare to another game to see the difference in influence in the industry.
library(dplyr)
Nonationalteams %>% group_by(Game) %>% summarize(count=n())

#Run analysis
observed = c(100,99,97,36,64,71,100,76,91,99)
expected = c(.19, .09, .09, .09, .09, .09, .09, .09, .09, .09)

chisq.test(x = observed, p = expected) 
#p value is <0.05 meaning that the influence of League of Legends is in fact not 19%

#MAKE A HISTOGRAM
library(ggplot2)
ggplot(esportsearningsteams, aes(x = Game)) + geom_bar()

#import data with no national teams. Used SQL to remove national teams from the mix of teams
Nonationalteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/NoNational.csv')

#