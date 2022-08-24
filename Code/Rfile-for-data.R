#import data
esportsearningsplayers <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/highest_earning_players.csv')
esportsearningsteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/highest_earning_teams.csv')
esportscountry <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/country-and-continent-codes-list.csv')
#showcase what game are currently competitive esports games
unique(esportsearningsteams$Game)
##
# [1] "Overwatch"                        "Starcraft II"                     "League of Legends"                "Fortnite"                        
#[5] "Counter-Strike: Global Offensive"    "Dota 2"                           "PUBG"                             "Heroes of the Storm"             
#[9] "Hearthstone" 

#Data Wrangling

#showcase the teams 
unique(esportsearningsteams$TeamName)

class(esportsearningsteams$Game)


#Questions to answer: It is believed that League of legends holds the most influence in E-Sports. Believed that it holds 19% of influence in the industry. Is this true?
#Use Chi_square testing with t-test? Maybe compare to another game to see the difference in influence in the industry.
library(dplyr)
Nonationalteams %>% group_by(Game) %>% summarize(count=n())

#Run analysis
observed = c(100,99,97,36,64,71,100,76,91,99)
expected = c(.19, .09, .09, .09, .09, .09, .09, .09, .09, .09)

chisq.test(x = observed, p = expected) 
#p value is <0.05 meaning that the influence of League of Legends is in fact not 19%
#get mean values, summaries, group_by
#anova over the means of the values
#####

#make Game numeric
esportsearningsteams$GameR <- NA
esportsearningsteams$GameR[esportsearningsteams$Game == 'Overwatch'] <- 0
esportsearningsteams$GameR[esportsearningsteams$Game == 'Starcraft II'] <- 1
esportsearningsteams$GameR[esportsearningsteams$Game == 'League of Legends'] <- 2
esportsearningsteams$GameR[esportsearningsteams$Game == 'Fortnite'] <- 3
esportsearningsteams$GameR[esportsearningsteams$Game == 'Counter-Strike: Global Offensive'] <- 4
esportsearningsteams$GameR[esportsearningsteams$Game == 'Dota II'] <- 5
esportsearningsteams$GameR[esportsearningsteams$Game == 'PUBG'] <- 6
esportsearningsteams$GameR[esportsearningsteams$Game == 'Heroes of the Storm'] <- 7
esportsearningsteams$GameR[esportsearningsteams$Game == 'Hearthstone'] <- 8


library(rcompanion)
Nonationalteams1 <- na.omit(Nonationalteams)
plotNormalHistogram(Nonationalteams1$TotalUSDPrize)
#square it 
Nonationalteams1$TotalUSDPrize_bySQRT <- sqrt(Nonationalteams1$TotalUSDPrize)
plotNormalHistogram(Nonationalteams1$TotalUSDPrize_bySQRT)
#USe log
Nonationalteams1$TotalUSDPrize_byLOG <- log(Nonationalteams1$TotalUSDPrize)
plotNormalHistogram(Nonationalteams1$TotalUSDPrize_byLOG)
#Using Log makes the graph Normally distributed. Use log
bartlett.test(GameR ~ TotalUSDPrize , data = esportsearningsteams)



#MAKE A HISTOGRAM
library(ggplot2)
ggplot(esportsearningsteams, aes(x = Game)) + geom_bar()

#import data with no national teams. Used SQL to remove national teams from the mix of teams
Nonationalteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/NoNational.csv')

#see the average prize money from genre
Average_Prize <- Nonationalteams %>% group_by(Genre) %>% summarize(ave.prize = mean(TotalUSDPrize)) 

#data wrangle for esportsearningsplayers
#remove data that is not needed
esportsplayers <- select(esportsearningsplayers, CurrentHandle, TotalUSDPrize, Game, Genre)
#see total earning by game
Gamesearnings <- esportsplayers %>% group_by(Game) %>% summarize(ave.prize = mean(TotalUSDPrize))

#Possible Question
#how does Total tournament impact TotalUSDPrize for games using "Genre"

#compare teams using total tournament and totalusdprize
#use group_by
TeamWinnings <- esportsearningsteams %>% group_by(TeamName) %>% summarise() group_by(TeamId)
#u

#make a datasheet for nationalities within players using esportsearningplayers and esportscountry
#lower case the county code in esportscountry
CountryCode <- (esportscountry$Two_Letter_Country_Code)
Country_Code <- tolower(Country_Code)
rename(esportscountry$Two_Letter_Country_Code, y = Country_Code)
Nationalities <- full_join(esportscountry, esportsearningsplayers, by="CountryCode")
                                                                     