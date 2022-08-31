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

#-----------------------------------------------------------------------------------------------------------------------------------------

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
Nonationalteams$GameR <- NA
Nonationalteams$GameR[Nonationalteams$Game == 'Overwatch'] <- 1
Nonationalteams$GameR[Nonationalteams$Game == 'Starcraft II'] <- 2
Nonationalteams$GameR[Nonationalteams$Game == 'League of Legends'] <- 3
Nonationalteams$GameR[Nonationalteams$Game == 'Fortnite'] <- 4
Nonationalteams$GameR[Nonationalteams$Game == 'Counter-Strike: Global Offensive'] <- 5
Nonationalteams$GameR[Nonationalteams$Game == 'Dota II'] <- 6
Nonationalteams$GameR[Nonationalteams$Game == 'PUBG'] <- 7
Nonationalteams$GameR[Nonationalteams$Game == 'Heroes of the Storm'] <- 8
Nonationalteams$GameR[Nonationalteams$Game == 'Hearthstone'] <- 9


#graphing 
library(rcompanion)
plotNormalHistogram(Nonationalteams$TotalUSDPrize)
#square it 
Nonationalteams$TotalUSDPrize_bySQRT <- sqrt(Nonationalteams$TotalUSDPrize)
plotNormalHistogram(Nonationalteams$TotalUSDPrize_bySQRT)
#USe log
Nonationalteams$TotalUSDPrize_byLOG <- log(Nonationalteams$TotalUSDPrize)
plotNormalHistogram(Nonationalteams$TotalUSDPrize_byLOG)
#Using Log makes the graph Normally distributed. Use log
Nonationalteams$GameR <- as.numeric(Nonationalteams$GameR)

#run a fligner test since columns are numeric
fligner.test(GameR ~ TotalUSDPrize_byLOG, data= Nonationalteams)
#since the p value is > than .05, there is significance and can continue with the analysis
NonationalANOVA <- aov(Nonationalteams$GameR ~ Nonationalteams$TotalUSDPrize)

summary(NonationalANOVA)

#run a MANCOVA analysis

#ensure Variables are Numeric
str(Nonationalteams$GameR)
str(Nonationalteams$TotalUSDPrize)
#Subsetting
keeps <- c("GameR", "TotalUSDPrize")
Nonationalteams1 <- Nonationalteams[keeps]

#format as a Matrix
Nonationalteams2 <- as.matrix(Nonationalteams1)

#Test Assumptions
#Load Libaries
library(mvnormtest)
#drop any missing values 
Nonationalteams3 <- na.omit(Nonationalteams2)

#Multivariate Normality
mshapiro.test(t(Nonationalteams3))
#It does not meat expectations since p value is >.05

#MAKE A HISTOGRAM
library(ggplot2)
ggplot(esportsearningsteams, aes(x = Game)) + geom_bar()
#----------------------------------------------------------------------------------------------------------------------------------------- 


#import data with no national teams. Used SQL to remove national teams from the mix of teams
Nonationalteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/NoNational.csv')

#see the average prize money and tournaments from genre
Average_Prize <- Nonationalteams %>% group_by(Genre) %>% summarize(ave.prize = mean(TotalUSDPrize), ave.tournament = mean(TotalTournaments))
#run an anova 

#-----------------------------------------------------------------------------------------------------------------------------------------
#How do top earning players impact the influence of game or genre based on TotalUSDPrize
#data wrangle for esportsearningsplayers
#remove data that is not needed
esportsplayers <- select(esportsearningsplayers, CurrentHandle, TotalUSDPrize, Game, Genre)
#Top Earners from each game:
OW <- esportsplayers %>% filter(Game == "Overwatch") #Rascal
SC <- esportsplayers %>% filter(Game == "Starcraft II") #Serral
LOL <- esportsplayers %>% filter(Game == "League of Legends") #Faker
Fort <- esportsplayers %>% filter(Game == "Fortnite") #Bugha
CSGO <- esportsplayers %>% filter(Game == "Counter-Strike: Global Offensive") #dupreeh
Dota <- esportsplayers %>% filter(Game == "Dota 2") #N0tail
PUBG <- esportsplayers %>% filter(Game == "PUBG") #Loki
HOTS <- esportsplayers %>% filter(Game == "Heroes of the Storm") #KyoCha
Hearth <- esportsplayers %>% filter(Game == "Hearthstone") #Thijs
 
#Now that we have identified the top earners, put them in their individual data set
Topearners <- c("Rascal", "Serral", "Faker", "Bugha", "dupreeh", "N0tail", "Loki", "KyoCha", "Thijs") 

Topearners2 <- mutate(esportsplayers, CurrentHandle = Topearners)
Topearners1 <- esportsplayers %>% subset(Topearners)

Topearners3 <- esportsplayers %>% distinct(Topearners)
Topearners4 <- subset(esportsplayers, CurrentHandle == c("Rascal", "Serral", "Faker", "Bugha", "dupreeh", "N0tail", "Loki", "KyoCha", "Thijs"))
#RIGHT ANALYSIS
Topearners5 <- esportsplayers[esportsplayers$CurrentHandle %in% Topearners,]

#see total earning by game
esportsplayers[c(1)]

#-----------------------------------------------------------------------------------------------------------------------------------------

#Possible Question
#how does Total tournament impact TotalUSDPrize for games using "Genre"




#compare teams using total tournament and totalusdprize
#use group_by"
TeamWinnings <- esportsearningsteams %>% group_by(TeamName, .groups = "drop") %>% summarise(TotalUSDPrize)
#u
TeamWinnings1 <- esportsearningsteams %>% group_by(TeamName) %>% summarise(TotalUSDPrize_byTeam = sum(TotalUSDPrize))

#RIGHT ANALYSIS
Teamwinnings2 <- esportsearningsteams %>% group_by(TeamName) %>% summarise(TotalUSDPrize_byTeam = sum(TotalUSDPrize), TotalTournaments_byTeam = sum(TotalTournaments))


#make a datasheet for nationalities within players using esportsearningplayers and esportscountry
#lower case the county code in esportscountry
class(esportsearningsplayers$CountryCode)
CountryCode <- (esportscountry$Two_Letter_Country_Code)
Country_Code <- tolower(Country_Code)
str(esportsearningsplayers$CountryCode)
esportsearningsplayers$CountryCode <- as.numeric(esportsearningsplayers$CountryCode)
rename(esportscountry$Two_Letter_Country_Code, y = Country_Code)
Nationalities <- full_join(esportscountry, esportsearningsplayers, by="CountryCode")

old_data <- read.csv()
                                                                     