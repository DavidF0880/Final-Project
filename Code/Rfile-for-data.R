#import data
esportsearningsplayers <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/highest_earning_players.csv')
esportsearningsteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/highest_earning_teams.csv')
esportscountry <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/country-and-continent-codes-list.csv')

#Run necessary libraries
library(dplyr)
library(rcompanion)
library(mvnormtest)
library(ggplot2)
library(car)
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

#run a MANOVA analysis

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

#drop any missing values 
Nonationalteams3 <- na.omit(Nonationalteams2)

#Multivariate Normality
mshapiro.test(t(Nonationalteams3))
#It does not meat expectations since p value is >.05

#computing Post Hocs with No Adjustment
#using pairwise.t.test()
pairwise.t.test(Nonationalteams$GameR, Nonationalteams$TotalUSDPrize)
#compute means and draw conclusion
NonationalMeans <- Nonationalteams %>% group_by(GameR) %>% summarize(Mean = mean(TotalUSDPrize_byLOG))
#MAKE A HISTOGRAM
ggplot(esportsearningsteams, aes(x = Game)) + geom_bar()
#CSGO has the most influence in gaming. Overall they have around the same amount of influence ranging from 10-12


#----------------------------------------------------------------------------------------------------------------------------------------- 


#import data with no national teams. Used SQL to remove national teams from the mix of teams
Nonationalteams <- read.csv('C:/Users/David/Documents/GitHub/Final-Project/Final-Project/Data/NoNational.csv')
sum(is.na(Nonationalteams))
Nonationalteams <- na.omit(Nonationalteams)
#see the average prize money and tournaments from genre
Average_Prize <- Nonationalteams %>% group_by(Genre) %>% summarize(ave.prize = mean(TotalUSDPrize), ave.tournament = mean(TotalTournaments))
Average_Prize <- na.omit(Average_Prize)
Average_Prize
sum(is.na(Average_Prize))
#Make values numeric
Average_Prize$GenreR <- NA 
Average_Prize$GenreR[Average_Prize$Genre == "Battle Royale"] <- 1
Average_Prize$GenreR[Average_Prize$Genre == "Collectible Card Game"] <- 2
Average_Prize$GenreR[Average_Prize$Genre == "First-Person Shooter"] <- 3
Average_Prize$GenreR[Average_Prize$Genre == "Multiplayer Online Battle Arena"] <- 4
Average_Prize$GenreR[Average_Prize$Genre == "Strategy"] <- 5

#plot 
plotNormalHistogram(Average_Prize$ave.prize)
#square it
Average_Prize$ave.prizeBYSQRT <- Average_Prize$ave.prize ^2
plotNormalHistogram(Average_Prize$ave.prizeBYSQRT)
#log
Average_Prize$ave.prizeBYLOG <- log(Average_Prize$ave.prize)
plotNormalHistogram(Average_Prize$ave.prizeBYLOG)
#run analysis with LOG

#run an anova 
bartlett.test(Genre ~ ave.prizeBYLOG, data= Average_Prize)

fligner.test(GenreR ~ ave.prize, data= Average_Prize)

#Tests show a NAN and NA for values but still continuing with analysis for practice.
Average_prizeANOVA <- aov(Average_Prize$GenreR ~ Average_Prize$ave.prize)

summary(Average_prizeANOVA)
#perform an Anova test

Ave.Anova <- lm(GenreR ~ ave.prize, data=Average_Prize)
Anova(Ave.Anova, Type="II", white.adjust=TRUE)
#compute post hocs with no adjustment
pairwise.t.test(Average_Prize$GenreR, Average_Prize$ave.prize, p.adjust="bonferroni")

Average_PrizeMeans <- Average_Prize %>% group_by(GenreR) %>% summarize(Mean = mean(ave.prize))

#First person shooters has the most influence in E Sports based on the mean of their ave.prize


#-----------------------------------------------------------------------------------------------------------------------------------------
#Seeing the other test for most influencial game, is it possible for a player from another game to hold top influence compared to the previous test
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
#Subset
Topearners <- c("Rascal", "Serral", "Faker", "Bugha", "dupreeh", "N0tail", "Loki", "KyoCha", "Thijs") 

Topearners2 <- mutate(esportsplayers, CurrentHandle = Topearners)
Topearners1 <- esportsplayers %>% subset(Topearners)

Topearners3 <- esportsplayers %>% distinct(Topearners)
Topearners4 <- subset(esportsplayers, CurrentHandle == c("Rascal", "Serral", "Faker", "Bugha", "dupreeh", "N0tail", "Loki", "KyoCha", "Thijs"))
#RIGHT ANALYSIS
Topearners5 <- esportsplayers[esportsplayers$CurrentHandle %in% Topearners,] 


#IV:Current Handle
#DV: TotalUSDPrize
plotNormalHistogram(Topearners5$TotalUSDPrize)
#square it
Topearners5$TotalUSDPrizeBYSQRT <- Topearners5$TotalUSDPrize^2
plotNormalHistogram(Topearners5$TotalUSDPrizeBYSQRT)
#cube it
Topearners5$TotalUSDPrizeBYCUBE <- Topearners5$TotalUSDPrize^3
plotNormalHistogram(Topearners5$TotalUSDPrizeBYCUBE)
#Log it
Topearners5$TotalUSDPrizeBYLOG <- log(Topearners5$TotalUSDPrize)
plotNormalHistogram(Topearners5$TotalUSDPrizeBYLOG)
#the closest to a normal distributed will be log so use log for the analysis

#make values numeric

Topearners5$CurrentHandleN <- NA
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'Rascal'] <- 1
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'Serral'] <- 2
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'Faker'] <- 3
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'Bugha'] <- 4
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'dupreeh'] <- 5
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'N0tail'] <- 6
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'Loki'] <- 7
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'KyoCha'] <- 8
Topearners5$CurrentHandleN[Topearners5$CurrentHandle == 'Thijs'] <- 9

Topearners6 <- na.omit(Topearners5)
#Run an ANOVA
bartlett.test(Topearners6$CurrentHandleN ~ Topearners6$TotalUSDPrizeBYLOG )


fligner.test(CurrentHandleN ~ TotalUSDPrizeBYLOG, data = Topearners6 )
#perform an Anova test
TopearnersANOVA <- aov(Topearners6$CurrentHandleN ~ Topearners6$TotalUSDPrize)

summary(TopearnersANOVA)


TopAnova <- lm(CurrentHandleN ~ TotalUSDPrize, data=Topearners6)
Anova(TopAnova, Type="II", white.adjust=TRUE)
#compute post hocs with bonferroni adjustment
pairwise.t.test(Topearners6$CurrentHandleN, Topearners6$TotalUSDPrize, p.adjust="bonferroni")

TopMeans <- Topearners6 %>% group_by(CurrentHandleN) %>% summarize(Mean = mean(TotalUSDPrize))
#just like in the test for most influencial game, the top player for the game also holds the most influence compared to the other top earners for other games
#-----------------------------------------------------------------------------------------------------------------------------------------

                                                                     