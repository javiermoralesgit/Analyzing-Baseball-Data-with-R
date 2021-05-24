#Libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggthemes)

#Question 2

#Get WD .csv
getwd()
draft_data = read.csv('/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/draft_data.csv', stringsAsFactors = F)
people = read.csv('/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/people.csv', stringsAsFactors = F)
draft_data
people

#Merging Data Files in to one Data Frame, using outer join in order to return all rows from both tables, joining matching keys of mlbamPlayerID to key_mlbam.
mlbdraftdata = merge(draft_data,people,by.x = c("mlbamPlayerID"),by.y = c("key_mlbam"),all = TRUE)

#Filtering out the 17th overall picks from the merged data frame
seventeeth_overallpicks= mlbdraftdata%>%
  select(name_first_last, year, overall)%>%
  filter(overall == "17")%>%
  arrange(year)

seventeeth_overallpicks

#Dataframe created to add signing bonus of 17th overall players, Source: Baseball Reference
seventeeth_overallpicks$signingbonus = c(57500,67500,60000,70000,100000,125000,125000,100000,126000,188000,192500,250000,410000,450000,450000,895000,900000,975000)
seventeeth_overallpicks

#Dataframe created based on 2019 CPI Index to adjust signing bonus to 2019, Source: Federal Reserve Bank of Minneapolis
seventeeth_overallpicks$CPI = c(247.6,273.2,290.0,299.3,312.2,323.2,329.4,341.4,355.4,372.5,392.6,409.3,421.7,434.1,445.4,457.9,471.3,482.4)
seventeeth_overallpicks

#Calculated Signing bonus in 2019 based on CPI Adjusted Formula, Source: Federal Reserve Bank of Minneapolis
seventeeth_overallpicks$signingbonusesCPIadjusted = seventeeth_overallpicks$signingbonus * (768.3/seventeeth_overallpicks$CPI)
seventeeth_overallpicks

#Mean of all signing bonuses
mean(seventeeth_overallpicks$signingbonusesCPIadjusted)

------------------------------------------------------------------------------------------------------------

#Question 3 #Markov Chain Simulation
data2011 <- read.csv("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/all2011.csv", header=FALSE)
fields <- read.csv("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/fields.csv")
names(data2011) <- fields[, "Header"]

#Variable HALF INNING creates a unique identification for each half-inning in each baseball game
data2011$HALF.INNING <- with(data2011, 
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))

#Variable RUNS.SCORED gives the number of runs scored in each play
data2011$RUNS.SCORED <- with(data2011, (BAT_DEST_ID > 3) +
                               (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

#Get.State defines a state variable based on the runners in each of the three bases and number of outs
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
}

#STATE gives the runner locations and number of out at the beginning of each play
RUNNER1 <- ifelse(as.character(data2011[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2011[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2011[,"BASE3_RUN_ID"])=="", 0, 1)
data2011$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)

#NEW STATE contains the runners and outs at conclusion of the play
NRUNNER1 <- with(data2011, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
NRUNNER2 <- with(data2011, as.numeric(RUN1_DEST_ID==2 | 
                                        RUN2_DEST_ID==2 | BAT_DEST_ID==2))
NRUNNER3 <- with(data2011, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
                                        RUN3_DEST_ID==3 | BAT_DEST_ID==3))
NOUTS <- with(data2011, OUTS_CT + EVENT_OUTS_CT)
data2011$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

#Subset focuses on plays where there is a change in the state or in the number of runs scored
data2011 <- subset(data2011, (STATE!=NEW.STATE) | (RUNS.SCORED>0))

#Subset, restrict attention to complete innings when there are 3 outs
library(plyr)
data.outs <- ddply(data2011, .(HALF.INNING), summarize,
                   Outs.Inning = sum(EVENT_OUTS_CT))
data2011 <- merge(data2011, data.outs)
data2011C <- subset(data2011, Outs.Inning == 3)

#Bat_Event_FL considers plays where there is a batting event, non-batting plays such as steals, wild pitches, passed balls are ignored
data2011C <- subset(data2011, BAT_EVENT_FL == TRUE)

#NEW STATE records runners locations when there was three outs since at that point the runners location doesn't matter when there are 3 outs, so recode formula was set to 3 when the number of outs is equal to 3
library(car)
data2011C$NEW.STATE <- recode(data2011C$NEW.STATE,
                              "c('000 3', '100 3', '010 3', '001 3',
                 '110 3', '101 3', '011 3', '111 3') = '3'")

#Computing the transition probabilities

#One could compute the frequencies of all possible transitions between states by use of the matrix of counts, T.matrix. 24 possible values of the beginning of each state and 25 values of the final state NEW.STATE including the 3 out states.
T.matrix <- with(data2011C, table(STATE, NEW.STATE))

#Matrix converted to probability matrix
P.matrix <- prop.table(T.matrix, 1)

#Row corresponding to transitions from 3 outs, when inning reaches 3-outs then it stays 3 outs, probability of staying in this state is 1
P.matrix <- rbind(P.matrix, c(rep(0, 24), 1))
margin.table(P.matrix, 1)

#"010 0" Starting State Means Runner on second with 0 out as described in case study
P1 <- round(P.matrix["010 0", ], 3)
data.frame(Prob = P1[P1 > 0])

# Simulating the Markov chain
#Function count.runners.out takes a state as input and returns the sum of the number of runners and outs. Function is applied across all the possible states.
count.runners.outs <- function(s)
  sum(as.numeric(strsplit(s,"")[[1]]), na.rm = TRUE)
runners.outs <- sapply(dimnames(T.matrix)[[1]], count.runners.outs)[-25]
R <- outer(runners.outs + 1, runners.outs, FUN="-")
dimnames(R)[[1]] <- dimnames(T.matrix)[[1]][-25]
dimnames(R)[[2]] <- dimnames(T.matrix)[[1]][-25]
R <- cbind(R, rep(0, 24))

# Simulation for Markov Chain is ready, inputs are the probability transition matrix P, the run matrix R and starting state integer with the output being the number of runs scored in the half-inning. For this case study it would be 3 since our situation is 0 outs and runner on second base.
simulate.half.inning <- function(P, R, start=3){
  s <- start; path <- NULL; runs <- 0
  while(s < 25){
    s.new <- sample(1:25, 1, prob = P[s, ])
    path <- c(path, s.new)
    runs <- runs + R[s, s.new]
    s <- s.new
  }
  runs
}

#Simulating 10000 half-innings starting with runner on second and 0 outs
RUNS <- replicate(10000, simulate.half.inning(T.matrix, R))

#Table result is interpreted as how many times does 0 runs occur in a half inning based on simulation, 1 run occurring in a half inning based on simulation.
table(RUNS)

#15% of the time a team score at least 1 run
(912+368+146+52+21+6+2+1+1+1)/10000

-------------------------------------------------------------------------------------------------------------------------------------------------------

#Wasn't able to figure out how to get retrosheet.org 2020 data. Attempted to do book instructions and different ways done through author's blog via my Terminal and parsing code directly in R studio.

#Bill Petti Method via his blog
require(baseballr)
require(dplyr)
require(readr)

get_retrosheet_data(path_to_directory = "/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/retrosheet", 
                    years_to_acquire = 2020)

read_csv("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/adownload.folder/retrosheet/download.folder/unzipped/all2020.csv") %>%
  glimpse()

#Author's Blog Instructions via Rstudio
library(devtools)
source_gist(8892981)
parse.retrosheet2.pbp = function(season){
  # ADJUSTED FOR MAC -- function will work for WINDOWS and MAC
  # download, unzip, append retrosheet data
  # assume current directory has a folder download.folder
  # download.folder has two subfolders unzipped and zipped
  # program cwevent.exe is in unzipped folder (for windows)
  
  download.retrosheet <- function(season){
    # get zip file from retrosheet website
    download.file(
      url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep="")
      , destfile=paste("download.folder", "/zipped/", season, "eve.zip", sep="")
    )
  }
  unzip.retrosheet <- function(season){
    #unzip retrosheet files
    unzip(paste("download.folder", "/zipped/", season, "eve.zip", sep=""), 
          exdir=paste("download.folder", "/unzipped", sep=""))
  }
  create.csv.file=function(year){
    # http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent
    # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
    wd = getwd()
    setwd("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/download.folder/unzipped")
    if (.Platform$OS.type == "unix"){
      system(paste(paste("cwevent -y", year, "-f 0-96"), 
                   paste(year,"*.EV*",sep=""),
                   paste("> all", year, ".csv", sep="")))} else {
                     shell(paste(paste("cwevent -y", year, "-f 0-96"), 
                                 paste(year,"*.EV*",sep=""),
                                 paste("> all", year, ".csv", sep="")))              
                   }
    setwd(wd)
  }
  create.csv.roster = function(year){
    # creates a csv file of the rosters
    filenames <- list.files(path = "/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/download.folder/unzipped/")
    filenames.roster = 
      subset(filenames, substr(filenames, 4, 11)==paste(year,".ROS",sep=""))
    read.csv2 = function(file)
      read.csv(paste("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/download.folder/unzipped/", file, sep=""),header=FALSE)
    R = do.call("rbind", lapply(filenames.roster, read.csv2))
    names(R)[1:6] = c("Player.ID", "Last.Name", "First.Name", 
                      "Bats", "Pitches", "Team")
    wd = getwd()
    setwd("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/download.folder/unzipped")
    write.csv(R, file=paste("roster", year, ".csv", sep=""))
    setwd(wd)
  }
  cleanup = function(){
    # removes retrosheet files not needed
    wd = getwd()
    setwd("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/download.folder/unzipped")
    if (.Platform$OS.type == "unix"){
      system("rm *.EVN")
      system("rm *.EVA")
      system("rm *.ROS")
      system("rm TEAM*")} else {
        shell("del *.EVN")
        shell("del *.EVA")
        shell("del *.ROS")
        shell("del TEAM*")
      }       
    setwd(wd)
    setwd("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/download.folder/zipped")
    if (.Platform$OS.type == "unix"){
      system("rm *.zip")} else {
        shell("del *.zip")
      }
    setwd(wd)
  }
  download.retrosheet(season)
  unzip.retrosheet(season)
  create.csv.file(season)
  create.csv.roster(season)
  cleanup()
}
compute.runs.expectancy <- function(season){
  # changed -- plyr function replaced with dplyr
  # (increases speed from 114 to 30 sec for 2013 data)
  
  # assume that files "allseason.csv" and "fields.csv"
  # are in current working folder
  # for example, if season = 1961, all1961.csv should be
  # available
  
  # returns play-by-play matrix with new variables
  # RUNS.ROI - runs scored in remainder of inning
  # STATE - current runners/outs state
  # NEW.STATE - new runners/outs state (after play)
  # RUNS.STATE - runs value of current runners/outs state
  # RUNS.NEW.STATE - runs value of new runners/outs state
  # RUNS.VALUE - runs value of play event
  
  data.file <- paste("all", season, ".csv", sep="")
  data <- read.csv(data.file, header=FALSE)
  #  fields <- read.csv("fields.csv")
  fields <- read.csv("https://raw.githubusercontent.com/beanumber/baseball_R/master/data/fields.csv")
  names(data) <- fields[, "Header"]
  
  data$RUNS <- with(data, AWAY_SCORE_CT + HOME_SCORE_CT)
  data$HALF.INNING <- with(data, 
                           paste(GAME_ID, INN_CT, BAT_HOME_ID))
  
  data$RUNS.SCORED <- with(data, (BAT_DEST_ID > 3) +
                             (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  RUNS.SCORED.INNING <- aggregate(data$RUNS.SCORED, 
                                  list(HALF.INNING = data$HALF.INNING), sum)
  
  RUNS.SCORED.START <- aggregate(data$RUNS, 
                                 list(HALF.INNING = data$HALF.INNING), "[", 1)
  
  MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  data <- merge(data, MAX)
  N <- ncol(data)
  names(data)[N] <- "MAX.RUNS"
  
  data$RUNS.ROI <- data$MAX.RUNS - data$RUNS
  
  get.state <- function(runner1, runner2, runner3, outs){
    runners <- paste(runner1, runner2, runner3, sep="")
    paste(runners, outs)                      
  }
  
  RUNNER1 <- ifelse(as.character(data[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data[,"BASE3_RUN_ID"])=="", 0, 1)
  data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
  
  NRUNNER1 <- with(data, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
  NRUNNER2 <- with(data, as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2))
  NRUNNER3 <- with(data, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
                                      RUN3_DEST_ID==3 | BAT_DEST_ID==3))
  NOUTS <- with(data, OUTS_CT + EVENT_OUTS_CT)
  
  data$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  
  data <- subset(data, (STATE!=NEW.STATE) | (RUNS.SCORED>0))
  
  #  require(plyr)
  #  data.outs <- ddply(data, .(HALF.INNING), summarize,
  #                     Outs.Inning = sum(EVENT_OUTS_CT))
  #  data <- merge(data, data.outs)
  
  require(dplyr)
  data.outs <- summarize(group_by(data, HALF.INNING),
                         Outs.Inning = sum(EVENT_OUTS_CT))
  data <- merge(data, data.outs)
  
  # for expected runs computation, only consider complete innings
  dataC <- subset(data, Outs.Inning == 3)
  
  RUNS <- summarize(group_by(dataC, STATE), Mean=mean(RUNS.ROI))
  RUNS$Outs <- substr(RUNS$STATE, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ]
  
  RUNS.POTENTIAL <- matrix(c(RUNS$Mean, rep(0, 8)), 32, 1)
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$STATE, "000 3","001 3",
                                     "010 3","011 3","100 3","101 3","110 3","111 3") 
  data$RUNS.STATE <- RUNS.POTENTIAL[data$STATE, ]
  data$RUNS.NEW.STATE <- RUNS.POTENTIAL[data$NEW.STATE, ]
  data$RUNS.VALUE <- data$RUNS.NEW.STATE - data$RUNS.STATE + 
    data$RUNS.SCORED
  
  data
}

# reads in 2020 retrosheet files, creating two new csv files
parse.retrosheet2.pbp(2020)
# move to folder containing all2020.csv, roster2020.csv files 
setwd("/Users/jmoral23/Documents/Jobs/Miami Marlins Interview/download.folder/unzipped")
# computes runs values and other variables for all states
d2020 <- compute.runs.expectancy(2020)