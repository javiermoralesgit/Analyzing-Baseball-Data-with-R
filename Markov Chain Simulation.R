#Libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggthemes)

#Markov Chain Simulation
data2011 <- read.csv("/Users/jmoral23/Documents/all2011.csv", header=FALSE)
fields <- read.csv("/Users/jmoral23/Documents/fields.csv")
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
