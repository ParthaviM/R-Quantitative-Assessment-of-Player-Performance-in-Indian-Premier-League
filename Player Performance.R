## DELIVERIES DATA
rm(list = ls())
FD <- Final_Deliveries

## EXTRA INDICATORS
FD$ExtraInd <- ifelse(FD$wide_runs!=0 | FD$noball_runs!=0, 1, 0)
View(FD)

# BALLS REMAINING
for (i in 1:nrow(FD)) {
  if (FD$inning[i]==1 & FD$over[i]==1 & FD$ball[i]==1) {
    FD$BallsRem[i] <- 119+FD$ExtraInd[i]
  } else if (FD$inning[i]==2 & FD$over[i]==1 & FD$ball[i]==1) {
    FD$BallsRem[i] <- 119+FD$ExtraInd[i]
  } else if (FD$inning[i]==3 & FD$over[i]==1 & FD$ball[i]==1) {
    FD$BallsRem[i] <- 5+FD$ExtraInd[i]
  } else if (FD$inning[i]==4 & FD$over[i]==1 & FD$ball[i]==1) {
    FD$BallsRem[i] <- 5+FD$ExtraInd[i]
  } else {
    FD$BallsRem[i] <- FD$BallsRem[i-1]-1+FD$ExtraInd[i]
  }
}

## MATCHES AND VENUE SCORES
M <- Matches
V <- Venue_Scores

## APPOINT AVERAGE SCORES TO MATCHES DATA
Merged <- merge(V,M)
View(Merged)
View(Matches)
View(Final_Deliveries)
View(Venue_Scores)

colnames(Merged)[colnames(Merged)=="id"] <- "match_id"

## APPOINT AVERAGE SCORES TO DELIVERIES DATA
MergeFin <- merge(Merged,FD,by = "match_id")
View(MergeFin)

## ORDER DATA
MergeFin <- MergeFin[order(MergeFin$match_id,MergeFin$inning, MergeFin$over, MergeFin$ball),]
View(MergeFin)

## TOTAL RUNS PER BALL PER INNING
for (i in 1:nrow(MergeFin)) {
  if (MergeFin$inning[i]==1 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalRuns[i] <- MergeFin$total_runs[i]
  } else if (MergeFin$inning[i]==2 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalRuns[i] <- MergeFin$total_runs[i]
  } else if (MergeFin$inning[i]==3 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalRuns[i] <- MergeFin$total_runs[i]
  } else if (MergeFin$inning[i]==4 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalRuns[i] <- MergeFin$total_runs[i]
  } else {
    MergeFin$TotalRuns[i] <- MergeFin$TotalRuns[i-1]+MergeFin$total_runs[i]
  }
}
View(MergeFin)
MergeFin <- MergeFin[,-41]

## INIT TARGET - VENUE SCORES FOR INNING 1
## INNINGS 2 AND 4 - ACTUAL TARGET FROM INNINGS 1 AND 3
colnames(MergeFin)[colnames(MergeFin)=="Average Score"] <- "IniTarget"
MergeFin$IniTarget <- ceiling(MergeFin$IniTarget)
for (i in 1:nrow(MergeFin)) {
  if (MergeFin$inning[i]==1) {
    MergeFin$InitTarget[i] <- MergeFin$IniTarget[i]
  } else if (MergeFin$inning[i]==3) {
    MergeFin$InitTarget[i] <- ceiling(MergeFin$IniTarget[i]/20)
  } else if (MergeFin$inning[i]==2){
    if (MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
      MergeFin$InitTarget[i] <- MergeFin$TotalRuns[i-1]+1
    } else {
      MergeFin$InitTarget[i] <- MergeFin$InitTarget[i-1]
    }
  } else if (MergeFin$inning[i]==4){
    if (MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
      MergeFin$InitTarget[i] <- MergeFin$TotalRuns[i-1]+1
    } else {
      MergeFin$InitTarget[i] <- MergeFin$InitTarget[i-1]
    }
  }
}

## RUNS REMAINING TO REACH THE TARGET
MergeFin$RunsRem <- MergeFin$InitTarget - MergeFin$TotalRuns

## WICKETS OUT INDICATOR
MergeFin$WktsOutInd <- ifelse(is.na(MergeFin$player_dismissed)==FALSE, 1, 0)

## TOTAL WICKETS OUT TILL THE CURRENT BALL PER INNING
for (i in 1:nrow(MergeFin)) {
  if (MergeFin$inning[i]==1 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalWkts[i] <- MergeFin$WktsOutInd[i]
  } else if (MergeFin$inning[i]==2 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalWkts[i] <- MergeFin$WktsOutInd[i]
  } else if (MergeFin$inning[i]==3 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalWkts[i] <- MergeFin$WktsOutInd[i]
  } else if (MergeFin$inning[i]==4 & MergeFin$over[i]==1 & MergeFin$ball[i]==1) {
    MergeFin$TotalWkts[i] <- MergeFin$WktsOutInd[i]
  } else {
    MergeFin$TotalWkts[i] <- MergeFin$TotalWkts[i-1]+MergeFin$WktsOutInd[i]
  }
}

## OVERS AVAILABLE
MergeFin$OversAvailable <- 20 - MergeFin$over

## DL TABLE
View(DL)
colnames(DL)[colnames(DL)=="Overs_Available"] <- "OversAvailable"
colnames(DL)[colnames(DL)=="Wickets_Lost"] <- "TotalWkts"

## REMAINING DL RESOURCES PER BALL - DELIVERIES AND DL
MergeFinal <- merge(MergeFin,DL)
View(MergeFinal)

## ORDER
MergeFinal <- MergeFinal[order(MergeFinal$match_id,MergeFinal$inning, MergeFinal$over, MergeFinal$ball),]

## TARGET UPDATION
MergeFinal$NewTarget <- MergeFinal$TotalRuns + (MergeFinal$InitTarget * MergeFinal$DL_Rem/100)
MergeFinal$NewTarget <- ceiling(MergeFinal$NewTarget)

for (i in 1:nrow(MergeFinal)) {
  if(MergeFinal$NewTarget[i] < MergeFinal$InitTarget[i]){
    MergeFinal$NewTarget[i] <- MergeFinal$InitTarget[i]
  } else {
    MergeFinal$NewTarget[i] <- MergeFinal$NewTarget[i]
  }
}

colnames(MergeFinal)[colnames(MergeFinal)=="season"] <- "Year"

## MODELING PLAYERS
PD <- Player_Data
Squads <- Squad_Final
Merge <- merge(Squads, PD)
View(Merge)
Merge <- Merge[order(Merge$Year, Merge$Players),]

## MISSING VALUE TREATMENT
Merge$Bat_Inns[which(is.na(Merge$Bat_Inns))] <- 0
Merge$Bowl_Inns[which(is.na(Merge$Bowl_Inns))] <- 0
Merge$Bat_Mat[which(is.na(Merge$Bat_Mat))] <- 0
Merge$Bat_Mat[which(Merge$Bat_Mat==0)] <- 99999
Merge$Bowl_Mat[which(is.na(Merge$Bowl_Mat))] <- 99999
Merge$BatScore[which(is.na(Merge$BatScore))] <- 0
Merge$BowlScore[which(is.na(Merge$BowlScore))] <- 0


## EXTRA
for (i in 1:nrow(Merge)) {
  if (Merge$Team[i]=="KIXP") {
    Merge$Team[i] <- "KXIP"
  } else {
    Merge$Team[i] <- Merge$Team[i]
  }
}

for (i in 1:nrow(MergeFinal)) {
  if (MergeFinal$batting_team[i]=="Rising Pune Supergiants") {
    MergeFinal$batting_team[i] <- "Rising Pune Supergiant"
  } else {
    MergeFinal$batting_team[i] <- MergeFinal$batting_team[i]
  }
}

for (i in 1:nrow(MergeFinal)) {
  if (MergeFinal$bowling_team[i]=="Rising Pune Supergiants") {
    MergeFinal$bowling_team[i] <- "Rising Pune Supergiant"
  } else {
    MergeFinal$bowling_team[i] <- MergeFinal$bowling_team[i]
  }
}

## TOTAL BAT AND BOWL SCORE PER SQUAD PER YEAR
t <- tapply(Merge$BatFin, list(Merge$Year, Merge$Team), sum)
TotalBatScore <- as.data.frame(t)

s <- tapply(Merge$BowlFin, list(Merge$Year, Merge$Team), sum)
TotalBowlScore <- as.data.frame(s)

## EXTRA 
for (i in 1:nrow(Merge)) {
  if (Merge$Team[i]=="CSK") {
    Merge$Team[i] <- "Chennai Super Kings"
  } else if (Merge$Team[i]=="MI") {
    Merge$Team[i] <- "Mumbai Indians"
  } else if (Merge$Team[i]=="RR") {
    Merge$Team[i] <- "Rajasthan Royals"
  } else if (Merge$Team[i]=="GL") {
    Merge$Team[i] <- "Gujarat Lions"
  } else if (Merge$Team[i]=="KXIP") {
    Merge$Team[i] <- "Kings XI Punjab"
  } else if (Merge$Team[i]=="DD") {
    Merge$Team[i] <- "Delhi Daredevils"
  } else if (Merge$Team[i]=="KKR") {
    Merge$Team[i] <- "Kolkata Knight Riders"
  } else if (Merge$Team[i]=="SRH") {
    Merge$Team[i] <- "Sunrisers Hyderabad"
  } else if (Merge$Team[i]=="RCB") {
    Merge$Team[i] <- "Royal Challengers Bangalore"
  } else if (Merge$Team[i]=="RPS") {
    Merge$Team[i] <- "Rising Pune Supergiant"
  } 
}

for (i in 1:nrow(Merge)) {
  if (Merge$Team[i]=="DC") {
    Merge$Team[i] <- "Delhi Capitals"
  } else {
    Merge$Team[i] <- Merge$Team[i]
  }
}


## IMPORT TOTAL BAT AND BOWL SCORES
colnames(TotalBatScore)[colnames(TotalBatScore)=="Team"] <- "batting_team"
colnames(TotalBowlScore)[colnames(TotalBowlScore)=="Team"] <- "bowling_team"
TotalBatScore$Year <- as.factor(TotalBatScore$Year)
MergeFinal$Year <- as.factor(MergeFinal$Year)

## DO NOT NEED 2019 DATA
TotalBowlScore <- TotalBowlScore[-(45:55),]
TotalBatScore <- TotalBatScore[-(45:55),]


## IMPORT AGAIN FROM EXCEL AFTER VLOOKUP
## MERGEDATAFINAL IS THE FINAL FILE

## CODE FOR REM BAT SCORE
for(i in 1:nrow(MergeDataFinal)){
  if(MergeDataFinal$inning[i] == 1 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] ==1){
    if(MergeDataFinal$WktsOutInd[i] == 0){
      MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i]
    } else{
      if(MergeDataFinal$player_dismissed[i] == MergeDataFinal$batsman[i]){
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinStriker[i]  
      } else{
          MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinNonStriker[i]  
        }
      }
  } else if(MergeDataFinal$inning[i] == 2 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] ==1){
    if(MergeDataFinal$WktsOutInd[i] == 0){
      MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i]
    } else{
      if(MergeDataFinal$player_dismissed[i] == MergeDataFinal$batsman[i]){
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinStriker[i]  
      } else{
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinNonStriker[i]  
      }
    }
  } else if(MergeDataFinal$inning[i] == 3 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] ==1){
    if(MergeDataFinal$WktsOutInd[i] == 0){
      MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i]
    } else{
      if(MergeDataFinal$player_dismissed[i] == MergeDataFinal$batsman[i]){
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinStriker[i]  
      } else{
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinNonStriker[i]  
      }
    }
  } else if(MergeDataFinal$inning[i] == 4 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] ==1){
    if(MergeDataFinal$WktsOutInd[i] == 0){
      MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i]
    } else{
      if(MergeDataFinal$player_dismissed[i] == MergeDataFinal$batsman[i]){
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinStriker[i]  
      } else{
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$TotalBatScore[i] - MergeDataFinal$BatFinNonStriker[i]  
      }
    }
  } else{
    if(MergeDataFinal$WktsOutInd[i] == 0){
      MergeDataFinal$remBatScore[i] <- MergeDataFinal$remBatScore[i-1]
    } else{
      if(MergeDataFinal$player_dismissed[i] == MergeDataFinal$batsman[i]){
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$remBatScore[i-1] - MergeDataFinal$BatFinStriker[i]  
      } else{
        MergeDataFinal$remBatScore[i] <- MergeDataFinal$remBatScore[i-1] - MergeDataFinal$BatFinNonStriker[i]  
      }
    }
  }
}



## REM BOWL SCORE
for(i in 1:nrow(MergeDataFinal)){
  if(MergeDataFinal$inning[i] == 1 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$remBowlScore[i] <- MergeDataFinal$TotalBowlScore[i] 
  } else if(MergeDataFinal$inning[i] == 2 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$remBowlScore[i] <- MergeDataFinal$TotalBowlScore[i]
  } else if(MergeDataFinal$inning[i] == 3 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$remBowlScore[i] <- MergeDataFinal$TotalBowlScore[i]
  } else if(MergeDataFinal$inning[i] == 4 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$remBowlScore[i] <- MergeDataFinal$TotalBowlScore[i]
  } else{
    MergeDataFinal$remBowlScore[i] <- MergeDataFinal$remBowlScore[i-1] - (1/24)*MergeDataFinal$BowlScore[i]
  }
}


# InitTarget is CORRECT
MergeDataFinal$iniRunRate <- MergeDataFinal$InitTarget/20

MergeDataFinal$RunsRem <- MergeDataFinal$NewTarget - MergeDataFinal$TotalRuns
MergeDataFinal$reqRunRate <- (MergeDataFinal$RunsRem/MergeDataFinal$BallsRem)*6

MergeDataFinal$k <- MergeDataFinal$RunsRem/MergeDataFinal$NewTarget
MergeDataFinal$r <- MergeDataFinal$reqRunRate/MergeDataFinal$iniRunRate

MergeDataFinal$b <- MergeDataFinal$remBatScore/MergeDataFinal$TotalBatScore
MergeDataFinal$w <- MergeDataFinal$remBowlScore/MergeDataFinal$TotalBowlScore

## ARBITRARY VALUES OF ALPHA AND BETA

alpha <- 2

beta <- 1.5

## WORK INDEX CALCULATIONS
MergeDataFinal$battingWorkIndex <- 100 * MergeDataFinal$k * (MergeDataFinal$r + alpha * (1 - MergeDataFinal$b) + (beta * MergeDataFinal$w))
MergeDataFinal$bowlingWorkIndex <- 100 * MergeDataFinal$k * ((1/MergeDataFinal$r) + (alpha * MergeDataFinal$b) + (beta * (1-MergeDataFinal$w)))


## CALCULATING DIFFERENCES IN CONSECUTIVE WORK INDICES FOR UTILITY SCORES
for(i in 1:nrow(MergeDataFinal)){
  if(MergeDataFinal$inning[i] == 1 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$batDiff[i] <- 0 
  } else if(MergeDataFinal$inning[i] == 2 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$batDiff[i] <- 0 
  } else if(MergeDataFinal$inning[i] == 3 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$batDiff[i] <- 0 
  } else if (MergeDataFinal$inning[i] == 4 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$batDiff[i] <- 0 
  } else if (MergeDataFinal$BallsRem[i] == 0){
    MergeDataFinal$batDiff[i] <- 0
  }
  else{
    MergeDataFinal$batDiff[i] <- MergeDataFinal$battingWorkIndex[i] - MergeDataFinal$battingWorkIndex[i-1]
  }
}


for(i in 1:nrow(MergeDataFinal)){
  if(MergeDataFinal$inning[i] == 1 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$bowlDiff[i] <- 0 
  } else if(MergeDataFinal$inning[i] == 2 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$bowlDiff[i] <- 0 
  } else if(MergeDataFinal$inning[i] == 3 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$bowlDiff[i] <- 0 
  } else if (MergeDataFinal$inning[i] == 4 & MergeDataFinal$over[i] == 1 & MergeDataFinal$ball[i] == 1){
    MergeDataFinal$bowlDiff[i] <- 0 
  } else if (MergeDataFinal$BallsRem[i] == 0){
    MergeDataFinal$bowlDiff[i] <- 0
  }
  else{
    MergeDataFinal$bowlDiff[i] <- MergeDataFinal$bowlingWorkIndex[i] - MergeDataFinal$bowlingWorkIndex[i-1]
  }
}


## REPLACING FIRST AND LAST VALUES
MergeDataFinal$batDiff[MergeDataFinal$batDiff == "Inf"] <- 0
MergeDataFinal$batDiff[MergeDataFinal$batDiff == "-Inf"] <- 0

MergeDataFinal$bowlDiff[MergeDataFinal$bowlDiff == "Inf"] <- 0
MergeDataFinal$bowlDiff[MergeDataFinal$bowlDiff == "-Inf"] <- 0


## SUMMING THE UTILITY SCORE FOR EACH BATSMAN PER MATCH
a <- aggregate(MergeDataFinal$batDiff, by = list(MergeDataFinal$match_id, MergeDataFinal$batsman), FUN = sum)
View(a)

colnames(a)[colnames(a) == "Group.1"] <- "match_id"
colnames(a)[colnames(a) == "Group.2"] <- "batsman"
colnames(a)[colnames(a) == "x"] <- "UtilityScore"


## SUMMING THE UTILITY SCORE FOR EACH BOWLER PER MATCH
b <- aggregate(MergeDataFinal$bowlDiff, by = list(MergeDataFinal$match_id, MergeDataFinal$bowler), FUN = sum)
View(b)


colnames(b)[colnames(b) == "Group.1"] <- "match_id"
colnames(b)[colnames(b) == "Group.2"] <- "bowler"
colnames(b)[colnames(b) == "x"] <- "UtilityScore"


## FINDING THE MEAN UTILITY OF BATSMEN
r_a <- aggregate(a$UtilityScore, by = list(a$batsman), FUN = mean)
View(r_a)
colnames(r_a)[colnames(r_a) == "Group.1"] <- "batsman"
colnames(r_a)[colnames(r_a) == "x"] <- "rank"

## FINDING THE MEAN UTILITY OF BOWLERS
r_b <- aggregate(b$UtilityScore, by = list(b$bowler), FUN = mean)
View(r_b)
colnames(r_b)[colnames(r_b) == "Group.1"] <- "bowler"
colnames(r_b)[colnames(r_b) == "x"] <- "rank"


## IMPORT UTILITYFINAL
Winning_Team <- Winning_Team[order(Winning_Team$`Match ID`, Winning_Team$`Final Utility`),]
View(Winning_Team)
m <- Winning_Team[match(unique(Winning_Team$`Match ID`), Winning_Team$`Match ID`),] 
View(m)
