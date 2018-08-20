#'@title NFLTeams
#'@description Tiebreaking Data for the 32 teams in the National Football League
#' The dataset containing necessary information for tiebreaking procedures.  The statistics can be updated using the UpdateData function
#' @format A dataset containing 32 rows and 14 variables
#' \describe{
#'  \item{Team}{Identifier for the team in the NFL}
#'  \item{Conference}{Identifier for the team's conference}
#'  \item{Division}{Identifier for the team's division}
#'  \item{Wins}{Total Games Won}
#'  \item{Losses}{Total Games Lost}
#'  \item{Ties}{Total Games Tied}
#'  \item{ConfWins}{Total Games Won against teams in their own conference}
#'  \item{ConfLosses}{Total Games Lost against teams in their own conference}
#'  \item{ConfTies}{Total Games Tied against teams in their own conference}
#'  \item{DivWins}{Total Games Won against teams in their own division}
#'  \item{DivLosses}{Total Games Lost against teams in their own division}
#'  \item{DivTies}{Total Games Tied against teams in their own division}
#'  \item{SOV}{Combined Win-Loss Record in games in which Team won}
#'  \item{SOS}{Combined Win-Loss Record in games played by Team}
#' }
#' @source \url{"http://www.dummies.com/sports/football/the-national-football-league-conferences/"}
#' @source \url{"http://www.nfl.com/schedules/2016"}
"NFLTeams"

#'@title WeeklyScores
#'@description This is a function that will update the scores from the specified week from NFL.com
#'@param Week the identifier for the week of scores you want (Default is 1)
#'@param Year the identifier for the year of scores you want (Default is 2017)
#'@export
#'@return A dataset of Scores of games according to the format description
#'@format A dataset containing a varying number of rows and 5 variables
#' \describe{
#'  \item{Date}{Day the Game was contested}
#'  \item{AwayTeam}{The road team in the game}
#'  \item{AwayScore}{The points scored by the road team in the game}
#'  \item{HomeTeam}{The home team in the game}
#'  \item{HomeScore}{The points scored by the home team in the game}
#' }
#' @examples
#' WeeklyScores()
#' WeeklyScores(Week=2)
#' @source \url{"http://www.nfl.com/schedules"}
WeeklyScores <- function(Year=2017,Week=1){
  require(rvest)
  require(tidyverse)
  require(stringr)
  url <- paste("http://www.nfl.com/schedules/",Year,"/REG",Week,sep="")
  html <- read_html(url)
  NFL <- html %>% html_nodes(".time,.away,.home,.schedules-list-date") %>% html_text()
  NFL <- gsub("\r","",gsub("\n","",gsub("\t","",NFL)))
  Games <- length(which(NFL=="FINAL"))
  for (i in 1:Games){
    Date <- ifelse(NFL[(8*(i-1)+1)]!="FINAL",NFL[(8*(i-1)+1)],Date)
    NFL <- if(i==1) {NFL} else if(NFL[(8*(i-1)+1)]==Date) {NFL} else {c(NFL[1:(8*(i-1))],Date,NFL[(8*(i-1)+1):(length(NFL))])}
  }
  tmp <- data.frame(matrix(NFL,nrow=Games,byrow=TRUE))
  tmp <- tmp %>%
    mutate(Date=as.character(X1),AwayTeam=as.character(X3),AwayScore=as.numeric(as.character(X5)),HomeTeam=as.character(X8),HomeScore=as.numeric(as.character(X6))) %>%
    dplyr::select(Date,AwayTeam,AwayScore,HomeTeam,HomeScore)
  tmp
}

#'@title WeeklyGames
#'@description This is a function that will gets games from weeks that you do not want to use the scores, or if the games have not yet been played that week
#'@param Week the identifier for the week of games you want (Default is 1)
#'@param Year the identifier for the year of scores you want (Default is 2017)
#'@export
#'@return A dataset of games according to the format description
#'@format A dataset containing a varying number of rows and 5 variables
#' \describe{
#'  \item{Date}{Day the Game was contested}
#'  \item{AwayTeam}{The road team in the game}
#'  \item{AwayScore}{Automatically \code{NA}}
#'  \item{HomeTeam}{The home team in the game}
#'  \item{HomeScore}{Automatically \code{NA}}
#' }
#' @examples
#' WeeklyGames()
#' WeeklyGames(Week=2)
#' @source \url{"http://www.nfl.com/schedules"}
WeeklyGames <- function(Year=2017,Week=1){
  require(rvest)
  require(tidyverse)
  url <- paste("http://www.nfl.com/schedules/",Year,"/REG",Week,sep="")
  html <- read_html(url)
  NFL <- html %>% html_nodes(".time,.away,.home,.schedules-list-date") %>% html_text()
  NFL <- gsub("\r","",gsub("\n","",gsub("\t","",NFL)))
  NFL <- NFL[-1]
  NFL <- sapply(NFL,function(x){str_split(x,"View")[[1]][1]})
  Games <- sum(str_count(NFL,":"))
  for (i in 1:Games){
    Date <- ifelse(str_count(NFL[(6*(i-1)+1)],":")==0,NFL[(6*(i-1)+1)],Date)
    NFL <- if(i==1) {NFL} else if(NFL[(6*(i-1)+1)]==Date) {NFL} else {c(NFL[1:(6*(i-1))],Date,NFL[(6*(i-1)+1):(length(NFL))])}
  }
  tmp <- data.frame(matrix(NFL,nrow=Games,byrow=TRUE))
  tmp <- tmp %>%
    mutate(Date=as.character(X1),AwayTeam=as.character(X3),AwayScore=rep("",Games),HomeTeam=as.character(X6),HomeScore=rep("",Games)) %>%
    dplyr::select(Date,AwayTeam,AwayScore,HomeTeam,HomeScore)
  tmp
}


#'@title WeeklyUpdate
#'@description This is a function that will gets all games for the season, with games being played from WeeklyScores and games yet to be played from WeeklyGames
#'@param Week the identifier for the latest week of games played you want (Default is 17)
#'@param Year the identifier for the year of data you want (Default is 2017)
#'@export
#'@return A dataset of Scores and Games according to the format description
#'@format A dataset containing a varying number of rows and 5 variables
#' \describe{
#'  \item{Date}{Day the Game was contested}
#'  \item{AwayTeam}{The road team in the game}
#'  \item{AwayScore}{The points scored by the road team in the game (\code{NA} if score is not to be recorded)}
#'  \item{HomeTeam}{The home team in the game}
#'  \item{HomeScore}{The points scored by the home team in the game (\code{NA} if score is not to be recorded)}
#' }
#' @examples
#' WeeklyUpdate()
#' WeeklyUpdate(Week=16)
#' @source \url{"http://www.nfl.com/schedules"}
WeeklyUpdate <- function(Year=2017,WeekID=17){
  require(rvest)
  require(tidyverse)
  if (WeekID==17){
    Weeks <- data.frame(Year=rep(Year,WeekID),Week=c(1:WeekID))
    Scores <- Weeks %>% mutate(Scores = map2(Year,Week,WeeklyScores))
    FinalScores <- Scores %>% unnest
    FinalScores
  } else if (WeekID==0) {
    Weeks <- data.frame(Year=rep(Year,17-WeekID),Week=c((WeekID+1):17))
    Games <- Weeks %>% mutate(Games = map2(Year,Week,WeeklyGames))
    RemainingGames <- Games %>% unnest
    RemainingGames %>% mutate(AwayScore=as.integer(AwayScore),HomeScore=as.integer(HomeScore))
  } else {
    Weeks <- data.frame(Year=rep(Year,WeekID),Week=c(1:WeekID))
    Scores <- Weeks %>% mutate(Scores = map2(Year,Week,WeeklyScores))
    FinalScores <- Scores %>% unnest
    Weeks <- data.frame(Year=rep(Year,17-WeekID),Week=c((WeekID+1):17))
    Games <- Weeks %>% mutate(Games = map2(Year,Week,WeeklyGames))
    RemainingGames <- Games %>% unnest
    rbind(FinalScores,RemainingGames) %>% mutate(AwayScore=as.integer(AwayScore),HomeScore=as.integer(HomeScore))
  }
}

#'@title UpdateTeams
#'@description This is a function that will Update all tiebreaking stats from the WeeklyUpdate output and using NFLTeams as a template
#'@param RealScores A vector of the Actual Game Scores
#'@param SimScores A vector of the Games that were simulated
#'@param Games A dataframe of the Games for the current season
#'@export
#'@return A dataset of the same dimensions and format as NFLTeams
UpdateTeams <- function(data,SimScores=NULL){
  if (!is.null(SimScores)) {
    scores <- c(data$ScoreDiff[!is.na(data$ScoreDiff)],SimScores[is.na(data$ScoreDiff)])
    data$ScoreDiff <- scores
  }
  for (i in 1:80){
    HomeID <- grep(data$HomeTeam[i],data$HomeTeam)
    AwayID <- grep(data$HomeTeam[i],data$AwayTeam)
    StatID <- grep(data$HomeTeam[i],NFLTeams$Team)
    HomeVector <- sapply(data$AwayTeam[HomeID],function(x){grep(x,NFLTeams$Team)})
    AwayVector <- sapply(data$HomeTeam[AwayID],function(x){grep(x,NFLTeams$Team)})
    NFLTeams$Wins[StatID] <-
      length(which(data$ScoreDiff[HomeID]>0))+
      length(which(data$ScoreDiff[AwayID]<0))
    NFLTeams$Losses[StatID] <-
      length(which(data$ScoreDiff[HomeID]<0))+
      length(which(data$ScoreDiff[AwayID]>0))
    NFLTeams$Ties[StatID] <-
      length(which(data$ScoreDiff[HomeID]==0))+
      length(which(data$ScoreDiff[AwayID]==0))
    NFLTeams$ConfWins[StatID] <-
      length(which(data$ScoreDiff[HomeID]>0 & NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]))+
      length(which(data$ScoreDiff[AwayID]<0 & NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]))
    NFLTeams$ConfLosses[StatID] <-
      length(which(data$ScoreDiff[HomeID]<0 & NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]))+
      length(which(data$ScoreDiff[AwayID]>0 & NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]))
    NFLTeams$ConfTies[StatID] <-
      length(which(data$ScoreDiff[HomeID]==0 & NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]))+
      length(which(data$ScoreDiff[AwayID]==0 & NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]))
    NFLTeams$DivWins[StatID] <-
      length(which(data$ScoreDiff[HomeID]>0 & NFLTeams$Division[HomeVector]==NFLTeams$Division[StatID]))+
      length(which(data$ScoreDiff[AwayID]<0 & NFLTeams$Division[AwayVector]==NFLTeams$Division[StatID]))
    NFLTeams$DivLosses[StatID] <-
      length(which(data$ScoreDiff[HomeID]<0 & NFLTeams$Division[HomeVector]==NFLTeams$Division[StatID]))+
      length(which(data$ScoreDiff[AwayID]>0 & NFLTeams$Division[AwayVector]==NFLTeams$Division[StatID]))
    NFLTeams$DivTies[StatID] <-
      length(which(data$ScoreDiff[HomeID]==0 & NFLTeams$Division[HomeVector]==NFLTeams$Division[StatID]))+
      length(which(data$ScoreDiff[AwayID]==0 & NFLTeams$Division[AwayVector]==NFLTeams$Division[StatID]))
  }
  for (i in 1:80){
    HomeID <- grep(data$HomeTeam[i],data$HomeTeam)
    AwayID <- grep(data$HomeTeam[i],data$AwayTeam)
    StatID <- grep(data$HomeTeam[i],NFLTeams$Team)
    HomeVector <- sapply(data$AwayTeam[HomeID],function(x){grep(x,NFLTeams$Team)})
    AwayVector <- sapply(data$HomeTeam[AwayID],function(x){grep(x,NFLTeams$Team)})
  W <-
    sum(NFLTeams$Wins[HomeVector][data$ScoreDiff[HomeID]>0]) +
    sum(NFLTeams$Wins[AwayVector][data$ScoreDiff[AwayID]<0])
  L <-
    sum(NFLTeams$Losses[HomeVector][data$ScoreDiff[HomeID]>0]) +
    sum(NFLTeams$Losses[AwayVector][data$ScoreDiff[AwayID]<0])
  T <-
    sum(NFLTeams$Ties[HomeVector][data$ScoreDiff[HomeID]>0]) +
    sum(NFLTeams$Ties[AwayVector][data$ScoreDiff[AwayID]<0])
  NFLTeams$SOV[StatID] <- (W+0.5*T)/(W+L+T)
  W <-
    sum(NFLTeams$Wins[AwayVector]) +
    sum(NFLTeams$Wins[HomeVector])
  L <-
    sum(NFLTeams$Losses[AwayVector]) +
    sum(NFLTeams$Losses[HomeVector])
  T <-
    sum(NFLTeams$Ties[AwayVector]) +
    sum(NFLTeams$Ties[HomeVector])
  NFLTeams$SOS[StatID] <- (W+0.5*T)/(W+L+T)
  }
  NFLTeams
}

#'@name CommonGames
#'@title CommonGames
#'@description These are a pair of functions that will take between 2-4 teams, see which opponents they have in common, and determine
#'either the teams win-loss record against those teams (\code{CommonGames}) or the net points scored in those games (\code{CommonGamesPts}).
#'If the total games played against common opponents between all evaluated teams is less than 4, the output will always be 0.
#'@param Team1 the main team whose statistic is being evaluated by the function
#'@param Team2 a team that \code{Team1} is being evaluated against
#'@param Team3 a team that \code{Team1} is being evaluated against (default is \code{NULL})
#'@param Team4 a team that \code{Team1} is being evaluated against (default is \code{NULL})
#'@param Data the data frame from which Scores are gathered (default is Week 17 \code{WeeklyUpdate})
#'@param SimScores the simulated scores, if a simulation was performed
#'@return \code{CommonGames} a win-loss record in games the other teams played as well.
#'\code{CommonGamesPts} the total net points in games the other teams played as well.
#' @examples
#' CommonGames("Patriots","Dolphins")
#' CommonGames("Patriots","Dolphins","Bills")
#' CommonGames("Patriots","Dolphins","Bills","Jets")
#' CommonGamesPts("Patriots","Dolphins")
#' CommonGamesPts("Patriots","Dolphins","Bills")
#' CommonGamesPts("Patriots","Dolphins","Bills","Jets")
#' @export
CommonGames <- function(Team1,Team2,Team3=NULL,Team4=NULL,data,SimScores=NULL){
  if (!is.null(SimScores)) {
    scores <- c(data$ScoreDiff[!is.na(data$ScoreDiff)],SimScores[is.na(data$ScoreDiff)])
    data$ScoreDiff <- scores
  }
  for(i in 1:80){
    if (data$HomeTeam[i]==Team1) {break}
  }
  for(j in 1:80){
    if (data$HomeTeam[j]==Team2) {break}
  }
  if (is.null(Team3)==FALSE){
    for(k in 1:80){
      if (data$HomeTeam[k]==Team3) {break}
    }
  }
  if (is.null(Team4)==FALSE){
    for(l in 1:80){
      if (data$HomeTeam[l]==Team4) {break}
    }
  }
  Games1 <- c(NFLTeams$Team[sapply(data$HomeTeam[grep(data$HomeTeam[i],data$AwayTeam)],function(x){grep(x,NFLTeams$Team)})],
              NFLTeams$Team[sapply(data$AwayTeam[grep(data$HomeTeam[i],data$HomeTeam)],function(x){grep(x,NFLTeams$Team)})])
  Games2 <- c(NFLTeams$Team[sapply(data$HomeTeam[grep(data$HomeTeam[j],data$AwayTeam)],function(x){grep(x,NFLTeams$Team)})],
              NFLTeams$Team[sapply(data$AwayTeam[grep(data$HomeTeam[j],data$HomeTeam)],function(x){grep(x,NFLTeams$Team)})])
  if (is.null(Team3)==FALSE){
    Games3 <- c(NFLTeams$Team[sapply(data$HomeTeam[grep(data$HomeTeam[k],data$AwayTeam)],function(x){grep(x,NFLTeams$Team)})],
                NFLTeams$Team[sapply(data$AwayTeam[grep(data$HomeTeam[k],data$HomeTeam)],function(x){grep(x,NFLTeams$Team)})])
  }
  if (is.null(Team4)==FALSE){
    Games4 <- c(NFLTeams$Team[sapply(data$HomeTeam[grep(data$HomeTeam[l],data$AwayTeam)],function(x){grep(x,NFLTeams$Team)})],
                NFLTeams$Team[sapply(data$AwayTeam[grep(data$HomeTeam[l],data$HomeTeam)],function(x){grep(x,NFLTeams$Team)})])
  }
  IDs1 <- Games1 %in% Games2
  if(is.null(Team3)==FALSE) {IDs2 <- Games1 %in% Games3}
  if(is.null(Team4)==FALSE) {IDs3 <- Games1 %in% Games4}
  if(is.null(Team4)==FALSE) {
    IDs <- IDs1 == TRUE & IDs2 == TRUE & IDs3 == TRUE
  } else if (is.null(Team3)==FALSE) {
    IDs <- IDs1 == TRUE & IDs2 == TRUE
  } else {IDs <- IDs1 == TRUE}
  CG <- (length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                        data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                        IDs == TRUE)) +
           0.5*(length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                               data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])==0 &
                               IDs == TRUE))))/length(which(IDs == TRUE))
  ifelse(length(which(IDs == TRUE))<=4,0,CG)
}

#'@title Head-to-Head
#'@description This function determines the record of one team against up to three other teams in the National Football League
#'@param Team1 the main team whose game results is being evaluated by the function
#'@param Team2 a team that \code{Team1} is being evaluated against
#'@param Team3 a team that \code{Team1} is being evaluated against (default is \code{NULL})
#'@param Team4 a team that \code{Team1} is being evaluated against (default is \code{NULL})
#'@param Data the data frame from which Scores are gathered (default is Week 17 \code{WeeklyUpdate})
#'@param SimScores the simulated scores, if a simulation was performed
#'@return \code{Wins} - \code{Losses} in all games for \code{Team1} against the other teams specified
#' @examples
#' HeadtoHead("Vikings","Packers")
#' HeadtoHead("Vikings","Packers","Bears")
#' HeadtoHead("Vikings","Packers","Bears","Lions")
#' @export
HeadtoHead <- function(Team1,Team2,Team3=NULL,Team4=NULL,data,SimScores=NULL){
for(i in 1:80){
  if (data$HomeTeam[i]==Team1) {break}
}
Games1 <- c(NFLTeams$Team[sapply(data$HomeTeam[grep(data$HomeTeam[i],data$AwayTeam)],function(x){grep(x,NFLTeams$Team)})],
            NFLTeams$Team[sapply(data$AwayTeam[grep(data$HomeTeam[i],data$HomeTeam)],function(x){grep(x,NFLTeams$Team)})])
HH <- (length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                       data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                      Games1 == Team2)) -
         (length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                          data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])<0 &
                         Games1 == Team2))))
if(is.null(Team3)==TRUE){
    HH <- HH
} else {HH <- HH +
  (length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                  data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                  Games1 == Team3)) -
     (length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                     data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])<0 &
                     Games1 == Team3))))
}

if(is.null(Team4)==TRUE){
    HH <- HH
} else {HH <- HH +
  (length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                  data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                  Games1 == Team4)) -
     (length(which(c(-data$ScoreDiff[grep(data$HomeTeam[i],data$AwayTeam)],
                     data$ScoreDiff[grep(data$HomeTeam[i],data$HomeTeam)])<0 &
                     Games1 == Team4))))
  }
HH
}

#'@name Division
#'@title Division Tiebreakers
#'@description These functions will break any ties between teams with the same win-loss record within the same division.
#'@param Team1
#'@param Team2
#'@param Team3
#'@param Team4
#'@param scores the data frame of scores used for calculating \code{Head-to-Head}, \code{CommonGames}, and \code{CommonGamesPts}
#'@param SimScores the simulated scores, if a simulation was performed
#'@details The order of statistics for
#' which ties are broken are as follows: Head-to-Head Record, win-loss record against teams within the division, win-loss record against teams common
#' opponents (\code{CommonGames}), win-loss record against teams within the conference, strength of victory (win-loss record against all teams defeated)
#' , strength of schedule (win-loss record against all teams played), combined rank for points scored and points allowed among conference teams,
#' combined rank for points scored and points allowed among all teams, net points against common opponents (\code{CommonGamesPts}), net points overall.
#'@return A vector of the tiebreaking results corresponding to each team in the function input.
#' @examples
#' TwoTieDiv("Vikings","Packers")
#' ThreeTieDiv("Vikings","Packers","Bears")
#' FourTieDiv("Vikings","Packers","Bears","Lions")
#' FinalDivRank()
#' @export
TwoTieDiv <- function(Team1,Team2,scores,SimScores=NULL){
  data = UpdateTeams(data=scores,SimScores)
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,])
  tmp2 <- c(HeadtoHead(Team1,Team2,data=scores,SimScores),HeadtoHead(Team2,Team1,data=scores,SimScores))
  dt <- rank(-tmp2,ties.method="min")
  tmp2 <- (tmp$DivWins+0.5*tmp$DivTies)/(tmp$DivWins+tmp$DivLosses+tmp$DivTies)
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,data=scores),CommonGames(Team2,Team1,data=scores))
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du
}
#' @rdname Division
#' @export
ThreeTieDiv <- function(Team1,Team2,Team3,scores,SimScores=NULL){
  data = UpdateTeams(data=scores,SimScores)
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,])
  tmp2 <- c(HeadtoHead(Team1,Team2,Team3,data=scores,SimScores),HeadtoHead(Team2,Team1,Team3,data=scores,SimScores),
            HeadtoHead(Team3,Team1,Team2,data=scores,SimScores))
  dt <- rank(-tmp2,ties.method="min")
  tmp2 <- (tmp$DivWins+0.5*tmp$DivTies)/(tmp$DivWins+tmp$DivLosses+tmp$DivTies)
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,Team3,data=scores),CommonGames(Team2,Team1,Team3,data=scores),
            CommonGames(Team3,Team1,Team2,data=scores))
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du[du==2]<-TwoTieDiv((c(Team1,Team2,Team3)[x==2])[1],(c(Team1,Team2,Team3)[x==2])[2],scores,SimScores)+1
  du
}
#' @rdname Division
#' @export
FourTieDiv <- function(Team1,Team2,Team3,Team4,scores,SimScores=NULL){
  data = UpdateTeams(data=scores,SimScores)
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,],data[data$Team==Team4,])
  tmp2 <- (tmp$DivWins+0.5*tmp$DivTies)/(tmp$DivWins+tmp$DivLosses+tmp$DivTies)
  dt <- rank(-tmp2,ties.method="min")
  tmp2 <- c(CommonGames(Team1,Team2,Team3,Team4,data=scores,SimScores),CommonGames(Team2,Team1,Team3,Team4,data=scores,SimScores),
            CommonGames(Team3,Team1,Team2,Team4,data=scores,SimScores),CommonGames(Team4,Team1,Team2,Team3,data=scores,SimScores))
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du[du==2]<-ThreeTieDiv((c(Team1,Team2,Team3)[x==2])[1],(c(Team1,Team2,Team3)[x==2])[2],
                         (c(Team1,Team2,Team3)[x==2])[3],scores,SimScores)+1
  du
}
#' @rdname Division
#' @export
FinalDivRank <- function(scores,SimScores=NULL) {
  data <- UpdateTeams(data=scores,SimScores)
  tmp <- data %>%
    nest(-Division) %>%
    mutate(Rank=lapply(data,function(x){rank(-(x$Wins+0.5*x$Ties)/(x$Wins+x$Losses+x$Ties),ties.method="min")})) %>%
    unnest()
  tmp2 <- tmp %>% mutate(Rank2=Rank) %>% nest(-Division,-Rank2)
  tmp2 <- tmp2 %>% mutate(DivRank=lapply(data,function(x){if(nrow(x)==4) {FourTieDiv(x$Team[1],x$Team[2],x$Team[3],x$Team[4],scores=scores,SimScores)+x$Rank[1]-1} else
    if(nrow(x)==3) {ThreeTieDiv(x$Team[1],x$Team[2],x$Team[3],scores=scores,SimScores)+x$Rank[1]-1} else
      if(nrow(x)==2) {TwoTieDiv(x$Team[1],x$Team[2],scores=scores,SimScores)+x$Rank[1]-1} else
        x$Rank}))
  tmp2 <- tmp2 %>% unnest() %>% select(-Rank2,-Rank)
  tmp2
}
#'@name Conference
#'@title Conference Tiebreakers
#'@description These functions will break any ties between teams with the same win-loss record with in different divisions, but in the same conference
#'@param Team1
#'@param Team2
#'@param Team3
#'@param Team4
#'@param scores the data frame of scores used for calculating \code{Head-to-Head} and \code{CommonGames}
#'@param SimScores the simulated scores, if a simulation was performed
#'@details The order of statistics for
#' which ties are broken are as follows: Apply division tiebreaker to eliminate all but the highest ranked team in each division,
#' Head-to-Head Record, win-loss record against teams within the conference, win-loss record against teams common opponents (\code{CommonGames}),
#' strength of victory (win-loss record against all teams defeated), strength of schedule (win-loss record against all teams played),
#' combined rank for points scored and points allowed among conference teams, combined rank for points scored and points allowed among all teams,
#' net points against conference opponents, net points overall.
#'
#' Seedings are as follows: the four division winners/leaders automatically get ranked 1-4 based on record and tiebreakers, while the remaining 12
#' teams will be seeded based on record and tiebreakers.
#'@return A vector of the tiebreaking results corresponding to each team in the function input.
#' @examples
#' TwoTieConf("Vikings","Giants")
#' ThreeTieDiv("Vikings","Giants","49ers")
#' FourTieDiv("Vikings","Giants","49ers","Falcons")
#' FinalDivRank()
#' @export
TwoTieConf <- function(Team1,Team2,scores,SimScores=NULL){
  data = UpdateTeams(data=scores,SimScores)
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team2]) {stop("Team1 and Team2 are in same division.  Please use TwoTieDiv()")}
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,])
  Team1 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team1)})==TRUE])[1]
  Team2 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team2)})==TRUE])[1]
  tmp2 <- (tmp$Wins+0.5*tmp$Ties)/(tmp$Wins+tmp$Losses+tmp$Ties)
  dt <- rank(-tmp2,ties.method="min")
  tmp2 <- c(HeadtoHead(Team1,Team2,data=scores,SimScores),HeadtoHead(Team2,Team1,data=scores,SimScores))
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,data=scores,SimScores),CommonGames(Team2,Team1,data=scores,SimScores))
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du
}
#' @rdname Conference
#' @export
ThreeTieConf <- function(Team1,Team2,Team3,scores,SimScores=NULL){
  data = UpdateTeams(data=scores,SimScores)
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team2]) {stop("Team1 and Team2 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team3]) {stop("Team1 and Team3 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team2]==data$Division[data$Team==Team3]) {stop("Team2 and Team3 are in same division.  Please use TwoTieDiv()")}
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,])
  Team1 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team1)})==TRUE])[1]
  Team2 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team2)})==TRUE])[1]
  Team3 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team3)})==TRUE])[1]
  tmp2 <- c(HeadtoHead(Team1,Team2,Team3,data=scores),HeadtoHead(Team2,Team1,Team3,data=scores),HeadtoHead(Team3,Team1,Team2,data=scores))
  dt <- sapply(tmp2,function(x){ifelse(x==2,1,ifelse(x==-2,3,1))})
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,Team3,data=scores,SimScores),CommonGames(Team2,Team1,Team3,data=scores,SimScores),
            CommonGames(Team3,Team1,Team2,data=scores,SimScores))
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du[du>=2] <- 2
  du
}
#' @rdname Conference
#' @export
FourTieConf <- function(Team1,Team2,Team3,Team4,scores,SimScores){
  data = UpdateTeams(data=scores,SimScores)
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team2]) {stop("Team1 and Team2 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team3]) {stop("Team1 and Team3 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team2]==data$Division[data$Team==Team3]) {stop("Team2 and Team3 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team4]) {stop("Team1 and Team4 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team2]==data$Division[data$Team==Team4]) {stop("Team2 and Team4 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team3]==data$Division[data$Team==Team4]) {stop("Team3 and Team4 are in same division.  Please use TwoTieDiv()")}
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,],data[data$Team==Team4,])
  Team1 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team1)})==TRUE])[1]
  Team2 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team2)})==TRUE])[1]
  Team3 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team3)})==TRUE])[1]
  Team4 <- (scores$HomeTeam[1:80][sapply(scores$HomeTeam[1:80],function(x){grepl(x,Team4)})==TRUE])[1]
  tmp2 <- c(HeadtoHead(Team1,Team2,Team3,Team4,data=scores,SimScores),HeadtoHead(Team2,Team1,Team3,Team4,data=scores,SimScores),
            HeadtoHead(Team3,Team1,Team2,Team4,data=scores,SimScores),HeadtoHead(Team4,Team1,Team2,Team3,data=scores,SimScores))
  dt <- sapply(tmp2,function(x){ifelse(x==3,1,ifelse(x==-3,4,1))})
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,Team3,Team4,data=scores,SimScores),CommonGames(Team2,Team1,Team3,Team4,data=scores,SimScores),
            CommonGames(Team3,Team1,Team2,Team4,data=scores,SimScores),CommonGames(Team4,Team1,Team2,Team3,data=scores,SimScores))
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du[du>=2] <- 2
  du
}
#' @rdname Conference
#' @export
FinalRank <- function(scores,SimScores=NULL){
  data=UpdateTeams(data=scores,SimScores)
  tmp <- FinalDivRank(scores=scores,SimScores)
  tmp$DivRank2 <- sapply(tmp$DivRank,function(x){min(x,2)})
  tmp2 <- tmp %>% nest(-Conference,-DivRank2)
  tmp2 <- tmp2 %>% mutate(Rank = lapply(data,function(x){rank(-(x$Wins+0.5*x$Ties)/(x$Wins+x$Losses+x$Ties),ties.method="min")+
      sapply(x$DivRank,function(y){min(y,2)})*4-4}))
  tmp2 <- tmp2 %>% unnest() %>% select(-DivRank2)
  while(sum(tmp2$Rank)<272){
    tmp3 <- tmp2 %>% nest(-Rank,-Division) %>% mutate(Rnk=lapply(data,function(x){rank(x$DivRank)-1})) %>% unnest() %>%
      mutate(ConfRnk=Rank+Rnk,Rank=Rank+Rnk)
    tmp3 <- tmp3 %>%
      nest(-Conference,-ConfRnk) %>%
      mutate(CRank=lapply(data,function(x){if(nrow(x)==4) {FourTieConf(x$Team[1],x$Team[2],x$Team[3],x$Team[4],scores=scores,SimScores)+x$Rank[1]-1} else
        if(nrow(x)==3) {ThreeTieConf(x$Team[1],x$Team[2],x$Team[3],scores=scores,SimScores)+x$Rank[1]-1} else
          if(nrow(x)==2) {TwoTieConf(x$Team[1],x$Team[2],scores=scores,SimScores)+x$Rank[1]-1} else
            x$Rank}))
    tmp3 <- tmp3 %>% unnest() %>% mutate(Rank=CRank) %>% select(-CRank,-ConfRnk,-Rnk)
    tmp2 <- tmp3
  }
  tmp2 <- tmp2 %>% mutate(ConfRank=Rank) %>% select(-Rank)
  tmp2 <- tmp2[,c(1,22,2:21)]
  tmp2
}


