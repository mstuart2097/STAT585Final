#'@title NFLTeams
#'@description Tiebreaking Data for the 32 teams in the National Football League
#' The dataset containing necessary information for tiebreaking procedures.  The statistics can be updated using the UpdateData function
#' @format A dataset containing 32 rows and 20 variables
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
#'  \item{ConfPtScored}{Total Points Scored in games played in their own conference}
#'  \item{ConfPtAllowed}{Total Points Allowed in games played in their own conference}
#'  \item{AllPtScored}{Total Points Scored in all games played by Team}
#'  \item{AllPtAllowed}{Total Points Allowed in all games played by Team}
#'  \item{ConfPtRank}{Sum of Rank within conference in terms of Points Scored and Points Allowed in games played within the conference}
#'  \item{AllPtRank}{Sum of Rank  in terms of Points Scored and Points Allowed in all games played}
#' }
#' @source \url{"http://www.dummies.com/sports/football/the-national-football-league-conferences/"}
#' @source \url{"http://www.nfl.com/schedules/2016"}
"NFLTeams"

#'@title WeeklyScores
#'@description This is a function that will update the scores from the specified week from NFL.com
#'@param Week the identifier for the week of scores you want (Default is 1)
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
#' @source \url{"http://www.nfl.com/schedules/2016"}
WeeklyScores <- function(Week=1){
  require(rvest)
  require(tidyverse)
  url <- paste("http://www.nfl.com/schedules/2016/REG",Week,sep="")
  html <- read_html(url)
  NFL <- html %>% html_nodes(".schedules-list-date,.time,.away,.home") %>% html_text()
  NFL <- gsub("\r","",gsub("\n","",gsub("\t","",NFL)))
  Games <- length(which(NFL=="FINAL"))
  for (i in 1:Games){
    Date <- ifelse(NFL[(8*(i-1)+1)]!="FINAL",NFL[(8*(i-1)+1)],Date)
    NFL <- if(i==1) {NFL} else if(NFL[(8*(i-1)+1)]==Date) {NFL} else {c(NFL[1:(8*(i-1))],Date,NFL[(8*(i-1)+1):(length(NFL))])}
  }
  tmp <- data.frame(matrix(NFL,nrow=Games,byrow=TRUE))
  tmp <- tmp %>%
    mutate(Date=as.character(X1),AwayTeam=as.character(X3),AwayScore=as.numeric(as.character(X5)),HomeTeam=as.character(X8),HomeScore=as.numeric(as.character(X6))) %>%
    select(Date,AwayTeam,AwayScore,HomeTeam,HomeScore)
  tmp
}

#'@title WeeklyGames
#'@description This is a function that will gets games from weeks that you do not want to use the scores, or if the games have not yet been played that week
#'@param Week the identifier for the week of games you want (Default is 1)
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
#' @source \url{"http://www.nfl.com/schedules/2016"}
WeeklyGames <- function(Week=1){
  require(rvest)
  require(tidyverse)
  url <- paste("http://www.nfl.com/schedules/2016/REG",Week,sep="")
  html <- read_html(url)
  NFL <- html %>% html_nodes(".schedules-list-date,.time,.team-name") %>% html_text()
  NFL <- gsub("\r","",gsub("\n","",gsub("\t","",NFL)))
  Games <- length(which(NFL=="FINAL"))
  for (i in 1:Games){
    Date <- ifelse(NFL[(4*(i-1)+1)]!="FINAL",NFL[(4*(i-1)+1)],Date)
    NFL <- if(i==1) {NFL} else if(NFL[(4*(i-1)+1)]==Date) {NFL} else {c(NFL[1:(4*(i-1))],Date,NFL[(4*(i-1)+1):(length(NFL))])}
  }
  tmp <- data.frame(matrix(NFL,nrow=Games,byrow=TRUE))
  tmp <- tmp %>%
    mutate(Date=as.character(X1),AwayTeam=as.character(X3),AwayScore=rep("",Games),HomeTeam=as.character(X4),HomeScore=rep("",Games)) %>%
    select(Date,AwayTeam,AwayScore,HomeTeam,HomeScore)
  tmp
}

#'@title WeeklyUpdate
#'@description This is a function that will gets all games for the season, with games being played from WeeklyScores and games yet to be played from WeeklyGames
#'@param Week the identifier for the latest week of games played you want (Default is 17)
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
#' @source \url{"http://www.nfl.com/schedules/2016"}
WeeklyUpdate <- function(WeekID=17){
  require(rvest)
  require(tidyverse)
  if (WeekID==17){
    Weeks <- data.frame(Week=c(1:WeekID))
    Scores <- Weeks %>% mutate(Scores = Week %>% map(WeeklyScores))
    FinalScores <- Scores %>% unnest
    FinalScores
  } else if (WeekID==0) {
    Weeks <- data.frame(Week=c((WeekID+1):17))
    Games <- Weeks %>% mutate(Games = Week %>% map(WeeklyGames))
    RemainingGames <- Games %>% unnest
    RemainingGames %>% mutate(AwayScore=as.integer(AwayScore),HomeScore=as.integer(HomeScore))
  } else {
    Weeks <- data.frame(Week=c(1:WeekID))
    Scores <- Weeks %>% mutate(Scores = Week %>% map(WeeklyScores))
    FinalScores <- Scores %>% unnest
    Weeks <- data.frame(Week=c((WeekID+1):17))
    Games <- Weeks %>% mutate(Games = Week %>% map(WeeklyGames))
    RemainingGames <- Games %>% unnest
    rbind(FinalScores,RemainingGames) %>% mutate(AwayScore=as.integer(AwayScore),HomeScore=as.integer(HomeScore))
  }
}

#'@title UpdateTeams
#'@description This is a function that will Update all tiebreaking stats from the WeeklyUpdate output and using NFLTeams as a template
#'@param data the WeeklyUpdate output that will be used to aggregate the data
#'@export
#'@return A dataset of the same dimensions and format as NFLTeams
#' @examples
#' UpdateTeams()
#' UpdateTeams(WeeklyUpdate(16))
UpdateTeams <- function(data = WeeklyUpdate()){
  data <- data[is.na(data$AwayScore)==FALSE,]
  for (i in 1:48){
    HomeID <- grep(data$HomeTeam[i],data$HomeTeam)
    AwayID <- grep(data$HomeTeam[i],data$AwayTeam)
    StatID <- grep(data$HomeTeam[i],NFLTeams$Team)
    HomeVector <- sapply(data$AwayTeam[HomeID],function(x){grep(x,NFLTeams$Team)})
    AwayVector <- sapply(data$HomeTeam[AwayID],function(x){grep(x,NFLTeams$Team)})
    NFLTeams$Wins[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]>0))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]<0))
    NFLTeams$Losses[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]<0))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]>0))
    NFLTeams$Ties[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]==0))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]==0))
    NFLTeams$ConfWins[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]>0 & NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]<0 & NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]))
    NFLTeams$ConfLosses[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]<0 & NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]>0 & NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]))
    NFLTeams$ConfTies[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]==0 & NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]==0 & NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]))
    NFLTeams$DivWins[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]>0 & NFLTeams$Division[HomeVector]==NFLTeams$Division[StatID]))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]<0 & NFLTeams$Division[AwayVector]==NFLTeams$Division[StatID]))
    NFLTeams$DivLosses[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]<0 & NFLTeams$Division[HomeVector]==NFLTeams$Division[StatID]))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]>0 & NFLTeams$Division[AwayVector]==NFLTeams$Division[StatID]))
    NFLTeams$DivTies[StatID] <-
      length(which(data$HomeScore[HomeID] - data$AwayScore[HomeID]==0 & NFLTeams$Division[HomeVector]==NFLTeams$Division[StatID]))+
      length(which(data$HomeScore[AwayID] - data$AwayScore[AwayID]==0 & NFLTeams$Division[AwayVector]==NFLTeams$Division[StatID]))
    NFLTeams$ConfPtScored[StatID]  <-
      sum((data$HomeScore[HomeID])[NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]]) +
      sum((data$AwayScore[AwayID])[NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]])
    NFLTeams$ConfPtAllowed[StatID]  <-
      sum((data$AwayScore[HomeID])[NFLTeams$Conference[HomeVector]==NFLTeams$Conference[StatID]]) +
      sum((data$HomeScore[AwayID])[NFLTeams$Conference[AwayVector]==NFLTeams$Conference[StatID]])
    NFLTeams$AllPtScored[StatID]  <-
      sum((data$HomeScore[HomeID])) +
      sum((data$AwayScore[AwayID]))
    NFLTeams$AllPtAllowed[StatID]  <-
      sum((data$AwayScore[HomeID])) +
      sum((data$HomeScore[AwayID]))
  }
  for (i in 1:48){
    HomeID <- grep(data$HomeTeam[i],data$HomeTeam)
    AwayID <- grep(data$HomeTeam[i],data$AwayTeam)
    StatID <- grep(data$HomeTeam[i],NFLTeams$Team)
    HomeVector <- sapply(data$AwayTeam[HomeID],function(x){grep(x,NFLTeams$Team)})
    AwayVector <- sapply(data$HomeTeam[AwayID],function(x){grep(x,NFLTeams$Team)})
  W <-
    sum(NFLTeams$Wins[HomeVector][data$HomeScore[HomeID] - data$AwayScore[HomeID]>0]) +
    sum(NFLTeams$Wins[AwayVector][data$HomeScore[AwayID] - data$AwayScore[AwayID]<0])
  L <-
    sum(NFLTeams$Losses[HomeVector][data$HomeScore[HomeID] - data$AwayScore[HomeID]>0]) +
    sum(NFLTeams$Losses[AwayVector][data$HomeScore[AwayID] - data$AwayScore[AwayID]<0])
  T <-
    sum(NFLTeams$Ties[HomeVector][data$HomeScore[HomeID] - data$AwayScore[HomeID]>0]) +
    sum(NFLTeams$Ties[AwayVector][data$HomeScore[AwayID] - data$AwayScore[AwayID]<0])
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
  NFLTeams$ConfPtRank <-
    c(rank(NFLTeams$ConfPtScored[NFLTeams$Conference=="AFC"]),rank(NFLTeams$ConfPtScored[NFLTeams$Conference=="NFC"])) +
    c(rank(-NFLTeams$ConfPtAllowed[NFLTeams$Conference=="AFC"]),rank(-NFLTeams$ConfPtAllowed[NFLTeams$Conference=="NFC"]))
  NFLTeams$AllPtRank <- rank(NFLTeams$AllPtScored)+rank(-NFLTeams$AllPtAllowed)
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
CommonGames <- function(Team1,Team2,Team3=NULL,Team4=NULL,data=WeeklyUpdate()){
  for(i in 1:48){
    if (data$HomeTeam[i]==Team1) {break}
  }
  for(j in 1:48){
    if (data$HomeTeam[j]==Team2) {break}
  }
  if (is.null(Team3)==FALSE){
    for(k in 1:48){
      if (data$HomeTeam[k]==Team3) {break}
    }
  }
  if (is.null(Team4)==FALSE){
    for(l in 1:48){
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
  CG <- (length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                        data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                        IDs == TRUE)) +
           0.5*(length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                               data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])==0 &
                               IDs == TRUE))))/length(which(IDs == TRUE))
  ifelse(length(which(IDs == TRUE))<=4,0,CG)
}
#' @rdname CommonGames
CommonGamesPts <- function(Team1,Team2,Team3=NULL,Team4=NULL,data=WeeklyUpdate()){
for(i in 1:48){
  if (data$HomeTeam[i]==Team1) {break}
}
for(j in 1:48){
  if (data$HomeTeam[j]==Team2) {break}
}
if (is.null(Team3)==FALSE){
  for(k in 1:48){
    if (data$HomeTeam[k]==Team3) {break}
  }
}
if (is.null(Team4)==FALSE){
  for(l in 1:48){
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
CG <- sum(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
            data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])[IDs == TRUE])
ifelse(length(which(IDs == TRUE))<=4,0,CG)
}

#'@title Head-to-Head
#'@description This function determines the record of one team against up to three other teams in the National Football League
#'@param Team1 the main team whose game results is being evaluated by the function
#'@param Team2 a team that \code{Team1} is being evaluated against
#'@param Team3 a team that \code{Team1} is being evaluated against (default is \code{NULL})
#'@param Team4 a team that \code{Team1} is being evaluated against (default is \code{NULL})
#'@param Data the data frame from which Scores are gathered (default is Week 17 \code{WeeklyUpdate})
#'@return \code{Wins} - \code{Losses} in all games for \code{Team1} against the other teams specified
#' @examples
#' HeadtoHead("Vikings","Packers")
#' HeadtoHead("Vikings","Packers","Bears")
#' HeadtoHead("Vikings","Packers","Bears","Lions")
#' @export
HeadtoHead <- function(Team1,Team2,Team3=NULL,Team4=NULL,data=WeeklyUpdate()){
for(i in 1:48){
  if (data$HomeTeam[i]==Team1) {break}
}
Games1 <- c(NFLTeams$Team[sapply(data$HomeTeam[grep(data$HomeTeam[i],data$AwayTeam)],function(x){grep(x,NFLTeams$Team)})],
            NFLTeams$Team[sapply(data$AwayTeam[grep(data$HomeTeam[i],data$HomeTeam)],function(x){grep(x,NFLTeams$Team)})])
HH <- (length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                      data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                      Games1 == Team2)) -
         (length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                         data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])<0 &
                         Games1 == Team2))))
if(is.null(Team3)==TRUE){
    HH <- HH
} else {HH <- HH +
    (length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                    data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                    Games1 == Team3)) -
       (length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                       data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])<0 &
                       Games1 == Team3))))
}

if(is.null(Team4)==TRUE){
    HH <- HH
} else {HH <- HH +
    (length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                    data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])>0 &
                    Games1 == Team4)) -
       (length(which(c(data$AwayScore[grep(data$HomeTeam[i],data$AwayTeam)] - data$HomeScore[grep(data$HomeTeam[i],data$AwayTeam)],
                       data$HomeScore[grep(data$HomeTeam[i],data$HomeTeam)] - data$AwayScore[grep(data$HomeTeam[i],data$HomeTeam)])<0 &
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
TwoTieDiv <- function(Team1,Team2,scores=WeeklyUpdate()){
  data = UpdateTeams(scores)
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,])
  tmp2 <- c(HeadtoHead(Team1,Team2,data=scores),HeadtoHead(Team2,Team1,data=scores))
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
  du <- if (sum(du)!=2) {du} else {rank(-tmp$ConfPtRank,ties.method="min")}
  du <- if (sum(du)!=2) {du} else {rank(-tmp$AllPtRank,ties.method="min")}
  tmp2 <- c(CommonGamesPts(Team1,Team2,data=scores),CommonGamesPts(Team2,Team1,data=scores))
  du <- if (sum(du)!=2) {du} else {rank(-tmp2,ties.method="min")}
  du <- if (sum(du)!=2) {du} else {rank(tmp$AllPtAllowed-tmp$AllPtScored,ties.method="first")}
  du
}
#' @rdname Division
ThreeTieDiv <- function(Team1,Team2,Team3,scores=WeeklyUpdate()){
  data = UpdateTeams(scores)
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,])
  tmp2 <- c(HeadtoHead(Team1,Team2,Team3,data=scores),HeadtoHead(Team2,Team1,Team3,data=scores),HeadtoHead(Team3,Team1,Team2,data=scores))
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
  du <- if (sum(du)!=3) {du} else {rank(-tmp$ConfPtRank,ties.method="min")}
  du <- if (sum(du)!=3) {du} else {rank(-tmp$AllPtRank,ties.method="min")}
  tmp2 <- c(CommonGamesPts(Team1,Team2,Team3,data=scores),CommonGamesPts(Team2,Team1,Team3,data=scores),
            CommonGamesPts(Team3,Team1,Team2,data=scores))
  du <- if (sum(du)!=3) {du} else {rank(-tmp2,ties.method="min")}
  du <- if (sum(du)!=3) {du} else {rank(tmp$AllPtAllowed-tmp$AllPtScored,ties.method="first")}
  if(length(which(du==1))==2) {du[du==1]<-TwoTieDiv((c(Team1,Team2,Team3)[x==1])[1],(c(Team1,Team2,Team3)[x==1])[2])} else {du <- du}
  du <- sapply(du,function(x){min(x,2)})
  du[du==2]<-TwoTieDiv((c(Team1,Team2,Team3)[x==2])[1],(c(Team1,Team2,Team3)[x==2])[2])+1
  du
}
#' @rdname Division
FourTieDiv <- function(Team1,Team2,Team3,Team4,scores=WeeklyUpdate()){
  data = UpdateTeams(scores)
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,],data[data$Team==Team4,])
  tmp2 <- (tmp$DivWins+0.5*tmp$DivTies)/(tmp$DivWins+tmp$DivLosses+tmp$DivTies)
  dt <- rank(-tmp2,ties.method="min")
  tmp2 <- c(CommonGames(Team1,Team2,Team3,Team4,data=scores),CommonGames(Team2,Team1,Team3,Team4,data=scores),
            CommonGames(Team3,Team1,Team2,Team4,data=scores),CommonGames(Team4,Team1,Team2,Team3,data=scores))
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du <- if (sum(du)!=4) {du} else {rank(-tmp$ConfPtRank,ties.method="min")}
  du <- if (sum(du)!=4) {du} else {rank(-tmp$AllPtRank,ties.method="min")}
  tmp2 <- c(CommonGamesPts(Team1,Team2,Team3,Team4,data=scores),CommonGamesPts(Team2,Team1,Team3,Team4,data=scores),
            CommonGamesPts(Team3,Team1,Team2,Team4,data=scores),CommonGamesPts(Team4,Team1,Team2,Team3,data=scores))
  du <- if (sum(du)!=4) {du} else {rank(-tmp2,ties.method="min")}
  du <- if (sum(du)!=4) {du} else {rank(tmp$AllPtAllowed-tmp$AllPtScored,ties.method="first")}
  if(length(which(du==1))==3) {du[du==1]<-ThreeTieDiv((c(Team1,Team2,Team3,Team4)[x==1])[1],(c(Team1,Team2,Team3,Team4)[x==1])[2],
                                                      (c(Team1,Team2,Team3,Team4)[x==1])[3])} else {du <- du}
  du <- sapply(du,function(x){min(x,2)})
  du[du==2]<-ThreeTieDiv((c(Team1,Team2,Team3)[x==2])[1],(c(Team1,Team2,Team3)[x==2])[2],
                         (c(Team1,Team2,Team3)[x==2])[3])+1
  du
}
#' @rdname Division
FinalDivRank <- function(scores=WeeklyUpdate()) {
  require(tidyverse)
  data <- UpdateTeams(scores)
  tmp <- data %>%
    nest(-Division) %>%
    mutate(Rank=lapply(data,function(x){rank(-(x$Wins+0.5*x$Ties)/(x$Wins+x$Losses+x$Ties),ties.method="min")})) %>%
    unnest()
  tmp2 <- tmp %>% mutate(Rank2=Rank) %>% nest(-Division,-Rank2)
  tmp2 <- tmp2 %>% mutate(DivRank=lapply(data,function(x){if(nrow(x)==4) {FourTieDiv(x$Team[1],x$Team[2],x$Team[3],x$Team[4],scores=scores)+x$Rank[1]-1} else
    if(nrow(x)==3) {ThreeTieDiv(x$Team[1],x$Team[2],x$Team[3],scores=scores)+x$Rank[1]-1} else
      if(nrow(x)==2) {TwoTieDiv(x$Team[1],x$Team[2],scores=scores)+x$Rank[1]-1} else
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
TwoTieConf <- function(Team1,Team2,scores=WeeklyUpdate()){
  data = UpdateTeams(scores)
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team2]) {stop("Team1 and Team2 are in same division.  Please use TwoTieDiv()")}
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,])
  Team1 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team1)})==TRUE])[1]
  Team2 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team2)})==TRUE])[1]
  tmp2 <- (tmp$Wins+0.5*tmp$Ties)/(tmp$Wins+tmp$Losses+tmp$Ties)
  dt <- rank(-tmp2,ties.method="min")
  tmp2 <- c(HeadtoHead(Team1,Team2,data=scores),HeadtoHead(Team2,Team1,data=scores))
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,data=scores),CommonGames(Team2,Team1,data=scores))
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=2) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du <- if (sum(du)!=2) {du} else {rank(-tmp$ConfPtRank,ties.method="min")}
  du <- if (sum(du)!=2) {du} else {rank(-tmp$AllPtRank,ties.method="min")}
  du <- if (sum(du)!=2) {du} else {rank(tmp$ConfPtAllowed-tmp$ConfPtScored,ties.method="min")}
  du <- if (sum(du)!=2) {du} else {rank(tmp$AllPtAllowed-tmp$AllPtScored,ties.method="first")}
  du
}
#' @rdname Conference
ThreeTieConf <- function(Team1,Team2,Team3,scores=WeeklyUpdate()){
  data = UpdateTeams(scores)
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team2]) {stop("Team1 and Team2 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team3]) {stop("Team1 and Team3 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team2]==data$Division[data$Team==Team3]) {stop("Team2 and Team3 are in same division.  Please use TwoTieDiv()")}
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,])
  Team1 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team1)})==TRUE])[1]
  Team2 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team2)})==TRUE])[1]
  Team3 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team3)})==TRUE])[1]
  tmp2 <- c(HeadtoHead(Team1,Team2,Team3,data=scores),HeadtoHead(Team2,Team1,Team3,data=scores),HeadtoHead(Team3,Team1,Team2,data=scores))
  dt <- sapply(tmp2,function(x){ifelse(x==2,1,ifelse(x==-2,3,1))})
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,Team3,data=scores),CommonGames(Team2,Team1,Team3,data=scores),
            CommonGames(Team3,Team1,Team2,data=scores))
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=3) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du <- if (sum(du)!=3) {du} else {rank(-tmp$ConfPtRank,ties.method="min")}
  du <- if (sum(du)!=3) {du} else {rank(-tmp$AllPtRank,ties.method="min")}
  du <- if (sum(du)!=3) {du} else {rank(tmp$ConfPtAllowed-tmp$ConfPtScored,ties.method="min")}
  du <- if (sum(du)!=3) {du} else {rank(tmp$AllPtAllowed-tmp$AllPtScored,ties.method="first")}
  if(length(which(du==1))==2) {du[du==1]<-TwoTieConf((c(Team1,Team2,Team3)[du==1])[1],(c(Team1,Team2,Team3)[du==1])[2])} else {du <- du}
  du[du>=2] <- 2
  du
}
#' @rdname Conference
FourTieConf <- function(Team1,Team2,Team3,Team4,scores=WeeklyUpdate()){
  data = UpdateTeams(scores)
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team2]) {stop("Team1 and Team2 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team3]) {stop("Team1 and Team3 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team2]==data$Division[data$Team==Team3]) {stop("Team2 and Team3 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team1]==data$Division[data$Team==Team4]) {stop("Team1 and Team4 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team2]==data$Division[data$Team==Team4]) {stop("Team2 and Team4 are in same division.  Please use TwoTieDiv()")}
  if(data$Division[data$Team==Team3]==data$Division[data$Team==Team4]) {stop("Team3 and Team4 are in same division.  Please use TwoTieDiv()")}
  tmp <- rbind(data[data$Team==Team1,],data[data$Team==Team2,],data[data$Team==Team3,],data[data$Team==Team4,])
  Team1 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team1)})==TRUE])[1]
  Team2 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team2)})==TRUE])[1]
  Team3 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team3)})==TRUE])[1]
  Team4 <- (dat$HomeTeam[1:48][sapply(dat$HomeTeam[1:48],function(x){grepl(x,Team4)})==TRUE])[1]
  tmp2 <- c(HeadtoHead(Team1,Team2,Team3,Team4,data=scores),HeadtoHead(Team2,Team1,Team3,Team4,data=scores),
            HeadtoHead(Team3,Team1,Team2,Team4,data=scores),HeadtoHead(Team4,Team1,Team2,Team3,data=scores))
  dt <- sapply(tmp2,function(x){ifelse(x==3,1,ifelse(x==-3,4,1))})
  tmp2 <- (tmp$ConfWins+0.5*tmp$ConfTies)/(tmp$ConfWins+tmp$ConfLosses+tmp$ConfTies)
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  tmp2 <- c(CommonGames(Team1,Team2,Team3,Team4,data=scores),CommonGames(Team2,Team1,Team3,Team4,data=scores),
            CommonGames(Team3,Team1,Team2,Team4,data=scores),CommonGames(Team4,Team1,Team2,Team3,data=scores))
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp2,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOV,ties.method="min")}
  dt <- if (sum(dt)!=4) {dt} else {rank(-tmp$SOS,ties.method="min")}
  du <- dt
  du <- if (sum(du)!=4) {du} else {rank(-tmp$ConfPtRank,ties.method="min")}
  du <- if (sum(du)!=4) {du} else {rank(-tmp$AllPtRank,ties.method="min")}
  du <- if (sum(du)!=4) {du} else {rank(tmp$ConfPtAllowed-tmp$ConfPtScored,ties.method="min")}
  du <- if (sum(du)!=4) {du} else {rank(tmp$AllPtAllowed-tmp$AllPtScored,ties.method="first")}
  if(length(which(du==1))==3) {du[du==1]<-ThreeTieConf((c(Team1,Team2,Team3,Team4)[du==1])[1],(c(Team1,Team2,Team3,Team4)[du==1])[2],
                                                       (c(Team1,Team2,Team3,Team4)[du==1])[3])} else
                                                         if(length(which(du==1))==2) {du[du==1]<-TwoTieConf((c(Team1,Team2,Team3,Team4)[du==1])[1],(c(Team1,Team2,Team3,Team4)[du==1])[2])} else {du <- du}
  du[du>=2] <- 2
  du
}
#' @rdname Conference
FinalRank <- function(scores=WeeklyUpdate()){
  require(tidyverse)
  data=UpdateTeams(scores)
  tmp <- FinalDivRank(scores=scores)
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
      mutate(CRank=lapply(data,function(x){if(nrow(x)==4) {FourTieConf(x$Team[1],x$Team[2],x$Team[3],x$Team[4],scores=scores)+x$Rank[1]-1} else
        if(nrow(x)==3) {ThreeTieConf(x$Team[1],x$Team[2],x$Team[3],scores=scores)+x$Rank[1]-1} else
          if(nrow(x)==2) {TwoTieConf(x$Team[1],x$Team[2],scores=scores)+x$Rank[1]-1} else
            x$Rank}))
    tmp3 <- tmp3 %>% unnest() %>% mutate(Rank=CRank) %>% select(-CRank,-ConfRnk,-Rnk)
    tmp2 <- tmp3
  }
  tmp2 <- tmp2 %>% mutate(ConfRank=Rank) %>% select(-Rank)
  tmp2 <- tmp2[,c(1,22,2:21)]
  tmp2
}

#'@name Sim
#'@title NFLSim
#'@description This is a function that will take specified outcomes of games yet to be played, and simulate multiple seedings based on those
#'specifications.
#'@param Games The outcomes of any specified games yet to be played (default is \code{NULL}).  Any string of the format (AwayTeam AwayScore HomeTeam
#'HomeScore) separated by any character not a letter or number will be sufficient
#'@param sims Number of simulations requested to be run (default is 100)
#'@param data The data frame of scores to be used for simulating scores for games yet to be played, that are not specified by the Games input
#'@details
#' All simulations will have the scores specified by the Games input.  Any other teams needing scores will be specified in the following way: the mean
#' and standard deviation from all previous games will be taken for each team, and a score will be generated from a normal distribution with the specified
#' mean and standard deviation.  Precautions are taken so that scores are a whole number, scores cannot be below zero, and scores cannot be one (a score of
#' one is impossible in American Football)
#'@return A data frame of each team's playoff seed based on the simulation results
#' @examples
#' NFLSim()
#' NFLSim(sims=1000)
#' NFLSim(Games="Cardinals13Rams17",sims=10)
NFLSim <- function(Games=NULL,sims=100,data=WeeklyUpdate()){
  require(rvest)
  require(tidyverse)
  rawscore <- data %>% gather(key="ID",value="Team",c(3,5)) %>% mutate(Score=ifelse(ID=="AwayTeam",AwayScore,HomeScore)) %>% select(Team,Score)
  rawagainst <- data %>% gather(key="ID",value="Team",c(3,5)) %>% mutate(Score=ifelse(ID=="AwayTeam",HomeScore,AwayScore)) %>% select(Team,Score)
  statsscore <- rawscore %>% group_by(Team) %>% summarise(mean=mean(Score,na.rm=TRUE),sd=sd(Score,na.rm=TRUE))
  statsagainst <- rawagainst %>% group_by(Team) %>% summarise(mean=mean(Score,na.rm=TRUE),sd=sd(Score,na.rm=TRUE))
  tmp <- data
  x <- Games
  if (is.null(x)==FALSE){
    y <- gregexpr("s[^A-Za-z]*[0-9]+",x)
    Spot <- as.vector(y[[1]])
    Lens <- attr(y[[1]],"match.length")
    z <- matrix(c(Spot,Lens),nrow=2,byrow=TRUE)
    z[2,] <- apply(z,2,function(x){x[2]=x[1]+x[2]})
    z[2,ncol(z)] <- nchar(x)
    a <- as.vector(c(0,z))
    b <- 0
    for (i in 1:(length(a)-1)) {
      b[i] <- substr(x,a[i]+1,a[i+1])
    }
    b[seq_along(b)[seq_along(b) %% 2 ==0]] <- gsub("[^0-9]","",b[seq_along(b) %% 2 == 0])
    c <- data.frame(matrix(b,ncol=4,byrow=TRUE))
    colnames(c) <- c("AwayTeam","AwayScore","HomeTeam","HomeScore")
    for (j in 1:nrow(c)){
      if (length(which(data$AwayTeam==c$AwayTeam[1] & data$HomeTeam==c$HomeTeam[1] & is.na(data$AwayScore))) == 0) {
        stop(paste(c$AwayTeam[1]," at ",c$HomeTeam[1]," is not a game still remaining to be played.",sep=""))
      }
      tmp$AwayScore[tmp$AwayTeam==c$AwayTeam[j] & tmp$HomeTeam==c$HomeTeam[j] & is.na(tmp$AwayScore)] <- c$AwayScore[j]
      tmp$HomeScore[tmp$AwayTeam==c$AwayTeam[j] & tmp$HomeTeam==c$HomeTeam[j] & is.na(tmp$HomeScore)] <- c$HomeScore[j]
    }
  }
  for (s in 1:sims){
    tmp$AwayScore <- as.numeric(apply(dat[,3:6],1,function(x){
      ifelse(is.na(x[2]),
        ifelse(max(round(rnorm(1,
          mean=mean(c(statsscore$mean[statsscore$Team==x[1]],statsagainst$mean[statsagainst$Team==x[3]])),
          sd=sqrt((statsscore$sd[statsscore$Team==x[1]]^2+(statsagainst$sd[statsagainst$Team==x[3]])^2)/4))),0)==1,
          2,
          max(round(rnorm(1,
          mean=mean(c(statsscore$mean[statsscore$Team==x[1]],statsagainst$mean[statsagainst$Team==x[3]])),
          sd=sqrt((statsscore$sd[statsscore$Team==x[1]]^2+(statsagainst$sd[statsagainst$Team==x[3]])^2)/4))),0)),x[2])}))
    tmp$HomeScore <- as.numeric(apply(dat[,3:6],1,function(x){
      ifelse(is.na(x[4]),
        ifelse(max(round(rnorm(1,
          mean=mean(c(statsscore$mean[statsscore$Team==x[3]],statsagainst$mean[statsagainst$Team==x[1]])),
          sd=sqrt((statsscore$sd[statsscore$Team==x[3]]^2+(statsagainst$sd[statsagainst$Team==x[1]])^2)/4))),0)==1,
          2,
          max(round(rnorm(1,
          mean=mean(c(statsscore$mean[statsscore$Team==x[3]],statsagainst$mean[statsagainst$Team==x[1]])),
          sd=sqrt((statsscore$sd[statsscore$Team==x[3]]^2+(statsagainst$sd[statsagainst$Team==x[1]])^2)/4))),0)),x[4])}))
    tm3 <- FinalRank(scores=tmp)
    y <- tm3 %>% select(Team,ConfRank)
    if (s==1) {x<-y} else {x<-full_join(x,y,by="Team")}
    tmp <- data
  }
  n <- c(1:sims)
  colnames(x) <- c("Team",sapply(n,function(x){paste("Sim",x,sep="")}))
  x
}

#'@name SeedPlot
#'@title SeedPlot
#'@description This is a function that will take an output from the \code{NFLSim} function and create a plotly of the seeding outcomes from the simulation
#'@param TeamX The specific team wanting to be analyzed in the graph
#'@param Sim The simulation data set used from the \code{NFLSim} function
#'@param Plotly An identifier to determine whether or not the user wants the graph to be a plotly
#'@return A (plotly) bar graph showing the frequency of times a teams obtains a specific seed in the conference based on the simulation settings.
#'If the graph is a plotly, the user can hover over a specific part of the graph to show the specific seed and the frequency that goes along with it.
#' @examples
#' SeedPlot()
#' SeedPlot(TeamX="Raiders")
#' SeedPlot(TeamX="Raiders",Sim=NFLSim(data=WeeklyUpdate(16)))                         
SeedPlot <- function(TeamX="Bills",Sim=NFLSim(data=WeeklyUpdate()),Plotly=TRUE){
  require(tidyverse)
  require(plotly)
  t <- Sim %>% filter(Team==TeamX) %>% select(-Team)
  count <- sapply(c(1:16),function(x){length(which(t==x))})
  num <- data.frame(count)
  num$seed <- c(1:16)
  gg <- ggplot(num,aes(x=seed,y=count))+geom_bar(stat="identity")+
    xlab("Seed")+ylab("Frequency")+ggtitle(paste("Seeding Plot of",TeamX))+
    theme(plot.title = element_text(hjust = 0.5))
  if(Plotly==TRUE) {ggplotly(gg)} else {gg}
}
