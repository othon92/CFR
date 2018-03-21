# ##################################################################################################
#
# Download crossfit data and store it
#
# ##################################################################################################

library(jsonlite)
library(plyr)

### Variables
outputFile = "CF-rawdata.csv"

# Notes:
#   Division
#     1 = Men
#     2 = Women
baseurl <- "https://games.crossfit.com/competitions/api/v1/competitions/open/2018/leaderboards?division=1&region=0&scaled=0&sort=0&occupation=0&page="


### Download the leaderboard

# Get the count of pages
mydata <- fromJSON(paste0(baseurl, 1), flatten=TRUE)
nbOfPages = mydata$pagination$totalPages

pages <- list()
for(i in 1:nbOfPages){
  message("Retrieving page ", i, "/", nbOfPages, "\r", appendLF = FALSE)
  pageData <- readLines(paste0(baseurl, i))
  
  # Handle issues with unescaped backslashes
  pageData2 <- gsub('([^,:\\[:space:]])\"([^:,])', '\\1\\2', pageData)
  
  mydata <- fromJSON(pageData2, flatten=TRUE)
  pages[[i+1]] <- mydata$leaderboardRows
}
message("")

leaderboard <- rbind_pages(pages)

### Flatten the scores and append it (create new columns) to the dataframe
message("Flattening scores... ")

oldScores = leaderboard$scores
newScores <- data.frame()

lNewScores <- list()
dfNewScores <- data.frame()
lengthOldScores = length(oldScores)
lChuncks = 1000
for(i in 1:lengthOldScores) {
    message("Score: ", i, "/", lengthOldScores, "\r", appendLF = FALSE)
    rOldScores <- oldScores[i]
    dfTmpNewScores <- as.data.frame(t(unlist(rOldScores, recursive = TRUE)))
    
    colnames(dfTmpNewScores) <- gsub("(\\d)","\\.\\1",colnames(dfTmpNewScores))
    
    j = (i-1)%%lChuncks + 1
    lNewScores[[j]] <- dfTmpNewScores
    
    if(j == lChuncks || i == lengthOldScores){
      dfNewScores <- rbind.fill(dfNewScores, rbind_pages(lNewScores))
      lNewScores <- list()
    }
}
message("")

newLeaderboard <- cbind(leaderboard, dfNewScores)

### Convert scores to strings
message("Convert scores to strings...")
newLeaderboard$scores <- as.character(newLeaderboard$scores)

### Convert all heights in cm
message("Convert heights in cm...")
convertHeight <- function(x) {
  y = x
  if(grepl(" in", x)) {
    x2 = gsub("[^[:digit:]]", "",x)
    y = cm(as.numeric(x2))
  } else if (grepl(" cm", x)) {
    y = as.numeric(gsub("[^[:digit:]]", "",x))
  } else if( x == "") {
    y = 0
  }
  y
}
newLeaderboard$entrant.height <- lapply(newLeaderboard$entrant.height, convertHeight)
newLeaderboard$entrant.height <- as.numeric(newLeaderboard$entrant.height)

### Convert all weights in kg
message("Convert weights in kg...")
convertWeight <- function(x) {
  y = x
  if(grepl(" lb", x)) {
    x2 = gsub("[^[:digit:]]", "",x)
    y = as.numeric(x2) * 0.45359237
  } else if (grepl(" kg", x)) {
    y = as.numeric(gsub("[^[:digit:]]", "",x))
  } else if( x == "") {
    y = 0
  }
  y
}
newLeaderboard$entrant.weight <- lapply(newLeaderboard$entrant.weight, convertWeight)
newLeaderboard$entrant.weight <- as.numeric(newLeaderboard$entrant.weight)

### store it
message("Store the data in ", outputFile, " ...")
write.csv(newLeaderboard, file = outputFile)
# leaderboard3 <- read.csv(file = outputFile)

# subset(newLeaderboard, grepl("Arche", entrant.affiliateName), select=c(entrant.affiliateName, entrant.competitorName, overallRank))
