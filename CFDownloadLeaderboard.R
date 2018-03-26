# ##################################################################################################
#
# Download crossfit leaderboard and store it
#
# ##################################################################################################

library(jsonlite)
library(plyr)

cfDownloadLeaderboard <- function(year = "2018", 
                                  competition = "open", 
                                  outputFile = "CFrawdata.SCT"){

  ### Variables
  outputFile = paste0(outputFile, ".", competition, ".", year)
  
  # Notes:
  #   Division:
    # l <- c("yip" = 1, "yop" = 2)
    # a = names(which(l == 1))
    # b = unname(l["yop"])
  
  divisions = c(
    "Men" = 1,
    "Women" = 2,
    "Men_45-49" = 3,
    "Women_45-49" = 4,
    "Men_50-54" = 5,
    "Women_50-54" = 6,
    "Men_55-59" = 7,
    "Women_55-59" = 8,
    "Men_60+" = 9,
    "Women_60+" = 10,
    "Team" = 11,
    "Men_40-44" = 12,
    "Women_40-44" = 13,
    "Boys_14-15" = 14,
    "Girls_14-15" = 15,
    "Boys_16-17" = 16,
    "Girls_16-17" = 17,
    "Men_35-39" = 18,
    "Women_35-39" = 19
  )

  #   Region:
  
  regions = c(
    "Worldwide" = 0,
    "Africa Middle East" = 25,
    "Asia" = 20,
    "Australasia" = 21,
    "Canada East" = 18,
    "Canada West" = 5,
    "Central America" = 26,
    "Central East" = 6,
    "Europe Central" = 23,
    "Europe North" = 22,
    "Europe South" = 24,
    "Mid Atlantic" = 9,
    "North Central" = 10,
    "North East" = 11,
    "South America" = 27,
    "South Central" = 14,
    "South East" = 15,
    "South West" = 17,
    " West Coast" = 19
  )
 
  #   Scaled:
  #       0 = Rx'd
  #       1 = Scaled
  
  baseurl <- paste0("https://games.crossfit.com/competitions/api/v1/competitions/",
                    competition, "/",
                    year, "/",
                    "leaderboards?",
                    "division=","1",
                    "&region=","0",
                    "&scaled=","0",
                    "&sort=","0",
                    "&occupation=","0",
                    "&page=")
  
  ### Download the leaderboard
  
  # Get the count of pages
  mydata <- fromJSON(paste0(baseurl, 1), flatten=TRUE)
  if(year == '2018') {
    nbOfPages = mydata$pagination$totalPages
  } else if (year == '2017') {
    nbOfPages = mydata$totalpages
  }
  
  pages <- list()
  for(i in 1:nbOfPages){
    message("\rRetrieving page ", i, "/", nbOfPages, appendLF = FALSE)
    pageData <- readLines(paste0(baseurl, i))
    
    # Handle issues with unescaped backslashes
    pageData2 <- gsub('([^,:{\\[:space:]])\"([^:,}])', '\\1\\2', pageData)
    
    #save(file = './dump.tmp', pageData2)
    mydata <- fromJSON(pageData2, flatten=TRUE)
    
    if(year == '2018') {
      pages[[i+1]] <- mydata$leaderboardRows
    } else if (year == '2017') {
      pages[[i]] <- mydata$athletes
    }

  }
  message("")
  
  # Fix issue with null pages returned
  message("Fixing issue with null pages...")
  nullPages <- which(vapply(pages, is.data.frame, logical(1)) == FALSE)
  for(i in sort(nullPages, decreasing = TRUE)){
    message(" Remove null page ", i)
    pages[[i]] <- NULL
  }
  
  # Create the dataframe
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
      message("\rScore: ", i, "/", lengthOldScores, appendLF = FALSE)
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
  
  ### Convert all heights in cm
  message("Convert heights in cm...")
  convertHeight <- function(x) {
    y = x
    
    # XXX in
    if(grepl(" in", x)) {
      x2 = gsub("[^[:digit:]]", "",x)
      y = cm(as.numeric(x2))
      
    # XXX cm
    } else if (grepl(" cm", x)) {
      y = as.numeric(gsub("[^[:digit:]]", "",x))
    
    # XX'XX"
    } else if(grepl('"', x)||grepl("'", x)) {
      x2f = as.numeric(gsub("^.*?(\\d*)'.*$", "\\1", x))
      x2i = as.numeric(gsub('^.*?(\\d*)".*$', "\\1", x))
      if(is.na(x2f)) { x2f = 0 }
      if(is.na(x2i)) { x2i = 0 }
      y = cm(x2f * 12 + x2i)
    
    # ""
    } else if( x == "") {
      y = 0
    }
    y
  }
  
  if(year == '2018') {
    newLeaderboard$entrant.height <- lapply(newLeaderboard$entrant.height, convertHeight)
    newLeaderboard$entrant.height <- as.numeric(newLeaderboard$entrant.height)
  } else if(year == '2017') {
    newLeaderboard$height <- lapply(newLeaderboard$height, convertHeight)
    newLeaderboard$height <- as.numeric(newLeaderboard$height)
  }
  
  
  ### Convert all weights in kg
  message("Convert weights in kg...")
  convertWeight <- function(x) {
    y = x
    
    # XXX lb
    if(grepl(" lb", x)) {
      x2 = gsub("[^[:digit:]]", "",x)
      y = as.numeric(x2) * 0.45359237
    
    # XXX kg
    } else if (grepl(" kg", x)) {
      y = as.numeric(gsub("[^[:digit:]]", "",x))
      
    # ""
    } else if( x == "") {
      y = 0
    }
    y
  }
  if(year == '2018') {
    newLeaderboard$entrant.weight <- lapply(newLeaderboard$entrant.weight, convertWeight)
    newLeaderboard$entrant.weight <- as.numeric(newLeaderboard$entrant.weight)
  } else if(year == '2017') {
    newLeaderboard$weight <- lapply(newLeaderboard$weight, convertWeight)
    newLeaderboard$weight <- as.numeric(newLeaderboard$weight)
  }
  
  ### store it in Rda format
  message("Store the data in ", outputFile, ".Rda ...")
  save(newLeaderboard, file = paste0(outputFile, ".Rda"))
  
  ### Convert scores to strings
  message("Convert scores to strings...")
  newLeaderboard$scores <- as.character(newLeaderboard$scores)
  
  ### store it in CSV format
  message("Store the data in ", outputFile, ".csv ...")
  write.csv(newLeaderboard, file = paste0(outputFile, ".csv"))
  # leaderboard3 <- read.csv(file = outputFile)

}

cfDownloadLeaderboard(year = "2017")

