library(jsonlite)

getCFResult <- function(division = 1, region = 0, athlete, scaled = 0) {
  
  
  url = paste('https://games.crossfit.com/competitions/api/v1/competitions/open/2018/leaderboards?',
              'division=', division,
              '&region=', region,
              '&athlete=', athlete,
              '&scaled=', scaled,
              '&sort=0',
              '&occupation=0', sep='')
  
  data <- fromJSON(url)
  
  # Get the total number of competitors
  totalCompetitors = data$pagination$totalCompetitors
  
  # Get the rank of the specified athlete
  leaderboard <- data$leaderboardRows
  athleteRank = leaderboard[leaderboard$entrant$competitorId == athlete,]$overallRank
  
  return(c(as.numeric(athleteRank), as.numeric(totalCompetitors)))
  
}


