par(mar = c(0,0,0,0), bg = "grey")

# Initialize the plot
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', bg = 'blue')
font.cex = 1.2
font.col = "white"
  
# Get the rank for the overall leaderboard for men, Worldwide
myRank <- getCFResult(athlete = 1309535)
mytext = paste0("Worldwide men: \n", format(myRank[1], big.mark=","), " / ", format(myRank[2], big.mark=","))
text(x = 0.25, y = 0.75, mytext, cex = font.cex, col = font.col)

# Get the rank for the overall leaderboard for men, South Europe
myRank <- getCFResult(athlete = 1309535, region=24)
mytext = paste0("South Europe men: \n", format(myRank[1], big.mark=","), " / ", format(myRank[2], big.mark=","))
text(x = 0.25, y = 0.25, mytext, cex = font.cex, col = font.col)

# Get the rank for the overall leaderboard for men 40-44, Worldwide
myRank <- getCFResult(athlete = 1309535, division = 12)
mytext = paste0("Worldwide men 40-44: \n", format(myRank[1], big.mark=","), " / ", format(myRank[2], big.mark=","))
text(x = 0.75, y = 0.75, mytext, cex = font.cex, col = font.col)

# Get the rank for the overall leaderboard for men 40-44, South Europe
myRank <- getCFResult(athlete = 1309535, division = 12, region=24)
mytext = paste0("South Europe men 40-44: \n", format(myRank[1], big.mark=","), " / ", format(myRank[2], big.mark=","))
text(x = 0.75, y = 0.25, mytext, cex = font.cex, col = font.col)

