library(ggplot2)
library(extrafont)
loadfonts()

windowsFonts(Times=windowsFont("TT Times New Roman"))

dat <- data.frame(
  y = 1:3,
  text = c("This is text", "Text with\nmultiple lines", "Some more text")
)

dat <- data.frame()
p <- ggplot(data.frame(), aes(x=0, y=0)) +
  scale_y_continuous(name="", limits=c(0,10), breaks=NULL) +
  scale_x_continuous(name="", limits=c(0,10), breaks=NULL)
  
p

p + annotate(geom="text", x=1, y=1, label="Annotation text", colour="red", size=7, family="Times", fontface="bold", angle=30)


p + geom_text(aes(label=text), family="TradeGothic LT CondEighteen", fontface="italic", lineheight=.8) +
  annotate(geom="text", x=1, y=1.5, label="Annotation text", colour="red", size=7, family="TradeGothic LT CondEighteen", fontface="bold", angle=30)

# --- Athletes per region

leaderboard3 <- read.csv(file = "CF-rawdata-temp.csv")
counts <- sort(table(leaderboard3$entrant.regionName))
counts
nbTotalAthletes = length(leaderboard3$entrant.regionName)
par(las = 1, mar = c(3, 8, 3, 3))
bp <- barplot(counts, main=paste0("Number of men athletes per region (total=", nbTotalAthletes, ")"), horiz=TRUE)
text(x = counts, y = bp, label = counts, pos = 2, cex = 0.8)
# TODO: Pie Chart => not really relevant

# --- Top ranking per region
cor(leaderboard3[,c('entrant.age', 'overallRank')])

# --- Median ranking per region

# --- Convert height to cm
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
leaderboard3$entrant.height <- lapply(leaderboard3$entrant.height, convertHeight)

# tmp <- lapply(leaderboard3$entrant.height, convertHeight)
# which(is.na(as.numeric(as.character(tmp))))

# --- Convert Lb to Kg
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
leaderboard3$entrant.weight <- lapply(leaderboard3$entrant.weight, convertWeight)
# tmp <- lapply(leaderboard3$entrant.weight, convertWeight)
# which(is.na(as.numeric(as.character(tmp))))


# --- Ranking vs Age
plot(leaderboard3$entrant.age, leaderboard3$overallRank)
