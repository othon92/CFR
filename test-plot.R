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

# ---

counts <- sort(table(leaderboard3$entrant.regionName))
counts
par(las = 1, mar = c(3, 8, 3, 3))
bp <- barplot(counts, horiz=TRUE)
text(x = counts, y = bp, label = counts, pos = 2, cex = 0)


