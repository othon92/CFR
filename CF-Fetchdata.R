

baseurl <- "https://games.crossfit.com/competitions/api/v1/competitions/open/2018/leaderboards?division=1&region=0&scaled=0&sort=0&occupation=0&page="
pages <- list()
for(i in 1:3){
  mydata <- fromJSON(paste0(baseurl, i), flatten=TRUE)
  message("Retrieving page ", i)
  pages[[i+1]] <- mydata$leaderboardRows
}

filings <- rbind_pages(pages)

nrow(filings)

filings[1:10,]
typeof(filings)
colnames(filings)
typeof(filings$scores)
filings$scores[1]
colnames(filings[1:3,]$scores[[1]])
do.call(cbind, filings[1:3,]$scores[[1]])

ufilings <- do.call(c, unlist(filings, recursive=FALSE))
ufilings[1:5,]
length(filings)

tdf = data.frame(A = c(1,1,1,1), B = c(2,2,2,2))
tdf
tdf$M.new <- mean(tdf$A, tdf$B)
do.call(cbind, tdf)

test <- structure(list(id = c(13, 27), seq = structure(list(
  `1` = c("1997", "1997", "1997", "2007"),
  `2` = c("2007", "2007", "2007", "2007", "2007", "2007", "2007")), 
  .Names = c("1", "2"))), .Names = c("penr", 
                                     "seq"), row.names = c("1", "2"), class = "data.frame")
do.call("c", test[["seq"]])


f <- function(l) {
  if (!is.list(l)) return(l)
  do.call('rbind', lapply(l, function(x) `length<-`(x, max(lengths(l)))))
}


lapply(filings[1:2,], f)

filings[1:5,2:4]
lapply(filings[1:5,], function(x) paste0(x))

write.csv(ufilings, file = "MyData.csv")

df <- data.frame(matrix(unlist(filings), nrow=length(filings), byrow=T))
typeof(df)


data1 <- fromJSON("https://api.github.com/users/hadley/repos")
data1
colnames(data1$owner)

colnames(as.data.frame(filings$scores))
typeof(filings$scores)

filings2 <- filings[1:5,]
filings2$scores = as.data.frame(filings2$scores)
filings2$scores[[,c("judge")]]
filings2$overallRank
scores2 <- filings2$scores

dd <- as.data.frame(unlist(scores2[1]))
dd  <-  as.data.frame(t(matrix(unlist(scores2), nrow=length(unlist(scores2[1])))))

filings3 <- cbind(filings2, dd)


expand(scores2)

colnames(flatten(filings))


x <- data.frame(driver = c("Bowser", "Peach"), occupation = c("Koopa", "Princess"))
x$vehicle <- data.frame(model = c("Piranha Prowler", "Royal Racer"))
x$vehicle$stats <- data.frame(speed = c(55, 34), weight = c(67, 24), drift = c(35, 32))
str(x)
str(flatten(x))
str(flatten(x, recursive = FALSE))
x
