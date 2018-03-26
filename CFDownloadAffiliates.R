# ##################################################################################################
#
# Download crossfit affiliate list
#
# ##################################################################################################

library(rvest)

outputFile = "affiliates.SCT"

### List of affiliates
baseurl <- "https://map.crossfit.com/getAllAffiliates.php"
webpage <- read_html(baseurl)
body <- toString(webpage %>% html_text())

# Transform the data into CSV format
affList <- strsplit(body, "\\],\\[")
affList[[1]] <- gsub('[', '', affList[[1]], fixed = TRUE)
affList[[1]] <- gsub(']', '', affList[[1]], fixed = TRUE)
reAffList <- strsplit(affList[[1]],",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", perl=TRUE)
uReAffList <- unlist(reAffList)
mAffList <- matrix(unlist(uReAffList), nrow = length(reAffList), byrow = TRUE)
dfAffList <- as.data.frame(mAffList)
colnames(dfAffList) <- c("latitude", "longitude", "affiliate.name", "affiliate.id", "affiliate.flag")
dfAffList$affiliate.id <- as.integer(gsub('"', '', dfAffList$affiliate.id))

### Scrape additional information
# https://games.crossfit.com/affiliate/3220
affBaseurl <- "https://games.crossfit.com/affiliate/"
message("Scraping additional information...")

dfAffList$affiliate.country <- NULL
dfAffList$affiliate.country[1] <- ""
dfAffList$affiliate.region <- NULL
dfAffList$affiliate.region[1] <- ""
dfAffList$affiliate.address <- NULL
dfAffList$affiliate.address[1] <- ""
dfAffList$affiliate.phone <- NULL
dfAffList$affiliate.phone[1] <- ""
dfAffList$affiliate.website <- NULL
dfAffList$affiliate.website[1] <- ""

nbOfAffiliates = nrow(dfAffList)
for(i in 1:nbOfAffiliates){
  affId = as.character(dfAffList$affiliate.id[i])
  affName = dfAffList$affiliate.name[i]
  message("\r                                                                                   ", appendLF = FALSE)
  message("\rAffiliate: ", affName, "/", affId, " (", i, "/", nbOfAffiliates, ")", appendLF = FALSE)
  
  affUrl <- paste0(affBaseurl, affId)
  affWebpage <- read_html(affUrl)
  
  dfAffList$affiliate.country[i] = affWebpage %>% html_node("li.item.sm-hide div.text small") %>% html_text()
  lText <- affWebpage %>% html_nodes("li.item.sm-inline div.text") %>% html_text()
  dfAffList$affiliate.region[i] = lText[1]
  dfAffList$affiliate.address[i] = lText[2]
  dfAffList$affiliate.phone[i] = lText[3]
  dfAffList$affiliate.website[i] = lText[4]
}

### Save the data
# TODO
save(dfAffList, file = paste0(outputFile, ".Rda"))

