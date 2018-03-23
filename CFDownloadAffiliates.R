# ##################################################################################################
#
# Download crossfit affiliate list
#
# ##################################################################################################

library(rvest)


### List of affiliates

baseurl <- "https://map.crossfit.com/getAllAffiliates.php"

webpage <- read_html(baseurl)


body <- toString(webpage %>% html_text())

l <- strsplit(body, "\\],\\[")
l[[1]] <- gsub('[', '', l[[1]], fixed = TRUE)
l[[1]] <- gsub(']', '', l[[1]], fixed = TRUE)
reL <- strsplit(l[[1]],",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", perl=TRUE)
uReL <- unlist(reL)
mL <- matrix(unlist(uReL), nrow = length(reL), byrow = TRUE)
dfl <- as.data.frame(mL)
colnames(dfl) <- c("latitude", "longitude", "affiliate.name", "affiliate.id", "affiliate.flag")
dfl$affiliate.id <- as.integer(gsub('"', '', dfl$affiliate.id))

### Scrape additional information
# https://games.crossfit.com/affiliate/3220
affBaseurl <- "https://games.crossfit.com/affiliate/"
message("Scraping additional information...")

nbOfAffiliates = nrow(dfl)
for(i in 1:nbOfAffiliates){
  affId = as.character(dfl$affiliate.id[i])
  affName = dfl$affiliate.name[i]
  message("\r                                                                                   ", appendLF = FALSE)
  message("\rAffiliate: ", affName, "/", affId, " (", i, "/", nbOfAffiliates, ")", appendLF = FALSE)
  
  affUrl <- paste0(affBaseurl, affId)
  affWebpage <- read_html(affUrl)
  
  dfl$affiliate.country[i] = affWebpage %>% html_node("li.item.sm-hide div.text small") %>% html_text()
  lText <- affWebpage %>% html_nodes("li.item.sm-inline div.text") %>% html_text()
  dfl$affiliate.region[i] = lText[1]
  dfl$affiliate.address[i] = lText[2]
  dfl$affiliate.phone[i] = lText[3]
  dfl$affiliate.website[i] = lText[4]
}

