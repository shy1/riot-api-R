## function to collect the match IDs of all the games in a player/summoner's match history
## when passed a summoner ID number as an argument
require(dplyr)
require(data.table)
require(jsonlite)


getMatchIds <- function(sid) {
     ## initialize variables
     key <-readLines("riotapi.key")
     bidx <- -15
     more.games <- TRUE
     f.data <- data.frame(summoner_id=character(), matchId=character(), queueType=character(), stringsAsFactors=FALSE)

     while (more.games == TRUE) {
          ## increment beginning index variable and acquire raw json data with api request
          ## api requests return a max of 15 matches
          bidx <- bidx + 15
          url <- paste0("https://na.api.pvp.net/api/lol/na/v2.2/matchhistory/", sid, "?beginIndex=", bidx, "&api_key=", key)
          raw.data <- readLines(url, warn = "F")
          c.data <- fromJSON(raw.data)

          ## check to make sure requests are still returning more results and
          ## set while loop condition to false if no more results are being sent
          len <- length(c.data)
          if (len < 1) {
               more.games <- FALSE
          } else {
               ## extract relevant columns from data frame and arrange in descending order
               c.data <- tbl_df(c.data$matches)
               s.data <- select(c.data, matchId, queueType)
               s.data <- arrange(s.data, desc(matchId))
               
               ## add summoner_id column to the data frame to allow data from different
               ## players to be separated when combined into one file/data frame.
               count <- nrow(s.data)
               scol <- vector("character", count)
               scol[1:count] <- sid
               s.data <- cbind("summoner_id" = scol, s.data)
               
               ## extract only ranked solo queue games and append them to the results
               ## of the previous api requests
               r.data <- filter(s.data, queueType == "RANKED_SOLO_5x5")
               l <- list(f.data, r.data)
               f.data <- rbindlist(l)
               
               ## pause to avoid triggering api request limits
               Sys.sleep(1.21)
          }
          
     }
     ## append results to a single file containing the data for
     ## all of the players whose match histories are being collected
     write.table(f.data, "matches.csv", quote=FALSE, append=TRUE, sep=",", qmethod="double", row.names=FALSE, col.names=FALSE)
}

## request full data/timeline of a specific match when passed a match ID as an argument
## and write the received JSON data as a local file named after the match ID

getMatch <- function(mid) {
	key <-readLines("riotapi.key")
	url <- paste0("https://na.api.pvp.net/api/lol/na/v2.2/match/", mid, "?includeTimeline=TRUE&api_key=", key)
	## in case of connection error wait 10 seconds and try again
	raw.data <- "error"
	while (raw.data == "error") {
		raw.data <- tryCatch(readLines(url, warn = "F"), error=function(e) {
		message(e)
		return("error")
	})
	if (raw.data == "error") Sys.sleep(10)
	}
	fpath <- paste0("./matches/", mid, ".json")
	writeLines(raw.data, fpath)
	Sys.sleep(1.25)
}
