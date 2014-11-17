## reads raw JSON match data from a file, extracts certain values into a data frame,
## then writes this match data to an individual csv file for each summoner

library(dplyr)
library(jsonlite)
setwd("B:/matches")

games <- list.files("B:/matches")
champList <- read.csv("C:/R/champlist.csv", stringsAsFactors=FALSE)

getPlayerData <- function(mfile) {
     
     m.data <- tryCatch(fromJSON(readLines(mfile)), error=function(e) {
                    elog <- data.frame(file=mfile, message=as.character(e))
                    write.table(elog, "C:/R/errlog.txt", quote=FALSE, append=TRUE, sep=",", qmethod="double", row.names=FALSE, col.names=FALSE)
                    return("error")
               })
 
     if (!(m.data == "error")) {
     if (m.data$matchDuration > 1200) {
          f.data <- tryCatch(data.frame(matchId=m.data$matchId, matchDuration=m.data$matchDuration, participantId=m.data$participants$participantId, teamId=m.data$participants$teamId, summonerId=m.data$participantIdentities$player$summonerId, summonerName=m.data$participantIdentities$player$summonerName, championId=m.data$participants$championId, championName=as.character(c(select(filter(champList, id==m.data$participants$championId[1]), name), select(filter(champList, id==m.data$participants$championId[2]), name), select(filter(champList, id==m.data$participants$championId[3]), name), select(filter(champList, id==m.data$participants$championId[4]), name), select(filter(champList, id==m.data$participants$championId[5]), name), select(filter(champList, id==m.data$participants$championId[6]), name), select(filter(champList, id==m.data$participants$championId[7]), name), select(filter(champList, id==m.data$participants$championId[8]), name), select(filter(champList, id==m.data$participants$championId[9]), name), select(filter(champList, id==m.data$participants$championId[10]), name))), lane=m.data$participants$timeline$lane, role=m.data$participants$timeline$role, spell1=m.data$participants$spell1Id, spell2=m.data$participants$spell2Id, winner=m.data$participants$stats$winner,
                          firstBlood=c(m.data$teams$firstBlood[1], m.data$teams$firstBlood[1], m.data$teams$firstBlood[1], m.data$teams$firstBlood[1], m.data$teams$firstBlood[1], m.data$teams$firstBlood[2], m.data$teams$firstBlood[2], m.data$teams$firstBlood[2], m.data$teams$firstBlood[2], m.data$teams$firstBlood[2]), firstTower=c(m.data$teams$firstTower[1], m.data$teams$firstTower[1], m.data$teams$firstTower[1], m.data$teams$firstTower[1], m.data$teams$firstTower[1], m.data$teams$firstTower[2], m.data$teams$firstTower[2], m.data$teams$firstTower[2], m.data$teams$firstTower[2], m.data$teams$firstTower[2]),
                          firstInhibitor=c(m.data$teams$firstInhibitor[1], m.data$teams$firstInhibitor[1], m.data$teams$firstInhibitor[1], m.data$teams$firstInhibitor[1], m.data$teams$firstInhibitor[1], m.data$teams$firstInhibitor[2], m.data$teams$firstInhibitor[2], m.data$teams$firstInhibitor[2], m.data$teams$firstInhibitor[2], m.data$teams$firstInhibitor[2]), firstBaron=c(m.data$teams$firstBaron[1], m.data$teams$firstBaron[1], m.data$teams$firstBaron[1], m.data$teams$firstBaron[1], m.data$teams$firstBaron[1], m.data$teams$firstBaron[2], m.data$teams$firstBaron[2], m.data$teams$firstBaron[2], m.data$teams$firstBaron[2], m.data$teams$firstBaron[2]),
                          firstDragon=c(m.data$teams$firstDragon[1], m.data$teams$firstDragon[1], m.data$teams$firstDragon[1], m.data$teams$firstDragon[1], m.data$teams$firstDragon[1], m.data$teams$firstDragon[2], m.data$teams$firstDragon[2], m.data$teams$firstDragon[2], m.data$teams$firstDragon[2], m.data$teams$firstDragon[2]), towerKills=c(m.data$teams$towerKills[1], m.data$teams$towerKills[1], m.data$teams$towerKills[1], m.data$teams$towerKills[1], m.data$teams$towerKills[1], m.data$teams$towerKills[2], m.data$teams$towerKills[2], m.data$teams$towerKills[2], m.data$teams$towerKills[2], m.data$teams$towerKills[2]),
                          inhibitorKills=c(m.data$teams$inhibitorKills[1], m.data$teams$inhibitorKills[1], m.data$teams$inhibitorKills[1], m.data$teams$inhibitorKills[1], m.data$teams$inhibitorKills[1], m.data$teams$inhibitorKills[2], m.data$teams$inhibitorKills[2], m.data$teams$inhibitorKills[2], m.data$teams$inhibitorKills[2], m.data$teams$inhibitorKills[2]), baronKills=c(m.data$teams$baronKills[1], m.data$teams$baronKills[1], m.data$teams$baronKills[1], m.data$teams$baronKills[1], m.data$teams$baronKills[1], m.data$teams$baronKills[2], m.data$teams$baronKills[2], m.data$teams$baronKills[2], m.data$teams$baronKills[2], m.data$teams$baronKills[2]),
                          dragonKills=c(m.data$teams$dragonKills[1], m.data$teams$dragonKills[1], m.data$teams$dragonKills[1], m.data$teams$dragonKills[1], m.data$teams$dragonKills[1], m.data$teams$dragonKills[2], m.data$teams$dragonKills[2], m.data$teams$dragonKills[2], m.data$teams$dragonKills[2], m.data$teams$dragonKills[2]), champLevel=m.data$participants$stats$champLevel, kills=m.data$participants$stats$kills, deaths=m.data$participants$stats$deaths, assists=m.data$participants$stats$assists, doubleKills=m.data$participants$stats$doubleKills, tripleKills=m.data$participants$stats$tripleKills, quadraKills=m.data$participants$stats$quadraKills,
                          pentaKills=m.data$participants$stats$pentaKills, unrealKills=m.data$participants$stats$unrealKills, largestKillingSpree=m.data$participants$stats$largestKillingSpree, totalDamageDealt=m.data$participants$stats$totalDamageDealt, totalDamageDealtToChampions=m.data$participants$stats$totalDamageDealtToChampions, totalDamageTaken=m.data$participants$stats$totalDamageTaken, totalHeal=m.data$participants$stats$totalHeal, minionsKilled=m.data$participants$stats$minionsKilled, neutralMinionsKilled=m.data$participants$stats$neutralMinionsKilled, neutralMinionsKilledTeamJungle=m.data$participants$stats$neutralMinionsKilledTeamJungle,
                          neutralMinionsKilledEnemyJungle=m.data$participants$stats$neutralMinionsKilledEnemyJungle, goldEarned=m.data$participants$stats$goldEarned, magicDamageDealtToChampions=m.data$participants$stats$magicDamageDealtToChampions, physicalDamageDealtToChampions=m.data$participants$stats$physicalDamageDealtToChampions, trueDamageDealtToChampions=m.data$participants$stats$trueDamageDealtToChampions, visionWardsBoughtInGame=m.data$participants$stats$visionWardsBoughtInGame, sightWardsBoughtInGame=m.data$participants$stats$sightWardsBoughtInGame, magicDamageDealt=m.data$participants$stats$magicDamageDealt, physicalDamageDealt=m.data$participants$stats$physicalDamageDealt,
                          trueDamageDealt=m.data$participants$stats$trueDamageDealt, magicDamageTaken=m.data$participants$stats$magicDamageTaken, physicalDamageTaken=m.data$participants$stats$physicalDamageTaken, trueDamageTaken=m.data$participants$stats$trueDamageTaken, firstBloodKill=m.data$participants$stats$firstBloodKill, firstBloodAssist=m.data$participants$stats$firstBloodAssist, firstTowerKill=m.data$participants$stats$firstTowerKill, firstTowerAssist=m.data$participants$stats$firstTowerAssist, firstInhibitorKill=m.data$participants$stats$firstInhibitorKill, firstInhibitorAssist=m.data$participants$stats$firstInhibitorAssist, playerInhibitorKills=m.data$participants$stats$inhibitorKills,
                          playerTowerKills=m.data$participants$stats$towerKills, wardsPlaced=m.data$participants$stats$wardsPlaced, wardsKilled=m.data$participants$stats$wardsKilled, largestMultiKill=m.data$participants$stats$largestMultiKill,  killingSprees=m.data$participants$stats$killingSprees, totalUnitsHealed=m.data$participants$stats$totalUnitsHealed, totalTimeCrowdControlDealt=m.data$participants$stats$totalTimeCrowdControlDealt, cs_PerMin0=m.data$participants$timeline$creepsPerMinDeltas$zeroToTen, cs_PerMin10=m.data$participants$timeline$creepsPerMinDeltas$tenToTwenty,xp_PerMin0=m.data$participants$timeline$xpPerMinDeltas$zeroToTen, xp_PerMin10=m.data$participants$timeline$xpPerMinDeltas$tenToTwenty,
                          g_PerMin0=m.data$participants$timeline$goldPerMinDeltas$zeroToTen, g_PerMin10=m.data$participants$timeline$goldPerMinDeltas$tenToTwenty, csDiff_PerMin0=m.data$participants$timeline$csDiffPerMinDeltas$zeroToTen, csDiff_PerMin10=m.data$participants$timeline$csDiffPerMinDeltas$tenToTwenty, xpDiff_PerMin0=m.data$participants$timeline$xpDiffPerMinDeltas$zeroToTen, xpDiff_PerMin10=m.data$participants$timeline$xpDiffPerMinDeltas$tenToTwenty, dt_PerMin0=m.data$participants$timeline$damageTakenPerMinDeltas$zeroToTen, dt_PerMin10=m.data$participants$timeline$damageTakenPerMinDeltas$tenToTwenty, dtDiff_PerMin0=m.data$participants$timeline$damageTakenDiffPerMinDeltas$zeroToTen, dtDiff_PerMin10=m.data$participants$timeline$damageTakenDiffPerMinDeltas$tenToTwenty, stringsAsFactors=FALSE), 
                   error=function(e) {
                        elog <- data.frame(file=mfile, message=as.character(e))
                        write.table(elog, "C:/R/errlog.txt", quote=FALSE, append=TRUE, sep=",", qmethod="double", row.names=FALSE, col.names=FALSE)
                        return("error")
                   })
     
          Sys.sleep(0.05)
          if (!(f.data == "error")) {
          for (i in 1:10) {
               fpath <- paste0("B:/summoners/", f.data$summonerId[i], ".csv")
               if (file.exists(fpath)==TRUE) {
                    write.table(f.data[i, ], fpath, quote=FALSE, append=TRUE, sep=",", qmethod="double", row.names=FALSE, col.names=FALSE)
               } else {
                    write.table(f.data[i, ], fpath, quote=FALSE, sep=",", qmethod="double", row.names=FALSE, col.names=TRUE)
               }
          }
          }
     }
     }

}

## code to allow script to be run in separate R session to release memory after 1000 games are processed
## see matchLoop.R
x <- as.integer(readLines("C:/R/itest.txt"))
y <- x + 999

for (i in x:y) {
     mfile <- games[i]
     writeLines(as.character(i), "C:/R/iteration.txt")
     getPlayerData(mfile)
}
