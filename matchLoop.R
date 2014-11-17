## writes iteration values to a file that matchFileData.R can read when 
## called in a new R session for memory management purposes.
## iterates by 1000 and matchData.R does 1000 games per session 

for (i in seq(40001, 88001, 1000)) {
     writeLines(as.character(i), "C:/R/itest.txt")
     system("R -f C:/R/matchFileData.R", show.output.on.console = FALSE)
}

