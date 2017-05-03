##' Downloads random chess games
##' @description Downloads random chess games in PGN format from chessgames.com
##' @param number the number of games to download
##' @param destination an empty directory in which to save the games
##' @details The destination specified should be an EMPTY DIRECTORY. The function counts the files in the directory to determine how many more random games to download.
##' @details For example, if you specify 100 random games and your directory already contains 50 files, then the function will stop after only 50 games.
##' @details The destination should end in a forward slash -- for example, "data/"
##' @export

download_random_games <- function(number,destination) {
  readUrl <- function(url) {
    out <- tryCatch(
      {
        suppressWarnings(readLines(con=url, warn=FALSE))
      },
      error=function(cond) {
        # Choose a return value in case of error
        if (as.character(cond) == "Error in file(con, \"r\"): cannot open the connection\n") {
          return(0)
        }
        else {
          return(1)
        }
      }
    )
    return(out)
  }
  if (substr(destination,nchar(destination),nchar(destination)) != "/") {
    stop("Make sure your destination folder name ends with a forward slash (/) !!!")
  }
  while (length(list.files(destination)) < number) {
    startingfiles <- length(list.files(destination))
    idvector <- sample(1000000:2000000,1)
    urls <- unlist(lapply("http://www.chessgames.com/perl/nph-chesspgn?text=1&gid=",paste0,idvector))
    connectionfailures <- character(0)
    for (i in 1:length(urls)) {
      urltext <- readUrl(urls[i])
      if (urltext != 0 && urltext != 1) {
        write(urltext,file=paste0(destination,idvector[i],".txt"))
    }
      else if (urltext == 1) {
        connectionfailures <- c(connectionfailures,idvector[i])
      }
    }
    afterfiles <- length(list.files(destination))
    if (afterfiles > startingfiles) {
      print(paste0("Files downloaded: ",afterfiles))
    }
  }
  return(paste0("Finished downloading ",number," chess games."))
}
