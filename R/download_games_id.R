##' Download chess games by ID
##' @description Allows users to download specific games by ID on chessgames.com
##' @param idvector a vector of game IDs
##' @param destination a location in which to save the games
##' @export
download_games_id <- function(idvector,destination) {
  # Function to
  readUrl <- function(url) {
    # read the url with readLines
    out <- tryCatch(
      {
        suppressWarnings(readLines(con=url, warn=FALSE))
      },
      error=function(cond) {
        # Choose a return value in case of error -- connection issues often throw errors
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
  # in case your destination is missing a forward slash
  if (substr(destination,nchar(destination),nchar(destination)) != "/") {
    stop("Make sure your destination folder name ends with a forward slash (/) !!!")
  }
  # make a vector of urls
  urls <- unlist(lapply("http://www.chessgames.com/perl/nph-chesspgn?text=1&gid=",paste0,idvector))
  # read each url -- save the error return in case any of them fail to read
  connectionfailures <- character(0)
  for (i in 1:length(urls)) {
    urltext <- readUrl(urls[i])
    if (urltext != 0 && urltext != 1) {
      write(urltext,file=paste0(destination,idvector[i],".txt"))
  }
    else if (urltext == 1) {
      connectionfailures <- c(connectionfailures,idvector[i])
    }
    # despite the slowdown, I like to keep track of progress
    # maybe make an argument toggle for this
    print(paste0("Done with ID: ",idvector[i]))
  }
  return(paste0("Finished downloading chess games from ",length(idvector), " game ids."))
}
