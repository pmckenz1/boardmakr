##' Get captures within each piece-specific partition
##' @description Partitions the game based on the number of a specific type of piece remaining on the board, and outputs the captures that occurred during that partition.
##' @param game a full game returned by get_boards()
##' @param partitionpiece the piece by which the moves in the game should be partitioned
##' @param capturingpiece optional, a specific piece that is making captures during each partition
##' @export
get_captures_by_partition <- function(game,partitionpiece,capturingpiece = NULL) { #partitionpiece = single letter (like "B"). Refers to the piece for which you want to break captures apart by (if bishop, it'll be partitioned into "two,one, and no bishop captures")

allblackmoves <- game$black.moves
allwhitemoves <- game$white.moves

premove <- unlist(lapply(allblackmoves,head,n=1),recursive = FALSE)
premovebishops <- lapply(premove,grep,pattern=partitionpiece)
num.black <- integer(0)
for (i in 1:length(premove)) {
  survivingpieces <-premove[[i]][premovebishops[[i]]]
  temp.num.black <- length(grep("b",survivingpieces))
  num.black <- c(num.black,temp.num.black)
}
premove <- unlist(lapply(allwhitemoves,head,n=1),recursive = FALSE)
premovebishops <- lapply(premove,grep,pattern=partitionpiece)
num.white <- integer(0)
for (i in 1:length(premove)) {
  survivingpieces <-premove[[i]][premovebishops[[i]]]
  temp.num.white <- length(grep("w",survivingpieces))
  num.white <- c(num.white,temp.num.white)
}
#Now get captures per move
two.black <- allblackmoves[(num.black == 2)]
one.black <- allblackmoves[(num.black == 1)]
no.black <- allblackmoves[(num.black == 0)]
two.white <- allwhitemoves[(num.white == 2)]
one.white <- allwhitemoves[(num.white == 1)]
no.white <- allwhitemoves[(num.white == 0)]
nummoves.two.black <- length(two.black)
nummoves.one.black <- length(one.black)
nummoves.no.black <- length(no.black)
nummoves.two.white <- length(two.white)
nummoves.one.white <- length(one.white)
nummoves.no.white <- length(no.white)

get_involved_spaces <- function(x) { # retrieves the two spaces involved in each move
  if (length(x) > 0) {
    involved_spaces <- integer(0)
    for(i in 1:(length(x))) {
      move <- x[[i]][[1]][!(x[[i]][[1]] == x[[i]][[2]])]
      if (length(move) == 2) {
        involved_spaces <- rbind(involved_spaces,move)
      }
    }
    involved_spaces
  }
  else { NULL }
}

two.black.spaces <- get_involved_spaces(two.black) # the two spaces involved in each black move
one.black.spaces <- get_involved_spaces(one.black)
no.black.spaces <- get_involved_spaces(no.black)
two.white.spaces <- get_involved_spaces(two.white) # the two spaces involved in each white move
one.white.spaces <- get_involved_spaces(one.white)
no.white.spaces <- get_involved_spaces(no.white)

get_captures <- function(x) {    # returns moves that are captures
  if (is.null(x)) {return(NULL)}
  captures <- x[-arrayInd(grep("none", x),dim(x))[,1],]
  if (length(captures)==0) {     # if all moves are captures
    captures <- x
  }
  if (is.null(dim(captures))) {
    dim(captures) <- c(1,2)
    }
  return(captures)
}

two.black.captures <- get_captures(two.black.spaces)
one.black.captures <- get_captures(one.black.spaces)
no.black.captures <- get_captures(no.black.spaces)
two.white.captures <- get_captures(two.white.spaces)
one.white.captures <- get_captures(one.white.spaces)
no.white.captures <- get_captures(no.white.spaces)

captures <- list(twopieces_white=two.white.captures,twopieces_black=two.black.captures,onepieces_white=one.white.captures,onepieces_black=one.black.captures,nopieces_white=no.white.captures,nopieces_black = no.black.captures)
num.moves <- c(nummoves.two.white,nummoves.two.black,nummoves.one.white,nummoves.one.black,nummoves.no.white,nummoves.no.black)


if (!is.null(capturingpiece)) {
  possiblepieces <- c(paste0(capturingpiece,"1_w"),paste0(capturingpiece,"2_w"),paste0(capturingpiece,"1_b"),paste0(capturingpiece,"2_b"))
  if (!is.null(captures$twopieces_white)) {
    piececaptures_white_two <- rbind(captures$twopieces_white[arrayInd(grep(possiblepieces[1], captures$twopieces_white),dim(captures$twopieces_white))[,1],],
                                     captures$twopieces_white[arrayInd(grep(possiblepieces[2], captures$twopieces_white),dim(captures$twopieces_white))[,1],])
  } else {
    piececaptures_white_two <- NULL
    }
  if (!is.null(captures$twopieces_black)) {
    piececaptures_black_two <- rbind(captures$twopieces_black[arrayInd(grep(possiblepieces[3], captures$twopieces_black),dim(captures$twopieces_black))[,1],],
                                     captures$twopieces_black[arrayInd(grep(possiblepieces[4], captures$twopieces_black),dim(captures$twopieces_black))[,1],])
  } else {piececaptures_black_two <- NULL}
  if (!is.null(captures$onepieces_white)) {
    piececaptures_white_one <- rbind(captures$onepieces_white[arrayInd(grep(possiblepieces[1], captures$onepieces_white),dim(captures$onepieces_white))[,1],],
                                     captures$onepieces_white[arrayInd(grep(possiblepieces[2], captures$onepieces_white),dim(captures$onepieces_white))[,1],])
  } else {piececaptures_white_one <- NULL}
  if (!is.null(captures$onepieces_black)) {
    piececaptures_black_one <- rbind(captures$onepieces_black[arrayInd(grep(possiblepieces[3], captures$onepieces_black),dim(captures$onepieces_black))[,1],],
                                     captures$onepieces_black[arrayInd(grep(possiblepieces[4], captures$onepieces_black),dim(captures$onepieces_black))[,1],])
  } else {piececaptures_black_one <- NULL}
  if (!is.null(captures$nopieces_white)) {
    piececaptures_white_none <- rbind(captures$nopieces_white[arrayInd(grep(possiblepieces[1], captures$nopieces_white),dim(captures$nopieces_white))[,1],],
                                      captures$nopieces_white[arrayInd(grep(possiblepieces[2], captures$nopieces_white),dim(captures$nopieces_white))[,1],])
  } else {piececaptures_white_none <- NULL}
  if (!is.null(captures$nopieces_black)) {
    piececaptures_black_none <- rbind(captures$nopieces_black[arrayInd(grep(possiblepieces[3], captures$nopieces_black),dim(captures$nopieces_black))[,1],],
                                      captures$nopieces_black[arrayInd(grep(possiblepieces[4], captures$nopieces_black),dim(captures$nopieces_black))[,1],])
  } else {piececaptures_black_none <- NULL}
  if (!(length(piececaptures_white_two) > 0)) {piececaptures_white_two <- NULL}
  if (!(length(piececaptures_black_two) > 0)) {piececaptures_black_two <- NULL}
  if (!(length(piececaptures_white_one) > 0)) {piececaptures_white_one <- NULL}
  if (!(length(piececaptures_black_one) > 0)) {piececaptures_black_one <- NULL}
  if (!(length(piececaptures_white_none) > 0)) {piececaptures_white_none <- NULL}
  if (!(length(piececaptures_black_none) > 0)) {piececaptures_black_none <- NULL}
  captures <- list(twopieces_white=piececaptures_white_two,twopieces_black=piececaptures_black_two,onepieces_white=piececaptures_white_one,onepieces_black=piececaptures_black_one,nopieces_white=piececaptures_white_none,nopieces_black = piececaptures_black_none)
}
list(captures = captures, number_moves_partition = num.moves)
}



