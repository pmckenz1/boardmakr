##' Produces a chess game for analysis
##' @description Accepts moves from the get_moves() function and outputs a chess game object.
##' @param moves the outputs of the get_moves() function
##' @export
get_boards <- function(moves) {
  #Set up initial board
  row8 <- c("R1_b","N1_b","B1_b","Q_b","K_b","B2_b","N2_b","R2_b")
  row7 <- c("p1_b","p2_b","p3_b","p4_b","p5_b","p6_b","p7_b","p8_b")
  row2 <- c("p1_w","p2_w","p3_w","p4_w","p5_w","p6_w","p7_w","p8_w")
  row1 <- c("R1_w","N1_w","B1_w","Q_w","K_w","B2_w","N2_w","R2_w")
  middle <- c(rep("none",8))
  board <- rbind(row1,row2,middle,middle,middle,middle,row7,row8)
  rownames(board) <- 1:8
  colnames(board) <- c("a","b","c","d","e","f","g","h")
  #Step through the PGN move by move using our white_move() and black_move() functions
  boardpositions <- list()
  boardpositions[[1]] <- board
  for (i in 1:min(c(length(moves[[1]]),length(moves[[2]])))) {
    boardpositions[[2*i]] <- white_move(moves[[1]][i],boardpositions[[2*i-1]])
    boardpositions[[2*i+1]] <- black_move(moves[[2]][i],boardpositions[[2*i]])
  }
  if (length(moves[[1]]) != length(moves[[2]])) {
    boardpositions[[2*length(moves[[1]])]] <- white_move(moves[[1]][length(moves[[1]])],boardpositions[[2*length(moves[[1]])-1]])
  }
  #Now name the moves
  names(boardpositions)[1] <- "init"
  for(i in 2:length(boardpositions)) {
    if ((i %% 2) == 0) {
    names(boardpositions)[i] <- paste0("w",(i/2))
    }
    else {
      names(boardpositions)[i] <- paste0("b",(i-1)/2)
    }
  }
  #Make return list of board snapshots and pairs of moves
  if (length(boardpositions) %% 2 == 1) { #number of board frames (including initial) is odd -- that is,equal number of black moves and white moves
    num.white.moves <- (length(boardpositions) - 1)/2
    num.black.moves <- (length(boardpositions) - 1)/2
  }
  if (length(boardpositions) %% 2 == 0) { #number of board frames (including initial) is even -- that is, game ends on a white move
    num.white.moves <- length(boardpositions)/2
    num.black.moves <- length(boardpositions)/2 - 1
  }
  whitemoves <- list()
  for (i in 1:num.white.moves) {
    pairwise <- list(boardpositions[[2*i-1]],boardpositions[[2*i]])
    whitemoves[[i]] <- pairwise
  }
  blackmoves <- list()
  for (i in 1:num.black.moves) {
    pairwise <- list(boardpositions[[2*i]],boardpositions[[2*i+1]])
    blackmoves[[i]] <- pairwise
  }
  chess_game <- list(boardpositions = boardpositions,white.moves = whitemoves,black.moves = blackmoves)
  chess_game
}

