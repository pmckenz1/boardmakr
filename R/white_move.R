##' Makes a move for white
##' @description Makes a move for white based on PGN format and a starting chess board.
##' @param move the move notation in PGN format
##' @param startingboard the layout of the board before the move
##' @export
white_move <- function(move,startingboard) {
#white to move
  #This removes the "check" plus signs
  if (substr(move,nchar(move),nchar(move)) == "+") {
    move <- substr(move,1,(nchar(move)-1))
  }
  # is it a promotion?
  if (sum(grep("=",move)) > 0) { # if a promotion
    if (nchar(move) == 4) { # just a move, for example c1=Q
      column <- substr(move,1,1)
      row <- 8
      newpiece <- substr(move,nchar(move),nchar(move))
      startingboard[row,column] <- paste0(newpiece,"new_w")
      if (substr(startingboard[(row-1),column],1,1) == "p") {
        startingboard[(row-1),column] <- "none"
      }
      else {
        stop("Can't find the piece being promoted.")
      }
      return(startingboard)
    }
    if (nchar(move) == 6) {        # a capture, for example dxe8=Q
      capcolumn <- substr(move,1,1)
      column <- substr(move,3,3)
      row <- 8
      newpiece <- substr(move,nchar(move),nchar(move))
      startingboard[row,capcolumn] <- paste0(newpiece,"new_w")
      startingboard[(row-1),column] <- "none"
      return(startingboard)
    }
  }

#This changes the format of capturing by major pieces to just look like a move
  if (nchar(move) == 4 | nchar(move) == 5) {
    #Capture
    if ("x" %in% unlist(strsplit(move,NULL))) {
      if (substr(move,1,1) %in% c("R","N","B","K","Q")) { # This removes the "x"
        move <- paste0(unlist(strsplit(move,NULL))[!(unlist(strsplit(move,NULL)) == "x")],collapse = "")
      }
    }
  }
if (nchar(move) == 2) {
  if (startingboard[as.numeric(substr(move,2,2))-1,substr(move,1,1)] != "none" && substr(startingboard[as.numeric(substr(move,2,2))-1,substr(move,1,1)],1,1) == "p") {
    movingpiece <- startingboard[as.numeric(substr(move,2,2))-1,substr(move,1,1)]
    startingboard[grep(movingpiece,startingboard)] <- "none"
    startingboard[substr(move,2,2),substr(move,1,1)] <- movingpiece
  }
  else if (as.numeric(substr(move,2,2))-2 == 2 && substr(startingboard[as.numeric(substr(move,2,2))-2,substr(move,1,1)],1,1) == "p") {
    movingpiece <- startingboard[as.numeric(substr(move,2,2))-2,substr(move,1,1)]
    startingboard[grep(movingpiece,startingboard)] <- "none"
    startingboard[substr(move,2,2),substr(move,1,1)] <- movingpiece
  }
  else {
    stop("Something is up with a pawn move.")
  }
}
if (nchar(move) == 3) {
  #Rook
  if (substr(move,1,1) == "R") {
    colnumber <- match(substr(move,2,2),colnames(startingboard))
    rownumber <- match(substr(move,3,3),rownames(startingboard))
    possible.coords <- list()
    # rows increasing
    orig.coords <- c(rownumber+1,colnumber)
    i <- 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","R1_w","R2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- c((orig.coords[1] + 1), (orig.coords[2]))
      i <- i + 1
    }
    # rows decreasing
    orig.coords <- c(rownumber-1,colnumber)
    i <- length(possible.coords) + 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","R1_w","R2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- c((orig.coords[1] - 1), (orig.coords[2]))
      i <- i + 1
    }
    # columns increasing
    orig.coords <- c(rownumber,colnumber+1)
    i <- length(possible.coords) + 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","R1_w","R2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- c((orig.coords[1]),(orig.coords[2] + 1))
      i <- i + 1
    }
    # columns decreasing
    orig.coords <- c(rownumber,colnumber-1)
    i <- length(possible.coords) + 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","R1_w","R2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- c((orig.coords[1]),(orig.coords[2] - 1))
      i <- i + 1
    }
    #Return moving piece name
    movingpiece <- integer(0)
    for (i in 1:length(possible.coords)) {
      if (startingboard[possible.coords[[i]][1],possible.coords[[i]][2]] == "R1_w") {
        movingpiece <- c(movingpiece,"R1_w")
      }
      if (startingboard[possible.coords[[i]][1],possible.coords[[i]][2]] == "R2_w") {
        movingpiece <- c(movingpiece,"R2_w")
      }
    }
    if (length(movingpiece) == 1) {
      startingboard[grep(movingpiece,startingboard)] <- "none"
      startingboard[substr(move,3,3),substr(move,2,2)] <- movingpiece
    }
    else {
      indices <- integer(0)
      for (i in 1:length(possible.coords)) {
        if (startingboard[possible.coords[[i]][1],possible.coords[[i]][2]] %in% c("R1_w","R2_w")) {
          indices <- c(indices,i)
        }
      }
      two.coords <- possible.coords[indices]
      moving.to.coords <- c(rownumber,colnumber)
      if (two.coords[[1]][1] == two.coords[[2]][1]) {
        if (abs(moving.to.coords[2]-two.coords[[1]][2]) > abs(moving.to.coords[2]-two.coords[[2]][2])) {
          movingpiece <- startingboard[two.coords[[2]][1],two.coords[[2]][2]]
        }
        else {
          movingpiece <- startingboard[two.coords[[1]][1],two.coords[[1]][2]]
        }
      }
      if (two.coords[[1]][2] == two.coords[[2]][2]) {
        if (abs(moving.to.coords[1]-two.coords[[1]][1]) > abs(moving.to.coords[1]-two.coords[[2]][1])) {
          movingpiece <- startingboard[two.coords[[2]][1],two.coords[[2]][2]]
        }
        else {
          movingpiece <- startingboard[two.coords[[1]][1],two.coords[[1]][2]]
        }
      }
      startingboard[grep(movingpiece,startingboard)] <- "none"
      startingboard[substr(move,3,3),substr(move,2,2)] <- movingpiece
    }
  }
  #Bishop
  if (substr(move,1,1) == "B") {
    colnumber <- match(substr(move,2,2),colnames(startingboard))
    rownumber <- match(substr(move,3,3),rownames(startingboard))
    possible.coords <- list()

    # +1 for both
    orig.coords <- c(rownumber+1,colnumber+1)
    i <- 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","B1_w","B2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- orig.coords + 1
      i <- i + 1
    }
    # -1 for both
    orig.coords <- c(rownumber-1,colnumber-1)
    i <- length(possible.coords) + 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","B1_w","B2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- orig.coords - 1
      i <- i + 1
    }
    #+1 -1
    orig.coords <- c(rownumber+1,colnumber-1)
    i <- length(possible.coords) + 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","B1_w","B2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- c((orig.coords[1] + 1),(orig.coords[2] - 1))
      i <- i + 1
    }
    #-1 +1
    orig.coords <- c(rownumber-1,colnumber+1)
    i <- length(possible.coords) + 1
    while (sum(orig.coords <= 8) == 2 && sum(orig.coords >= 1) == 2 && (startingboard[orig.coords[1],orig.coords[2]] %in%
                                                                        c("none","B1_w","B2_w"))) {
      possible.coords[[i]] <- orig.coords
      orig.coords <- c((orig.coords[1] - 1),(orig.coords[2] + 1))
      i <- i + 1
    }
    #name moving piece
    movingpiece <- integer(0)
    for (i in 1:length(possible.coords)) {
      if (startingboard[possible.coords[[i]][1],possible.coords[[i]][2]] == "B1_w") {
        movingpiece <- c(movingpiece,"B1_w")
      }
      if (startingboard[possible.coords[[i]][1],possible.coords[[i]][2]] == "B2_w") {
        movingpiece <- c(movingpiece,"B2_w")
      }
    }
    if (length(movingpiece) == 1) {
      startingboard[grep(movingpiece,startingboard)] <- "none"
      startingboard[substr(move,3,3),substr(move,2,2)] <- movingpiece
    }
    else {
      stop("You have two separate bishops in the same line that can do this.")
    }
  }
  #Knight
  if (substr(move,1,1) == "N") {
    colnumber <- match(substr(move,2,2),colnames(startingboard))
    rownumber <- match(substr(move,3,3),rownames(startingboard))
    possible.coords <- list()
    possible.coords[[1]] <- c(rownumber + 1, colnumber + 2)
    possible.coords[[2]] <- c(rownumber + 1, colnumber - 2)
    possible.coords[[3]] <- c(rownumber - 1, colnumber + 2)
    possible.coords[[4]] <- c(rownumber - 1, colnumber - 2)
    possible.coords[[5]] <- c(rownumber + 2, colnumber + 1)
    possible.coords[[6]] <- c(rownumber + 2, colnumber - 1)
    possible.coords[[7]] <- c(rownumber - 2, colnumber + 1)
    possible.coords[[8]] <- c(rownumber - 2, colnumber - 1)
    #Now remove any that have values outside of our 1:8 bounds
    possible.coords <- possible.coords[as.logical(unlist(lapply(possible.coords,function(x) if (any(x<1 | x > 8)) {0} else {1})))]

    for (i in 1:length(possible.coords)) {
      if (startingboard[possible.coords[[i]][1],possible.coords[[i]][2]] == "N1_w") {
        movingpiece <- "N1_w"
      }
      if (startingboard[possible.coords[[i]][1],possible.coords[[i]][2]] == "N2_w") {
        movingpiece <- "N2_w"
      }
    }
    startingboard[grep(movingpiece,startingboard)] <- "none"
    startingboard[substr(move,3,3),substr(move,2,2)] <- movingpiece
  }
  #Queen
  if (substr(move,1,1) == "Q") {
    movingpiece <- "Q_w"
    startingboard[grep(movingpiece,startingboard)] <- "none"
    startingboard[substr(move,3,3),substr(move,2,2)] <- movingpiece
  }
  #King
  if (substr(move,1,1) == "K") {
    movingpiece <- "K_w"
    startingboard[grep(movingpiece,startingboard)] <- "none"
    startingboard[substr(move,3,3),substr(move,2,2)] <- movingpiece
  }
  #Kingside castle
  if (substr(move,1,3) == "O-O") {
    movingpiece1 <- "K_w"
    movingpiece2 <- "R2_w"
    startingboard[grep(movingpiece1,startingboard)] <- "none"
    startingboard[grep(movingpiece2,startingboard)] <- "none"
    startingboard["1","f"] <- movingpiece2
    startingboard["1","g"] <- movingpiece1
  }
}
  if (nchar(move) == 4) {
    #Capture
    if (substr(move,2,2) == "x") {
      if (startingboard[(substr(move,4,4)),substr(move,3,3)] != "none") {
        movingpiece <- startingboard[(as.numeric(substr(move,4,4))-1),substr(move,1,1)]  #+1 if black (not -1)
        startingboard[grep(movingpiece,startingboard)] <- "none"
        startingboard[substr(move,4,4),substr(move,3,3)] <- movingpiece
      }
      #en passant
      else if ((startingboard[(as.numeric(substr(move,4,4))),substr(move,3,3)] == "none") && as.numeric(substr(move,4,4)) == 6) {
        movingpiece <- startingboard[(as.numeric(substr(move,4,4))-1),substr(move,1,1)]
        startingboard[grep(movingpiece,startingboard)] <- "none"
        startingboard[(as.numeric(substr(move,4,4))-1),substr(move,3,3)] <- "none"
        startingboard[substr(move,4,4),substr(move,3,3)] <- movingpiece
      }
    }
    else if (substr(move,2,2) %in% letters[1:8]) {
      column <- as.vector(startingboard[,substr(move,2,2)])
      movingpiece <- column[grepl(substr(move,1,1),column)+grepl("w",column) == 2]
      startingboard[grep(movingpiece,startingboard)] <- "none"
      startingboard[substr(move,4,4),substr(move,3,3)] <- movingpiece
    }
    else if (substr(move,2,2) %in% 1:8) {
      row <- as.vector(startingboard[substr(move,2,2),])
      movingpiece <- row[grepl(substr(move,1,1),row)+grepl("w",row) == 2]
      startingboard[grep(movingpiece,startingboard)] <- "none"
      startingboard[substr(move,4,4),substr(move,3,3)] <- movingpiece
    }
  }
  if (nchar(move) == 5) {
    if (substr(move,3,3) == "x") {
      if (substr(move,2,2) %in% letters[1:8]) { #the moving piece is in a particular column
        column <- as.vector(startingboard[,substr(move,2,2)])
        movingpiece <- column[grepl(substr(move,1,1),column)+grepl("w",column) == 2]
        startingboard[grep(movingpiece,startingboard)] <- "none"
        startingboard[substr(move,5,5),substr(move,4,4)] <- movingpiece
      }
    }
    if (move == "O-O-O") { #Queenside Castle
      movingpiece1 <- "K_w"
      movingpiece2 <- "R1_w"
      startingboard[grep(movingpiece1,startingboard)] <- "none"
      startingboard[grep(movingpiece2,startingboard)] <- "none"
      startingboard["1","d"] <- movingpiece2
      startingboard["1","c"] <- movingpiece1
    }
  }
  startingboard
}
