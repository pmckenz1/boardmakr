##' Gets individual moves in PGN
##' @description Cleans PGN games and produces a list of moves
##' @param text the text from a PGN format game
##' @export
get_moves <- function(text) {
gametext <- text[(which(text == "") + 1):length(text)]
moves <- unlist(strsplit(gametext," "))
result <- moves[length(moves)]
moves <- moves[-length(moves)]
#Are there spaces between numbers and moves?
  if (moves[1] == "1.") {
    moves <- moves[c(FALSE,TRUE,TRUE)]
    white.moves <- moves[c(TRUE,FALSE)]
    black.moves <- moves[c(FALSE,TRUE)]
    game <- list(white.moves = white.moves,black.moves = black.moves)
    game
  }
#If not, we separate them.
  else {
    moves[c(TRUE,FALSE)] <- unlist(strsplit(moves[c(TRUE,FALSE)],"[.]"))[c(FALSE,TRUE)] #This takes away move numbers
    white.moves <- moves[c(TRUE,FALSE)]
    black.moves <- moves[c(FALSE,TRUE)]
    game <- list(white.moves = white.moves,black.moves = black.moves)
    game
    }
}
