#'Will my lottery win or not?
#'
#'This function tells you whether you won the lottery or not.
#'
#' @examples
#'
#' my_lottery(c(2,14,28,33,19,43,23))
my_lottery<-function(num_vec){
  win<-sample(1:45,7)
  n <- 0
  bonus <- 0 # Bonus numbers are not included in the number of matches,
  # but are only used to distinguish second and third places.
  for (i  in 1:7){
    if (num_vec[i] %in% win[1:6]) {n<-n+1}

    if (num_vec[i] == win[7]) {bonus<-1}
  }

  if (n == 6) {winner = "Congratulations! 1st"
  } else if (n == 3) {winner = "Congratulations! 5rd"
  } else if (n == 4) {winner = "Congratulations! 4rd"
  } else if (n == 5 & bonus == 0) {winner = "Congratulations! 3rd"
  } else if (n == 5 & bonus == 1){winner = "Congratulations! 2rd"
  } else {winner = "losing ticket"}
  cat(winner,", Winning number of the lottery is",paste(win,seq=" "))
}
