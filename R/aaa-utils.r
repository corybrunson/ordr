
as_bibble_recognized <- function(x) {
  class(x) <- c("bbl", class(x))
  x
}
