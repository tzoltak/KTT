#' @title Formatowanie liczb
#' @description Funkcja wyrównuje podane ciągi znaków na miejscu dziesiętnym.
#' @param x wektor tekstowy
#' @return wektor tekstowy
wyrownaj_do_lewej = function(x) {
  stopifnot(is.vector(x))
  stopifnot(is.character(x))

  szerPMD = sapply(sub("[.,].*$", "", x), nchar)
  szerPMD = max(szerPMD) - szerPMD
  for (i in 1:length(x)) {
    x[i] = paste0(rep(" ", szerPMD[i]), x[i], collapse = "")
  }
  return(x)
}
