#' @title Moc różnicująca
#' @description Funkcja służy do oszacowania mocy różnicującej zadania.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @return
#' Funkcja zwraca milcząco dwuelementową listę, której elementy zawierają:
#' \itemize{
#'    \item{\code{korelacjePearsona} wartości współczynników korelacji Pearsona
#'          (punktowo-dwuseryjnych) pomiędzy wynikiem całego testu, a wynikami
#'          poszczególnych zadań,}
#'    \item{\code{korelacjeDwuseryjne} wartości współczynników korelacji
#'          dwuseryjnych pomiędzy wynikiem całego testu, a wynikami
#'          poszczególnych zadań.}
#' }
#' @export
#' @importFrom stats var cor dnorm qnorm setNames
moc_roznicujaca = function(x, na.rm = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE))

  suma = rowSums(x, na.rm = na.rm)
  korP = apply(x, 2, cor, y = suma,
               use = ifelse(na.rm, "pairwise.complete.obs", "everything"))
  maska = apply(x, 2, function(x) {
    return(all(x %in% x(0, 1, NA)) & all(c(0, 1) %in% x))})
  p = ifelse(maska,
             apply(maska, 2, function(x) {return(sum(x == 1) / sum(!is.na(x)))}),
             NA)
  korDS = ifelse(maska,
                 korP * sqrt(p * (1 - p)) / dnorm(qnorm(max(c(p, 1 - p)))),
                 NA)
  if (any(korDS > 1)) {
    warning(paste0("Uzyskano korelacje dwuseryjne większe od jedności.\n",
                   "Analiza wartości korelacji dwuseryjnych zapewne nie ma sensu.\n",
                   "Być może przyczyną jest to, że rozkład sumy punktów ",
                   "znacznie odbiega od rozkładu normalnego."))
    korDS[korDS > 1 & !is.na(korDS)] = 1
  }
  return(list(korelacjePearsona = setNames(korP, names(x)),
              korelacjeDwuseryjne = setNames(korDS, names(x))))
}
