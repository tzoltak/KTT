#' @title Rozkład wyników zadania
#' @description Funkcja rysuje wykres rozkładu sumy punktów uzyskanych w teście.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param wartosci opcjonalnie wektor liczb zawierający wszystkie wartości,
#' jakie może przyjąć wynik zadania
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @return Funkcja zwraca ramkę danych z rozkładem punktów uzyskanych za zadanie.
#' @examples
#' wykres_rwz(apply(wynikiSymTest[, 1:4], 1, sum))
#' @export
#' @importFrom graphics barplot grid abline
#' @importFrom stats na.omit
wykres_rwz = function(x, wartosci = NULL, verbose = TRUE) {
  assert_w(x)
  if (!is.null(wartosci)) {
    assert_w(wartosci)
    if (!all(na.omit(x) %in% wartosci)) {
      stop(paste0("W wektorze podanym argumentem 'x' pojawiają się  wartości, ",
                  "które nie występują w wektorze podanym argumentem 'wartosci'."))
    }
    x = factor(x, wartosci)
  }

  x = table(x, useNA = "no")
  y = prop.table(x)
  #dimnames(y)$x[is.na(dimnames(y)$x)] = "b.d."
  temp = data.frame(
    "l.pkt." = as.vector(dimnames(y)$x), "l.zdających" = as.vector(x),
    "ods.zdających" = paste0(format(round(100 * y, 1), nsmall = 1), " %"),
    check.names = FALSE)

  if (verbose) {
    cat("Rozkład wyników zadania:\n\n")
    print(temp)
  }
  oldPar = par(no.readonly = TRUE)
  on.exit({par(oldPar)})
  par(mar = c(4, 5, 1, 1) + 0.1)
  barplot(y, xlab = "liczba punktów", ylab = "częstość", col = "lightblue")
  grid(nx = NA, ny = NULL, col = grey(0.5))
  abline(h = 0)

  invisible(temp)
}
