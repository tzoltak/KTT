#' @title Rozklad wyboru dystraktorow
#' @description Funkcja rysuje wykres rozkładu częstości wyboru dystraktorów
#' w danym zadaniu.
#' @param x wektor odpowiedzi na zadanie
#' @param poprawnaOdpowiedz ciąg znaków lub liczba będąca poprawną odpowiedzią
#' @param odpowiedzi opcjonalnie wektor tekstowy lub liczbowy zawierający
#' wszystkie wartości, jakie może przyjąć wynik zadania
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @details
#' Jako dystraktory rozumie się tu tylko niepoprawne odpowiedzi (odpowiedź
#' poprawna nie zostanie uwzględniona na wykresie).
#'
#' Argument \code{odpowiedzi} powinien zostać wykorzystany, jeśli niektóre
#' dystraktory w ogóle nie zostały wybrane - inaczej nie będą mogły być
#' uwzględnione na wykresie.
#' @return Funkcja zwraca milcząco wektor z częstościami wyboru dystraktorów.
#' @examples
#' wykres_rd(daneEWD$Z4, "B")
#' @export
#' @importFrom graphics barplot grid
#' @importFrom stats na.omit
wykres_rd = function(x, poprawnaOdpowiedz, odpowiedzi = NULL, verbose = TRUE) {
  if (!is.vector(poprawnaOdpowiedz)) {
    stop(paste0("Argument 'poprawnaOdpowiedź' musi być ciągiem znaków ",
                "(jednoelementowym wektorem typu 'character') lub liczbą."))
  }
  if (length(poprawnaOdpowiedz) != 1 | any(is.na(poprawnaOdpowiedz))) {
    stop(paste0("Argument 'poprawnaOdpowiedź' musi być ciągiem znaków ",
                "(jednoelementowym wektorem typu 'character') lub liczbą."))
  }
  if (poprawnaOdpowiedz == "") {
    stop("Argument 'poprawnaOdpowiedź' nie może być pustym ciągiem znaków.")
  }
  if (is.null(odpowiedzi)) {
    odpowiedzi = setdiff(sort(na.omit(unique(x))), "")
  } else {
    if (!is.vector(odpowiedzi)) {
      stop(paste0("Argument 'odpowiedzi' musi być wektorem typu 'character' ",
                  "lub typu 'numeric' (lub factorem)."))
    }
  }
  stopifnot(verbose %in% c(FALSE, TRUE))
  stopifnot(length(verbose) == 1)
  odpowiedzi = as.character(odpowiedzi)

  poprawnaOdpowiedz = as.character(poprawnaOdpowiedz)
  if (!all(unique(x) %in% c(odpowiedzi, NA, ""))) {
    stop(paste0("Wśród odpowiedzi na zadanie podanych argumentem 'x' występują ",
                "takie, które nie należą do zestawu możliwych odpowiedzi, ",
                "podanych argumentem 'odpowiedzi'."))
  }
  x = as.character(x)
  x[is.na(x) | x == ""] = "b.o."
  if (any(x == "b.o.")) {
    odpowiedzi = c(odpowiedzi, "b.o.")
  }
  x = factor(x, odpowiedzi)
  n = table(x, exclude = poprawnaOdpowiedz)
  p = n / length(x)
  if (verbose) {
    cat("Rozkład wyboru dystraktorów:\n\n")
    print(data.frame(dystraktor = dimnames(n)[[1]],
                     "częstość" = paste0(format(round(100 * as.vector(p), 2),
                                                nsmall = 2), " %"),
                     "liczebność" = as.vector(n),
                     check.names = FALSE), row.names = FALSE)
    cat("\n")
  }

  oldPar = par(no.readonly = TRUE)
  on.exit({par(oldPar)})
  par(mar = c(5, 5, 1, 1) + 0.1)
  barplot(p, col = "lightblue", ylab = "częstość",
          xlab = "dystraktory\n(b.o. - brak odpowiedzi)")
  grid(nx = NA, ny = NULL, col = grey(0.5))

  invisible(setNames(as.vector(p), dimnames(x)[[1]]))
}
