#' @name latwosc
#' @title Trudność zadań
#' @description Funkcje do szacowania łatwości/trudności zadań.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param min opcjonalnie wektor liczb całkowitych opisujący minimalną
#' wartość, jaką może przyjąć wynik poszczególnych zadań
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @details
#' \bold{Maksymalne i minimalne wartości}
#'
#' Domyślnie, zgodnie z konwencją typową dla testów wiedzy/umiejętności,
#' jako minimalna możliwa do przyjęcia wartość wyniku zadania przyjmowane jest 0.
#' Maksymalna możliwa do przyjęcia wartość określana jest z kolei empirycznie,
#' jako największa wartość, która występuje w danych.
#'
#' Wykorzystanie argumentu \code{maks} jest konieczne, gdy ze względu na
#' niewielką liczbę obserwacji w zbiorze i/lub dużą trudność zadania dla
#' pewnych zadań w danych nie występuje maksymalna możliwa do osiągnięca liczba
#' punktów. Wektor przekazywany argumentem \code{maks} może zawierać braki
#' danych - dla odpowiednich zadań maksymalna możliwa do uzyskania wartość
#' zostanie określona na podstawie danych (j.w.).
#'
#' Wykorzystanie argumentu \code{min} jest konieczne w przypadku niektórych
#' schematów kodowania odpowiedzi na pytania np. w testach psychologicznych,
#' używających skal Likerta, czy dyferencjałów semantycznych. Często w takich
#' przypadkach odpowiedzi kodowane są kolejnymi liczbami naturalnymi, począwszy
#' od 1. Ogólnie rzecz biorąc, jeśli minimalną wartość, jaką może przyjąć wynik
#' danego zadania jest różna od 0, należy podać ją przy pomocy argumentu
#' \code{min}. Wektor przekazywany argumentem \code{min} może zawierać braki
#' danych - dla odpowiednich zadań jako minimalna możliwa do uzyskania wartość
#' zostanie przyjęte 0.
#'
#' \bold{Obliczania łatwości/trudności dla zadań o minimalnej możliwej wartości różnej od 0}
#'
#' Jeśli minimalna możliwa do uzyskania wartość wyniku zadania jest różna od 0,
#' łatwość obliczona zostanie jako \code{E(X - min) / (max - min)}, gdzie
#' \code{X} oznacza wynik zadania, a \code{min} i \code{max} odpowiednio
#' najmniejszą i największą możliwą do uzyskania wartość wyniku zadania.
#' @seealso \code{\link{parametry_zadan}}, \code{\link{wykres_lmr}}
#' @return
#' Funkcje zwracają milcząco wektor z oszacowaniami łatwości/trudności zadań.
#' @examples
#' trudnosc(wynikiSymTest)
#' latwosc(wynikiSymTest)
#' @export
trudnosc = function(x, maks = NULL, min = NULL, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))
  stopifnot(length(na.rm) == 1, length(verbose) == 1)
  if (is.null(maks) & "maks" %in% names(attributes(x))) {
    maks = attributes(x)$maks
  }
  if (is.null(min) & "min" %in% names(attributes(x))) {
    min = attributes(x)$min
  }
  maks = assert_maks(maks, x)
  min = assert_min(min, x)

  trudnosci = setNames(as.numeric(rep(NA, ncol(x))), colnames(x))
  for (i in 1:ncol(x)) {
    trudnosci[i] =
      1 - (mean(x[, i], na.rm = na.rm) - min[i]) / (maks[i] - min[i])
  }
  if (verbose) {
    cat("Oszacowanie trudności zadań:\n\n",
        info_macierz_danych(x), "\n\n", sep = "")
    print(data.frame(zadanie = colnames(x), "maks.pkt." = maks,
                     "trudność" = format(round(trudnosci, 2), nsmall = 2),
                     "łatwość" = format(round(1 - trudnosci, 2), nsmall = 2),
                     check.names = FALSE), row.names = FALSE)
    cat("\n")
  }

  attributes(trudnosci)$maks = setNames(maks, colnames(x))
  attributes(trudnosci)$min = setNames(min, colnames(x))
  invisible(trudnosci)
}
#' @rdname latwosc
#' @export
latwosc = function(x, maks = NULL, na.rm = TRUE, verbose = TRUE) {
  latwosci =
    1 - trudnosc(x, maks = maks, min = NULL, na.rm = na.rm, verbose = verbose)
  invisible(latwosci)
}
