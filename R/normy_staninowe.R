#' @title Normy staninowe dla sumy punktow
#' @description Funkcja oblicza normy staninowe dla sumy punktów i przelicza
#' wyniki zdających na skalę staninową.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param min opcjonalnie wektor liczb całkowitych opisujący minimalną
#' wartość, jaką może przyjąć wynik poszczególnych zadań
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @return
#' Funkcja zwraca milcząco dwuelementową listę, której elementy zawierają:
#' \itemize{
#'    \item{\code{normyStaninowe} ramka danych zawierająca 9 obserwacji, dla
#'          opisywane przez kolumny: \code{stanin}, \code{min_pkt},
#'          \code{maks_pkt};}
#'    \item{\code{wynikiStaninowe} wektor liczbowy o długości równej liczbie
#'          wierszy w \code{x}, zawierający wyniki poszczególnych zdających
#'          wyrażone na skali staninowej.}
#' }
#' @examples
#' normy_staninowe(wynikiSymTest)
#' @export
#' @importFrom stats sd pnorm
normy_staninowe = function(x, maks = NULL, min = NULL, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))
  stopifnot(length(na.rm) == 1, length(verbose) == 1)
  if (is.null(maks) & "maks" %in% names(attributes(x))) {
    maks = attributes(x)$maks
  }
  if (is.null(min) & "min" %in% names(attributes(x))) {
    min = attributes(x)$min
  }
  maks = sum(assert_maks(maks, x))
  min = sum(assert_min(min, x))

  suma = rowSums(x, na.rm = na.rm)
  punktyCiecia = mean(suma, na.rm = na.rm) +
    (seq(1.5, 8.5, by = 1) - 5) / 2 * sd(suma, na.rm = na.rm)
  normyStaninowe = data.frame(stanin = 1:9, min_pkt = NA, maks_pkt = NA)
  wynikiStaninowe = as.numeric(rep(NA, nrow(x)))
  for (i in 1:9) {
    normyStaninowe$min_pkt[i] = ceiling(c(min, punktyCiecia)[i])
    normyStaninowe$maks_pkt[i] = floor(c(punktyCiecia, maks)[i])
    wynikiStaninowe[(suma >= normyStaninowe$min_pkt[i]) &
                      (suma <= normyStaninowe$maks_pkt[i]) &
                      !is.na(suma)] = i
    if ((normyStaninowe$min_pkt[i] > normyStaninowe$maks_pkt[i])) {
      normyStaninowe$min_pkt[i] = normyStaninowe$maks_pkt[i] = NA
    }
  }
  normyStaninowe = within(normyStaninowe, {
    min_pkt[!is.na(min_pkt)][1] = min
    maks_pkt[!is.na(maks_pkt)][sum(!is.na(maks_pkt))] = maks
  })

  if (verbose) {
    rozkladStaninow = as.vector(table(factor(wynikiStaninowe, 1:9)))
    rozkladTeoretyczny = c(pnorm(seq(1.5, 8.5, by = 1), 5, 2), 1) -
      c(0, pnorm(seq(1.5, 8.5, by = 1), 5, 2))
    cat("Normy staninowe:\n\n")
    temp = with(normyStaninowe,
                data.frame(stanin = stanin,
                           "l.punktów" = paste0(min_pkt, " - ", maks_pkt),
                           "l.zdających" = rozkladStaninow,
                           "ods.zdających" = paste0(format(round(
                             100 * rozkladStaninow / sum(rozkladStaninow), 1),
                             nsmall = 1), " %"),
                           "ods.teoretyczny" = paste0(format(round(
                             100 * rozkladTeoretyczny, 1),
                             nsmall = 1), " %"),
                           check.names = FALSE, stringsAsFactors = FALSE))
    temp[grep("^NA | NA$", temp[, 2]), 2] = ""
    print(temp, row.names = FALSE)
    cat("\n")
  }

  return(list(normyStaninowe = normyStaninowe,
              wynikiStaninowe = wynikiStaninowe))
}
#' @title Normy staninowe dla sumy punktow
#' @description Funkcja przekodowuje wynik wyrażony w staninach na wartości
#' opisujące przydział do trzech grup: 1-3, 4-6, 7-9 stanin.
#' @param x wektor liczb całkowitych z przedziału [1;9]
#' @return wektor ciągów tekstów
#' @examples
#' x = normy_staninowe(wynikiSymTest)$wynikiStaninowe
#' ftable(x, grupuj_staniny(x))
#' @export
grupuj_staniny = function(x) {
  stopifnot(all(x %in% 1:9))
  return(c("staniny 1-3", "staniny 4-6", "staniny 7-9")[ceiling(x / 3)])
}
