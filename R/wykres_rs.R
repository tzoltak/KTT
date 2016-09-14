#' @title Rozkład sumy punktów
#' @description Funkcja rysuje wykres rozkładu sumy punktów uzyskanych w teście.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param min opcjonalnie wektor liczb całkowitych opisujący minimalną
#' wartość, jaką może przyjąć wynik poszczególnych zadań
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @param verbose wartość logiczna - czy wydrukować wyniki analizy
#' @details
#' Szczegóły związane z użyciem argumentów \code{maks} i \code{min} opisane są
#' w sekcji \code{Details} pomocy do funkcji \code{\link{latwosc}}
#' i \code{\link{trudnosc}}.
#' @seealso \code{\link{normy_staninowe}}, \code{\link{alfaC}}
#' @return Funkcja nic nie zwraca.
#' @examples
#' wykres_rs(wynikiSymTest)
#' @export
#' @importFrom graphics hist arrows grid abline
#' @importFrom grDevices grey
#' @importFrom stats quantile pnorm
wykres_rs = function(x, maks = NULL, min = NULL, na.rm = TRUE, verbose = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE), verbose %in% c(FALSE, TRUE))
  stopifnot(length(na.rm) == 1, length(verbose) == 1)
  if (is.null(maks) & "maks" %in% names(attributes(x))) {
    maks = attributes(x)$maks
  }
  if (is.null(min) & "min" %in% names(attributes(x))) {
    min = attributes(x)$min
  }
  maks = round(sum(assert_maks(maks, x)), 0) + 0.5
  min = round(sum(assert_min(min, x)), 0) - 0.5

  suma = rowSums(x, na.rm = na.rm)
  sr = mean(suma)
  oS = sd(suma)
  normyStaninowe = normy_staninowe(x, verbose = FALSE)
  rozkladStaninow = as.vector(table(factor(normyStaninowe$wynikiStaninowe, 1:9)))
  normyStaninowe = normyStaninowe$normyStaninowe
  h =  h = hist(suma, breaks = seq(min, maks, by = 1), plot = FALSE)
  xTemp = seq(min, maks, by = 0.1)
  obwNorm = nrow(x) * dnorm(xTemp, sr, oS)

  oldPar = par(no.readonly = TRUE)
  on.exit({par(oldPar)})
  par(mar = c(4, 5, 1, 1) + 0.1)
  hist(suma, breaks = seq(min, maks, by = 1),
       xlim = c(min, maks), ylim = c(0, max(c(obwNorm, h$counts))),
       col = "lightblue", border = grey(0.4),
       xaxp = c(min + 0.5, maks - 0.5, maks - min - 1),
       main = "", xlab = "suma punktów", ylab = "częstość")
  grid(nx = NA, ny = NULL, col = grey(0.5))
  lines(xTemp, obwNorm, lwd = 3, lty = 2, col = 1)
  abline(v = sr, lwd = 3, lty = 1, col = 4)
  for (i in c(1:3, 7:9, 4:6)) {
    if (all(!is.na(normyStaninowe[i, 2:3]))) {
      arrows(normyStaninowe$min_pkt[i] - 0.5, 0,
             normyStaninowe$maks_pkt[i] + 0.5, 0,
             angle = 90, code = 3, lwd = 3, length = 1,
             col = 1 + ifelse(i < 4, 1, 0) + ifelse(i > 6, 2, 0))
      text((normyStaninowe$maks_pkt[i] + normyStaninowe$min_pkt[i]) / 2,
           max(h$counts) / 10, paste0(i, ". st."), font = 2)
    }
  }

  if (verbose) {
    kwantyle = quantile(suma, seq(0, 1, by = 0.25), na.rm = na.rm)
    temp = wyrownaj_do_lewej(c(
      format(round(kwantyle, 1), nsmall = 0),
      format(round(sr, 1), nsmall = 1),
      format(round(kwantyle[5] - kwantyle[1], 1), nsmall = 0),
      format(round(c((kwantyle[4] - kwantyle[2]) / 2, oS), 1), nsmall = 1)))
    cat("Parametry rozkładu sumy punktów:\n\n",
        "  minimum    = ", temp[1], "\n",
        "  1. kwartyl = ", temp[2], "\n",
        "  mediana    = ", temp[3], "\n",
        "  3. kwartyl = ", temp[4], "\n",
        "  maksimum   = ", temp[5], "\n",
        "  średnia    = ", temp[6], "\n",
        " -------------------------------\n",
        "  rozstęp           = ", temp[7], "\n",
        "  odch. ćwiartkowe  = ", temp[8], "\n",
        "  odch. standardowe = ", temp[9], "\n\n",
        sep = "")
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

  invisible(NULL)
}
