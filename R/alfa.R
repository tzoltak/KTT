#' @title Szacowanie rzetelności
#' @description Funkcja służy do oszacowania rzeteleności testu przy pomocy
#' współczynnika alfa Cronbacha. Zwraca oszacowanie dla całego testu oraz
#' z wyłączeniem poszczególnych zadań.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @return
#' Funkcja zwraca milcząco trzyelementową listę, której elementy zawierają:
#' \itemize{
#'    \item{\code{alfa} wartość współczynnika alfa Cronbacha dla całego testu,}
#'    \item{\code{alfaBZ} wektor liczbowy zawierający współczynniki alfa
#'          Cronbacha obliczone dla testu z wyłączeniem danego zadania,}
#'    \item{\code{wsp = "alfa Cronbacha".}}
#' }
#' @export
#' @importFrom stats var setNames
alfaC = function(x, na.rm = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE))

  suma = rowSums(x, na.rm = na.rm)
  war = apply(x, 2, var, na.rm = na.rm)
  alfa = (ncol(x) / (ncol(x) - 1)) *
    (1 - sum(war, na.rm = na.rm) / var(suma, na.rm = na.rm))
  cat("Oszacowanie rzetelności współczynnikiem alfa Cronbacha:\n\n",
      "test złożony z ", ncol(x), " zadań\n\n",
      "alfa Cronbacha = ", round(alfa, 3), "\n\n", sep = "")

  if (ncol(x) > 2) {
    alfaBZ = setNames(vector("numeric", ncol(x)), colnames(x))
    for (i in 1:ncol(x)) {
      alfaBZ[i] = ((ncol(x) - 1) / (ncol(x) - 2)) *
        (1 - sum(war[, -i], na.rm = na.rm) / var(suma - x[, i], na.rm = na.rm))
    }
    cat("Oszacowania rzetelności bez poszczególnych zadań:\n",
        paste0("  ", names(x), " ", round(alfaBZ, 3),
               ifelse(alfaBZ > alfa, " (!)", ""), "\n", collapse = ""),
        "Zadania, po usunięciu których rzetelność rośnie oznaczono '(!)'.\n\n",
        sep = "")
  } else {
    alfaBZ = NULL
    cat("Oszacowania rzetelności bez poszczególnych zadań niemożliwe -",
        "test zawiera tylko dwa zadania.\n\n")
  }


  wynik = list(alfa = alfa, alfaBZ = alfaBZ, wsp = "alfa Cronbacha")
  class(wynik) = c(class(wynik), "oszacowanieRzetelnosci")
  invisible(wynik)
}
#' @title Szacowanie rzetelności
#' @description Funkcja służy do oszacowania rzeteleności testu przy pomocy
#' współczynnika alfa Feldt-Raju. Zwraca oszacowanie dla całego testu oraz
#' z wyłączeniem poszczególnych zadań.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param na.rm wartość logiczna - czy przy obliczeniach ignorować braki danych
#' @return
#' Funkcja zwraca milcząco trzyelementową listę, której elementy zawierają:
#' \itemize{
#'    \item{\code{alfa} wartość współczynnika alfa Cronbacha dla całego testu,}
#'    \item{\code{alfaBZ} wektor liczbowy zawierający współczynniki alfa
#'          Cronbacha obliczone dla testu z wyłączeniem danego zadania,}
#'    \item{\code{wsp = "alfa Feldt-Raju".}}
#' }
#' @export
#' @importFrom stats var cov setNames
alfaFR = function(x, na.rm = TRUE) {
  assert_mdfn(x)
  stopifnot(na.rm %in% c(FALSE, TRUE))

  suma = rowSums(x, na.rm = na.rm)
  warSuma = var(suma, na.rm = na.rm)
  war = apply(x, 2, var, na.rm = na.rm)
  lambda = apply(x, 2, cov, y = suma,
                 use = ifelse(na.rm, "complete.obs", "everything"))
  lambda = lambda / warSuma
  alfa = (1 / (1 - sum(lambda^2, na.rm = na.rm))) * (1 - sum(war) / warSuma)
  cat("Oszacowanie rzetelności współczynnikiem alfa Feldt-Raju:\n\n",
      "test złożony z ", ncol(x), " zadań\n\n",
      "alfa Cronbacha = ", round(alfa, 3), "\n\n", sep = "")

  if (ncol(x) > 2) {
    alfaBZ = setNames(vector("numeric", ncol(x)), colnames(x))
    for (i in 1:ncol(x)) {
      warSuma = var(suma - x[, i], na.rm = na.rm)
      lambda = apply(x[, -i], 2, cov, y = suma,
                         use = ifelse(na.rm, "complete.obs", "everything"))
      lambda = lambda / warSuma
      alfaBZ[i] = (1 / (1 - sum(lambda^2, na.rm = na.rm))) *
        (1 - sum(war[, -i]) / warSuma)
    }
    cat("Oszacowania rzetelności bez poszczególnych zadań:\n",
        paste0("  ", names(x), " ", round(alfaBZ, 3),
               ifelse(alfaBZ > alfa, " (!)", ""), "\n", collapse = ""),
        "Zadania, po usunięciu których rzetelność rośnie oznaczono '(!)'.\n\n",
        sep = "")
  } else {
    alfaBZ = NULL
    cat("Oszacowania rzetelności bez poszczególnych zadań niemożliwe -",
        "test zawiera tylko dwa zadania.\n\n")
  }


  wynik = list(alfa = alfa, alfaBZ = alfaBZ, wsp = "alfa Feldt-Raju")
  class(wynik) = c(class(wynik), "oszacowanieRzetelnosci")
  invisible(wynik)
}
