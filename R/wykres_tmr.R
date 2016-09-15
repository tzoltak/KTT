#' @title Parametry zadan
#' @description Funkcja rysuje wykres trudności i mocy różnicującej zadań.
#' @param x macierz typu \code{numeric} lub ramka danych (data frame)
#' zawierająca zmienne typu \code{numeric}
#' @param wsk wskaźnik mocy różnicującej, który ma być użyty: 'Pearson',
#' 'dwuseryjna' lub 'bez zadania'
#' @param maks opcjonalnie wektor liczb całkowitych opisujący maksymalną
#' liczbę puntków możliwych do uzyskania za poszczególne zadania
#' @param min opcjonalnie wektor liczb całkowitych opisujący minimalną
#' wartość, jaką może przyjąć wynik poszczególnych zadań
#' @details
#' \bold{Interpretacja wykresu}
#' Ogólnie rzecz biorąc pożądane jest równomierne rozłożenie zadań ze względu
#' na trudność oraz niewystępowanie zadań o niskiej mocy różnicującej.
#' \bold{Wybór wskaźnika mocy różnicującej}
#' \itemize{
#'   \item{\code{Pearson} - korelacja liniowa Pearsona (punktowo-dwuseryjna);
#'         dla zadań z wąskimi skalami oceny (w szczególności ocenianych
#'         binarnie: 0 lub 1) w praktyce niemożliwe jest osiągnięcie korelacji
#'         bliskich jedności; maksymalna możliwa do osiągnięcia korelacja jest
#'         tym mniejsza, im bardziej trudność/łatwość zadania odbiega od 0.5.}
#'   \item{\code{dwuseryjna} - korelacja dwuseryjna; adekwatna tylko do zadań
#'         ocenianych binarnie (0 lub 1), jest zawsze większa od korelacji
#'         Pearsona, ale jeśli rozkład sumy wyraźnie różni się od rozkładu
#'         normalnego może przeszacowywać natężenie związku (w tym przyjmować
#'         bezsensowne wartości większe niż 1);}
#'   \item{\code{bez zadania} - korelacja liniowa Pearsona (j.w.), ale z sumą
#'         punktów pomniejszoną o liczbę punktów uzyskanych za dane zadanie;
#'         bardziej adekwatna od zwykłej korelacji Pearsona dla krótkich testów
#'         lub w sytuacji, gdy dane zadanie ma dużo szerszą skalę oceny niż
#'         pozostałe;}
#' }
#' \bold{Kolory punktów}
#' Punkty reprezentujące zadania kolorowane są w zależności od wartości
#' wskaźnika rzetelności bez zadania (używany jest współczynnik alfa Feldt-Raju,
#' p. \code{\link{alfa_fr}}. Zielone reprezentują zadania, po usunięciu których
#' reztelność testu spada, a czerwone te, po usunięciu których rzetelność testu
#' rośnie (ta druga sytuacja jest niepożądana).
#' \bold{Obwiednia maksymalnych wartości korelacji}
#' Jeśli argument \code{wsp = 'Pearson'}, na wykresie niebieskim kolorem
#' naniesiona zostanie obwiednia wyznaczające maksymalne możliwe do uzyskania
#' wartości korelacji \bold{pomiędzy zadaniem punktowanym binarnie, a sumą
#' punktów o rozkładzie zbliżonym do normalnego}. Jeśli pewne zadania mają
#' szersze skale oceny i/lub rozkład sumy wyraźnie różni się od rozkładu
#' normalnego, możliwe jest uzyskanie wyższych wartości korelacji.
#' @seealso \code{\link{parametry_zadan}}
#' @return
#' Funkcja nic nie zwraca.
#' @examples
#' wykres_tmr(wynikiSymTest)
#' @export
#' @importFrom graphics abline lines par plot text
#' @importFrom grDevices grey
wykres_tmr = function(x, wsk = "Pearson", maks = NULL, min = NULL) {
  wsk = tolower(wsk)
  if (!(wsk %in% c("pearson", "dwuseryjna", "bez zadania", "p", "d", "b"))) {
    stop(paste0("Parametr 'wsk' musi przyjmować jedną z wartości: 'pearson', ",
                "'dwuseryjna', 'bez zadania'."))
  }
  wsk = substr(wsk, 1, 1)
  if (is.null(maks) & "maks" %in% names(attributes(x))) {
    maks = attributes(x)$maks
  }
  if (is.null(min) & "min" %in% names(attributes(x))) {
    min = attributes(x)$min
  }
  maks = assert_maks(maks, x)
  min = assert_min(min, x)

  trudnosci = trudnosc(x, na.rm = TRUE, verbose = FALSE)
  mocRoznicujaca = moc_roznicujaca(x, na.rm = TRUE, verbose = FALSE)
  alfyFR = alfa_fr(x, na.rm = TRUE, verbose = FALSE)
  alfyFR = c(3, 2)[1 + as.numeric(alfyFR$alfaBZ > alfyFR$alfa)]

  if (wsk == "p") {
    mocRoznicujaca = mocRoznicujaca$korelacjePearsona
    wskEtykieta = "(korelacja Pearsona)"
  } else if (wsk == "d") {
    mocRoznicujaca = mocRoznicujaca$korelacjeDwuseryjne
    wskEtykieta = "(korelacja dwuseryjna)"
    maska = apply(x, 2, max, na.rm = TRUE) > 1
    if (any(maska)) {
      warning(paste0("Zadania: '",
                     paste0(colnames(x)[maska], collapse = "', '"),
                     "' nie zostały pokazane na wykresie, gdyż nie są oceniane ",
                     "binarnie (0 lub 1 pkt), a jako wskaźnik mocy różnicuącej ",
                     "wybrano korelację dwuseryjną (która ma sens tylko dla ",
                     "zadań ocenianych binarnie)."))
    }
  } else {
    mocRoznicujaca = mocRoznicujaca$korelacjeBezZadania
    wskEtykieta = "(korelacja Pearsona bez danego zadania)"
  }
  maska = is.na(trudnosci) | is.na(mocRoznicujaca)
  if (any(maska)) {
    warning(paste0("Zadania: '",
                   paste0(colnames(x)[maska], collapse = "', '"),
                   "' nie zostały pokazane na wykresie, gdyż nie dało się ",
                   "dla nich obliczyć trudności i/lub mocy różnicującej."))
  }

  oldPar = par(no.readonly = TRUE)
  on.exit({par(oldPar)})
  par(mar = c(4, 5, 1, 1) + 0.1)
  xlim = c(0, 1)
  ylim = range(c(0, 1, range(mocRoznicujaca)))
  ylim = c(floor(10 * ylim[1]) / 10, ceiling(10 * ylim[2]) / 10)
  plot(trudnosci, mocRoznicujaca, type = "p", pch = 21, bg = alfyFR,
       xlim = xlim, ylim = ylim,
       xlab = "trudność", ylab = paste0("moc różnicująca\n", wskEtykieta),
       xaxp = c(0, 1, 10),
       yaxp = c(ylim[1], ylim[2], (ylim[2] - ylim[1]) / 0.1))
  for (i in seq(0, 1, by = 0.1)) {
    abline(v = i, lty = 3, col = grey(0.5))
  }
  for (i in seq(0, 1, by = 0.2)) {
    abline(v = i, lty = 2, col = grey(0.5))
  }
  for (i in seq(ylim[1], ylim[2], by = 0.1)) {
    abline(h = i, lty = 3, col = grey(0.5))
  }
  for (i in seq(ylim[1], ylim[2], by = 0.2)) {
    abline(h = i, lty = 2, col = grey(0.5))
  }
  abline(h = 0, lty = 1, col = 2)
  if (wsk == "p") {
    temp = seq(0.05, 0.95, 0.01)
    lines(temp, dnorm(qnorm(ifelse(temp > 0.5, temp, 1 - temp))) /
            sqrt(temp * (1 - temp)),
          lty = 2, col = 4, lwd = 2)
  }
  text(trudnosci, mocRoznicujaca, colnames(x), pos = 1)

  invisible(NULL)
}
