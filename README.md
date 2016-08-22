---
title: "Pomiar edukacyjny i psychologiczny w programie R dla początkujących"
author: "Tomasz Żółtak"
date: "22 sierpnia 2016"
output: html_document
---

##  1. Przygotowanie do warsztatów

### 1.1. Instalacja programu R

Instalator R należy pobrać ze strony: <http://cran.at.r-project.org/bin/windows/base/>  
(w przypadku systemów operacyjnych innych niż Windows: <http://cran.at.r-project.org/>)

Uruchamamiamy instalator i przechodzimy przez kolejne etapy klikając "OK" lub "dalej", nie zmieniając domyślnie wybranych ustawień (chyba że ktoś jest pewien, co robi).

### 1.2. Instalacja programu RStudio

RStudio jest środowiskiem programistycznym zapewniającym wsparcie dla tworzenia projektów wykorzystujących język R. Ma znacznie większe możliwości, niż edytor kodu dostarczany z samym R.

Instalator RStudio należy pobrać ze strony: <https://www.rstudio.com/products/rstudio/download2/>

Uruchamamiamy instalator i przechodzimy przez kolejne etapy klikając "OK" lub "dalej", nie zmieniając domyślnie wybranych ustawień (chyba że ktoś jest pewien, co robi).

### 1.3. Instalacja potrzebnych pakietów R

Uruchamiamy program RStudio i w panel po lewej stronie okna programu, zawierający konsolę R wpisujemy (wklejamy) polecenia:

```
install.packages(c("devtools"))
devtools::install_github('tzoltak/KTT')
```

Możemy przy tym otrzymać komnikat, że R utworzy folder z biblioteką pobranych pakietów, w folderze domowym użytkownika (akceptujemy to).
