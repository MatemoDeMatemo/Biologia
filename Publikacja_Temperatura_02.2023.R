# Konsolidacja danych meteo
x <-  rep('k_m_d_2001.csv', 27)
writeClipboard(x)

setwd("D:/Git/Dane/Temperatura")
dane <- c("k_m_d_1996_2000.csv", "k_m_d_2001.csv", "k_m_d_2002.csv", "k_m_d_2003.csv", "k_m_d_2004.csv", 
          "k_m_d_2005.csv", "k_m_d_2006.csv", "k_m_d_2007.csv", "k_m_d_2008.csv", "k_m_d_2009.csv", 
          "k_m_d_2010.csv", "k_m_d_2011.csv", "k_m_d_2012.csv", "k_m_d_2013.csv", "k_m_d_2014.csv", 
          "k_m_d_2015.csv", "k_m_d_2016.csv", "k_m_d_2017.csv", "k_m_d_2018.csv", "k_m_d_2019.csv", 
          "k_m_d_2020.csv", "k_m_d_2021.csv", "k_m_d_2022.csv")
test <- read.csv("k_m_d_1996_2000.csv", header=FALSE, stringsAsFactors = TRUE, sep = ",", dec = ".")
nrow(test)
# Pobieranie i sklejanie
for (i in 1:length(dane)) {
  if (i == 1){
    df = read.csv(dane[i], header=FALSE, stringsAsFactors = TRUE, sep = ",", dec = ".")
    print(i)
    print(paste0(dane[i], "Liczba wierszy pliku: ", nrow(df)))
    print(paste0("Aktualna liczba wierszy: ", nrow(df)))
  } else {
  a <- read.csv(dane[i], header=FALSE, stringsAsFactors = TRUE, sep = ",", dec = ".")
  df <- rbind(df, a)
  print(i)
  print(paste0(dane[i], "Liczba wierszy pliku: ", nrow(a)))
  print(paste0("Aktualna liczba wierszy: ", nrow(df)))
    }
}

# Nazwy kolumn
colnames(df) <-  c("Kod_stacji", "Nazwa_stacji", "Rok", "Miesiac", "Absolutna_temperatura_maksymalna_[°C]", "Status_pomiaru_TMAX", 
  "srednia_temperatura_maksymalna_[°C]", "Status_pomiaru_TMXS", "Absolutna_temperatura_minimalna_[°C]", 
  "Status_pomiaru_TMIN", "srednia_temperatura_minimalna_[°C]", "Status_pomiaru_TMNS", 
  "srednia_temperatura_miesieczna_[°C]", "Status_pomiaru_STM", "Minimalna_temperatura_przy_gruncie_[°C]", 
  "Status_pomiaru_TMNG", "Miesieczna_suma_opadow_[mm]", "Status_pomiaru_SUMM", "Maksymalna_dobowa_suma_opadow_[mm]", 
  "Status_pomiaru_OPMX", "Pierwszy_dzien_wystapienia_opadu_maksymalnego", "Ostatni_dzien_wystapienia_opadu maksymalnego", 
  "Maksymalna_wysokosc_pokrywy_snieznej_[cm]", "Status_pomiaru_PKSN", "Liczba_dni_z_pokrywa_sniezna", 
  "Liczba_dni_z_opadem_deszczu", "Liczba_dni_z_opadem_sniegu")

write.csv2(df ,"Meteo_1995_2022.csv", row.names = FALSE)
