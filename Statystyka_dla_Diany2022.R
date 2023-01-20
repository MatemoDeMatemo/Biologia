### Pobranie Danych

df_5 <- read.csv("5_Diana.csv", stringsAsFactors = TRUE, sep = ";", dec = ",")
df_10 <- read.csv("10_Diana.csv", stringsAsFactors = TRUE, sep = ";", dec = ",")
df_20 <- read.csv("20_Diana.csv", stringsAsFactors = TRUE, sep = ";", dec = ",")
df_Dry <- read.csv("Dry_Mass_Diana.csv", stringsAsFactors = TRUE, sep = ";", dec = ",")

df_5
df_10
df_20
df_Dry

str(df_5)

# Ustawienia Poczatkowe

options(scipen = 999) # wyrzuca adnotacje matematyczn¹ z ca³osci, "0" odwraca trend
format(cos["Pr(>F)1"], scientific = FALSE)

# Tworzê tabelke ze statystykami finalnymi


Statystyka_Tabela <- data.frame(NazwaTestu = character(),
                  First_F_Value =double(),
                  First_P_Value =double(),
                  Second_F_Value =double(),
                  Second_P_Value =double())

#Statystyka_Tabela [nrow(Statystyka_Tabela ) +1, ] <- c("cos", 1, 2, 3, 4)  # Tak dodajemy nowe wiersze

############################################ 1) Anova D³ugoœci ca³kowitych oraz g³ównych pêdów. ##############################################################

#### Anova Jenoczynnikowa ####

# £acze dane w ramke
Main_Length <-  c(df_5$After_Length_Main, df_10$After_Length_Main, df_20$After_Length_Main)
Total_Length <- c(df_5$After_Length_Main + df_5$After_Total_Length_Offshoots, 
                  df_10$After_Length_Main + df_10$After_Total_Length_Offshoots, 
                  df_20$After_Length_Main + df_20$After_Total_Length_Offshoots)
Typ_Jeziora <- c(df_5$Type_of_Lake, df_10$Type_of_Lake, df_20$Type_of_Lake)
Temperatura = c(rep("5.C",32), rep("10.C",32), rep("20.C",32))


## Ramka danych z d³ugoœciami

Anv_Lenght <- data.frame(Temperatura, Typ_Jeziora, Main_Length, Total_Length)
Anv_Lenght

## PLOT 
library(ggpubr)
library(ggplot2)

level_order <- c('5.C', '10.C', '20.C')
#factor(Temperatura, level = level_order)

ggplot(Anv_Lenght, aes(x=factor(Temperatura, level = level_order), y=Main_Length)) + 
  stat_boxplot(geom='errorbar') +
  geom_boxplot(fill = "lightgrey") +
  ylim(10,50) + 
  labs(y = "D³ugoœæ pêdu g³ównego [cm] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na d³ugoœæ pêdu g³ównego" , italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20))

ggplot(Anv_Lenght, aes(x=factor(Temperatura, level = level_order), y=Total_Length)) + 
  stat_boxplot(geom='errorbar') +
  geom_boxplot(fill = "lightgrey") +
  ylim(10,50) + 
  labs(y = "D³ugoœæ ca³kowita pêdu g³ównego oraz pêdów bocznych [cm] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na d³ugoœæ ca³kowit¹" , italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20))

ggplot(Anv_Lenght, aes(x=factor(Temperatura, level = level_order), y=Total_Length, fill=as.character(Typ_Jeziora))) +
  stat_boxplot(geom='errorbar') +
  geom_boxplot() +
  ylim(10,50) + 
  labs(y = "D³ugoœæ ca³kowita pêdu g³ównego oraz pêdów bocznych [cm] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na wzrost", italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20), legend.text = element_text(size=18), legend.title = element_text(size=20)) +
  scale_fill_discrete(name = "Jezioro pochodzenia", labels = c("Ptasie Wyspy", "Nowy Duninów"))


### Anova jednoczynnikowa: Temperatura

### D³ugoœæ pêdów g³ównych
Jednoczynnikowa_Anova <- aov(Main_Length ~ Temperatura, data = Anv_Lenght) # pêdy siê ró¿ni¹ wzglêdem temperatury
summary(Jednoczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Jednoczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Jednoczynnikowa_Anova_Dla_Dlugosci_Pedu_Glownego",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result


## Test Post_hoc

TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95)  # Wszystkie grupy sie roznia
Tuckey_a <- (TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95))
Tuckey_a <-  as.data.frame(Tuckey_a[1:1])

### D³ugoœæ pêdów g³ównych i bocznych razem
Jednoczynnikowa_Anova <- aov(Total_Length ~ Temperatura, data = Anv_Lenght) # pêdy siê ró¿ni¹ wzglêdem temperatury
summary(Jednoczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Jednoczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Jednoczynnikowa_Anova_Dla_Calkowitej_Dlugosci",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result


## Test Post_hoc

TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95)  # Wszystkie grupy sie roznia
Tuckey_b <- TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95) 
Tuckey_b <-  as.data.frame(Tuckey_b[1:1])
#### Anova Dwuczynnikowa ############################################################

## D³ugoœci pêdów g³ównych
Dwuczynnikowa_Anova <-  aov(Main_Length ~ Temperatura + Typ_Jeziora, data = Anv_Lenght)   # Na ró¿nice wp³ywa temperatura a nie pochodzenie roœlin
summary(Dwuczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Dwuczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Dwuczynnikowa_Anova_Dla_Dlugosci_Pedu_Glownego",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result

## D³ugoœci ca³kowite
Dwuczynnikowa_Anova <-  aov(Total_Length ~ Temperatura + Typ_Jeziora, data = Anv_Lenght)  # Na ró¿nice wp³ywa temperatura a nie pochodzenie roœlin
summary(Dwuczynnikowa_Anova )

# Wrzucam wyniki do Tabeli
result <- summary(Dwuczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Dwuczynnikowa_Anova_Dla_Calkowitej_Dlugosci",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result


############################################ 2) Anova Masy. ##############################################################################################

#### Anova Jenoczynnikowa ####

# £acze dane w ramke
After_DryMass <-  c(df_5$After_Dry_Mass, df_10$After_Dry_Mass, df_20$After_Dry_Mass)
Typ_Jeziora <- c(df_5$Type_of_Lake, df_10$Type_of_Lake, df_20$Type_of_Lake)
Temperatura = c(rep("5.C",32), rep("10.C",32), rep("20.C",32))

# Ramka danych z d³ugoœciami

Anv_DryMass <- data.frame(Temperatura, Typ_Jeziora, After_DryMass)
Anv_DryMass


## PLOT 
library(ggpubr)
library(ggplot2)

level_order <- c('5.C', '10.C', '20.C')


ggplot(Anv_DryMass, aes(x=factor(Temperatura, level = level_order), y=After_DryMass)) +
  stat_boxplot(geom='errorbar') +
  geom_boxplot(fill = "lightgrey") +
  ylim(0,0.20) + 
  labs(y = "Masa osobników [g] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na masê" , italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20))

ggplot(Anv_DryMass, aes(x=factor(Temperatura, level = level_order), y=After_DryMass, fill=as.character(Typ_Jeziora))) +
  stat_boxplot(geom='errorbar') +
  geom_boxplot() +
  ylim(0,0.20) + 
  labs(y = "Masa osobników [g] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na masê", italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20), legend.text = element_text(size=18), legend.title = element_text(size=20)) +
  scale_fill_discrete(name = "Jezioro pochodzenia", labels = c("Ptasie Wyspy", "Nowy Duninów"))

### Anova jednoczynnikowa: Temperatura

## D³ugoœæ pêdów g³ównych
Jednoczynnikowa_Anova <- aov(After_DryMass ~ Temperatura, data = Anv_DryMass) # pêdy siê ró¿ni¹ wzglêdem temperatury
summary(Jednoczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Jednoczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Jednoczynnikowa_Anova_Masy",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result

## Test Post_hoc

TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95)  # Wszystkie grupy sie poza jedna: 5 i 10 takie same
Tuckey_c <- TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95) 
Tuckey_c <-  as.data.frame(Tuckey_c[1:1])

#### Anova Dwuczynnikowa ####

## Sucha masa
Dwuczynnikowa_Anova <-  aov(After_DryMass ~ Temperatura + Typ_Jeziora, data = Anv_DryMass)   # Na ró¿nice wp³ywa temperatura oraz typ jeziora [!]
summary(Dwuczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Dwuczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Dwuczynnikowa_Anova_Masy",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result


######################################### 3) Anova ilosci pedow bocznych #######################################################################

#### Anova Jenoczynnikowa ####

# £acze dane w ramke
After_NumberOfShoots <-  c(df_5$After_number_Offshoots, df_10$After_number_Offshoots, df_20$After_number_Offshoots)
Typ_Jeziora <- c(df_5$Type_of_Lake, df_10$Type_of_Lake, df_20$Type_of_Lake)
Temperatura = c(rep("5.C",32), rep("10.C",32), rep("20.C",32))

# Ramka danych z d³ugoœciami

Anv_NumberOfShoots <- data.frame(Temperatura, Typ_Jeziora, After_NumberOfShoots)
Anv_NumberOfShoots

## PLOT
library(ggplot2)
level_order <- c('5.C', '10.C', '20.C')


ggplot(Anv_NumberOfShoots, aes(x=factor(Temperatura, level = level_order), y=After_NumberOfShoots))  + 
  stat_boxplot(geom='errorbar') +
  geom_boxplot(fill = "lightgrey") +
  ylim(0,10) + 
  labs(y = "Iloœæ pêdów bocznych  [szt.] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na iloœæ pêdów bocznych" , italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20))

ggplot(Anv_NumberOfShoots, aes(x=factor(Temperatura, level = level_order), y=After_NumberOfShoots, fill=as.character(Typ_Jeziora)))  + 
  stat_boxplot(geom='errorbar') +
  geom_boxplot() +
  ylim(0,10) + 
  labs(y = "Iloœæ pêdów bocznych [szt.] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na iloœæ pêdów bocznych", italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20), legend.text = element_text(size=18), legend.title = element_text(size=20)) +
  scale_fill_discrete(name = "Jezioro pochodzenia", labels = c("Ptasie Wyspy", "Nowy Duninów"))

### Anova jednoczynnikowa: Temperatura

## Ilosc pedow bocznych
Jednoczynnikowa_Anova <- aov(After_NumberOfShoots ~ Temperatura, data = Anv_NumberOfShoots ) # il. pêdow ró¿ni siê wzglêdem temperatury
summary(Jednoczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Jednoczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Jednoczynnikowa_Anova_Ilosci_Pedow_Bocznych",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result

## Test Post_hoc

TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95)  # Wszystkie grupy sie poza jedna
Tuckey_d <- TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95) 
Tuckey_d <-  as.data.frame(Tuckey_d[1:1]) 

#### Anova Dwuczynnikowa ####

## Ilosc pedow bocznych
Dwuczynnikowa_Anova <-  aov(After_NumberOfShoots ~ Temperatura + Typ_Jeziora, data = Anv_NumberOfShoots)   # Na ró¿nice wp³ywa temperatura nie typ jeziora
summary(Dwuczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Dwuczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Dwuczynnikowa_Anova_Ilosci_Pedow_Bocznych",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result




######################################### 4) Anova ilosci miedzywezli pedu glownego    #######################################################################

#### Anova Jenoczynnikowa ####

# £acze dane w ramke
After_NumberOfInternodes <-  c(df_5$After_number_Internodes_main, df_10$After_number_Internodes_main, df_20$After_number_Internodes_main)
Typ_Jeziora <- c(df_5$Type_of_Lake, df_10$Type_of_Lake, df_20$Type_of_Lake)
Temperatura = c(rep("5.C",32), rep("10.C",32), rep("20.C",32))

# Ramka danych z d³ugoœciami

Anv_NumberOfInternodes <- data.frame(Temperatura, Typ_Jeziora, After_NumberOfInternodes)
Anv_NumberOfInternodes

## PLOT
library(ggplot2)
level_order <- c('5.C', '10.C', '20.C')

ggplot(Anv_NumberOfInternodes, aes(x=factor(Temperatura, level = level_order), y=After_NumberOfInternodes)) + 
  stat_boxplot(geom='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6, fill = "lightgrey") +
  ylim(10, 80) + 
  labs(y = "Liczba miêdzywêŸli pêdu g³ównego [szt.] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na iloœæ miêdzywêŸli pêdu g³ównego" , italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20))
  

ggplot(Anv_NumberOfInternodes, aes(x=factor(Temperatura, level = level_order), y=After_NumberOfInternodes, fill=as.character(Typ_Jeziora))) + 
  stat_boxplot(geom='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  ylim(10, 80) + 
  labs(y = "Liczba miêdzywêŸli pêdu g³ównego [szt.] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Wp³yw temperatury na iloœæ miêdzywêŸli pêdu g³ównego", italic(" Elodea nuttallii")))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20), legend.text = element_text(size=18), legend.title = element_text(size=20)) +
  scale_fill_discrete(name = "Jezioro pochodzenia", labels = c("Ptasie Wyspy", "Nowy Duninów"))


### Anova jednoczynnikowa: Temperatura

## Ilosc pedow bocznych
Jednoczynnikowa_Anova <- aov(After_NumberOfInternodes ~ Temperatura, data = Anv_NumberOfShoots ) # il. pêdow ró¿ni siê wzglêdem temperatury
summary(Jednoczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Jednoczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Jednoczynnikowa_Anova_Ilosci_Miedzywezli_PeduGlownego",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result

## Test Post_hoc

TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95)  # 20 i 10 sa takie same, 5 rozni sie od wszystkich
Tuckey_e <- TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95) 
Tuckey_e <-  as.data.frame(Tuckey_e[1:1])
#### Anova Dwuczynnikowa ####

## Ilosc miedzywezli
Dwuczynnikowa_Anova <-  aov(After_NumberOfInternodes ~ Temperatura + Typ_Jeziora, data = Anv_NumberOfInternodes)   # Na ró¿nice wp³ywa temperatura oraz typ jeziora [!]
summary(Dwuczynnikowa_Anova)

# Wrzucam wyniki do Tabeli
result <- summary(Dwuczynnikowa_Anova)
result <- unlist(result)

names(result)
result <-  c("Dwuczynnikowa_Anova_Ilosci_Miedzywezli_PeduGlownego",  result["F value1"], result["Pr(>F)1"], result["F value2"], result["Pr(>F)2"])
result

Statystyka_Tabela[nrow(Statystyka_Tabela ) +1, ] <- result







############################################### 5) RGR dla d³ugosci ##################################################################################

# RGR = ((log(data_lokalizacje$Mean_masa[a+1]) - log(data_lokalizacje$Mean_masa[a])) / data_lokalizacje$DayDifference[a])

# Robie ramke z dlugoscia przed/po oraz masa przed/po

Before_Total_length_5 <-  df_5$Before_Length_Main +  df_5$Before_Total_Length_Offshoots
Before_Total_length_10 <-  df_10$Before_Length_Main +  df_10$Before_Total_Length_Offshoots
Before_Total_length_20 <-  df_20$Before_Length_Main +  df_20$Before_Total_Length_Offshoots

After_Total_length_5 <-  df_5$After_Length_Main +  df_5$After_Total_Length_Offshoots
After_Total_length_10 <-  df_10$After_Length_Main +  df_10$After_Total_Length_Offshoots
After_Total_length_20 <-  df_20$After_Length_Main +  df_20$After_Total_Length_Offshoots


RGR_dane <- data.frame(df_5$Before_Length_Main, df_5$After_Length_Main, Before_Total_length_5, After_Total_length_5,
                       df_10$Before_Length_Main, df_10$After_Length_Main, Before_Total_length_10, After_Total_length_10,
                       df_20$Before_Length_Main, df_20$After_Length_Main, Before_Total_length_20, After_Total_length_20)
RGR_dane

LiczbaDni = 19 # ile dni trwa³ eksperyment? Podobno zaczynalismy 16 czerwca i konczylismy 6 lipca = 19


## Rgr dla calkowitej dlugosci

RGR_5_TotalLength = ((log(mean(RGR_dane$After_Total_length_5)) - log(mean(RGR_dane$Before_Total_length_5))) / LiczbaDni)
RGR_5_TotalLength

RGR_10_TotalLength = ((log(mean(RGR_dane$After_Total_length_10)) - log(mean(RGR_dane$Before_Total_length_10))) / LiczbaDni)
RGR_10_TotalLength

RGR_20_TotalLength = ((log(mean(RGR_dane$After_Total_length_20)) - log(mean(RGR_dane$Before_Total_length_20))) / LiczbaDni)
RGR_20_TotalLength


## Rgr dla glownych pedow

RGR_5_MainLength = ((log(mean(RGR_dane$df_5.After_Length_Main)) - log(mean(RGR_dane$df_5.Before_Length_Main))) / LiczbaDni)
RGR_5_MainLength

RGR_10_MainLength = ((log(mean(RGR_dane$df_10.After_Length_Main)) - log(mean(RGR_dane$df_10.Before_Length_Main))) / LiczbaDni)
RGR_10_MainLength

RGR_20_MainLength = ((log(mean(RGR_dane$df_20.After_Length_Main)) - log(mean(RGR_dane$df_20.Before_Length_Main))) / LiczbaDni)
RGR_20_MainLength


## PLOT Histogram z przed i po
Temperatura_RGR = c(rep("5.C",2), rep("10.C",2), rep("20.C",2))
Termin = c(rep(c("przed","po"),3))
Dlugosci = c(mean(RGR_dane$df_5.Before_Length_Main), mean(RGR_dane$df_5.After_Length_Main), mean(RGR_dane$df_5.Before_Length_Main), mean(RGR_dane$df_10.After_Length_Main), mean(RGR_dane$df_5.Before_Length_Main), mean(RGR_dane$df_20.After_Length_Main))
Wzrost_Dlugosci_RGR <- data.frame(Dlugosci, Temperatura_RGR, Termin)

level_order <- c('5.C', '10.C', '20.C')
termin_order <-  c("przed", "po")
ggplot(Wzrost_Dlugosci_RGR, aes(x=factor(Temperatura_RGR, level = level_order), y=Dlugosci, fill=as.character(Termin))) + 
  geom_col()  + 
  ylim(0, 45) + 
  labs(y = "D³ugoœæ pêdu g³ównego [cm] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("D³ugoœæ pêdów g³ównych", italic(" Elodea nuttallii"), " przed i po zakoñczeniu hodowli"))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20), legend.text = element_text(size=18), legend.title = element_text(size=20)) +
  scale_fill_discrete(name = " ", labels = c("po hodowli", "przed hodowl¹"))



Temperatura_RGR = c(rep("5.C",2), rep("10.C",2), rep("20.C",2))
Termin = c(rep(c("przed","po"),3))
Dlugosci = c(mean(Before_Total_length_5), mean(After_Total_length_5), mean(Before_Total_length_10), mean(After_Total_length_10), mean(Before_Total_length_20), mean(After_Total_length_20))
Wzrost_Dlugosci_RGR <- data.frame(Dlugosci, Temperatura_RGR, Termin)

level_order <- c('5.C', '10.C', '20.C')
termin_order <-  c("przed", "po")
ggplot(Wzrost_Dlugosci_RGR, aes(x=factor(Temperatura_RGR, level = level_order), y=Dlugosci, fill=as.character(Termin))) + 
  ylim(0, 45) + 
  geom_col()  + 
  labs(y = "D³ugoœæ ca³kowita pêdu g³ównego wraz z pêdami bocznymi [cm] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("D³ugoœæ ca³kowita osobników", italic(" Elodea nuttallii"), " przed i po zakoñczeniu hodowli"))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20), legend.text = element_text(size=18), legend.title = element_text(size=20)) +
  scale_fill_discrete(name = " ", labels = c("po hodowli", "przed hodowl¹"))
 


############################################### 6) RGR dla masy ##################################################################################

RGR_dane <- data.frame(df_Dry$Start_Dry_Mass, df_5$After_Dry_Mass,  df_10$After_Dry_Mass,  df_20$After_Dry_Mass)
RGR_dane

LiczbaDni = 19 # ile dni trwa³ eksperyment? Podobno zaczynalismy 16 czerwca i konczylismy 6 lipca = 19 dni

## Rgr dla masy

RGR_5_DryMass = ((log(mean(df_5$After_Dry_Mass)) - log(mean(df_Dry$Start_Dry_Mass))) / LiczbaDni)
RGR_5_DryMass

RGR_10_DryMass = ((log(mean(df_10$After_Dry_Mass)) - log(mean(df_Dry$Start_Dry_Mass))) / LiczbaDni)
RGR_10_DryMass

RGR_20_DryMass = ((log(mean(df_20$After_Dry_Mass)) - log(mean(df_Dry$Start_Dry_Mass))) / LiczbaDni)
RGR_20_DryMass

A_mass <-  df_Dry$Start_Dry_Mass
B_mass <-  df_Dry$Start_Dry_Mass
C_mass <-  df_Dry$Start_Dry_Mass

## PLOT Histogram z przed i po
Temperatura_RGR = c(rep("5.C",2), rep("10.C",2), rep("20.C",2))
Termin = c(rep(c("przed","po"),3))
Masa = c(mean(A_mass), mean(df_5$After_Dry_Mass), mean(B_mass), mean(df_10$After_Dry_Mass), mean(C_mass), mean(df_20$After_Dry_Mass))
Wzrost_Masa_RGR <- data.frame(Masa, Temperatura_RGR, Termin)

level_order <- c('5.C', '10.C', '20.C')
termin_order <-  c("przed", "po")
ggplot(Wzrost_Masa_RGR, aes(x=factor(Temperatura_RGR, level = level_order), y=Masa, fill=as.character(Termin))) + 
  geom_col()  + 
  ylim(0, 0.15) + 
  labs(y = "Waga wysuszonych osobników [g] \n ", x = (" \n Temperatura hodowli [°C]"), title=expression(paste("Sucha masa osobników ", italic(" Elodea nuttallii"), " przed i po zakoñczeniu hodowli"))) +
  theme(plot.title=element_text(hjust=0.5, size = 15), axis.title = element_text(size = 20), legend.text = element_text(size=18), legend.title = element_text(size=20)) +
  scale_fill_discrete(name = " ", labels = c("po hodowli", "przed hodowl¹"))


##### EXPORT DANYCH ###############################################################

Statystyka_Tabela 
write.csv2(Statystyka_Tabela ,"Diana_WynikiAnova.csv", row.names = TRUE)
Post_Hoc <- cbind(Tuckey_a, Tuckey_b, Tuckey_c, Tuckey_d, Tuckey_e)
write.csv2(Post_Hoc ,"Post_Hoc.csv", row.names = TRUE)
##### Pomys³y/Problemy ####
# Czy roœliny siê ró¿ni³y miedzy jeziorami po zebraniu? dlugosc/masa
# Zrobiæ RGR dla masy i d³ugoœci [V]
# Osobne rgr dla ró¿nych jezior
# [!] mam tylko dane co do miedzywezli dla odgalezien wiekszych niz 1 cm












############### Przydatne #################################################

# Wyciaganie wyników z tuckey
#str(tuckey)
#tuckey$Temperatura[,"p adj"]

#options(scipen = 999) # wyrzuca adnotacje matematyczn¹ z ca³osci, "0" odwraca trend
#format(cos["Pr(>F)1"], scientific = FALSE) # pokazuje wyniki jako normalne liczby zmienno przecinkowe
"
TK<-(TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95))
TK_data<-as.data.frame(TK[1:1]) # the [1:1] locates the part of the output to be exported as a dataframe
write.csv2(TK_data, 'TK_data.csv')"
