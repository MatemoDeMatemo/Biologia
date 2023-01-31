#### Polaczenie z baza danych oraz paczki ####
## Dane dla polaczenia
library(RMariaDB)
library(dplyr)
library(rstatix) # proste testy statystyczne 
`%!in%` <- Negate(`%in%`)  # brak tej funcki w r, trzeba ja zdefiniowac

db_name<- "diana_licencjat_2022"
rmariadb.settingsfile<-"D:/Kody_Do_Git/My_Sql_Klucz.cnf"
rmariadb.db<-"diana_licencjat_2022"

## Connection
MyConnection<-dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db, dbname = db_name)

dbListTables(MyConnection)

#"hodowla_diana_after"
db_table <- dbListTables(MyConnection)[1]
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df_After <- dbFetch(runQuery, n =-1)

# "hodowla_diana_before"
db_table <- dbListTables(MyConnection)[2]
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df_Before <- dbFetch(runQuery, n =-1)

## Close the connection
dbClearResult(runQuery)
dbDisconnect(MyConnection)

#### Statystyka ####
str(data.frame(df_Before))
str(data.frame(df_After))

# to do: anova dla: 1) D³ugoœci ca³kowitych oraz g³ównych pêdów 2) Anova Masy 3) Anova ilosci pedow bocznych 4) Anova ilosci miedzywezli pedu glownego 
df_After
df_A_Length %>% 
  filter(Type_of_Lake == 0) %>% 
  filter(Temperatura_Hodowli == 10) %>% 
  select(Id, After_Length_Main)

df_A_Length <- df_After
df_A_Length$Temperatura_Hodowli <- as.factor(df_A_Length$Temperatura_Hodowli)
df_A_Length$After_Total_Length <- df_A_Length$After_Length_Main + df_A_Length$After_Total_Length_Offshoots
colnames(df_A_Length)
# Sprawdzam zalozenia

# Szukam outlajerów
df_A_Length %>% 
  filter(Type_of_Lake == 0) %>% 
  select(Id, Temperatura_Hodowli, After_Length_Main) %>% 
  group_by(Temperatura_Hodowli) %>%
  identify_outliers(After_Length_Main)
summary(df_A_Length)

Jednoczynnikowa_Anova <- aov( After_Length_Main ~ factor(Temperatura_Hodowli), data = df_A_Length) # dlugosc glownych pedow vs temp *
summary(Jednoczynnikowa_Anova)
TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95) 

# testy

df_A_Length$Type_of_Lake
df_A_Length %>% 
  filter(!Id %in% c(33, 41)) %>% # select(Id, After_Length_Main)
  filter(Type_of_Lake == 0) -> a
  boxplot(a$After_Length_Main ~ a$Temperatura_Hodowli)

#Check normality assumption by groups
df_A_Length %>%
  filter(Type_of_Lake == 0) %>% 
  filter(!Id %in% c(33, 41))  %>%  # wywalam outlajery
  group_by(factor(Temperatura_Hodowli)) %>%
  shapiro_test(After_Length_Main)

#homogeneity of variance
df_A_Length %>% 
  filter(Type_of_Lake == 0) %>% 
  filter(!Id %in% c(33, 41))  %>% 
  levene_test(After_Length_Main ~ factor(Temperatura_Hodowli))


Jednoczynnikowa_Anova <- aov( After_Total_Length_Offshoots ~ factor(Temperatura_Hodowli), data = df_A_Length) # dlugosc bocznych pedow vs temp #
summary(Jednoczynnikowa_Anova)
TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95) 


Jednoczynnikowa_Anova <- aov( After_Total_Length ~ factor(Temperatura_Hodowli), data = df_A_Length) # dlugosc calkowita pedow vs temp **
summary(Jednoczynnikowa_Anova)
TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95) 
