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
sum5 <- summary(df_5)
str(sum5)

library(pandas)

TK<-(TukeyHSD(Jednoczynnikowa_Anova, conf.level=0.95))
TK_data<-as.data.frame(sum5) # the [1:1] locates the part of the output to be exported as a dataframe
write.csv2(TK_data, 'TK_data.csv')

library(dplyr)

Before_Total_Length <- c(df_5$Before_Length_Main + df_5$Before_Total_Length_Offshoots, 
                  df_10$Before_Length_Main + df_10$Before_Total_Length_Offshoots, 
                  df_20$Before_Length_Main + df_20$Before_Total_Length_Offshoots)
After_Total_Length <- c(df_5$After_Length_Main + df_5$After_Total_Length_Offshoots, 
                  df_10$After_Length_Main + df_10$After_Total_Length_Offshoots, 
                  df_20$After_Length_Main + df_20$After_Total_Length_Offshoots)

nrow(df_5)

df_5$Temperatura <- c(rep(5,nrow(df_5)))
df_10$Temperatura <- c(rep(10,nrow(df_10)))
df_20$Temperatura <- c(rep(20,nrow(df_20)))
Dane_Po <-  rbind(df_5, df_10, df_20)
Dane_Po <-  cbind(Dane_Po, After_Total_Length, Before_Total_Length)

nrow(Dane_Po)

colnames(Dane_Po)
Dane_Po %>% select(-"Nr", -"Type_of_Lake" ) %>% 
  select(-starts_with('Before')) %>% 
  group_by(Temperatura) %>% 
  summarise(across(, c(MEAN = mean, MEDIAN = median, MIN = min, MAX = max, SD = sd, WARIANCJA = var), na.rm =  TRUE, .names = "{col}_{fn}")) %>% 
  arrange(Temperatura) -> Export_Dane
Export_Dane

write.csv2(Export_Dane ,"Diana_AFTER_PodstawoweStatystyki.csv", row.names = TRUE)

Dane_Po %>% select(-"Nr", -"Type_of_Lake" ) %>% 
  select(-starts_with('AFTER')) %>% 
  group_by(Temperatura) %>% 
  summarise(across(, c(MEAN = mean, MEDIAN = median, MIN = min, MAX = max, SD = sd, WARIANCJA = var), na.rm =  TRUE, .names = "{col}_{fn}")) %>% 
  arrange(Temperatura) -> Export_Dane
Export_Dane

write.csv2(Export_Dane ,"Diana_BEFORE_PodstawoweStatystyki.csv", row.names = TRUE)

#### Excele z uwzglêdnieniem jeziora pochodzenia

colnames(Dane_Po)
Dane_Po %>% select(-"Nr") %>% 
  select(-starts_with('Before')) %>% 
  group_by(Temperatura, Type_of_Lake) %>% 
  summarise(across(, c(MEAN = mean, MEDIAN = median, MIN = min, MAX = max, SD = sd, WARIANCJA = var), na.rm =  TRUE, .names = "{col}_{fn}")) %>% 
  arrange(Temperatura) -> Export_Dane
Export_Dane

write.csv2(Export_Dane ,"Diana_AFTER_PodstawoweStatystyki.csv", row.names = TRUE)

Dane_Po %>% select(-"Nr" ) %>% 
  select(-starts_with('AFTER')) %>% 
  group_by(Temperatura, Type_of_Lake) %>% 
  summarise(across(, c(MEAN = mean, MEDIAN = median, MIN = min, MAX = max, SD = sd, WARIANCJA = var), na.rm =  TRUE, .names = "{col}_{fn}")) %>% 
  arrange(Temperatura) -> Export_Dane
Export_Dane

#### DRY_Mass

Dry_Total_Length <- c(df_Dry$Before_Length_Main + df_Dry$Before_Total_Length_Offshoots)

df_Dry$Dry_Total_Length <-  Dry_Total_Length

Dane_Po <- df_Dry

Dane_Po %>% select(-"Nr", -"Type_of_Lake" ) %>% 
  summarise(across(, c(MEAN = mean, MEDIAN = median, MIN = min, MAX = max, SD = sd, WARIANCJA = var), na.rm =  TRUE, .names = "{col}_{fn}")) -> Export_Dane
Export_Dane

write.csv2(Export_Dane ,"Diana_DRY_PodstawoweStatystyki.csv", row.names = TRUE)

#### ZROBIÆ TE¯ DA RÓ¯NYCH JEZIOR
