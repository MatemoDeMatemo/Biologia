# Analiza PCA, MySQL 25.01.2023
library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(RMariaDB)
library(factoextra) # Wizualizacja PCA
library(corrplot) # Contribution
library(umap)

#### Polaczenie z baza danych oraz paczki ####
## establishing connection
db_name<- "test_csv" # nazwa bazy
rmariadb.settingsfile<-"D:/Kody_Do_Git/My_Sql_Klucz.cnf"
rmariadb.db<-"diana_licencjat_2022"
## Connection
MyConnection<-dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db, dbname = db_name)
dbListTables(MyConnection)

## importing data
#"morfologia_wszystkie_gatunki"
db_table <- dbListTables(MyConnection)[6]
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df_Morfologia <- dbFetch(runQuery, n =-1)
# monitoring_elodea"
db_table <- dbListTables(MyConnection)[4]
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df_Monitoring <- dbFetch(runQuery, n =-1)

## Close the connection
dbClearResult(runQuery)
dbDisconnect(MyConnection)

#### Data processing ####
# Monitoring
df_Monitoring
data.frame(colnames(df_Monitoring)) # pokazuje numery kolumn
str(df_Monitoring)
wymiana_wartosci = c('brak gat' = '0', '0,10%' = "0.001", '0,11-1%' = "0.01", '1,1-2,5%' = "0.025", '2,51-5%' = "0.05", '5,1-10%' = "0.1", '10,1-25%' = "0.25", '25,1-50%' = "0.50", '50,1-75%' = "0.75", '75,1-100%' = "1")
df_Monitoring %>% 
  select(-c(1, 2, 5, 12,14:25, 32:33)) %>% 
  filter(NrMon == 1) %>% 
  mutate(PokrGat_Inwazyjnego =  as.double(str_replace_all(PokrGat_Inwazyjnego,  wymiana_wartosci))) %>% 
  #mutate(across(where(is.character), as.factor)) %>% 
  relocate(where(is.character))-> DF_Monitoring
str(DF_Monitoring)

# Morfologia  
str(df_Morfologia)
#view(df_Morfologia)
data.frame(colnames(df_Morfologia))
df_Morfologia$NrTrans <- as.integer(df_Morfologia$NrTrans)
df_Morfologia %>% 
  select(-c(1, 10, 18)) %>% 
  filter(Czy_Dane_Glowne == 1, Gatunek == "Elodea nuttallii", NrMon == 1) %>% 
  #mutate(across(where(is.character), as.factor)) %>% 
  relocate(where(is.character))  %>% 
  select( -NrMon) -> DF_Morfologia
str(DF_Morfologia) 

DF_Morfologia %>% 
  group_by(Stanowisko, NrTrans, Metoda ) %>% 
  summarize(mean_Total_Plant_Length_mm = mean(Total_Plant_Length_mm), 
            mean_Total_Offshoots_Length_mm = mean(Total_Offshoots_Length_mm), 
            mean_Mainshoot_Length_mm  = mean(Mainshoot_Length_mm ), 
            mean_Dry_Mass_g = mean(Dry_Mass_g), 
            mean_Ratio_Length_To_Mass = mean(`Ratio_Length_To_Mass_[m/g]`)) -> wynik
#view(wynik) 
# zamieniam tranzekt 0 na 3 identyczne o innych numerach dla E8 i E11
Row <- wynik[32,]
str(Row)
AddRow1 <- rbind(Row, Row, Row)
AddRow1[1,2] <- 1
AddRow1[2,2] <- 2
AddRow1[3,2] <- 3
wynik <-  wynik[-32,]

Row <- wynik[7,]
AddRow2 <- rbind(Row, Row, Row)
AddRow2[1,2] <- 1
AddRow2[2,2] <- 2
AddRow2[3,2] <- 3
wynik <-  wynik[-7,]
DF_Morfologia <- as.data.frame(rbind(wynik, AddRow1, AddRow2))
str(DF_Morfologia)

# LEFT JOIN
df_Final <- merge(x=DF_Monitoring,y=DF_Morfologia , 
                  by=c("Stanowisko", "NrTrans"), all.x=TRUE)
# Uladnianie
str(df_Final)
df_Final$NrTrans <-  as.factor(df_Final$NrTrans)
df_Final %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  relocate(where(is.factor)) -> DF_Final


#### DATA READY FOR ANALYSE ####
str(DF_Final)
data.frame(colnames(DF_Final))

PCA_DF_Final <- DF_Final[-c(1:8)]
str(PCA_DF_Final)

FA_DF_FINAL <- DF_Final[-c(1, 2, 4, 6, 8)]

#### PCA ####
res.pca <- prcomp(PCA_DF_Final, scale = TRUE)
res.pca

# Wykres Osypiska
fviz_eig(res.pca)

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation
options(scipen = 100)
get_eigenvalue(res.pca)


## Contribution - visualization ##
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr = FALSE)                      # Corr plot
fviz_cos2(res.pca, choice = "var", axes = 1:2) +
  theme(axis.text.x = element_text(size = 14,angle = 70,vjust = 1)) #Total contribution on PC1 and PC2

## Wykresy
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
options(ggrepel.max.overlaps = Inf)

# Jeziora
fviz_pca_ind(res.pca,
             label="none",
             habillage= DF_Final$Jezioro,
             pointsize = 3.5
)

# Stanowiska
fviz_pca_ind(res.pca,
             label="none",
             habillage= DF_Final$Stanowisko,
             addEllipses=TRUE, ellipse.level=0.95,
             pointsize = 3.5
)

#### BiPlot ####

# BiPlot
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
# BiPlot na tle grup
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                habillage= DF_Final$Stanowisko,
                addEllipses=TRUE, ellipse.level=0.95,
                pointsize = 3.5
)

# Visualize variable with cos2 >= 0.6
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, select.var = list(cos2 = 0.6))

# top 5 contributing individuals and variable
fviz_pca_biplot(res.pca, select.ind = list(cos2 = 0.6), 
                select.var = list(cos2 = 0.6),
                habillage= DF_Final$Stanowisko,
                pointsize = 3.5)

#### UMAP ####
str(DF_Final)
UMAP_DF_Final <- DF_Final[-7] # minus NrMon
UMAP_DF_Final$Id <-  1:nrow(UMAP_DF_Final) # dodaje col Id

Factor_DF_Final <- UMAP_DF_Final %>% 
  select(where(is.factor), Id)

Numeric_DF_Final <- UMAP_DF_Final%>%
  select(where(is.numeric)) %>%
  column_to_rownames("Id")

umap_df <- umap(scale(Numeric_DF_Final)) # UMAP

umap_df <- umap_df$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(Id=row_number())%>%
  inner_join(Factor_DF_Final, by="Id")
str(umap_df)
## Wykres 
umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             color = Stanowisko,
             shape = Jezioro))+
  geom_point(size = 2)+
  scale_shape_manual(values=c(1:14)) +
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot")

#ggsave("UMAP_plot_example1.png")



#### Wybieranie optymalnych danych ####
str(DF_Final)
view(DF_Final)
Save <- DF_Final
`%!in%` <- Negate(`%in%`)
DF_Final %>% 
  filter(Jezioro %!in% c("Wicko_Male")) -> DF_Final
#### Przydatne ####
data.frame(colnames(df_Monitoring)) # pokazuje numer kolumn
