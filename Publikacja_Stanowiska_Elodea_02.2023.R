## establishing connection
library(RMariaDB)
library(ggplot2)
library(dplyr)

#### Database connection ####
db_name<- "publikacja_stanowiska" # nazwa Schemy
rmariadb.settingsfile<-"D:/Kody_Do_Git/Publikacja_Stanowiska_2023_Sql_Klucz.cnf"
rmariadb.db<-"dane_elodea_2023_mg"
MyConnection<-dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db, dbname = db_name)
dbListTables(MyConnection)

# E. nutt
db_table <- dbListTables(MyConnection)[2]
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df_Morfologia_Elodea <- dbFetch(runQuery, n =-1)

# A. fill
db_table <- dbListTables(MyConnection)[1]
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df_Morfologia_Azolla <- dbFetch(runQuery, n =-1)

## Close the connection
dbClearResult(runQuery)
dbDisconnect(MyConnection)

#### Data preparation ####
rok <- df_Morfologia_Elodea$Pierwsza_Obserwacja

df_Morfologia_Elodea %>% 
  count(Pierwsza_Obserwacja) -> rok
rok$n <- cumsum(rok$n) # sumowanie

# Dodaje Azolle
rok_Azolla <- df_Morfologia_Azolla$Pierwsza_Obserwacja
df_Morfologia_Azolla %>% 
  filter(Pierwsza_Obserwacja > 1990) %>% 
  count(Pierwsza_Obserwacja) -> rok_Azolla
rok_Azolla$n <- cumsum(rok_Azolla$n) # sumowanie

# Scalam tabele w jedno 
rok$species <- rep("Elodea nuttallii",length(rok[[1]]))
rok_Azolla$species <- rep("Azolla filiculoides",length(rok[[1]]))
#### Wykres #### Empirical Cumulative Distribution Function Plot

colnames(rok) <- c("x", "y") # Elodea
colnames(rok_Azolla) <- c("x", "y") # Azolla

ggplot() +
  geom_step(data=rok, mapping=aes(x=x, y=y)) +
  geom_point(data=rok, mapping=aes(x=x, y=y), color='black', fill='white', shape=21, size = 3) +
  
  geom_step(rok_Azolla, mapping=aes(x=x, y=y), color="red") +
  geom_point(rok_Azolla, mapping=aes(x=x, y=y), color='red', fill='white', shape=21, size = 3) + 
  scale_colour_manual(values = c("pink", "green"))+
  
  scale_x_continuous(breaks = seq(1990, 2023, by = 4)) + 
  scale_y_continuous(breaks = seq(0, 90, by = 10), limits = c(0,80) )+
  #ylim +
  labs(title="Stanowiska z Elodea nuttallii na terenie Polski na przestrzeni lat") +
  
  xlab("Lata") +
  ylab("Sumaryczna ilosc stanowisk") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=11,face="bold"))



#### Przydatne ####
# Steps colory https://stackoverflow.com/questions/8737454/r-ggplot2-colouring-step-plot-depending-on-value
d=data.frame(x=c(1,2,4,5,7,8,9), y=c(1,2,3,5,6,7,9))
ggplot() +
  geom_step(data=d, mapping=aes(x=x, y=y), ) +
  geom_step(data=d, mapping=aes(x=x, y=y), direction="vh", linetype=3) +
  geom_point(data=d, mapping=aes(x=x, y=y), color="red") +
  opts(title="geom_step", plot.title=theme_text(size=40, vjust=1.5))

#create scatterplot with custom labels on x-axis
ggplot(df, aes(x=points, y=assists)) +
  geom_point(size=2) + 
  scale_x_continuous(breaks=c(5, 15, 25), labels=c('five', 'fifteen', 'twenty-five'))
