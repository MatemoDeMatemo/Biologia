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

db_table <- dbListTables(MyConnection)
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df_Morfologia <- dbFetch(runQuery, n =-1)

#### Data preparation ####
rok <- df_Morfologia$Pierwsza_Obserwacja

df_Morfologia %>% 
  count(Pierwsza_Obserwacja) -> rok
rok$n <- cumsum(rok$n) # sumowanie

#### Wykres ####

colnames(rok) <- c("x", "y")

ggplot() +
  geom_step(data=rok, mapping=aes(x=x, y=y)) +
  geom_point(data=rok, mapping=aes(x=x, y=y), color='black', fill='white', shape=21, size = 3) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 4)) + 
  scale_y_continuous(breaks = seq(0, 90, by = 10), limits = c(0,80) )+
  #ylim +
  labs(title="Stanowiska z Elodea nuttallii na terenie Polski na przestrzeni lat") +
  xlab("Lata") +
  ylab("Sumaryczna ilosc stanowisk") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=11,face="bold"))



#### Przydatne ####

d=data.frame(x=c(1,2,4,5,7,8,9), y=c(1,2,3,5,6,7,9))
ggplot() +
  geom_step(data=d, mapping=aes(x=x, y=y)) +
  geom_step(data=d, mapping=aes(x=x, y=y), direction="vh", linetype=3) +
  geom_point(data=d, mapping=aes(x=x, y=y), color="red") +
  opts(title="geom_step", plot.title=theme_text(size=40, vjust=1.5))

#create scatterplot with custom labels on x-axis
ggplot(df, aes(x=points, y=assists)) +
  geom_point(size=2) + 
  scale_x_continuous(breaks=c(5, 15, 25), labels=c('five', 'fifteen', 'twenty-five'))
