# Dane dla polaczenia
library(RMariaDB)

db_name<- "minigrant_sloma_2022"
D:\Kody_Do_Git
rmariadb.settingsfile<-"D:/Kody_Do_Git/My_Sql_Klucz.cnf"
rmariadb.db<-"diana_licencjat_2022"
rmariadb.db<- "minigrant_sloma_2022"

## The connection method below uses a password stored in a variable.
# MyConnection<-dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db)
MyConnection<-dbConnect(RMariaDB::MariaDB(),default.file=rmariadb.settingsfile,group=rmariadb.db, dbname = db_name)

dbListTables(MyConnection)
db_table <- dbListTables(MyConnection)[1]
query <- paste0("select * from ", db_table)
runQuery <- dbSendQuery(MyConnection , query)
df <- dbFetch(runQuery, n =-1)

str(data.frame(df))

# Close the connection
dbClearResult(runQuery)
dbDisconnect(MyConnection)

