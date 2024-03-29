#### Code for finding closest IMGW meteo station to the data gathered in the field and merging it with the meteo data from IMGW

#### Input ############################################################################################################################

## Packages
install.packages("climate")
install_github("bczernecki/climate")

library(climate) # downloads the data from IMGW
library(dplyr)
library(sf)

## Data

# Complex meteo data downloaded from IMGW, for each month for all meteo station from 1991:2023
Meteo <- meteo_imgw(interval = "monthly", rank = "synop", year = 1991:2023, coords = TRUE)

# Data from the field: contiains every record of IAS form Poland with year and location of finding
Field_Data_Inv <- read.csv2("C:/.../publikacja1/Dane_25_04/Dane_gat_obce_Do_Pracy.csv") # pobieram plik

## Functions ##

# --#---

#### Output ##########################################################################################################################

# Mutated Field_Data_Inv data. Added closest meteo stations with ther numers. Only location related columns were left.
Field_Data_Location

# Final result: merge of field and meteo data (Field_Data_Location + Meteo)
Df_Field_m_Meteo




#### Code ################################################################################################################################


##########################################################################################################################################

################################################################ Start ###################################################################

str(Meteo)
str(Field_Data_Inv)

# Select only location related columns
Field_Data_Location <- Field_Data_Inv %>% 
  select(Id, Gatunek, X, Y, Wojewodztwo, Pierwsze_Stwierdzenie)
# Set correct CRS
Field_Data_Location <- st_as_sf(Field_Data_Location, coords = c("X", "Y"))
st_crs(Field_Data_Location) <- 2180  # 4326 for google coordinates
Field_Data_Location <- st_transform(Field_Data_Location, 4326 )

#st_write(Field_Data_Location, "C:/Users/.../Desktop/CheckCRS.shp")

## Finding closest station and its number
Field_Data_Location$station <- base::rep("Stacja",nrow(Field_Data_Location)) # przygotowuje kolumne na nazwy najblizszyvh stacji
Field_Data_Location$Nr_station <- base::rep("Nr_Stacja",nrow(Field_Data_Location))


# Loop for finding a closest Meteo Station for each point. Input needs to be in CRS 4326, You may specify year
for(i in 1:nrow(Field_Data_Location)){
  wynik <- nearest_stations_imgw(point = as.vector(unlist(st_coordinates(Field_Data_Location)[i,])), no_of_stations = 1)
  Najblizsza_Stacja <- wynik["station"]
  Najblizsza_Stacja_id <- wynik["id"]
  kolumna_nr_stacja <- which( colnames(Field_Data_Location)=="station")
  kolumna_nr_id <- which( colnames(Field_Data_Location)=="Nr_station")
  Field_Data_Location[i, kolumna_nr_stacja] <- Najblizsza_Stacja
  Field_Data_Location[i, kolumna_nr_id] <- Najblizsza_Stacja_id
  print(i)
  Sys.sleep(0.05)
}


## Preaparing for the merge
Field_Data_Location <-  as.data.frame(Field_Data_Location)
names(Meteo)[which(colnames(Meteo)=="yy")] <- "Pierwsze_Stwierdzenie"
names(Meteo) = sub('yy', 'Pierwsze_Stwierdzenie', names(Meteo))


Df_Field_m_Meteo <- merge(Field_Data_Location, Meteo, by = c("station", "Pierwsze_Stwierdzenie"), all.x=TRUE)


Df_Field_m_Meteo <- Df_Field_m_Meteo %>% 
  arrange(Id, Pierwsze_Stwierdzenie, mm)
Df_Field_m_Meteo$station <- iconv(Df_Field_m_Meteo$station, to='ASCII//TRANSLIT') # remove polish marks


## Write as csv2 format
write.csv2(Df_Field_m_Meteo,"C:/Users/.../Df_Field_m_Meteo.csv")


############################################################### End #####################################################################

#########################################################################################################################################


### Cool Functions ###

nearest_stations_imgw(point = c(15.75238, 51.76896)) 
st_write(Field_Data_Location, "C:/.../.../Desktop/CheckCRS.shp")
Field_Data_Location <- st_transform(Field_Data_Location, 4326 ) # change CRS to that of the google maps
st_coordinates(Field_Data_Location) # get two vectors of coordinates from one col
Df_Field_m_Meteo$station <- iconv(Df_Field_m_Meteo$station, to='ASCII//TRANSLIT') # remove polish marks


### Pre Merge modifications to the Field_Data_Location ###

# znajduje nowe dane do niektorych stacji
Df_Field_m_Meteo[59,]
Field_Data_Location[21] <- as.numeric(Field_Data_Location[21])
Field_Data_Location[22] <- as.numeric(Field_Data_Location[22])
data.frame(names(Field_Data_Location))
unlist(Field_Data_Location[281,c(2,3, 6, 21, 22)])[c(3,6,7)]
nearest_stations_imgw(point = c(16.8583187639896, 51.1129720035506))
Meteo[-39] %>% 
  filter(station == "LEGNICA",  Pierwsze_Stwierdzenie ==  2013)

Field_Data_Location$station[59] <- "LEGNICA"
Field_Data_Location$Nr_station[59] <- "351160415"
Field_Data_Location$station[61] <- "LEGNICA"
Field_Data_Location$Nr_station[61] <- "351160415"
Field_Data_Location$station[65] <- "LEGNICA"
Field_Data_Location$Nr_station[65] <- "351160415"

Field_Data_Location$station[84] <- "KRAKÓW-BALICE"
Field_Data_Location$Nr_station[84] <- "350190566"
Field_Data_Location$Pierwsze_Stwierdzenie[94] <- 1991
Field_Data_Location$Pierwsze_Stwierdzenie[94] <- 1991
Field_Data_Location$station[147] <- "RESKO-SMÓLSKO"
Field_Data_Location$Nr_station[147] <- "353150210"

Field_Data_Location$station[176] <- "PIŁA"
Field_Data_Location$Nr_station[176] <- "353160230"
Field_Data_Location$station[191] <- "PIŁA"
Field_Data_Location$Nr_station[191] <- "353160230"
Field_Data_Location$station[213:215] <- "WŁODAWA"
Field_Data_Location$Nr_station[213:215] <- "351230497"
Field_Data_Location$Pierwsze_Stwierdzenie[235:240] <- 1991
Field_Data_Location$Pierwsze_Stwierdzenie[235:240] <- 1991

Field_Data_Location$station[249] <- "KĘTRZYN"
Field_Data_Location$Nr_station[249] <- "354210185"
Field_Data_Location$station[251:252] <- "KĘTRZYN"
Field_Data_Location$Nr_station[251:252] <- "354210185"
Field_Data_Location$station[281] <- "LEGNICA"
Field_Data_Location$Nr_station[281] <- "351160415"

Field_Data_Location$Nr_station <- as.numeric(Field_Data_Location$Nr_station)

