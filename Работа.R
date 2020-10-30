library(tidyverse)
library(readr)
library(sf)
library(ggplot2)

Первая домашняя работа 
library(readr)
tab <- read_delim("tab.csv", ";", escape_double = FALSE, 
                  col_types = cols(Tno = col_double(), 
                                   `% Variation` = col_double()), locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)
View(tab)
install.packages("ggplot2")

install.packages("tidyverse")

ggplot(tab, aes(x =`Ht (m)`, y = `Crown Diameter (m)`, color = Species)) + geom_point()

Вторая домашняя работа
# #################################
# Все переменные имеют корректный тип данных
# Повторяющиеся переменные убраны
# Из имен переменных убраны размерности
# Всем переменам заданы их реальные размерности
# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# Категориальные переменные должны быть факторами
# Категории переменной из имени должны быть убраны
# Коды категориальных переменных заменены их категориями
# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
# Виды должны быть переименованы на латыне
# #########################################

Все переменные имеют корректный тип данных

Повторяющиеся переменные убраны

dbh mm
HR
install.packages("sf")


tab = tab %>% select(-`dbh (mm)`, -HR)

Из имен переменных убраны размерности 

tab = tab %>% rename(dbh = `dbh (m)`)
tab = tab %>% rename(Ht = `Ht (m)`)
tab = tab %>% rename(Clearance_Ht = `Clearance Ht (m)`)
tab = tab %>% rename(Crown_Depth = `Crown Depth (m)`)
tab = tab %>% rename(Average_Radial_Crown_spread = `Average Radial Crown spread (m)`)
tab = tab %>% rename(Total_Mean_Radial_Crown_Spread = `Total Mean Radial Crown Spread (m)`)
tab = tab %>% rename(Crown_Diameter = `Crown Diameter (m)`)
tab = tab %>% rename(Stem_diameter_Jan_2017 = `Stem diameter Jan 2017 (mm)`)
tab = tab %>% rename(Annual_Girth_Increment = `Annual Girth Increment (mm)`)
tab = tab %>% rename(Two_yr_dia_gain = `2yr dia gain (mm)`)
tab = tab %>% rename(Total_NSEW_Radial_Crown_Spread = `Total N,S,E,W Radial Crown Spread (m)`)

Всем переменам заданы их реальные размерности


units(tab$dbh) = as_units("m")
units(tab$Ht) = as_units("m")
units(tab$Clearance_Ht) = as_units("m")
units(tab$Crown_Depth) = as_units("m")
units(tab$Average_Radial_Crown_spread) = as_units("m")
units(tab$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(tab$Total_Mean_Radial_Crown_Spread) = as_units("m")
units(tab$Crown_Diameter) = as_units("m")
units(tab$Stem_diameter_Jan_2017) = as_units("mm")
units(tab$Two_yr_dia_gain) = as_units("mm")
units(tab$Annual_Girth_Increment) = as_units("mm")
units(tab$`Predicted crown diamet using combined formulla`) = as_units("m")
units(tab$`Predicted Crown Diameter`) = as_units("m")

tab %>% as.data.frame()

Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной

tab = tab %>% mutate(error = `Predicted crown diamet using combined formulla` - Crown_Diameter)

tab$error

tab = tab %>% rename(Crown_Diameter_Using_Combined_Formulla_Error = Crown_Diameter_Error)

tab = tab %>% mutate(Crown_Diameter_Error = `Predicted Crown Diameter` - Crown_Diameter)

tab = tab %>% select(-Difference, Diference)

Категориальные переменные должны быть факторами

library(forcats)

names(tab)

tab$`Age Index 1=Y 2=SM 3=EM 4=M`
tab = tab %>% mutate(`Age Index 1=Y 2=SM 3=EM 4=M` = as.numeric(`Age Index 1=Y 2=SM 3=EM 4=M`))

tab = tab %>%
  mutate(AgeIndex = as_factor(`Age Index 1=Y 2=SM 3=EM 4=M`)) %>%
  mutate(AgeIndex = fct_recode(AgeIndex,Y = "1", SM = "2",EM = "3", M = "4"))

tab$AgeIndex[tab$AgeIndex == "<NA>"]

tab$AgeIndex

tab$`Data Set      1=Norwich                0= Peterborough`
tab = tab %>% 
  mutate(DataSet = as_factor(`Data Set      1=Norwich                0= Peterborough`)) %>%
  mutate(DataSet = fct_recode(DataSet, Norwich = "1", Peterborough = "0"))

tab$DataSet

tab$`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`
tab = tab %>%
  mutate(PruningIndex = as_factor(`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`)) %>%
  mutate(PruningIndex = fct_recode(PruningIndex,`pruned within 5yrs` = "5", `pruned between 5 and 10yrs` = "10"))

tab$PruningIndex

tab$`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`
tab = tab %>%
  mutate(TypeOfPruning = as_factor(`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`)) %>%
  mutate(TypeOfPruning = fct_recode(TypeOfPruning,None = "0", CR = "1", Other = "2", Both = "3"))
tab$TypeOfPruning

tab$`Soil Code 1=sand and gravel 2= Clay 3=silt`
tab = tab %>%
  mutate(SoilCode = as_factor(`Soil Code 1=sand and gravel 2= Clay 3=silt`)) %>%
  mutate(SoilCode = fct_recode(SoilCode,`Sand and Gravel` = "1", Clay = "2", Slit = "3"))
tab$SoilCode

library(tidyverse)

tab$SoilCode

tab$SoilCode %>% as.integer()

tab = tab %>% rename(geology = `Superfical Geology From British Geological Survey Geology of Britain Viewer`)
tab$geology

tab = tab %>% 
  mutate(is_river = geology %>% str_detect("River"))
mutate(Soil= case_when(
  is_river & SoilCode == "Sand and Gravel" ~ "River Sand and Gravel",
  is_river & SoilCode == "Clay" ~ "River Clay",
  is_river & SoilCode == "Silt" ~ "River Silt",
  TRUE ~ as.character(Soil)
) )

tab$is_river

Виды должны быть переименованы на латыне
# Transform all to latin 
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis

tab$Species
tab$Species[tab$Species == "Oak"] = "Quercus robur"
tab$Species[tab$Species == "Norway maple"] = "Acer platanoides"
tab$Species[tab$Species == "Norway Maple"] = "Acer platanoides"
tab$Species[tab$Species == "Silver Birch"] = "Betula pendula"
tab$Species[tab$Species == "Sycamore"] = "Platanus occidentalis"

Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84

library(stringr)
tab$`Grid Reference`
coord = str_replace_all(tab$`Grid Reference`,' ','')
coord_N = str_trunc(coord, 12, "right", ellipsis = "") %>% str_trunc(5,"left", ellipsis = "")
coord_E = str_trunc(coord, 7, "right", ellipsis = "") %>% str_trunc( 5, "left", ellipsis = "")
quadr = str_trunc(coord, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_E), as.integer(coord_N), quadr)

names(table_c)=c("E", "N", "quadr")
head(table_c)
table_c = na.exclude(table_c)

table_c = table_c %>% mutate("Easting_BC" = case_when(
  quadr == "TF" ~ E +600000,
  quadr == "TG" ~ E +700000,
  quadr == "TL" ~ E +600000,
))
table_c %>% mutate("Northing_BC" = case_when(
  quadr == "TF" ~ N +300000,
  quadr == "TG" ~ N +300000,
  quadr == "TL" ~ N +200000,
))

table_c = na.exclude(table_c)


library(sf)

table_WGS = 
  table_c %>%
  st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>% as.data.frame()

table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head

tab$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)

tab


