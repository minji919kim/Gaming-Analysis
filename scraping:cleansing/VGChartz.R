library(tidyverse)
library(polite)
library(rvest)
library(purrr)
library(readxl)
library(stringr)

#read csv
sales <- read.csv('gamesales.csv')
sales <- sales %>% select(2:4)

#filter out series and all
sales1 <- sales %>% filter(console != 'Series') %>% filter(console != 'All')

#change m with numeric value
sales2 <- sales1 %>% mutate(sales = case_when(
    str_detect(sales, "m")~ as.numeric(str_extract(sales, "[\\d\\.]+")) * 1000000
))

#change console names
sales3 <- sales2 %>% mutate(console = case_when(
  str_detect(console, "PS5")~ "PlayStation 5",
  str_detect(console, "PS4")~ "PlayStation 4",
  str_detect(console, "PSV")~ "PlayStation Vita",
  str_detect(console, "PS3")~ "PlayStation 3",
  str_detect(console, "PS2")~ "PlayStation 2",
  str_detect(console, "N64")~ "Nintendo 64",
  str_detect(console, "PS")~ "PlayStation",
  str_detect(console, "DC")~ "Dreamcast",
  str_detect(console, "X360")~"Xbox 360",
  str_detect(console, "WiiU")~ "Wii U",
  str_detect(console, "Wii")~ "Wii",
  str_detect(console, "XOne")~ "Xbox One",
  str_detect(console, "NS")~ "Switch",
  str_detect(console, "GC")~ "GameCube",
  str_detect(console, "XB")~ "Xbox",
  
  str_detect(console, "XS")~ "Xbox Series X",
  str_detect(console, "GBA")~ "Game Boy Advance",
  
  
  
  
  TRUE ~ console
))

write.csv(sales3,'C:\\Users\\13114\\Desktop\\cleaned_gamesales.csv',row.names = FALSE)

