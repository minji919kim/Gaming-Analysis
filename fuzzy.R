library(usethis) 
usethis::edit_r_environ() # Write R_MAX_VSIZE=100Gb

install.packages("fuzzyjoin", dependencies = FALSE)
library(tidyverse)
library(stringr)
library(fuzzyjoin)
library(stringdist)

vg_df <- read.csv('cleaned_gamesales.csv', encoding = "UTF-8")
meta_df <- read.csv('metagame_data.csv')

vg_df <- vg_df %>% mutate(cleaned_game_name = tolower(game_name))
meta_df <- meta_df %>% mutate(cleaned_game_name = tolower(game_name))

#cleaned special characters and change to all lower case to speed up the fuzzy join
vg_df <- vg_df %>%
  mutate(
    cleaned_game_name = str_replace_all(
      string = cleaned_game_name,
      pattern =  "\\'|&|-", 
      replacement = ''), 
    cleaned_game_name =str_replace_all(
      string = cleaned_game_name,
      pattern =  '[:punct:]', 
      replacement = ''), 
    cleaned_game_name =str_replace_all(
      string = cleaned_game_name,
      pattern =  'è', 
      replacement = 'e')
    ) 



meta_df <- meta_df %>%
  mutate(
    cleaned_game_name = str_replace_all(
      string = cleaned_game_name,
      pattern =  "\\'|&|-|ñ", 
      replacement = ''), 
    cleaned_game_name =str_replace_all(
      string = cleaned_game_name,
      pattern =  '[:punct:]', 
      replacement = ''), 
    cleaned_game_name =str_replace_all(
      string = cleaned_game_name,
      pattern =  'è', 
      replacement = 'e'))

meta_df <- meta_df %>%
  mutate(
    genre = (str_extract(
      string = genre,
      pattern = '[[:alpha:][:space:]]*(?=\\,)'))
  )

meta_df_asc <- meta_df %>% arrange(cleaned_game_name) %>% 
  mutate(final_name = paste(cleaned_game_name, platform), 
         final_name = str_replace_all(
  string = final_name,
  pattern =  " ", 
  replacement = '')) %>% arrange(final_name)
vg_df_asc <- vg_df %>% rename('platform' = 'console') %>%
  arrange(cleaned_game_name) %>% 
  mutate(final_name = paste(cleaned_game_name, platform), 
         final_name = str_replace_all(
    string = final_name,
    pattern =  " ", 
    replacement = '')) %>% 
  arrange(final_name)
fuzzy_table <- meta_df_asc %>% stringdist_inner_join(vg_df_asc[50001:60711,], 
                                                  by=c('final_name'), #match based on team
                                                  max_dist=1, 
                                                  distance_col='dist') %>%
  group_by(cleaned_game_name.x) %>%
  slice_min(order_by=dist, n=1) 
write.csv(fuzzy_table,"/Users/minjikim/Desktop/Current Semester/Data Analytics/Final Project/fuzzy6.csv")

#The final fuzzy join file
final_fuzzy <- read.csv('FUZZY.csv')

final_fuzzy <- final_fuzzy %>%
  mutate(
    cleaned_game_name.x = str_replace_all(
      string = cleaned_game_name.x,
      pattern =  "<ca> ", 
      replacement = ''), 
    cleaned_game_name.x = str_replace_all(
      string = cleaned_game_name.x,
      pattern =  " ", 
      replacement = ''),
    cleaned_game_name.y = str_replace_all(
      string = cleaned_game_name.y,
      pattern =  "<ca> ", 
      replacement = ''), 
    cleaned_game_name.y = str_replace_all(
      string = cleaned_game_name.y,
      pattern =  " ", 
      replacement = '')
    )
    
final_fuzzy_dis0 <- final_fuzzy %>%
  filter(final_fuzzy$dist == '0')

final_fuzzy$meta_name = substr(final_fuzzy$final_name.x,1,nchar(final_fuzzy$final_name.x)-3)
final_fuzzy$vg_name = substr(final_fuzzy$final_name.y,1,nchar(final_fuzzy$final_name.y)-3)

final_fuzzy$meta_compare = str_remove_all(final_fuzzy$cleaned_game_name.x, "[:digit:]+|i|ii|iii|iv|v|[:space:]")
final_fuzzy$vg_compare = str_remove_all(final_fuzzy$cleaned_game_name.y, "[:digit:]+|i|ii|iii|iv|v|[:space:]")

final_fuzzy_dis1 <- final_fuzzy %>%
  filter(final_fuzzy$dist == '1') %>%
  filter(platform.x == platform.y) %>% 
  filter(meta_name!=vg_name) %>%
  filter(meta_compare != vg_compare)

final_fuzzy_dis2 <- final_fuzzy_dis1 %>%
  mutate(vg_compare= str_replace(string = final_fuzzy_dis1$vg_compare,
              pattern = 'è', 
              replacement = 'e')) %>%
  filter(vg_compare == meta_compare) %>% 
  select (-meta_name, -vg_name, -meta_compare, -vg_compare)

final_fuzzy_dis0 <- rbind(final_fuzzy_dis0, final_fuzzy_dis2) 
final_fuzzy_dis0 <- final_fuzzy_dis0 %>% select(-cleaned_game_name.x, -cleaned_game_name.y, -final_name.x, -final_name.y, -dist)
write.csv(final_fuzzy_dis0,"/Users/minjikim/Desktop/Current Semester/Data Analytics/Final Project/final_fuzzy.csv")

