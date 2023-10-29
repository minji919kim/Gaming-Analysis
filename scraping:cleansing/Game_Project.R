library(tidyverse)
library(polite)
library(rvest)
library(purrr)
library(readxl)
library(stringr)
library(mosaic)
library(sys)
library(lubridate)
library(stringi)
library(fuzzyjoin)


# Metacritic Scraping -----------------------------------------------------

# polite & bow
metacritic_game <- bow(
  url = 'https://www.metacritic.com/browse/games/score/metascore/all/all/filtered?view=detailed&page=',
  user_agent = "Jing Du (BC)",
  force = TRUE
)
print(metacritic_game)

# Get the total pages
meta_pages <- read_html('https://www.metacritic.com/browse/games/score/metascore/all/all/filtered?page=0') %>% 
  html_element('div.pages') %>% 
  html_elements('li:last-child') %>% 
  html_elements('a.page_num') %>% 
  html_text2() %>% as.numeric()
meta_pages

# Scraping
scrape_metacritic <- function(PAGE){
  #Sys.sleep(5)
  print(paste0('Scraping page:',PAGE))
  
  # Scrape main pages politely
  html_meta <- scrape(metacritic_game, 
                      query = list(page = as.character(PAGE-1)))
  
  # scraping elements of the main pages
  game <- html_meta %>% 
    html_elements('div.browse_list_wrapper .title h3') %>% 
    html_text2()
  platform <- html_meta %>% 
    html_elements('div.browse_list_wrapper .platform .data') %>% 
    html_text2()
  release_date <- html_meta %>% 
    html_elements('div.browse_list_wrapper .clamp-details') %>% 
    html_elements('.platform + span') %>% 
    html_text2()
  metascore <- html_meta %>% 
    html_elements('div.clamp-metascore .metascore_w') %>% 
    html_text2() %>% as.numeric()
  userscore <- html_meta %>% 
    html_elements('div.clamp-userscore .metascore_w') %>% 
    html_text2() %>% as.numeric()
  
  # get the url links for each individual item in the main pages
  detail_link <- paste0('https://www.metacritic.com', 
                        html_meta %>% html_elements('td.clamp-summary-wrap a.title') %>% 
                          html_attr('href'))
  
  # put elements together into a data frame
  df_metacritic <- data.frame(game, platform, release_date,metascore,userscore)
  
  # create empty set for elements in links of the main pages
  developer <- c()
  genre <- c()
  num_critic_reviews <- c()
  num_user_rating <- c()
  
  # run loop to scrape each link of link
  for (x in detail_link){
    
    # nod for permission to scrape links of link
    html_nod <- nod(metacritic_game,x)
    
    # If some pages are not allowed to scrape, skip to the next iteration
    if(is.null(html_nod$delay)){next}
    
    # scrape politely
    html_detail <- scrape(html_nod)
    
    # If certain links of link turn out to be NULL (404 not found)
    # all elements will be assigned NA, and skip to the next iteration
    # else scrape the contents of each element in the links of link
    if(is.null(html_detail)){
      developer = c(developer,NA)
      genre = c(genre,NA)
      num_critic_reviews = c(num_critic_reviews,NA)
      num_user_rating = c(num_user_rating,NA)
      next} else{
      developer <- c(developer,
                     html_detail %>% 
                       html_elements('li.summary_detail.developer span.data') %>% 
                       html_text2() %>% 
                       {if(length(.)==0) NA else .})
      genre <- c(genre,
                 paste(html_detail %>% 
                         html_elements('div.details.side_details li.summary_detail.product_genre span.data') %>% 
                         html_text2(),collapse = ', ') %>% 
                   {if(length(.)==0) NA else .})
      num_critic_reviews <- c(num_critic_reviews,
                              html_detail %>% 
                                html_elements('div.details.main_details .summary .count a span:first-child') %>% 
                                html_text2() %>% as.numeric() %>% 
                                {if(length(.)==0) NA else .})
      num_user_rating <- c(num_user_rating,
                           html_detail %>% 
                             html_elements('div.details.side_details .summary .count a') %>% 
                             html_text2() %>% str_extract("\\d+") %>% as.numeric() %>% 
                             {if(length(.)==0) NA else .})
    }
  }
  # add the columns from links of link to the corresponding dataframe
  df_metacritic <- mutate(df_metacritic,developer,genre,num_critic_reviews,num_user_rating)
  
  return (df_metacritic)
}

# Run loop to scrape all pages
all_dfs <- data.frame()
for (num in seq(1,meta_pages)){
  all_dfs <- bind_rows(all_dfs, scrape_metacritic(num))
  write.csv(finaldf,"C:/Users/dujin/Documents/Study/Boston College/Fall 2022/DA in Practice/metagame_data.csv",row.names=FALSE)
}


# Data Processing ---------------------------------------------------------

# read csv
meta <- read_csv('metagame_data.csv')
vg <- read_csv('gamesales.csv')

# Clean Platforms
distinct(meta,platform) %>% print(n=22)
distinct(vg,console) %>% print(n=80)
vg1 <- vg %>% filter(console %in% c('N64','PS','PS3','DC','X360','Wii',
                                           'XOne','PC','NS','PS2','PS4','GC','XB',
                                           'WiiU','XS','PS5','GBA','3DS','DS','PSV','PSP'))%>% 
  mutate(console = case_when(console=='N64'~'Nintendo 64',
                             console=='PS'~'PlayStation',
                             console=='PS3'~'PlayStation 3',
                             console=='DC'~'Dreamcast',
                             console=='X360'~'Xbox 360',
                             console=='XOne'~'Xbox One',
                             console=='NS'~'Switch',
                             console=='PS2'~'PlayStation 2',
                             console=='PS4'~'PlayStation 4',
                             console=='GC'~'GameCube',
                             console=='XB'~'Xbox',
                             console=='WiiU'~'Wii U',
                             console=='XS'~'Xbox Series X',
                             console=='PS5'~'PlayStation 5',
                             console=='GBA'~'Game Boy Advance',
                             console=='PSV'~'PlayStation Vita',
                             TRUE~console)) %>%
  rename('game'='game_name','platform'='console')
vgfinal <- vg1 %>% mutate(sales=1000000*as.numeric(str_extract(string=sales,pattern='[0-9\\.]+'))) %>% 
  mutate(game=str_replace_all(game,'�','e'))

vgfinal %>% filter(!is.na(sales))
vg %>% filter(sales != 'N/A')

# Join Data - fuzzyjoin - failed
meta1 <- meta %>% mutate(game = stri_enc_toutf8(game,validate=TRUE))
df <- stringdist_left_join(meta1, vgfinal, by = c('game','platform'), max_dist = 1, 
                     method = "jw", distance_col='dist')

# Join Data - basicR join
df <- left_join(meta, vgfinal, by = c('game','platform')) %>% select(-page)
df %>% filter(!is.na(sales))

# Clean genre
dfgenre<- df %>% mutate(main_genre=str_extract(genre, '[A-Za-z0-9 -]+(?=,)'),
                      sub_genre=str_extract(genre,'(?<=, )[A-Za-z0-9 -]+'))

dfgame <- dfgenre %>% filter(!duplicated(dfgenre))

write.csv(dfgame,"C:/Users/dujin/Documents/Study/Boston College/Fall 2022/DA in Practice/dfgame_basicRjoin.csv",row.names=FALSE)

# Summary Statistics ------------------------------------------------------

dfgame <- read_csv('dfgame_basicRjoin.csv')

# Mean critic score: 70.72467/100
mean(dfgame$metascore)

# Mean user score: 6.970875/10
mean(dfgame$userscore,na.rm=T)

# Number of games on each platform: PC(5219),PS4(2114), Xbox 360(1665).
count(dfgame,platform) %>% arrange(-n)

# Number of games developed by each developer: Capcom(316), Telltale Games(227),EA Sports(192). 
count(dfgame,developer) %>% arrange(-n)

# Mean number of critic reviews: 22.90577
mean(dfgame$num_critic_reviews,na.rm=T)

# Mean number of user rating: 210.504
mean(dfgame$num_user_rating,na.rm=T)

# Number of games in each main genre: Action(6353), Action Adventure(2459), Role-Playing(1970).
count(dfgame, main_genre) %>% arrange(-n)

# Number of games in each sub genre: General(2479), Shooter(2025),Platformer(1306)
count(dfgame, sub_genre) %>% arrange(-n)

# Number of games in each year: 2018(1151), 2020(1091),2017(1058)
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year) %>% summarize(n=n()) %>% arrange(-year) %>% print(n=30)
    #ggplot(aes(x=year,y=n))+geom_point()+geom_smooth()+
  scale_y_continuous(name='Number of Games',limits=c(0,1200),breaks=seq(0,1200,by=200))+
  scale_x_continuous(name='Year',limits=c(1995,2022),breaks=seq(1995,2022,by=3))+
  labs(title='Number of Games per Year from 1995 to 2022')+ theme_light()

# Mean Sales:2301409
mean(dfgame$sales,na.rm=T)

# Mean metascore by year
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year) %>% summarize(score=mean(metascore)) %>% arrange(year) %>% 
  ggplot(aes(x=year,y=score))+geom_point()

# Mean userscore by year
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year) %>% summarize(score=mean(userscore,na.rm=T)) %>% arrange(year) %>% 
  ggplot(aes(x=year,y=score))+geom_point()

# Mean sales by year
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year) %>% summarize(sale=mean(sales,na.rm=T)) %>% arrange(year) %>% 
  ggplot(aes(x=year,y=sale))+geom_point()

# metascore by main genre (more than 5 games in each genre): Action RPG(78.9), PC-style RPG(77.9), Japanese-Style(76.6)
dfgame %>% group_by(main_genre) %>% summarize(score=mean(metascore),n=n()) %>% filter(n>=5) %>% arrange(-score)

# userscore by main genre: Console-stype RPG(7.87), Fighting(7.66),Fantasy(7.36)
dfgame %>% group_by(main_genre) %>% summarize(score=mean(sales,na.rm=T),n=n()) %>% filter(n>=5) %>% arrange(-score)

# best user-scored genre each year (with more than 5 games in genre)
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year,main_genre) %>% summarize(meta=mean(metascore),user=mean(userscore,na.rm=T),n=n()) %>% 
  filter(n>=5, main_genre !='NA') %>% arrange(-user) %>% slice(1) %>% print(n=50)

# Number of games on each platform by year
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year,platform) %>% summarize(n=n()) %>% arrange(-n) %>% slice(1) %>% print(n=50)

# Mean score of platform by year (more than 5 games on the platform)
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year,platform) %>% summarize(meta=mean(metascore),user=mean(userscore,na.rm=T),n=n()) %>% 
  filter(n>=5) %>% arrange(-user) %>% slice(1) %>% print(n=50)

# Plot userscore against sales
dfgame %>% ggplot(aes(x=metascore,y=sales)) +geom_point()

# best scored genre over time
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year,main_genre) %>% summarize(meta=mean(metascore,na.rm=T),n=n()) %>% filter(n>=3) %>%
  arrange(-meta) %>% slice(1) %>% print(n=30)

dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year) %>% summarize(num = mean(num_critic_reviews,na.rm=T)) %>% arrange(year) %>%
  ggplot(aes(x=year,y=num)) +geom_point()
                             
dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year) %>% distinct(developer) %>% summarize(n=n())

dfgame %>% mutate(year=year(release_date))%>%
  group_by(year, main_genre) %>% summarize(meta = mean(metascore)) %>% arrange(-year) %>% ungroup() %>%
  group_by(main_genre) %>% summarize(diff = first(meta)-last(meta)) %>% arrange(-diff)

dfgamemmo <- dfgame %>% filter(main_genre =='Third-Person')

dfgame %>% mutate(year=year(release_date)) %>%
  group_by(year,main_genre) %>% summarize(n=n()) %>% arrange(-n) %>% slice(1) %>%print(n=50)

dfgame %>% filter(main_genre=='Driving') %>% mutate(year=year(release_date)) %>% group_by(year) %>%
  summarize(meta=mean(metascore),n=n()) %>% print(n=30)

dfgame %>% filter(num_user_rating < 30000) %>% ggplot(aes(x=num_user_rating,y=userscore)) +geom_point()+geom_smooth(method=lm)

dfgame %>% group_by(developer) %>% summarize(sale = mean(sales,na.rm=T), meta=mean(metascore,na.rm=T),
                                             user=mean(userscore,na.rm=T),numcritic=mean(num_critic_reviews,na.rm=T),
                                             numrating=mean(num_user_rating,na.rm=T),n=n()) %>% filter(n>=3) %>%
  arrange(-numrating)
