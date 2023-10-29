library(tidyverse)
library(stringr)
library(fuzzyjoin)
library(stringdist)

fuzzy_join <- read.csv('/Users/minjikim/Desktop/Current Semester/Data Analytics/Final Project/final_fuzzy.csv')

# Sales changes in each genre


#Extract Years from Released Dates

scores_data  <- fuzzy_join %>% mutate(year = str_extract(release_date, "[:digit:]*(?=/)"))%>%select(release_date,game_name.meta,genre,metascore, year, userscore) %>% arrange(release_date) %>% distinct()


score_group <-scores_data %>% filter(!is.na(userscore)) %>% group_by(year) %>% summarise(mean_metascore = mean(metascore), mean_userscore = mean(userscore)*10)

#Pivot_Longer Table
score_group <- score_group %>% 
  pivot_longer(
    cols = starts_with('mean_'), 
    names_to = 'score_type', 
    names_prefix = 'mean_', 
    values_to = 'scores'
  ) 


ggplot(data = score_group, aes(x = year, y = scores, group = score_type,color = score_type)) +
  geom_point() + 
  geom_line()+
  theme_light() +
  labs(
    title = "Users' and Critics' Scores over Time",
    x = 'Year',
    y = 'Score') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 22)) 
# Plot scores over years
ggplot(data = score_group) +
  geom_point(aes(x = year, y = scores, color = score_type)) + 
  geom_smooth(aes(x = year, y = scores, color = score_type))
stat_smooth(method = "lm", se = FALSE) +
  theme_light() +
  labs(
    title = "Users' and Critics' Scores over Time",
    x = 'Year',
    y = 'Score') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 22)) 

#How out which genre has the highest numbers of games (TOP 6)
scores_data %>% filter(!is.na(userscore)) %>% group_by(genre) %>% summarise(mean_metascore = mean(metascore), mean_userscore = mean(userscore*10), count = n()) %>% filter(count >3) %>% arrange(-count) %>% head(6)

#Group games based on genre and year (TOP 6)
genre_score_group <- scores_data %>% filter(!is.na(userscore)) %>% group_by(year, genre) %>% summarise(mean_metascore = mean(metascore), mean_userscore = mean(userscore)*10, count = n()) %>% filter(count >3, genre %in% c('Action', 'Action Adventure', 'Playing', 'Sports', 'Strategy', 'Miscellaneous'))

#Pivot_Longer Table
genre_score_group <- genre_score_group %>% 
  pivot_longer(
    cols = starts_with('mean_'), 
    names_to = 'score_type', 
    names_prefix = 'mean_', 
    values_to = 'scores'
  ) 

# Plot scores over time by each genre
ggplot(data = genre_score_group, aes(x = year, y = scores, group = score_type,color = score_type)) +
  geom_point() + 
  geom_line()+
  theme_light() +
  facet_wrap(~ genre) +  
  labs(
    title = "Users' and Critics' Scores by Genres (TOP 6)",
    x = 'Year',
    y = 'Score') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 22))

# Biggest Gap In Action (2006 & 2022)
diff_scores_ACTION <- scores_data %>% filter(!is.na(userscore)) %>% group_by(year, genre) %>% summarise(mean_metascore = mean(metascore), mean_userscore = mean(userscore*10), diff = mean_userscore- mean_metascore) %>% filter(genre =='Action') 

# Biggest Gap In Adventure (2004 & 2022)
diff_scores_ADVENTURE <- scores_data %>% filter(!is.na(userscore)) %>% group_by(year, genre) %>% summarise(mean_metascore = mean(metascore), mean_userscore = mean(userscore)*10, diff = mean_userscore- mean_metascore) %>% filter(genre =='Action Adventure') 

# Biggest Gap In Miscellaneous (2002 & 2022)
diff_scores_MISCELLANEOUS <- scores_data %>% filter(!is.na(userscore)) %>% group_by(year, genre) %>% summarise(mean_metascore = mean(metascore)/10, mean_userscore = mean(userscore), diff = mean_userscore- mean_metascore) %>% filter(genre =='Miscellaneous') 

#List of Games in Action either in 2006 or 2022
action_games <- scores_data %>% filter(!is.na(userscore))  %>% mutate(metascore = metascore/10, diff = userscore- metascore) %>% filter(genre =='Action', year == 2006) %>% arrange(-diff)

#List of Games in Action either in 2004 or 2022
adventure_games <- scores_data %>% filter(!is.na(userscore))  %>% mutate(metascore = metascore/10, diff = userscore- metascore) %>% filter(genre =='Action Adventure', year == 2022) %>% arrange(diff)

#List of Games in Miscellaneous either in 2002 or 2022
misscell_games <- scores_data %>% filter(!is.na(userscore))  %>% mutate(userscore = userscore*10, diff = userscore- metascore) %>% filter(genre =='Miscellaneous', year == 2002) %>% arrange(-diff)

#List of Games in Sports in 2019
group_sports <- scores_data %>% filter(!is.na(userscore))  %>% mutate(metascore = metascore/10, diff = userscore- metascore) %>% filter(genre =='Sports', year==2019) %>% arrange(diff)

