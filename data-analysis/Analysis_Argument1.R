library(tidyverse)
library(mosaic)
library(stringr)
library(lubridate)
library(ggplot2)
library(gganimate)

#read the data
sales <- read.csv('final_fuzzy.csv')

#data cleaning 

##data manipulation
#number of games analysis
  sales1 <- sales %>% 
    mutate(release_date = ymd(release_date)) %>% 
    mutate(year = year(release_date)) %>%
    filter(genre != 'NA') %>%
    filter(year != '1995') %>%
    mutate(genre = case_when(genre =='Up' ~'Beat Em Up',
                             genre == 'D' ~ '3D',
                             genre == 'Fi' ~ 'Sci-Fi',
                             genre == 'Based' ~ 'Turn-Based',
                             genre == 'Playing' ~ 'Role-Playing',
                             TRUE~genre
      
    ))

  sales_num_game<- sales1 %>% 
    group_by(year) %>% 
    mutate(year_num_game = n())
  
  sales_num_game %>%
    group_by(genre) %>% 
    summarise(sum_game_genre = n()) %>%
    arrange(desc(sum_game_genre))
  
  sales_num_game2 <- sales_num_game %>%
    group_by(genre) %>% 
    mutate(sum_game_genre = n()) %>%
    ungroup() %>%
    filter(sum_game_genre >= 600)
  
  sales_num_game3<- sales_num_game2 %>% 
    group_by(year,genre) %>% 
    mutate(num_game_released = n())

  #number of games released on each console analysis
  sales_num_game3<- sales_num_game3 %>% 
    group_by(year,platform.x) %>% 
    mutate(num_game_console = n())

#developers analysis
sales_num_dev<- sales_num_game3 %>%
  group_by(year) %>%
  mutate(num_dev = n_distinct(developer))

#review analysis
  #add total number of reviews column 
  sales_review <- sales_num_game3 %>%
    group_by(game_name.x) %>%
    mutate(num_review = num_critic_reviews)

  #add total number of reviews of each year
  sales_review1 <- sales_review %>%
    group_by(year) %>%
    mutate(total_review = sum(num_review, na.rm=T))
  
#sales analysis
  #add total sales each year
  salesdf <- sales_num_game3 %>%
    group_by(year) %>%
    mutate(total_sales = sum(sales, na.rm=T))
  
  #add total sales of each genre each year
  salesdf <- salesdf %>%
    group_by(year, genre) %>%
    mutate(total_sales_genre = sum(sales, na.rm=T)) 

##regression
  # number of games
    #before 2002
    sales_pre2001 <- sales_num_game3 %>% filter(year <= 2002)
    reg_yr_gamenum_pre2001 <- lm(year_num_game ~ year, data = sales_pre2001)
    coef(reg_yr_gamenum_pre2001)
    summary(reg_yr_gamenum_pre2001)
    
    #after 2002 but before 2015
    sales_btw20012005 <- sales_num_game3 %>% filter(year > 2002) %>% filter(year < 2015)
    reg_yr_gamenum_btw20012005 <- lm(year_num_game ~ year, data = sales_post2001)
    coef(reg_yr_gamenum_btw20012005)
    summary(reg_yr_gamenum_btw20012005)
    
    #after 2015 before 2020
    sales_post2015 <- sales_num_game3 %>% filter(year > 2015) %>% filter(year < 2019)
    reg_yr_gamenum_post2015 <- lm(year_num_game ~ year, data = sales_post2001)
    coef(reg_yr_gamenum_post2015)
    summary(reg_yr_gamenum_post2015)
    
    #from start to 2019
    sales_to2019 <- sales_num_game3 %>% filter(year <= 2019)
    reg_yr_gamenum_to2019 <- lm(year_num_game ~ year, data = sales_to2019)
    coef(reg_yr_gamenum_to2019)
    summary(reg_yr_gamenum_to2019)
    
    
  # number of devs
    #before 2001
    devs_pre2001 <- sales_num_dev %>% filter(year <= 2002)
    reg_yr_devnum_pre2001 <- lm(num_dev ~ year, data = devs_pre2001)
    coef(reg_yr_devnum_pre2001)
    summary(reg_yr_devnum_pre2001)
    
    #after 2001 but before 2015
    devs_btw20012005 <- sales_num_dev %>% filter(year > 2002) %>% filter(year < 2015)
    reg_yr_devnum_btw20012005 <- lm(num_dev ~ year, data = devs_pre2001)
    coef(reg_yr_devnum_btw20012005)
    summary(reg_yr_devnum_btw20012005)
    
    #after 2015
    devs_post2015 <- sales_num_dev %>% filter(year > 2015)
    reg_yr_devnum_post2015 <- lm(num_dev ~ year, data = devs_post2015)
    coef(reg_yr_devnum_post2015)
    summary(reg_yr_devnum_post2015)
    
  #number of total reviews 

    reg_yr_gamenum <- lm(year_num_game ~ year, data = sales_num_game3)
    coef(reg_yr_gamenum)
    summary(reg_yr_gamenum)


##graphing

#Number of Games
  #Number of games over the years
  ggplot(data = sales_num_game, aes(x=year, y = year_num_game)) + 
    geom_line(size=1) +
    labs(
      title = "Changes in the Total Number of Games Released Each Year",
      x = 'Year',
      y = 'Number of Video Games'
    ) +
    theme_light()

  #number of games by genre over the years 
    #line chart
    ggplot(data = sales_num_game3, aes(x=year, y = num_game_released, color = genre)) + 
      geom_line(size=1) +
      labs(
        title = "Changes in the Total Number of Games of Different Genre Released Each Year",
        x = 'Year',
        y = 'Number of Video Games'
      ) +
      theme_light()
    
    #point chart
    ggplot(data = sales_num_game3, aes(x=year, y = num_game_released, size = num_game_released, color = genre)) + 
      geom_point() +
      labs(
        title = "Changes in the Total Number of Games of Different Genre Released Each Year",
        x = 'Year',
        y = 'Number of Video Games'
      ) +
      theme_light()
    
    p_genre <- ggplot(sales_num_game3, aes(x=year, y = num_game_released, size = num_game_released, colour = genre)) +
      geom_point(alpha = 0.7, show.legend = FALSE) +
      scale_size(range = c(2, 12)) +
      scale_x_log10() +
      facet_wrap(~genre) +
      theme_light()
    p_genre
    
    gif_genre <- p_genre + 
      labs(title = 'Year: {frame_time}', x = 'Year', y = 'Number of Video Games Released') +
      transition_time(year) +
      ease_aes('linear')
    animate(gif_genre)
  
  #number of games released on each console over the years
    ggplot(data = sales_num_game3, aes(x=year, y = num_game_console, color = platform.x)) + 
      geom_line(size=1) +
      labs(
        title = "Changes in the Total Number of Games of Different Genre Released Each Year",
        x = 'Year',
        y = 'Number of Video Games'
      ) +
      theme_light()
    
    p_console <- ggplot(sales_num_game3, aes(x=year, y = num_game_console, size = num_game_console, colour = platform.x)) +
      geom_point(alpha = 0.7, show.legend = FALSE) +
      scale_size(range = c(2, 12)) +
      scale_x_log10() +
      facet_wrap(~platform.x) +
      theme_light()
    p_console
    gif_console <- p_console + 
      labs(title = 'Year: {frame_time}', x = 'Year', y = 'Number of Video Games') +
      transition_time(year) +
      ease_aes('linear')
    animate(gif_console)

#Number of Developers over the years
  ggplot(data = sales_num_dev, aes(x=year, y = num_dev)) +
    geom_line(size=1)+
    labs(
      title = "Changes in the Total Number of Developers Each Year",
      x = 'Year',
      y = 'Number of Developers'
    ) +
    theme_light()

#Number of Reviews over the years
  ggplot(data = sales_review1, aes(x=year, y = total_review)) +
    geom_line(size=1)+
    labs(
      title = "Changes in the Total Number of Reviews Each Year",
      x = 'Year',
      y = 'Number of Reviews'
    ) +
    theme_light()

#Sales
    #Total sales over the years
    ggplot(data = salesdf, aes(x=year, y = total_sales)) +
      geom_line()+
      labs(
        title = "Changes in the Total Sales of Video Games Each Year",
        x = 'Year',
        y = 'Copies Sold'
      ) +
      theme_light()
    
    #Sales by genre
    ggplot(data = salesdf, aes(x=year, y = total_sales_genre, color = genre)) + 
      geom_line() +
      labs(
        title = "Changes in the Total Sales of Video Games of Different Genre Each Year",
        x = 'Year',
        y = 'Copies Sold'
      ) +
      theme_light()


