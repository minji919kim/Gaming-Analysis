# Background
The video game industry is booming and attracting a diverse range of players in recent years. However, the overall positive trend does not represent the environments and stories underneath, including changes in gamers’ preferences, popularity trends, media critics, game qualities, potential bias, etc. A great example is the backlashes from gamers to one of the most popular games in 2020 – The Last of Us Part II. This game received an average rating of 93 out of 100 from 121 critics, which was the second-highest Metacritic score in 2020. However, it receives a poor 5.7 out of 10 from 160,000 players. The large gap between critics’ and gamers’ reviews inevitably led us to question whether video game developers nowadays pay too much attention to meet the expectations of critics yet ignore the interests of common gamers. Driven by questions like this, our team was eager to learn more about the trends and changes underneath the fast expansion of the video game industry and present a story based on our findings.

# Objective
Advanced data scraping and visualization techniques have been employed in this project to unveil nuanced insights. We aim to uncover subtle patterns amidst the flourishing trajectory of the gaming industry. By doing so, we help them make informed decisions that cater both to their audience's preferences and their own profitability. 

# Dataset 
To get our data, we scraped two websites: Metacritic and VGChartz. From the main pages of Metacritic, we acquired information about the names, platforms, release dates, meta scores, user scores, and URL links. We ran a for loop within the function to click into each URL and scrape more information regarding the developers, genres, and the number of critic reviews and user ratings. For VGChartz, the only information provided is the sales of each game and its console. 

# Data Preprocessing
During the data scraping phase, we asked permission from web servers using the bow command from the polite package. We primarily used the rvest package to gather relevant information. To merge two datasets, we standardized game titles, like adjusting special characters in titles like those in Pokémon games. After refining the titles, we employed the fuzzyjoin function with a max_dist of 0 to combine the datasets, ensuring they matched perfectly, and the strict matching might lead to some game omissions. The final merged dataset contained 13,805 entries. To align metascores with userscores, we adjusted the latter by multiplying by 10, setting both on a 100-point scale.

# Findings 
Before delving into the data, we made three null hypotheses:
1. The video game industry hasn't changed significantly in the past 30 years.
2. Game scores and ratings aren't related.
3. There's minimal difference between critics' and users' scores.
However, our findings contradicted these hypotheses with various regressions and evidence:
1. The video game industry has largely evolved over the last three decades, though certain areas like sports games and the PC market may have exceptions.
2. There is a clear connection between game scores and ratings: games with E and PG ratings typically have higher user scores, while M and T-rated games often score better with critics.
3. There is a notable gap between critics’ and users’ scores, likely due to inherent biases and differing priorities among the two groups.


