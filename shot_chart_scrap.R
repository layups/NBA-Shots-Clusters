library(tidyverse)
library(NBAr)

players <- get_players(2019) %>% pull(player_id)
shots <- lapply(players,get_shotchart,season=2019)
data <- data.frame()
for(i in 1:540){
  data <- bind_rows(data,shots[[i]])
}

write_csv(data,'all_19-20_shots.csv')
