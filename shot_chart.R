library(tidyverse)
library(NBAr)
library(ggrepel)



setwd('C://Users//mogon//Desktop//Layups & Threes//ShotChart')


shots <- read_csv('all_19-20_shots.csv')
shots <- shots %>% select(player_name,action_type) %>% group_by(player_name) %>% 
 dplyr::count(action_type)
shots$action_type %>% unique()

player <- shots %>% dplyr::group_by(player_name) %>% dplyr::summarize(Total=sum(n))
shots <- shots %>% pivot_wider(names_from = action_type,values_from=n,values_fill=0 )
shots <- inner_join(player,shots)
shots <- shots %>% mutate_at(3:48,.funs=~./Total)
shots <- shots %>% filter(Total>100) %>% select(-Total)

set.seed(42)
clusters <- kmeans(shots[,2:47],centers=6)
clusters$betweenss/clusters$totss
shots$cluster <- clusters$cluster
pca <- prcomp(shots[,2:47])

shot_profiles <- shots %>% select(player_name,cluster) %>% mutate(cluster=case_when(
  cluster==1~'Arremessador Versátil',
  cluster==2~'Arremessador Parado',
  cluster==3~'Big Interior',
  cluster==4~'Big Espaçador',
  cluster==5~'Infiltrador',
  cluster==6~'Criador de Arremessos'
  
))
shot_profiles$cluster <- factor(shot_profiles$cluster,levels = c('Arremessador Parado',
                                                   'Arremessador Versátil',
                                                   'Criador de Arremessos',
                                                   'Infiltrador',
                                                   'Big Interior',
                                                   'Big Espaçador'))

shot_profiles$PC1 <- scale(pca$x[,1])
shot_profiles$PC2 <- scale(pca$x[,2])

base_stats <- get_general(2019,type='Player',measure_type = 'Base') %>% select(player_name,pts)
shot_profiles <- inner_join(shot_profiles,base_stats)
shot_profiles2 <- shot_profiles %>% group_by(cluster) %>% filter(pts>quantile(pts,0.75))

g <- ggplot(shot_profiles,aes(x=PC1,y=PC2))+
  geom_point(aes(color=cluster))+
  geom_text_repel(data=shot_profiles2,aes(color=cluster,label=player_name))+
  labs(title='Classes de jogadores por tipo de arremesso executado')+theme_minimal()
g
ggsave('nba_shot_class.png',g,device = 'png',height =9,width=15)

clust <- shots %>% group_by(cluster) %>% summarise_at(2:47,mean) %>% 
  mutate_at(2:47,~dense_rank(desc(.)))

shot_profiles %>% select(player_name,cluster) %>% write_csv('shot_classes.csv')

