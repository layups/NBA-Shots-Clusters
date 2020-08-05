# Pacotes Necessários
library(NBAr)
library(tidyverse)
library(ggrepel)
extrafont::loadfonts(device = 'win')

# Consolidação dos Dados Iniciais
shots <- read_csv('all_19-20_shots.csv') %>% 
  count(player_name, action_type)

player <- shots %>% 
  group_by(player_name) %>% 
  summarize(total=sum(n))

total_shots <- shots %>% 
  pivot_wider(names_from = action_type, values_from=n, values_fill=0 ) %>% 
  inner_join(player, 'player_name') %>% 
  mutate(across(2:47, ~ .x/total)) %>%
  filter(total>100) %>% 
  select(-total)

# Criação de Clusters
set.seed(42)
clusters <- kmeans(total_shots[,2:47],centers=6)
clusters$betweenss/clusters$totss

df_cluster <- total_shots %>% 
  bind_cols(cluster = clusters$cluster)

pca <- prcomp(df_cluster[,2:47])

# Definição dos perfis
df_profiles <- df_cluster %>% 
  select(player_name, cluster) %>% 
  mutate(cluster=case_when(
    cluster==1~'Arremessador Versátil',
    cluster==2~'Arremessador Parado',
    cluster==3~'Big Interior',
    cluster==4~'Big Espaçador',
    cluster==5~'Infiltrador',
    cluster==6~'Criador de Arremessos'
    )) %>% 
  mutate(cluster = factor(cluster, levels = c('Arremessador Parado', 
                                              'Arremessador Versátil', 
                                              'Criador de Arremessos',
                                              'Infiltrador',
                                              'Big Interior',
                                              'Big Espaçador')
                          )
         ) %>% 
  bind_cols(PC1 = scale(pca$x[,1]),
            PC2 = scale(pca$x[,2]))


# Df Final
base_stats <- get_general(2019,type='Player',measure_type = 'Base') %>% 
  select(player_name, pts)

df_final <- df_profiles %>% 
  inner_join(base_stats, 'player_name') %>% 
  group_by(cluster) %>% 
  mutate(best_players = case_when(pts > quantile(pts, 0.75) ~ 'sim', T ~ 'nao')) %>% 
  ungroup()

# Visualização
df_final %>% 
  ggplot(aes(x = PC1, y = PC2))+
  geom_point(aes(color=cluster))+
  geom_text_repel(data = filter(df_final, best_players == 'sim'), 
                  aes(color=cluster,label=player_name))+
  labs(x = NULL, y = NULL, color = NULL,
       title = 'Qual o tipo de arremesso preferido de cada jogador?',
       subtitle = 'Classificação dos jogadores com base no estilo ofensivo',
       caption = 'Dados: Nba.com') + 
  hrbrthemes::theme_ft_rc() +
  guides(color = guide_legend(override.aes = list(size=rel(3)))) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'top',
        legend.text = element_text(color = 'white', 
                                   size = rel(1)),
        plot.caption = element_text(color = 'white', 
                                    face = 'bold',
                                    size = rel(1.2)),
        plot.title = element_text(size = rel(2)),
        plot.subtitle = element_text(size = rel(1.2)))

ggsave('nba_shot_class.png',device = 'png',height =9,width=15, dpi = 800)

# Salva as classes
rank_clusters <- df_cluster %>% 
  group_by(cluster) %>% 
  summarise(across(2:47, mean)) %>% 
  mutate_at(2:47,~dense_rank(desc(.)))

df_final %>% 
  select(player_name, cluster) %>% 
  write_csv('shot_classes.csv')


read_csv('shot_classes.csv')
