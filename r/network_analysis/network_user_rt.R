library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraphdata)

rm(list=ls())
theme_set(theme_minimal(base_size = 14))

# LIMPIEZA
df <- read.csv('/Volumes/MemoriaEle/HeavyData/tweets_sismo/tweets_limpios_2021_05_12.csv')

dim(df)
names(df)
head(df[,c(1,2,4,5,6)])

usuarios <- tibble(df$username, df$retweet_user)

# hacemos una separación de todos los usuarios mencionados
# que están en un solo renglón
s <- strsplit(df$retweet_user, split =',')
usuarios <- tibble(u_propietario = 
                     rep(df$username, sapply(s, length)),
                   u_retuiteado = unlist(s))

# quitamos espacios
usuarios$u_retuiteado <- str_replace_all(usuarios$u_retuiteado, " ", "")
usuarios$u_retuiteado <- str_replace_all(usuarios$u_retuiteado, ":", "")


# RED: red dirigida

# tbl_graph identifica los nodos y los vertices
users_nodes_edges <- usuarios %>% as_tbl_graph()

# obtenemos los vertices
vertices_users <- users_nodes_edges %>% 
  activate(edges) %>% 
  select(to, from) %>% 
  as_tibble()
vertices_users

# agregamos los vertices por frecuencia
vertices_agregados_users <- vertices_users %>% 
  group_by(to, from) %>% 
  summarise(freq_vert = n())
vertices_agregados_users

# observamos los más frecuentes
vertices_agregados_users %>% 
  arrange(desc(freq_vert)) %>% 
  filter(freq_vert>10) %>% 
  ggplot(aes(x=freq_vert)) +
  geom_histogram(bins = 50,col='blue',
                 fill='blue',alpha=0.7)+
  xlab('Frecuencia de aristas rt')+
  ylab('')+
  ggsave("~/Documents/EquiposGit/analisis-de-tweets/docs/images/histogram_rt.png")#,

# obtenemos solo los nodos 
nodos_users <- users_nodes_edges %>% 
  activate(nodes) %>% 
  as_tibble() 
nodos_users

# volvemos a armar la red
users_nodes_edges_2 <- tbl_graph(nodes = nodos_users, 
                                 edges = vertices_agregados_users) 
users_nodes_edges_2

# filtramos para los que tienen más frecuencia
corte_freq_vert <- 20
users_grandes <- users_nodes_edges_2 %>% 
  activate(edges) %>% 
  filter(freq_vert > corte_freq_vert) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) #eliminar nodos que quedan sin conexiones

users_grandes %>% 
  activate(nodes) %>% 
  ggraph(layout = 'fr') + 
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
                 alpha = 0.5, colour="gray") + 
  geom_node_point(alpha=0.5,colour='turquoise3',size=3) +
  geom_node_text(aes(label=name), repel = TRUE, size=2)+
  theme_graph(base_family = "sans") +
  ggsave("~/Documents/EquiposGit/analisis-de-tweets/docs/images/red_simple_mencion_rt.png")#,


# Componentes
compo_users <-  users_grandes %>% 
  activate(nodes) %>% 
  mutate(componente = group_components())

compo_users %>% 
  as_tibble %>% 
  group_by(componente) %>% 
  tally()

# filtramos por componente conexa más grande
usi <- users_grandes %>% 
  activate(nodes) %>% 
  mutate(componente = group_components()) %>% 
  filter(componente == 1)

# Calculamos la intermediación
usi <- usi %>% activate(nodes) %>% 
  mutate(intermediacion = centrality_betweenness())

# Graficamos
usi %>% 
  activate(nodes) %>% 
  ggraph(layout = 'fr') +
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
                 alpha = 0.3, colour="gray") + 
  geom_node_point(aes(size = intermediacion),
                  colour='orange') +
  geom_node_text(aes(label=name),repel = TRUE,  size=2)+
  theme_graph(base_family = "sans")+
  ggsave("~/Documents/EquiposGit/analisis-de-tweets/docs/images/red_intermediacion_mencion_rt.png")#,


usi <- usi %>%
  activate(nodes) %>% 
  mutate(central_eigen = centrality_eigen())


usi %>%
  activate(nodes) %>% 
  ggraph(layout = 'graphopt', spring.constant = 0.25, charge = 0.05, niter = 300) + 
  geom_edge_link2(arrow = arrow(length = unit(2, 'mm')), alpha = 0.05, colour="gray") + 
  geom_node_point(aes(size = central_eigen, colour=central_eigen)) +
  geom_node_text(aes(label=name),  repel = TRUE,size=2)+
  theme_graph() +
  scale_color_viridis()+
  ggsave("~/Documents/EquiposGit/analisis-de-tweets/docs/images/red_centraleigen_mencion_rt.png")#,

usi %>%
  activate(nodes) %>% 
  ggraph(layout = 'graphopt', spring.constant = 0.25, charge = 0.05, niter = 300) + 
  geom_edge_link2(arrow = arrow(length = unit(2, 'mm')), alpha = 0.2, colour="gray") + 
  geom_node_point(aes(size = central_eigen, colour=central_eigen)) +
  geom_node_text(aes(label=name),  repel = TRUE,size=2)+
  theme_graph() +
  scale_color_viridis() +
  ggsave("~/Documents/EquiposGit/analisis-de-tweets/docs/images/red_centraleigenpro_mencion_rt.png")#,









