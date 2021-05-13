library(tidyverse)
library(tidygraph)
library(ggraph)
#library(network)
rm(list=ls())

# LIMPIEZA
df <- read.csv('/Volumes/MemoriaEle/HeavyData/tweets_sismo/sismo_cdmx_sin_repeticiones_12052021.csv')

usuarios <- tibble(df$username, df$usuarios_mencionados)

s <- strsplit(df$usuarios_mencionados, split =',')

usuarios <- tibble(u_propietario = rep(df$username, sapply(s, length)),
                       u_mencionado = unlist(s))


usuarios$u_mencionado <- str_replace_all(usuarios$u_mencionado, " ", "")



#################version raw(NO HACER CASO)####################
head(usuarios)

de_donde <- usuarios %>% 
  distinct(u_propietario) %>% 
  rename(label = u_propietario)

a_donde <- usuarios %>% 
  distinct(u_mencionado) %>% 
  rename(label = u_mencionado)

nodos <- full_join(de_donde, a_donde, by = "label")
nodos

nodos <- nodos %>% rowid_to_column("id")
nodos

repeticiones <- usuarios %>%  
  group_by(u_propietario, u_mencionado) %>%
  summarise(weight = n()) %>% 
  ungroup()

aris <- repeticiones %>% 
  left_join(nodos, by = c("u_propietario" = "label")) %>% 
  rename(from = id)

aris <- aris %>% 
  left_join(nodos, by = c("u_mencionado" = "label")) %>% 
  rename(to = id)

aris <- select(aris, from, to, weight)

# agregamos

aris_agregados <- aris %>% 
  group_by(to, from) %>% 
  summarise(pax = sum(weight))

nodos_users <- 

#dim(edges)
#edges <- sample_n(edges,100)

#red_usuarios <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)



############version FElipe#########

users_chidos <- usuarios %>% as_tbl_graph()


vertices_users <- users_chidos %>% 
  activate(edges) %>% 
  select(to, from) %>% as_tibble()

# agregar
vertices_agregados_users <- vertices_users %>% 
  group_by(to, from) %>% 
  summarise(pax = n())

vertices_agregados_users %>% 
  arrange(desc(pax)) %>% 
  filter(pax>3) %>% 
  tail()



# nodos, y agregar estado
nodos_users <- users_chidos %>% activate(nodes) %>% 
  as_tibble() 

users_chidos_2 <- tbl_graph(nodes = nodos_users, edges = vertices_agregados_users) 
users_chidos_2


mouse <- users_chidos_2 %>% activate(edges) %>% 
  select(from, to , pax)
quantile(pull(mouse, pax), seq(0, 1, 0.1))

corte_pax <- 15
users_grandes <- users_chidos_2 %>% activate(edges) %>% 
  filter(pax > corte_pax) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) #eliminar nodos que quedan sin conexiones

users_grandes %>% 
  activate(nodes) %>% 
  #mutate(color_ca = ifelse(estado == "CA", "CA", "Otros")) %>% 
  ggraph(layout = 'fr', niter = 2000) + 
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), alpha = 0.1, colour="gray") + 
  geom_node_point(aes(colour='blue'),alpha=0.5) +
  geom_node_text(aes(label=name),  size=2)+
  theme_graph(base_family = "sans")





















