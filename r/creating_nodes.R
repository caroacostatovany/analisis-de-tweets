library(tidyverse)
library(tidygraph)
library(ggraph)
#library(network)
rm(list=ls())
df <- read.csv('/Volumes/MemoriaEle/HeavyData/tweets_sismo/sismo_cdmx_sin_repeticiones_12052021.csv')

usuarios <- tibble(df$username, df$usuarios_mencionados)

s <- strsplit(df$usuarios_mencionados, split =',')

usuarios <- tibble(u_propietario = rep(df$username, sapply(s, length)),
                       u_mencionado = unlist(s))


usuarios$u_mencionado <- str_replace_all(usuarios$u_mencionado, " ", "")

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

#plot(red_usuarios, vertex.cex = 3)






