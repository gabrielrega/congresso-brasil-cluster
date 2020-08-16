library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(stringr)

df <- read_csv2("votacoesVotos-2019.csv")

individual <- df %>% 
  select(idVotacao, deputado_nome, voto) %>% 
  mutate(voto = str_replace(voto,"Não","0")) %>% 
  mutate(voto = str_replace(voto,"Sim","1")) %>% 
  mutate(voto = str_replace(voto,"Obstrução","0.5")) %>% 
  mutate(voto = str_replace(voto,"Abstenção","0.5")) %>% 
  mutate(voto = str_replace(voto,"Artigo 17","0.5")) %>% 
  mutate(voto = replace_na(voto,"0.5")) %>% 
  mutate(voto = as.numeric(voto)) %>% 
  pivot_wider(names_from = idVotacao, values_from = voto) %>% 
  transmute_all(~replace_na(.,"0.5")) %>% 
  column_to_rownames("deputado_nome")
  

ind <- lapply(individual,as.double) %>% as.data.frame() %>% lapply(scale) %>% as.data.frame()
rownames(ind) <- rownames(individual)

distance <- get_dist(ind)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

library(NbClust)

k <- NbClust(ind,
             distance = "euclidean",
             min.nc = 2,
             max.nc = 15,
             method = "kmeans",
             index = "silhouette")

k$Best.nc
k$All.index
k$Best.partition


k2 <- kmeans(ind, centers = 2, nstart = 1250, iter.max = 1000)
str(k2)
k2
k2$size
fviz_cluster(k2, data = ind, geom = "point", ellipse.type = "norm")

k3 <- kmeans(ind, centers = 3, nstart = 1250)
str(k3)
k3

fviz_cluster(k3, data = ind)

k4 <- kmeans(ind, centers = 4, nstart = 1250)
str(k4)
k4

fviz_cluster(k4, data = ind)

# Outras abordagens:

library(dbscan)

v <- dbscan(ind, minPts = 2, eps = 17)

n <- 15:25
map_df(n, dbscan(ind, minPts = 2, eps = n))


library(meanShiftR)

meanShiftR::knn_meanShift(trainData =  ind, points = 333)
