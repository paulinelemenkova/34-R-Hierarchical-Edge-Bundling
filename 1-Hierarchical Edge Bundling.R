library(ggraph)
library(igraph) 

# ЧАСТЬ-1. создаем логический нетворк, 3 уровня: (ядро, лепестки от него 25 шт. (25 профилей) и узлы 10 шт. (геолог-батим. факторов в каждом профиле) create a data frame giving the hierarchical structure of individuals
	# create a data frame giving the hierarchical structure of individuals
d1=data.frame(from="origin", to=paste("group", seq(1,25), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,250), sep="_"))
hierarchy=rbind(d1, d2)
 	# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) )
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices ) 

# ЧАСТЬ-2. теперь визуализиуем
	# 1 вариант, через igraph: сетью-нетворком
plot(mygraph, vertex.label="", edge.arrow.size=0, vertex.size=2)
	# 2 вариант
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_link() +
  theme_void()
	# 3 вариант, цветком
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  theme_void()

# ЧАСТЬ-3. соединяем узлы (допустим, № 18,20 и 30 с 19, 50 и 70-м:)
# 1 вариант straight lines (tension=0)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(alpha=0.1) +
  geom_conn_bundle(data = get_con(from = c(18,20,30), to = c(19, 50, 70)), alpha=1, width=1, colour="skyblue", tension = 0) +
  theme_void()

# 2 вариант ellipse lines (tension=1) 
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(alpha=0.1) +
  geom_conn_bundle(data = get_con(from = c(18,20,30), to = c(19, 50, 70)), alpha=1, width=1, colour="skyblue", tension = 1) +
  theme_void()

# ЧАСТЬ-4 соединяем все узлы со всеми
	# create a dataframe with connection between leaves (individuals)
all_leaves=paste("subgroup", seq(1,100), sep="_")
connect=rbind( data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
 	# The connection object must refer to the ids of the leaves:
from = match( connect$from, vertices$name)
to = match( connect$to, vertices$name) 

	# 1 вариант прямыми
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()
 
	# 2 вариант эллипсами, изогнутость tension = 0.9 
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()

	# 3 вариант гнутыми эллипсами, tension = 1
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 1) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()
