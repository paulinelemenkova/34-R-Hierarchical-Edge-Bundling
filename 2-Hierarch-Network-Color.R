# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
 	# 1. create a data frame giving the hierarchical structure of individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
hierarchy=rbind(d1, d2)
 	# 2. create a dataframe with connection between leaves (individuals)
all_leaves=paste("subgroup", seq(1,100), sep="_")
connect=rbind( data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
connect$value=runif(nrow(connect))
 	# 3. Сreate a vertices data.frame. One line per object of the hierarchy
vertices = data.frame(
  name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) , 
  value = runif(111)) 
	# 4. Add a column with the group of each name. It will be used later to color points
vertices$group = hierarchy$from[ match( vertices$name, hierarchy$to ) ]
 	# 5. Create a graph object
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )
 	# 6. The connection object must refer to the ids of the leaves:
from = match( connect$from, vertices$name)
to = match( connect$to, vertices$name)
 	# 7. Basic graph
p = ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
#	geom_edge_diagonal(alpha=0.1) +
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()
	# 8. Add connections 
p +  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", width=0.9, tension=1) 

	# 9. Color lines
	# вариант-1: Use the 'value' column of the connection data frame for the color:
p + geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), aes(colour=value, alpha=value))
	# вариант-2: color palette
p + geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), aes(colour=value)) +
  scale_edge_color_continuous(low="white", high="red")
	# вариант-3: color palette Named
p + geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), aes(colour=value)) +   scale_edge_colour_distiller(palette = "BuPu")
	# вариант-4: Color depends of the index: the from and the to are different
p + geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), width=1, alpha=0.2, aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  theme(legend.position = "none")
  
  	# 10 Color points
# можно покрасить контуры точек в голубой // the x*1.05 allows to make a space between the points and the connection ends:
p + geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05), colour="skyblue", alpha=0.3, size=3)
# можно покрасить точки по группам:
p + geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=group),   size=3) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30))
# можно поменять размер точек // adjust the size to whatever variable quite easily!
p + geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) 
 
# красим точки по группам // color the points following their group appartenance
colorp<- p + geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), width=1, alpha=0.2, 	
	aes(colour=..index..)) +
	scale_edge_colour_distiller(palette = "RdPu") +
	theme(legend.position = "right") +
	geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=group),   size=3) +
	  scale_size_continuous( range = c(0.1,10) ) +
	scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30))
colorp
	
