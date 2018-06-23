library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

# ЧАСТЬ-1
 	# шаг-1. создавем иерархическиую структуру датафрейма (группы и листья)
d1=data.frame(from="origin", to=paste("profile", seq(1:25), sep="_"))
d2=data.frame(from=rep(d1$to, each=4), to=paste(c("Slope angle", "Bathymetry", "Sedimental thickness", "Tectonic plates"), seq(1,100)))
edges=rbind(d1, d2)
 	# делаем соединения между листьями (individuals)
all_leaves=paste(c("Slope angle", "Bathymetry", "Sedimental thickness", "Tectonic plates"), seq(1,100))
connect=rbind( data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), data.frame( from=sample(all_leaves[175:180], 30, replace=T) , to=sample( all_leaves[155:160], 30, replace=T)) )
connect$value=runif(nrow(connect))
 	# создаем верхушки-пики, одна линия к каждому листу (create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(126)) 
	# добавляем столбец с названиями групп, чтобы по нему красить точки по группам
vertices$group = edges$from[ match( vertices$name, edges$to ) ]

 # ЧАСТЬ-2 создаем информацию о ярлычках: угол, наклон и переворот
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves
 	# calculate the alignment of labels: right or left 
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
 	# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# ЧАСТЬ-3 Создаем графический объект
mygraph <- graph_from_data_frame( edges, vertices=vertices )
 	# The connection object must refer to the ids of the leaves:
from = match( connect$from, vertices$name)
to = match( connect$to, vertices$name)

# ЧАСТЬ-4 рисуем график
M<- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value), alpha=0.2) +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2, alpha=1) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=0.5, aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "BuPu") +  
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) + 
  theme_void() +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4)) +
   labs(
	title = "Mariana Trench",
	subtitle = "Structured Edge Bundling: \nConnections Between Leaves of a Hierarchical Network") +
theme(
    legend.position="right",
	legend.text = element_text(family = "Times New Roman", colour="black", size=6, face=1),
    legend.title = element_text(family = "Times New Roman", colour="black", size=8, face=1),
    plot.title = element_text(family = "Times New Roman", face = 2, size = 12),
    plot.subtitle = element_text(family = "Times New Roman", face = 1, size = 10),
    plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) 
M