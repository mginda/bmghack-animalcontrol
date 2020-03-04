library(ggplot2)
library(ggraph)
library(networkD3)
library(dplyr)
library(magrittr)
#Removes older id - pre 2017
a_id <- animals_s[year(as.Date(animals_s$intakedate))>="2017",]$id

#Filter interactions from IDs
interact <- interact %>% 
  filter(id %in% a_id) %>%
  mutate(name=paste0(seq,"-",type,"-",interaction))

#indice of row.names for last interaction - subset out of edge list
l_row <- 0
for(i in 1:length(a_id)){
  if(i>=2){
    l_row <- c(l_row,row.names(tail(interact[interact$id==a_id[i],],1)))
  } else {
    l_row <- row.names(tail(interact[interact$id==a_id[i],],1)) 
  }
}
rm(i)
l_row <- as.numeric(l_row)

#Data transformation
#Generate node list
nodes <- interact %>%
          group_by(name,seq,type,interaction) %>%    #These are the general nodes for sankey
          summarise(animal = n_distinct(id)) %>%        #Count of the number of animals with a given kind of interaction case 
          as.data.frame()
rownames(nodes) <- nodes$name

#Edges
edges <- cbind(interact[-nrow(interact),c("id","name")],
               interact[2:nrow(interact),c("name")])
names(edges) <- c("id","from","to")
edges$from <- as.factor(edges$from)
edges <- edges[!row.names(edges) %in% l_row,] %>%
  group_by(from,to) %>%
  summarise(weight=n_distinct(id)) %>%
  as.data.frame()
edges <- edges %>% 
  mutate("source" = as.numeric(edges$from)-1) %>%
  mutate("target" = as.numeric(edges$to)-1) 

g <- graph_from_data_frame(edges,vertices = nodes,directed=T)

sankeyNetwork(Links= edges,
              Source = "source",
              Target = "target",
              Value = "weight",
              Nodes = nodes,
              NodeID = "name",
              NodeGroup = "type",
              units = "Animals",
              iterations=500,
              sinksRight = F)

#colorScale = ,
#fontSize = ,
#fontFamily = ,
#nodePadding = ,
#margin = c(top=0,right=0,bottom=0,left=0),
#height = c(),
#width = c()

write.csv(edges, file=paste0(getwd(),"/data/","bmg-sankey-edges.csv"),)
write.csv(nodes, file=paste0(getwd(),"/data/","bmg-sankey-nodes.csv"),)



