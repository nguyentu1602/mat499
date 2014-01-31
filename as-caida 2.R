#t1 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/as-caida/as-caida20040105.txt',header=FALSE,sep='\t', comment.char='#')
#head(t1)

### Load all the files at once:

setwd('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/as-caida/')
dataFiles <- lapply(Sys.glob("as-caida*.txt"), read.table)
head(dataFiles[[1]])

### Manipulate the data:
# Erase all 1: Provider->Customer is an invalid direction
# Change all 2 into 0: siblings are the same as p2p
# Change all -1 to 1: Customer->Provider should cost 1 per move

for (i in 1:length(dataFiles)) {
dataFiles[[i]]  <- subset(dataFiles[[i]] , V3!=1) # Erase all 1s
dataFiles[[i]]$V3 = dataFiles[[i]]$V3 %% 2 # Change all -1s to 1s and all 2s to 0
names(dataFiles[[i]]) = c('fromNode','toNode', 'weight') 
} # The dataFiles list is now cleaned and suitable for our purpose

t1 <- dataFiles[[1]]

### Saving the data;
save(dataFiles,file="as-caida-cleaned.R")
load('as-caida-cleaned.R')
rm(dataFiles)
require(igraph)
### Load the dataFiles into a list of graphs
graphList <- list()
graphList[[1]] <- graph.data.frame(dataFiles[[1]], directed = TRUE)
graphList[[1]]
plot(graphList[[1]])

ifor (i in 1:length(dataFiles)) {
  graphList[[i]] <- graph.data.frame(dataFiles[[i]], directed = TRUE)  
}


get.shortest.paths(graphList[[1]],1,1000)

v <- get.all.shortest.paths(graphList[[1]],1,1000)








# Ch10: Visualization -----------------------------------------------------


library(RCytoscape)
gD.cyt <- igraph.to.graphNEL(graphList[[1]])
gD.cyt <- initEdgeAttribute (gD.cyt, "weight", 'integer', 0)
gDCW <- new.CytoscapeWindow("Les Miserables", graph = gD.cyt, overwriteWindow = TRUE)

# We can display graph, with defaults color/size scheme
displayGraph(gDCW)


# Ch11: Small graph for illustration --------------------------------------
install.packages('rgl')
require(rgl)

sampleData1 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph1.txt')
G1 <- graph.data.frame(sampleData1, directed=TRUE)
sampleData2 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph2.txt')
G2 <- graph.data.frame(sampleData2, directed=TRUE)
sampleData3 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph3.txt')
G3 <- graph.data.frame(sampleData3, directed=TRUE)
sampleData4 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph4.txt')
G4 <- graph.data.frame(sampleData4, directed=TRUE)


# Plot grpah from t=1 to t=4:
plot.igraph(G1, layout=layout.grid, vertex.size = 20, edge.label= sampleData1$V3 )
plot.igraph(G2, layout=layout.grid, vertex.size = 20, edge.label= sampleData2$V3 )
plot.igraph(G3, layout=layout.grid, vertex.size = 20, edge.label= sampleData3$V3 )
plot.igraph(G4, layout=layout.grid, vertex.size = 20, edge.label= sampleData4$V3 )

#rglplot.igraph(sampleGraph)

# Build a list of edges from the latest graph, i.e. sampleGraph4
G1.e <- get.edgelist(G1)  # Extract all edges
G2.e <- get.edgelist(G2)  # Extract all edges
G3.e <- get.edgelist(G3)  # Extract all edges
G4.e <- get.edgelist(G4)  # Extract all edges



curEdges <- get.edgelist(G4)  # Extract all edges
curEdges <- cbind(curEdges, paste(curEdges[,1], curEdges[,2]))  # add a column of edge names
curEdges <- cbind(curEdges, matrix(nrow=length(curEdges[,1]),ncol=20))
curEdges <- data.frame(curEdges)
names(curEdges) <- c('from',"to","nodeName",'inG1','inG2','inG3','weight','fromInDegree','fromOutDegree','toInDegree','toOutDegree','fromECentrality', 'toECentrality','fromCloseness', 'toCloseness','betweeness')

curEdges$inG1 <-  ((curEdges$from %in% G1.e[,1]) & (curEdges$to %in% G1.e[,2]))
curEdges$inG2 <-  ((curEdges$from %in% G2.e[,1]) & (curEdges$to %in% G2.e[,2]))
curEdges$inG3 <-  ((curEdges$from %in% G3.e[,1]) & (curEdges$to %in% G3.e[,2]))
curEdges$weight <- get.edge.attribute(G4,name='V3')


curEdges$fromInDegree <- degree(G4,v=G4.e[,1],mode='in')
curEdges$fromOutDegree <- degree(G4,v=G4.e[,1],mode='out')
curEdges$toInDegree <- degree(G4,v=G4.e[,2],mode='in')
curEdges$toOutDegree <- degree(G4,v=G4.e[,2],mode='out')
curEdges$fromECentrality <- alpha.centrality(graph=G4,nodes=G4.e[,1])
curEdges$toECentrality <- alpha.centrality(graph=G4,nodes=G4.e[,2])

curEdges$fromCloseness <- closeness(graph=G4, vids=G4.e[,1],mode = c("out", "in", "all", "total") )
curEdges$toCloseness <- closeness(graph=G4, vids=G4.e[,2],mode = c("out", "in", "all", "total") )
curEdges$betweeness <- edge.betweenness(G4, e=E(G4), directed=TRUE, weights=NULL)

curEdges

# Possible variables to consider: betweeness, closeness, 