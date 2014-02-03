install.packages('multicore')
require(multicore)

### hash() and unHash() are functions to create hashCode

hash <- function (x, y) {
  if (x == max(x,y)){
    return (x^2 + x + y);}
  else
    {return (x + y^2);}
}

unHash <- function (z) {
  if ( (z - (trunc(sqrt(z)))^2) < trunc(sqrt(z)) ) {
    return ( c(z - (trunc(sqrt(z)))^2, trunc(sqrt(z))) ); 
  }
  else {
    return ( c(trunc(sqrt(z)), z - (trunc(sqrt(z)))^2 - (trunc(sqrt(z)))) ); 
  }
} #unHash

# Testing hash() and unHash()
# Note: it crashes when we have (10^6, 10^6)
v1 <- c(1:10000000)
v2 <- seq(from=10000000,to=1,by=-1)
v2
v3 <- c(1:10000000)

for (i in 1:10000000) {
  temp = unHash(hash(v1[i], v2[i])) - c(v1[i], v2[i])
  v3[i] = !((temp[1] == 0) && (temp[2] == 0))
}
  
unHash(hash(10000000,10000000))

### Load all the files at once:

#setwd('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/as-caida/')
#dataFiles <- lapply(Sys.glob("as-caida*.txt"), read.table)
#head(dataFiles[[1]])

### Manipulate the data:
# Erase all 1: Provider->Customer is an invalid direction
# Change all 2 into 0: siblings are the same as p2p
# Change all -1 to 1: Customer->Provider should cost 1 per move

# for (i in 1:length(dataFiles)) {
# dataFiles[[i]]  <- subset(dataFiles[[i]] , V3!=1) # Erase all 1s
# dataFiles[[i]]$V3 = dataFiles[[i]]$V3 %% 2 # Change all -1s to 1s and all 2s to 0
# names(dataFiles[[i]]) = c('fromNode','toNode', 'weight') 
} # The dataFiles list is now cleaned and suitable for our purpose

# t1 <- dataFiles[[1]]

### Saving the data;
save(dataFiles,file="as-caida-cleaned.R")
# load('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/as-caida/as-caida-cleaned.R')
load('~/csc207/Git/mat499/as-caida-cleaned.R')

rm(dataFiles)
require(igraph)
### Load the dataFiles into a list of graphs
graphList <- list()
graphList[[1]] <- graph.data.frame(dataFiles[[1]], directed = TRUE)
graphList[[1]]
plot(graphList[[1]])
rm(list=ls())

system.time(graphList <- lapply(dataFiles, graph.data.frame))
system.time(graphList <- mclapply(dataFiles, graph.data.frame))

# Get all edges list
edgeListStr <- mclapply(graphList, get.edgelist)
length(edgeListStr)

#edgeList <- mclapply(edgeList, function (mat) {cbind(mat, paste(mat[,1], mat[,2]))})
f1 <- function (mat) {
  return( cbind (as.numeric(mat[,1]), as.numeric(mat[,2])));}

edgeList <- list()

edgeList <- mclapply(edgeListStr, f1)


#system.time(edgeList <- mclapply(edgeList, data.frame))


system.time( mclapply(edgeList, function (frame) { 
  cbind(frame, matrix(nrow=length(frame[,1]),ncol=3));      
        }
  ))

#rm(edgeList)
edgeList[[1]]
         
dim(edgeList[[122]])
length(edgeList)


G3.e <- cbind(G3.e, matrix(nrow=length(G3.e[,1]),ncol=27))






head(edgeList[[1]])
















# Ch10: Visualization -----------------------------------------------------


# library(RCytoscape)
# gD.cyt <- igraph.to.graphNEL(graphList[[1]])
# gD.cyt <- initEdgeAttribute (gD.cyt, "weight", 'integer', 0)
# gDCW <- new.CytoscapeWindow("Les Miserables", graph = gD.cyt, overwriteWindow = TRUE)
# 
# # We can display graph, with defaults color/size scheme
# displayGraph(gDCW)


# Ch11: Small graph for illustration --------------------------------------
install.packages('rgl')
require(rgl)


