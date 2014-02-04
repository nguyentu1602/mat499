install.packages('multicore')
install.packages('foreach')
install.packages('biglm')
install.packages('bigmemory')
install.packages('doParallel')
install.packages('parallel')



require(foreach)
require(multicore)
require(biglm)
require(bigmemory)
require(doParallel)


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
# Note: it gives wrong round-off when we have (10^8, 10^8), but anything less works 
# v1 <- c(1:10000000)
# v2 <- seq(from=10000000,to=1,by=-1)
# v2
# v3 <- c(1:10000000)
# 
# for (i in 1:10000000) {
#   temp = unHash(hash(v1[i], v2[i])) - c(v1[i], v2[i])
#   v3[i] = !((temp[1] == 0) && (temp[2] == 0))
# }


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

rm(list=ls())

system.time(graphList <- mclapply(dataFiles, graph.data.frame))

# Get all edges list
edgeListStr <- mclapply(graphList, get.edgelist)
length(edgeListStr)

#edgeList <- mclapply(edgeList, function (mat) {cbind(mat, paste(mat[,1], mat[,2]))})
f1 <- function (mat) {
  return( cbind (as.numeric(mat[,1]), as.numeric(mat[,2])));}

edgeList <- list()
edgeList <- mclapply(edgeListStr, f1)
edgeList[[1]]

# Created a list of 122 vectors, each containing hashcodes of all the edges

ID <- list()
ID <- mclapply(edgeList, function (mat) {
  v <- vector(mode="numeric", length=length(mat[,1])); 
  for (i in 1:length(mat[,1])) {
    v[i] = hash(mat[i,1], mat[i,2])};
  return (v);})

# Created a list of 122-1 vectors of response variable Y: was it in the next graph
Y <- list()
percentSurvive <- vector(length=121)

for (i  in 1:(length(ID) - 1)) {         
  Y[[i]] = (ID[[i+1]] %in% ID[[i]]);
  percentSurvive[i] = sum(Y[[i]])/length(Y[[i]]);
}

# A graph showing survivor rate over time
plot(percentSurvive)
# Very interesting that the survival date for 20070917 is only 35% - I might have to left out 8 last graphs

### Create the feature matrix. This is the big one
X <- list()

#c1 <- makeCluster(3)
nodes <- detectCores()
c1 <- makeCluster(nodes)
registerDoParallel(c1)

system.time (foreach (i = 1:20)  %do% {
  X[[i]] = matrix(data=NA,nrow=length(ID[[20+i]]),ncol=100);
} 
)


system.time (for (i in 1:10) {
  X[[i]] = matrix(data=NA,nrow=length(ID[[20]]),ncol=200);
} 
)


X[[1]]









G3.e <- cbind(G3.e, matrix(nrow=length(G3.e[,1]),ncol=27))

degree(graph=graphList[[1]], v="23231", mode ="in")

a




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


