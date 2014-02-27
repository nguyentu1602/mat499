install.packages('multicore')
install.packages('foreach')
install.packages('biglm')
install.packages('bigmemory')
install.packages('doParallel')
install.packages('parallel')
install.packages('doMC')
install.packages('ggplot2')
install.packages('sampling')


require(foreach)
require(multicore)
require(biglm)
require(bigmemory)
require(doParallel)
require(doMC)
require(igraph)
require(ggplot2)
require(sampling)

# declare global variable
lag <- 40



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
load('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/as-caida/as-caida-cleaned.R')
# load('~/csc207/Git/mat499/as-caida-cleaned.R')

rm(dataFiles)
require(igraph)
### Load the dataFiles into a list of graphs
graphList <- list()
graphList[[1]] <- graph.data.frame(dataFiles[[1]], directed = TRUE)
graphList[[1]]


<<<<<<< Updated upstream
=======
degree(graph=graphList[[1]], v=1000, mode="in" )

system.time(graphList <- lapply(dataFiles, graph.data.frame))
>>>>>>> Stashed changes
system.time(graphList <- mclapply(dataFiles, graph.data.frame))

# Get all edges list
edgeListStr <- mclapply(graphList, get.edgelist)
length(edgeListStr)

#edgeList <- mclapply(edgeList, function (mat) {cbind(mat, paste(mat[,1], mat[,2]))})
edgeList <- list()
edgeList <- mclapply(edgeListStr, function (mat) {
  return( cbind (as.numeric(mat[,1]), as.numeric(mat[,2])));})

edgeList[[1]]

# Created a list of 122 vectors, each containing hashcodes of all the edges

ID <- list()
system.time(ID <- mclapply(edgeList, 
                           function (mat) {
                             v <- vector(mode="numeric", length=length(mat[,1])); 
                             for (i in 1:length(mat[,1])) {
                               v[i] = hash(mat[i,1], mat[i,2])};
                             return (v)}) )

edgeList

# Created a list of 122-1 vectors of response variable Y: was it in the next graph
Y <- list()
percentSurvive <- vector(length=121-lag)

for (i  in 1:(length(ID) - lag)) {         
  Y[[i]] = (ID[[i+lag-1]] %in% ID[[i+lag]]); # which edges in G20 are still in G21
  percentSurvive[i] = sum(Y[[i]])/length(Y[[i]]);
}

# A graph showing survivor rate over time
plot(percentSurvive,ylim=c(0.1,1) )
# Very interesting that the survival date for 20070917 is only 35% - I might have to left out 8 last graphs

??reset
??refresh


# Create the feature matrix -----------------------------------------------

X <- list()
registerDoMC(cores=4)

system.time (for (i in 1:10) {
  X[[i]] = matrix(data=NA,nrow=length(ID[[lag-1+i]]),ncol=100);
}  
)

X[[1]]

#Check the length of the lag-th edgelist and the # of rows of first matrix in X 
length(X[[1]][,1]) - length(ID[[lag]])

#For X[[1]], extracting x1 -> x(lag-1): was it in the last (lag-1) graphs?
for (j in 1:10) {
  for (i in 1:(lag-1)) {  # loop through lag-1 previous graphs 
    X[[j]][,i] = ID[[lag-1+j]] %in% ID[[i+j-1]];
  }}

# Testing x1 -> x(lag-1)
sum(X[[7]][,lag-1])/length(X[[7]][,lag-1])

# Now get the weights for the nodes, put in x20
summary(graphList[[lag]])
  # Trick to name only one column:
colnames(X[[1]]) <- 1:dim(X[[1]])[2]
colnames(X[[1]])[lag:(lag+7)] <- c("weight",'fromInDegree', 'fromOutDegree', 'toInDegree',
                            'toOutDegree','fromCloseness', 'toCloseness', 'betweeness')
X[[1]][,lag] <- get.edge.attribute(graph=graphList[[lag]],name="weight")
# Degrees:
X[[1]][,lag+1] <- degree(graph=graphList[[lag]],v=edgeListStr[[lag]][,1],mode='in')
X[[1]][,lag+2] <- degree(graph=graphList[[lag]],v=edgeListStr[[lag]][,1],mode='out')
X[[1]][,lag+3] <- degree(graph=graphList[[lag]],v=edgeListStr[[lag]][,2],mode='in')
X[[1]][,lag+4] <- degree(graph=graphList[[lag]],v=edgeListStr[[lag]][,2],mode='out')

# alpha.centrality crash my workstation last time. It collides with "multicore" package too
#X[[1]][,25] <- alpha.centrality(graph=graphList[[20]],nodes=edgeListStr[[20]][,1])
X[[1]][,lag+5] <- closeness(graph=graphList[[lag]],vids=edgeListStr[[lag]][,1],mode=c('out','in','all','total'))
X[[1]][,lag+6] <- closeness(graph=graphList[[lag]],vids=edgeListStr[[lag]][,2],mode=c('out','in','all','total'))

head(X[[1]])

# Betweeness
X[[1]][,lag+7] <- edge.betweenness(graph=graphList[[lag]],e=E(graphList[[lag]]),directed=TRUE, weights = NULL)

# Done extracting the first 47 features: First 39 columns are presence in the first 39 graphs. 


# Advance features of switching of node ------------------------------------

colnames(X[[1]])[(lag+8): (lag+10)] <- c("percentOf'1'",'countOfSwitch','timeUntilLast1')
head(X[[1]])
# X[[1]][,lag+8] : counts of 1s in the previous 39 column
X[[1]][,lag+8] <- rowSums(X[[1]][,1:(lag-1)])/(lag-1)

# X[[1]][,lag+9] :number of switch in the previous 39 column
 # create a function to take in a matrix of size 39, then return a vector of number of switch
switchCount <- function (mat) {
  len <- length(mat[,1]);
  ncol <- length(mat[1,]);
  # Make a buffered matrix:
  switchMat <- matrix(nrow=len, ncol=ncol);
  
  # For loop to catch number of switch
  for (i in 1: (ncol-1)) {
    switchMat[,i] <- abs(mat[,i+1]-mat[,i]);
  }
  switchMat[,ncol] <- rowSums(switchMat[,1:(ncol-1)]);
  return (switchMat[,ncol]);
}

X[[1]][,lag+9] <- switchCount(X[[1]][,1:39])

head(X[[1]])
# X[[1]][,lag+10] :time to the closet 0, i.e. how long this node has been alive stably
# write a function to take a vector and count back ward until it reaches the first 0
countFirstZero <- function (vec) {
  i <- length(vec);
  while ((vec[i] > 0) && (i >= 1)) {
    i <- i - 1;
  }
  return (length(vec) - i);
}


countFirstZero (X[[1]][10,1:39])
X[[1]][10,1:39]

# Two ways: A for loop and apply() give the same result:
 # For loop
for(k in 1:length(X[[1]][,1]) ) {
  X[[1]][k,lag+10] <- countFirstZero (X[[1]][k,1:39]);  
}
  # apply()  
X[[1]][,lag+10] <- apply(X[[1]][,1:39] , MARGIN=1, FUN=countFirstZero)
head(X[[1]])


# Extracting new features -------------------------------------------------

  # New features 1-4: column lag+11 to 14: 

colnames(X[[1]])[(lag+11): (lag+14)] <- c("fromInDegreeLag1",'fromOutDegreeLag1','toInDegreeLag1', "toOutDegreeLag1")

head(edgeListStr[[lag-1]])

# Create a temp vector for fromInDegreeLag1 and fromOutDegreeLag1
head(X[[1]])

tempFrom <- cbind( unique(edgeListStr[[lag-1]][,1]),
               degree(graph=graphList[[lag-1]],v=unique(edgeListStr[[lag-1]][,1]),mode='in',),
               degree(graph=graphList[[lag-1]],v=unique(edgeListStr[[lag-1]][,1]),mode='out')
            )


tempTo <- cbind( unique(edgeListStr[[lag-1]][,2]),
                   degree(graph=graphList[[lag-1]],v=unique(edgeListStr[[lag-1]][,2]),mode='in',),
                   degree(graph=graphList[[lag-1]],v=unique(edgeListStr[[lag-1]][,2]),mode='out')
)
positionFrom <- match(edgeListStr[[lag]][,1], tempFrom[,1], nomatch=0)
positionTo <- match(edgeListStr[[lag]][,2], tempTo[,1], nomatch=0)
positionFrom[2]

is.vector(tempFrom[1,2:3])


for (i in 1:length(positionFrom)) {
  if (positionFrom[i] != 0) {
    X[[1]][i,(lag+11):(lag+12)] <- as.numeric(tempFrom[positionFrom[i],2:3])
  }
  else {X[[1]][i,(lag+11):(lag+12)] <- 0}
  
  if (positionTo[i] != 0) {
    X[[1]][i,(lag+13):(lag+14)] <- as.numeric(tempTo[positionTo[i],2:3])
  }
  else {X[[1]][i,(lag+13):(lag+14)] <- 0}
} # Done creating the 4 features for 1st lag

 # New features 5: 2nd level degrees

# return in and out degrees of all vertexes

nodeInDeg <- degree(graph=graphList[[lag]],mode='in')
nodeOutDeg <- degree(graph=graphList[[lag]],mode='out')

# length(nodeInDeg)
head(nodeInDeg)
is.vector(nodeInDeg)
sum(nodeInDeg)

# This will return a list of adjacent vertices to a vertex
adjNodeIn <- get.adjlist(graph=graphList[[lag]],mode="in")
adjNodeOut <- get.adjlist(graph=graphList[[lag]],mode="out")
nodeList <- names(adjNodeIn)


head(adjNodeIn)
is.vector(adjNodeIn['8434'][1])
is.numeric(adjNodeIn['8434'][1])
adjNodeIn['8434'][[1]][1] 

# Now to the features:


# Initiate numbers of indegree of inNode, indeg of Onode, outdeg of inNode, outdeg of outNode
inNodeInDeg <-vector()
inNodeOutDeg <-vector()
outNodeOutDeg <-vector()
outNodeInDeg <-vector()

adjNodeIn[1]#[[1]]
sum(nodeInDeg[adjNodeIn[1][[1]]])

for (i in 1:length(adjNodeIn)) {
  inNodeInDeg[i] <- sum(nodeInDeg[adjNodeIn[i][[1]]]);
  inNodeOutDeg[i] <- sum(nodeOutDeg[adjNodeIn[i][[1]]]);
  outNodeOutDeg [i]  <- sum(nodeOutDeg[adjNodeOut[i][[1]]]);
  outNodeInDeg [i]  <- sum(nodeInDeg[adjNodeOut[i][[1]]]);
}
edgeListStr[[lag]]

posFrom <- match(x=edgeListStr[[lag]][,1],table=nodeList )
posTo <- match(x=edgeListStr[[lag]][,2],table=nodeList )


head(posFrom)
### Now to put these info into the big matrix: (hard!)
colnames(X[[1]])[(lag+15): (lag+18)] <- c('fromSecondLevelInNodeInDeg', 'fromSecondLevelOutNodeOutDeg',
                                          'toSecondLevelInNodeInDeg', 'toSecondLevelOutNodeOutDeg' )

X[[1]][,(lag+15)] <- inNodeInDeg[posFrom]
X[[1]][,(lag+16)] <- outNodeOutDeg[posFrom]
X[[1]][,(lag+17)] <- inNodeInDeg[posTo]
X[[1]][,(lag+18)] <- outNodeOutDeg[posTo]

X[[1]][,lag+19] <- page.rank.old(eps=0.00001,niter=100000,graph=graphList[[lag]],vids=nodeList,directed=TRUE,
                             damping = 0.85)[posFrom]
X[[1]][,lag+20] <- page.rank.old(niter=100000,graph=graphList[[lag]],vids=nodeList,directed=TRUE,
                                 damping = 0.85, eps = 0.00001)[posTo]
head(X[[1]])


names(adjNodeOut)
adjNodeOut[names(adjNodeOut)[1000]]
length(adjNodeIn[4][[1]])
nodeList[1:4]


# Neighbourhood Discovery -------------------------------------------------

  # Very useful to get 
nei <- neighborhood(graph=graphList[[lag]],order=3,mode="all") #tend to be very slow



# Crash on me:  label.propagation.community(graph=graphList[[lag]],)
# Does not work on directed graph: fastgreedy

head(nei[[2]])

nei[[2]]
length(nei)
head(X[[1]])

# Save the feature matrix to a csv file:
write.csv(x=cbind(X[[1]][,1:60],Y[[1]]),file="fullData39Lag.csv")  

getwd()
# First logistic model ----------------------------------------------------

lapply(list(Y[[1]],ID[[lag]],Y[[2]],ID[[lag+1]],X[[1]][,1]), length)

logit1 <- glm(Y[[1]]~X[[1]][,2:60],family=binomial )
summary(logit1)

fit2 <- step.up(logit1)



sum(Y[[1]])-length(Y[[1]])
#This part should not be on GitHub

# Sampling and plotting ---------------------------------------------------

# Sampling
testMat <- cbind(X[[1]][ ,c(1:24,27:28) ], Y[[1]])
head(testMat)
dim(testMat)

testMat0 <- testMat[testMat[,27] == 0,]
testMat1 <- testMat[testMat[,27] == 1,]
head(testMat0)


# Strastified sampling
testMatSamp <- rbind(testMat0[sample(nrow(testMat0), 100),], testMat1[sample(nrow(testMat1), 100),])

colnames(testMatSamp)[27] <- "y"
head(testMatSamp)
dim(testMatSamp)
testMatSamp <- data.frame(testMatSamp)




#Log the variable
testMatSampLog <- data.frame(testMatSamp)
testMatSampLog[20:26] <- log(testMatSamp[20:26])
# Pair plot before log. Blue is false orange is True
pairs(main= "Pair plot before log", jitter(testMatSamp[19:26]), col=c("blue", "orange")[unclass(as.factor(testMatSamp$y))] )
# Pair plot after log
pairs(main= "Pair plot after log", testMatSampLog[19:26], col=c("blue", "orange")[unclass(as.factor(testMatSampLog$y))])

c("green3", "black")[unclass(as.factor(testMatSamp$y))]

                     
                     
                     
summary(logit1)
names(logit1)
logit1$R
head(X[[1]])

edgeListStr[[1]]

pairs(X[[1]][,1:12], col= Y[[1]])




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


