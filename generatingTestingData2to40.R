# Extract the testing set: fulldata39lag_2_to_40

increment = 1

# Y is Y[[2]]: check length:
length(Y[[2]]) - length(ID[[41]])

# X


#Check the length of the lag-th edgelist and the # of rows of first matrix in X 
length(X[[1+increment]][,1]) - length(ID[[lag+increment]])


# Testing x1 -> x(lag-1)
sum(X[[1+ increment]][,lag-1])/length(X[[1+increment]][,lag-1])

# Now get the weights for the nodes, put in x20
summary(graphList[[lag]])
# Trick to name only one column:
colnames(X[[1 + increment]]) <- 1:dim(X[[1+ increment]])[2]
colnames(X[[1+ increment]])[lag:(lag+7)] <- c("weight",'fromInDegree', 'fromOutDegree', 'toInDegree',
                                   'toOutDegree','fromCloseness', 'toCloseness', 'betweeness')
X[[1+ increment]][,lag] <- get.edge.attribute(graph=graphList[[lag+ increment]],name="weight")
# Degrees:
X[[1+ increment]][,lag+1] <- degree(graph=graphList[[lag+ increment]],v=edgeListStr[[lag+ increment]][,1],mode='in')
X[[1+ increment]][,lag+2] <- degree(graph=graphList[[lag+ increment]],v=edgeListStr[[lag+ increment]][,1],mode='out')
X[[1+ increment]][,lag+3] <- degree(graph=graphList[[lag+ increment]],v=edgeListStr[[lag+ increment]][,2],mode='in')
X[[1+ increment]][,lag+4] <- degree(graph=graphList[[lag+ increment]],v=edgeListStr[[lag+ increment]][,2],mode='out')

# alpha.centrality crash my workstation last time. It collides with "multicore" package too
#X[[1]][,25] <- alpha.centrality(graph=graphList[[20]],nodes=edgeListStr[[20]][,1])
X[[1+ increment]][,lag+5] <- closeness(graph=graphList[[lag+ increment]],vids=edgeListStr[[lag+ increment]][,1],mode=c('out','in','all','total'))
X[[1+ increment]][,lag+6] <- closeness(graph=graphList[[lag+ increment]],vids=edgeListStr[[lag+ increment]][,2],mode=c('out','in','all','total'))

head(X[[1+ increment]])

# Betweeness
X[[1+ increment]][,lag+7] <- edge.betweenness(graph=graphList[[lag+ increment]],e=E(graphList[[lag+ increment]]),directed=TRUE, weights = NULL)

# Done extracting the first 47 features: First 39 columns are presence in the first 39 graphs. 


# Advance features of switching of node ------------------------------------

colnames(X[[1+ increment]])[(lag+8): (lag+10)] <- c("percentOf'1'",'countOfSwitch','timeUntilLast1')
head(X[[1+ increment]])
# X[[1]][,lag+8] : counts of 1s in the previous 39 column
X[[1+ increment]][,lag+8] <- rowSums(X[[1+ increment]][,1:(lag-1)])/(lag-1)

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

X[[1+ increment]][,lag+9] <- switchCount(X[[1+ increment]][,1:39])

head(X[[1+ increment]])
# X[[1]][,lag+10] :time to the closet 0, i.e. how long this node has been alive stably
# write a function to take a vector and count back ward until it reaches the first 0
countFirstZero <- function (vec) {
  i <- length(vec);
  while ((vec[i] > 0) && (i >= 1)) {
    i <- i - 1;
  }
  return (length(vec) - i);
}


countFirstZero (X[[1+ increment]][10,1:39])
X[[1+ increment]][10,1:39]

# Two ways: A for loop and apply() give the same result:
# For loop
for(k in 1:length(X[[1]][,1]) ) {
  X[[1]][k,lag+10] <- countFirstZero (X[[1]][k,1:39]);  
}
# apply()  
X[[1+ increment]][,lag+10] <- apply(X[[1+ increment]][,1:39] , MARGIN=1, FUN=countFirstZero)




# Extracting new features -------------------------------------------------

# New features 1-4: column lag+11 to 14: 

colnames(X[[1+ increment]])[(lag+11): (lag+14)] <- c("fromInDegreeLag1",'fromOutDegreeLag1','toInDegreeLag1', "toOutDegreeLag1")

head(edgeListStr[[lag-1+ increment]])

# Create a temp vector for fromInDegreeLag1 and fromOutDegreeLag1
head(X[[1+ increment]])

tempFrom <- cbind( unique(edgeListStr[[lag-1+ increment]][,1]),
                   degree(graph=graphList[[lag-1+ increment]],v=unique(edgeListStr[[lag-1+ increment]][,1]),mode='in',),
                   degree(graph=graphList[[lag-1+ increment]],v=unique(edgeListStr[[lag-1+ increment]][,1]),mode='out')
)


tempTo <- cbind( unique(edgeListStr[[lag-1+ increment]][,2]),
                 degree(graph=graphList[[lag-1+ increment]],v=unique(edgeListStr[[lag-1+ increment]][,2]),mode='in',),
                 degree(graph=graphList[[lag-1+ increment]],v=unique(edgeListStr[[lag-1+ increment]][,2]),mode='out')
)
positionFrom <- match(edgeListStr[[lag+ increment]][,1], tempFrom[,1], nomatch=0)
positionTo <- match(edgeListStr[[lag+ increment]][,2], tempTo[,1], nomatch=0)
positionFrom[2]

is.vector(tempFrom[1,2:3])


for (i in 1:length(positionFrom)) {
  if (positionFrom[i] != 0) {
    X[[1+ increment]][i,(lag+11):(lag+12)] <- as.numeric(tempFrom[positionFrom[i],2:3])
  }
  else {X[[1+ increment]][i,(lag+11):(lag+12)] <- 0}
  
  if (positionTo[i] != 0) {
    X[[1+ increment]][i,(lag+13):(lag+14)] <- as.numeric(tempTo[positionTo[i],2:3])
  }
  else {X[[1+ increment]][i,(lag+13):(lag+14)] <- 0}
} # Done creating the 4 features for 1st lag

# New features 5: 2nd level degrees

# return in and out degrees of all vertexes

nodeInDeg <- degree(graph=graphList[[lag+ increment]],mode='in')
nodeOutDeg <- degree(graph=graphList[[lag+ increment]],mode='out')

# length(nodeInDeg)
head(nodeInDeg)
is.vector(nodeInDeg)
sum(nodeInDeg)

# This will return a list of adjacent vertices to a vertex
adjNodeIn <- get.adjlist(graph=graphList[[lag+ increment]],mode="in")
adjNodeOut <- get.adjlist(graph=graphList[[lag+ increment]],mode="out")
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
edgeListStr[[lag+ increment]]

posFrom <- match(x=edgeListStr[[lag+ increment]][,1],table=nodeList )
posTo <- match(x=edgeListStr[[lag+ increment]][,2],table=nodeList )


head(posFrom)
### Now to put these info into the big matrix: (hard!)
colnames(X[[1+ increment]])[(lag+15): (lag+20)] <- c('fromSecondLevelInNodeInDeg', 'fromSecondLevelOutNodeOutDeg',
                                          'toSecondLevelInNodeInDeg', 'toSecondLevelOutNodeOutDeg','X59', 'X60')

X[[1+ increment]][,(lag+15)] <- inNodeInDeg[posFrom]
X[[1+ increment]][,(lag+16)] <- outNodeOutDeg[posFrom]
X[[1+ increment]][,(lag+17)] <- inNodeInDeg[posTo]
X[[1+ increment]][,(lag+18)] <- outNodeOutDeg[posTo]

X[[1+ increment]][,lag+19] <- page.rank.old(eps=0.00001,niter=100000,graph=graphList[[lag+ increment]],vids=nodeList,directed=TRUE,
                                 damping = 0.85)[posFrom]
X[[1+ increment]][,lag+20] <- page.rank.old(niter=100000,graph=graphList[[lag+ increment]],vids=nodeList,directed=TRUE,
                                 damping = 0.85, eps = 0.00001)[posTo]

tail(X[[1+ increment]])


names(adjNodeOut)
adjNodeOut[names(adjNodeOut)[1000]]
length(adjNodeIn[4][[1]])
nodeList[1:4]




# Save the feature matrix to a csv file:
write.csv(x=cbind(X[[1+ increment]][,1:60],Y[[1+ increment]]),file="fulldata39lag_2_to_40.csv")  
