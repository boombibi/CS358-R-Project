
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

Predict <- function(tree, features){
  if(tree$children[[1]]$isLeaf) return(tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return(Predict(child, features))
}

partitionData <- function( data, fractionOfDataForTrainingData = 0.7 )
{
  numberOfRows <- nrow(data)
  randomRows   <- runif(numberOfRows)
  index         <- randomRows <= fractionOfDataForTrainingData
  trainingData <- data[ index, ]
  testingData  <- data[ !index, ]
  dataSetSplit <- list( trainingData = trainingData, testingData = testingData )
}

number_class <- function(x){
  x <- length(levels(x))
}

Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}
