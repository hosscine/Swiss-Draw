ShowHistory <- function(hist){
  if(is.null(hist)) return(NULL)
  return(cbind(deck.user[hist[,2]],deck.user[hist[,3]],hist[,4:5]))
}

ShowKOfromDeck <- function(name,hist){
  hist <- ShowHistory(hist)
  kos <- rbind(hist[hist[,1]==name,],hist[hist[,2]==name,])
  for (i in it(nrow(kos))) {
    if(kos[i,2] == name){
      kos[i,1:2] <- c(kos[i,2],kos[i,1])
      kos[i,3:4] <- c(kos[i,4],kos[i,3])
    }
  }
  colnames(kos) <- c("deck","opponent",paste(name,"win"),paste(name,"lose"))
  return(as.data.frame(kos))
}

ShowKOSummary <- function(hist){
  summary <- matrix(0,decks,4)
  rownames(summary) <- deck.user[,1]
  for (name in deck.user[,1]) {
    kos <- ShowKOfromDeck(name,hist)
    for (i in it(nrow(kos))) {
      if (kos[i,3]==2) {
        if (kos[i,4]==1) {
          summary[name,2] <- summary[name,2] + 1
        }
        else {
          summary[name,1] <- summary[name,1] + 1
        }
      }
      else if(kos[i,3]==1){
        summary[name,3] <- summary[name,3] + 1
      }
      else {
        summary[name,4] <- summary[name,4] + 1
      }
    }
  }
  return(apply(summary,2,as.character))
}