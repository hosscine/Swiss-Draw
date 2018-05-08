data.member <- cbind(mid=1:length(member.name),name=member.name)
data.deck <- cbind(did=1:length(deck.user[,1]),deck=deck.user[,"deck"],mid=deck.user[,"mid"])
decks <- nrow(data.deck)

# Functions ----------------------------------------------------------------
it <- function(terminal){
  if(terminal > 0) return(1:terminal)
  return(NULL)
}

IdenticalMatch <- function(x,hist){
  x <- as.numeric(x)
  if (is.null(hist)) {return(T)}
  else{
    for (game in 1:nrow(hist)) {
      if(setequal(hist[game,1:2],x)){return(F)}
    }
  }
  
  return(T)
}

calcKos <- function(kos,hist,error){
  kos[,4] <- 0
  for (i in it(nrow(hist))) {
    h <- hist[i,]
    if (h[3]==2) {
      kos[h[1],4] <- as.numeric(kos[h[1],4]) + 1
    }
    else if (h[4]==2) {
      kos[h[2],4] <- as.numeric(kos[h[2],4]) + 1
    }
    else {
      error$sttext <- "Error: Result that wins under 2 games was tried to save."
    }
  }
  kos[,1] <- order2(kos[,4],decreasing=T)
  return(kos)
}

Have <- function(x,element){
  return(ifelse(length(which(x==element)) > 0,yes = T,no = F))
}

could_be_numeric <- function(value){!is.na(suppressWarnings(as.numeric(value)))}

calcOponent <- function(kos,hist,error){
  wins <- as.numeric(kos[,4])
  strong <- numeric(length(wins))
  for (w in rev(unique(sort(wins)))) {
    strong[which(wins==w)] <- mixVector(which(wins==w))
  }
  
  opponent <- matrix(0,decks/2,2)
  for (match in 1:(decks/2)) {
    for (l in 1:decks) {
      if (Have(strong[l],opponent)==F) {
        
        for (r in 1:decks) {
          if (Have(strong[r],opponent)==F && IdenticalMatch(strong[c(l,r)],hist) && data.deck[strong[l],3]!=data.deck[strong[r],3]) {
            opponent[match,] <- strong[c(l,r)]
            break
          }
          if (r==decks) 
            error$stext <- "Error: Could not solve lottery. Please minute adjustment."
        }
        
        break
      }
    }
  }
  return(opponent)
}

mixVector <- function(x){
  return(x[order(runif(length(x)))])
}

order2 <- function(x, decreasing = F){
  v <- sort(unique(x))
  if(decreasing){v <- rev(v)}
  o <- numeric(length(x))
  for (val in 1:length(v)) {
    o[which(x==v[val])] <- val
  }
  return(o)
}
