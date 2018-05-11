startTournament <- function(entry) tournament$new(entry)

require(myfs)

tournament <- R6::R6Class(
  "tournament",
  
# public field ------------------------------------------------------------

  public = list(
    
    initialize = function(entry){
      private$entry <- entry
      
      private$nwin <- private$nlose <- rep(0, self$ndeck)
      
      private$fight.result <- data.frame(didl = 1:4,
                                         didr = c(3,6,2,2),
                                         winl = c(2, 1, 0, 0),
                                         winr = c(1, 2, 2, 2))
    },
    
    #' Checks if the fight card is a new card
    #'
    #' @param p1 player1 id
    #' @param p2 player2 id
    #' 
    is.newFightCard = function(p1, p2){
      card.lr <- c(p1, p2)
      card.rl <- c(p2, p1)
      
      is.conflict.lr <- FALSE %in% (rowMinus(self$result.pid, card.lr) %>% rowNorm %>% as.logical)
      is.conflict.rl <- FALSE %in% (rowMinus(self$result.pid, card.rl) %>% rowNorm %>% as.logical)
      
      return(!is.conflict.lr || is.conflict.rl)
    },
    
    #' Updates number of win and lose of each decks
    #'
    updateWinLose = function(){
      # if left player of the fight.result is win?
      fight.result_winner.is.left <- private$fight.result$winl == 2
      
      # process in case left is win
      winners.l <- private$fight.result[fight.result_winner.is.left,]$didl
      losers.l <- private$fight.result[fight.result_winner.is.left,]$didr
      
      # process in case right is win
      winners.r <- private$fight.result[!fight.result_winner.is.left,]$didr
      losers.r <- private$fight.result[!fight.result_winner.is.left,]$didl

      win.times <- table(c(winners.l, winners.r))
      lose.times <- table(c(losers.l, losers.r))
      
      # updates property
      private$nwin[names(win.times) %>% as.numeric] <- win.times
      private$nlose[names(lose.times) %>% as.numeric] <- lose.times
    },
    
    setNewFightCard <- function(){
      # flags that each deck is incorporated or not
      deck.done.flag <- logical(self$ndeck)
      
      # fight card vector
      card <- numeric(0)
      
      for (wint in 1:(private$nwin %>% unique %>% sort(decreasing = T))) {
        # find candidates of fight card that won larger than "wint" and not set "done flag"
        cand <- which(!deck.done.flag & private$nwin > wint)
        
        # even random sort vector for cand
        random <- ifelse(length(cand) %% 2 == 0, length(cand), length(card) - 1) %>% 
          runif %>% sort
        
        card <- c(card, cand[random])
        deck.done.flag <- 1:self$ndeck %in% card
      }
      
    }
    
  ),

# private field -----------------------------------------------------------

  private = list(
    entry = NULL,
    
    fight.result = NULL,
    nwin = NULL,
    nlose = NULL,
    
    shuffleOrder <- function(x){
      o <- order(runif(length(x)))
      o[order(x[o])]
    }
  ),

# active binding ----------------------------------------------------------

  active = list(
    results = function() private$fight.result,
    
    #' Player id matrix of past fight card
    #' 
    result.pid = function() private$fight.result[, 1:2],
    
    ndeck = function() private$entry$ndeck,
    nplayer = function() private$entry$nplayer,
    
    nwins = function() private$nwin,
    nloses = function() private$nlose
  )
)

