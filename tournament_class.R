startTournament <- function(entry) tournament$new(entry)

require(myfs)

tournament <- R6::R6Class(
  "tournament",
  
  # public field ------------------------------------------------------------
  
  public = list(
    
    #' Construct method
    #'
    #' @param entry \code{entryData} object
    #'
    #' @return R6 object of tournament
    #' 
    initialize = function(entry){
      private$entry <- entry
      
      private$nwin <- private$nlose <- rep(0, self$ndeck)
      private$nfight.card <- ifelse(self$ndeck %% 2 == 0, self$ndeck / 2, self$ndeck / 2 - 1)
      
      private$fight.result <- data.frame(didl = 1:4,
                                         didr = c(3,6,2,2),
                                         winl = c(2, 1, 0, 0),
                                         winr = c(1, 2, 2, 2))
    },
    
    #' Checks if the fight card is a new card
    #'
    #' @param d1 deck1 id
    #' @param d2 deck2 id
    #' 
    is.newFightCard = function(d1, d2){
      card.lr <- c(d1, d2)
      card.rl <- c(d2, d1)
      
      is.conflict.lr <- FALSE %in% (rowMinus(self$result.pid, card.lr) %>% rowNorm %>% as.logical)
      is.conflict.rl <- FALSE %in% (rowMinus(self$result.pid, card.rl) %>% rowNorm %>% as.logical)
      
      return(!is.conflict.lr || is.conflict.rl)
    },
    
    #' Checks if the fight players is sampe
    #'
    #' @param d1 deck1 id
    #' @param d2 deck2 id
    #' 
    is.samePlayersDeck = function(d1, d2) self$deck.player.id[d1] == self$deck.player.id[d2],
    
    #' Checks if the fight card is valid(the card is a new card and its player is not same)
    #'
    #' @param d1 deck1 id
    #' @param d2 deck2 id
    #' 
    is.validFightCard = function(d1, d2)
      self$is.newFightCard(d1, d2) && !self$is.samePlayersDeck(d1, d2),
    
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
    
    #' Sets new fight cards depending on \code{nwin}
    #'
    #' @param max.try max iteration times to find valid fight cards
    #'
    #' @return if failed to find valid fight cards, returns \code{FALSE}
    #' 
    setNewFightCard = function(max.try = 10){
      try.times <- 1
      # vector contains new fight card
      card <- numeric(0)
      cardlen <- self$nfcard * 2
      
      # try untile gets complete new fight card ------------------------------------------
      while(try.times < max.try && length(card) != cardlen){
        # deck id orderd by nwin with randomly
        deck.order <- private$shuffleOrder(private$nwin, decreasing = T)
        
        # decks that is not chosen for fight card
        deck.not.chosen <- 1:self$ndeck
        
        # initialize fight card and left/right deck target
        card <- numeric(0)
        l <- 1
        r <- 2
        
        while (l < cardlen && r <= cardlen) {
          while (r <= cardlen) {
            # debugText(l, r, deck.order[c(l, r)], deck.order, deck.not.chosen, cardlen)
            
            # if a fight card(l and r) is valid card?
            # when not valid, change r to next storongest deck
            if (self$is.validFightCard(deck.order[l], deck.order[r]) &&
                deck.not.chosen[l] == l &&
                deck.not.chosen[r] == r){
              
              card <- c(card, deck.order[c(l, r)])
              deck.not.chosen[c(l, r)] <- Inf
              l <- min(deck.not.chosen)
              r <- l + 1
            }
            else r <- r + 1
          }
        }
        try.times <- try.times + 1
      }
      
      private$fight.current <- data.frame(didl = card[1:(cardlen / 2) * 2 - 1],
                                          didr = card[1:(cardlen / 2) * 2])
      
      if(length(card) == cardlen) return(FALSE)
      else return(TRUE)
    }
    
  ),
  
  # private field -----------------------------------------------------------
  
  private = list(
    entry = NULL,
    
    fight.current = NULL,
    
    fight.result = NULL,
    nwin = NULL,
    nlose = NULL,
    nfight.card = NULL,
    current.round = 0,
    
    shuffleOrder = function(x, decreasing = F){
      o <- order(runif(length(x)))
      o[order(x[o], decreasing = decreasing)]
    }
  ),
  
  # active binding ----------------------------------------------------------
  
  active = list(
    results = function() private$fight.result,
    fight.card = function() data.frame(dnml = private$entry$deck[private$fight.current$didl],
                                       dnmr = private$entry$deck[private$fight.current$didr]),
    fight.card.id = function() private$fight.current,
    fight.card.list = function() list(left = as.character(self$fight.card$dnml),
                                      right = as.character(self$fight.card$dnmr)),
    
    #' Player id matrix of past fight card
    #' 
    result.pid = function() private$fight.result[, 1:2],
    
    ndeck = function() private$entry$ndeck,
    nplayer = function() private$entry$nplayer,
    nfcard = function() private$nfight.card,
    
    deck.player = function()private$entry$deck.player.id,
    deck.player.id = function() private$entry$deck.player.id,
    
    nwins = function() private$nwin,
    nloses = function() private$nlose,
    round = function(value){
      if (missing(value)) return(private$current.round)
      else private$current.round <- value
    }
  )
)
