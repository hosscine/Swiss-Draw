startTournament <- function(entry) tournament$new(entry)

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
      private$fight.result.summary <- 
        data.frame(matrix(0, nrow = self$ndeck, ncol = 4,
                          dimnames = list(private$entry$deck,
                                          c("w2_0", "w2_1", "l1_2", "l0_2"))))
      
      private$nfight.card <- ifelse(self$ndeck %% 2 == 0, self$ndeck / 2, self$ndeck / 2 - 1)
      private$deck.ranking <- 1:self$ndeck
      
      private$fight.result <- data.frame(didl = NA, didr = NA,
                                         winl = NA, winr = NA)[numeric(0),]
      # private$fight.result <- data.frame(didl = 1:4,
      #                                    didr = c(3,6,2,2),
      #                                    winl = c(2, 1, 0, 0),
      #                                    winr = c(1, 2, 2, 2))
    },
    
    #' Checks if the fight card is a new card
    #'
    #' @param d1 deck1 id
    #' @param d2 deck2 id
    #' 
    is.newFightCard = function(d1, d2){
      if (nrow(private$fight.result) == 0) return(TRUE)
      
      card.lr <- c(d1, d2)
      card.rl <- c(d2, d1)
      result.pid <- private$fight.result[,1:2]
      
      is.conflict.lr <- FALSE %in% (rowMinus(result.pid, card.lr) %>% rowNorm %>% as.logical)
      is.conflict.rl <- FALSE %in% (rowMinus(result.pid, card.rl) %>% rowNorm %>% as.logical)
      
      return(!is.conflict.lr && !is.conflict.rl)
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
      if (nrow(private$fight.result) == 0) return(1)
      
      # if left player of the fight.result is win?
      fight.result_winner.is.left <- private$fight.result$winl == 2
      
      # process in case left deck is win
      result.winl <- private$fight.result[fight.result_winner.is.left,]
      
      winner2_0 <- result.winl[result.winl$winr == 0,]$didl
      winner2_1 <- result.winl[result.winl$winr == 1,]$didl
      loser1_2 <- result.winl[result.winl$winr == 1,]$didr
      loser0_2 <- result.winl[result.winl$winr == 0,]$didr
      
      # process in case right deck is win
      result.winr <- private$fight.result[!fight.result_winner.is.left,]
      
      winner2_0 <- c(winner2_0, result.winr[result.winr$winl == 0,]$didr)
      winner2_1 <- c(winner2_1, result.winr[result.winr$winl == 1,]$didr)
      loser1_2 <- c(loser1_2, result.winr[result.winr$winl == 1,]$didl)
      loser0_2 <- c(loser0_2, result.winr[result.winr$winl == 0,]$didl)
      
      # cross tabulation
      win.times <- table(c(winner2_0, winner2_1))
      lose.times <- table(c(loser0_2, loser1_2))
      win2_0.times <- table(winner2_0)
      win2_1.times <- table(winner2_1)
      lose1_2.times <- table(loser1_2)
      lose0_2.times <- table(loser0_2)
      
      # updates property
      private$nwin[names(win.times) %>% as.numeric] <- win.times
      private$nlose[names(lose.times) %>% as.numeric] <- lose.times
      private$fight.result.summary[names(win2_0.times) %>% as.numeric,]$w2_0 <- win2_0.times
      private$fight.result.summary[names(win2_1.times) %>% as.numeric,]$w2_1 <- win2_1.times
      private$fight.result.summary[names(lose1_2.times) %>% as.numeric,]$l1_2 <- lose1_2.times
      private$fight.result.summary[names(lose0_2.times) %>% as.numeric,]$l0_2 <- lose0_2.times
      
      private$deck.ranking <- order(private$fight.result.summary$w2_0 +
                                      private$fight.result.summary$w2_1,
                                    private$fight.result.summary$w2_0,
                                    private$fight.result.summary$w2_1,
                                    -private$fight.result.summary$l0_2,
                                    -private$fight.result.summary$l1_2, decreasing = T)
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
    },
    
    #' Adds fight result
    #'
    #' @param didl deck1 id
    #' @param didr deck2 id
    #' @param winl number of times of deck1 win
    #' @param winr number of times of deck2 win
    #'
    addFightResult = function(didl, didr, winl, winr){
      private$fight.result <- rbind(private$fight.result, c(didl, didr, winl, winr))
      colnames(private$fight.result) <- c("didl", "didr", "winl", "winr")
    },
    
    #' Sets modified fight result
    #'
    #' @param new.fight.result 
    #'
    modifyFightResult = function(new.fight.result){
      private$fight.result <- new.fight.result
      self$updateWinLose()
    }
    
  ),
  
  # private field -----------------------------------------------------------
  
  private = list(
    entry = NULL,
    
    fight.current = NULL,
    
    fight.result = NULL,
    fight.result.summary = NULL,
    
    nwin = NULL,
    nlose = NULL,
    
    deck.ranking = NULL,
    nfight.card = NULL,
    current.round = 0,
    
    shuffleOrder = function(x, decreasing = F){
      o <- order(runif(length(x)))
      o[order(x[o], decreasing = decreasing)]
    }
  ),
  
  # active binding ----------------------------------------------------------
  
  active = list(
    title.ja = function() private$entry$title.ja,
    title.en = function() private$entry$title.en,
    get.entry = function() private$entry,
    
    result = function() private$fight.result,
    result.complete = function(){
      ret <- private$fight.result[, c(1, 3, 4, 2)]
      ret$didl <- private$entry$deck[ret$didl]
      ret$didr <- private$entry$deck[ret$didr]
      colnames(ret) <- c("Deck1", "Win1", "Win2", "Deck2")
      return(ret)
    },
    result.summary = function() private$fight.result.summary,
    
    ranking = function() private$entry$deck[private$deck.ranking],
    ranking.id = function() private$deck.ranking,
    ranking.summary = function() data.frame(rank = order(self$ranking.id),
                                            deck = private$entry$deck,
                                            player = private$entry$deck.player, win = private$nwin),
    
    fight.card = function() data.frame(dnml = private$entry$deck[private$fight.current$didl],
                                       dnmr = private$entry$deck[private$fight.current$didr]),
    fight.card.id = function() private$fight.current,
    fight.card.list = function() list(left = as.character(self$fight.card$dnml),
                                      right = as.character(self$fight.card$dnmr)),
    
    ndeck = function() private$entry$ndeck,
    nplayer = function() private$entry$nplayer,
    nfcard = function() private$nfight.card,
    
    deck.player = function() private$entry$deck.player.id,
    deck.player.id = function() private$entry$deck.player.id,
    
    nwins = function() private$nwin,
    nloses = function() private$nlose,
    round = function(value){
      if (missing(value)) return(private$current.round)
      else private$current.round <- value
    }
  )
)
