startTournament <- function(entry) tournament$new(entry)

require(myfs)

tournament <- R6::R6Class(
  "tournament",
  
# public field ------------------------------------------------------------

  public = list(
    
    initialize = function(entry){
      private$entry <- entry
      
      private$fight.result <- rbind( c(1, 5, 2, 1),
                                      c(3, 4, 0, 2))
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
    }
  ),

# private field -----------------------------------------------------------

  private = list(
    entry = NULL,
    
    fight.result = NULL
  ),

# active binding ----------------------------------------------------------

  active = list(
    
    #' Player id matrix of past fight card
    #' 
    result.pid = function() private$fight.result[, 1:2]
  )
)
