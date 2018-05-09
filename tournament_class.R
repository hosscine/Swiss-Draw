startTournament <- function(entry){
  tor <- tornament
}


tournament <- R6::R6Class(
  "tournament",
  
# public field ------------------------------------------------------------

  public = list(
    
    initialize = function(entry){
      private$entry <- entry
      
      private$fight.history <- rbind( c(1, 5, 2, 1),
                                      c(3, 4, 0, 2))
    }
  ),

# private field -----------------------------------------------------------

  private = list(
    entry = NULL,
    
    fight.history = NULL
  ),

# active binding ----------------------------------------------------------

  active = list(
    

  )
)
