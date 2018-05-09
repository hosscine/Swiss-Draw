
loadEntryData <- function(file = "sample2.entry"){
  load(file)
  ent <- entryData$new(deck.names = as.character(deck$deck.name), deck.player.id = deck$member.id,
                       player.names = member)
  return(ent)
}


entryData <- R6::R6Class(
  "entryData",

  # public field ------------------------------------------------------------

  public = list(
    initialize = function(deck.names, deck.player.id, player.names){
      private$decks <- deck.names
      private$decks.player.id <- deck.player.id
      private$players <- player.names
    }
  ),

  # private field -----------------------------------------------------------

  private = list(
    decks = NULL,
    decks.player.id = NULL,
    
    players = NULL
  ),

  # active binding-----------------------------------------------------------

  active = list(
    ndeck = function() private$decks %>% length,
    nplayer = function() private$players %>% length,
    
    deck = function() private$decks,
    
    deck.id = function(){
      ret <- 1:self$ndeck; names(ret) <- self$deck; return(ret)},
    
    deck.player = function(){
      ret <- private$players[private$decks.player.id]; names(ret) <- self$deck; return(ret)},  
    
    deck.player.id = function(){
      ret <- private$decks.player.id; names(ret) <- self$deck; return(ret)},
    
    player = function() private$players,
    
    player.id = function(){
      ret <- 1:self$nplayer; names(ret) <- self$player; return(ret)}
  )
)
