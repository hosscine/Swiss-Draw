
loadEntryData <- function(file){
  if(!missing(file)) load(file)
  ent <- entryData$new(deck.names = as.character(deck$deck.name), deck.player.id = deck$member.id,
                       player.names = member, entry.title = title)
  return(ent)
}


entryData <- R6::R6Class(
  "entryData",

  # public field ------------------------------------------------------------

  public = list(
    initialize = function(deck.names, deck.player.id, player.names, entry.title){
      private$titlename <- entry.title 
      private$decks <- deck.names
      private$decks.player.id <- deck.player.id
      private$players <- player.names
    }
  ),

  # private field -----------------------------------------------------------

  private = list(
    titlename = NULL,
    
    decks = NULL,
    decks.player.id = NULL,
    
    players = NULL
  ),

  # active binding-----------------------------------------------------------

  active = list(
    title = function() private$titlename,
    
    # deck property ------------------------------------------
    ndeck = function() private$decks %>% length,
    deck = function() private$decks,
    deck.id = function(){
      ret <- 1:self$ndeck; names(ret) <- self$deck; return(ret)},
    deck.player = function(){
      ret <- private$players[private$decks.player.id]; names(ret) <- self$deck; return(ret)},  
    deck.player.id = function(){
      ret <- private$decks.player.id; names(ret) <- self$deck; return(ret)},

    # player property ------------------------------------------
    nplayer = function() private$players %>% length,    
    player = function() private$players,
    player.id = function(){
      ret <- 1:self$nplayer; names(ret) <- self$player; return(ret)}
    
  )
)
