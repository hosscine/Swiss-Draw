
loadEntryData <- function(file){
  load(file)
  ent <- entryData$new(deck.names = as.character(deck$deck.name), deck.user.id = deck$member.id,
                       user.names = member)
  return(ent)
}


entryData <- R6::R6Class(
  "entryData",

# public field ------------------------------------------------------------

  public = list(
    initialize = function(deck.names, deck.user.id, user.names){
      private$decks <- deck.names
      private$deck.user.id <- deck.user.id
      private$users <- user.names
    }
  ),

# private field -----------------------------------------------------------

  private = list(
    decks = NULL,
    deck.user.id = NULL,
    
    users = NULL
  ),

# active binding-----------------------------------------------------------

  active = list(
    deck = function() return(private$decks),
    user = function() return(private$users),
    deck.user = function() return(private$users[private$deck.user.id])
  )
)