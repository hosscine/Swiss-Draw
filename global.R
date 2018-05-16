require(magrittr)
require(myfs)

source("entry_class.R")
source("tournament_class.R")

# save(deck, member, title, file = "sample.entry")
load("sample.tournament")
ent <- tor$get.entry