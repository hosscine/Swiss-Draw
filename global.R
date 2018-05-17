require(magrittr)
require(myfs)

source("entry_class.R")
source("tournament_class.R")

# save(deck, member, title, file = "sample.entry")
datapath <- "tournaments/sample.tournament"
load(datapath)
ent <- tor$get.entry
