require(magrittr)

source("entry_class.R")
source("tournament_class.R")

# save(deck, member, title, file = "sample3.entry")
ent <- loadEntryData("sample3.entry")
tor <- startTournament(ent)