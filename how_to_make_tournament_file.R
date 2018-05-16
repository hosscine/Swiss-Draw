
# required packages -------------------------------------------------------
  # myfs
  # magrittr
  # R6

source("entry_class.R")
source("tournament_class.R")


# edit tournament title ---------------------------------------------------
title <- "battle of A and B"


# edit player names -------------------------------------------------------
member <- c("A", "B")


# edit deck.name and deck.player.id ---------------------------------------
deck <- data.frame(
  deck.names = c("Kaiju", "Kozmo"),
  deck.player.id = c(1, 2)
)

# save(deck, member, title, file = "entry_name.entry")
# ent <- loadEntryData("entry_name.entry")
ent <- loadEntryData()
tor <- startTournament(ent)


# edit the file name ------------------------------------------------------
save(tor, file = "tornament_name.tournament")


# RUN SHINY APP and READ TOURNAMENT FILE! ---------------------------------