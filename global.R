require(magrittr)
require(myfs)

source("entry_class.R")
source("tournament_class.R")

# save(deck, member, title, file = "sample.entry")

if (file.exists(".default")){
  load(".default")
  load("default.tournament")
}
if (!file.exists(".default")) load("sample.tournament")
ent <- tor$get.entry