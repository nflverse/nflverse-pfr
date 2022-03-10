source("auto/rushing.R")
source("auto/receiving.R")
source("auto/defense.R")
source("auto/passing.R")

list.files("build", full.names = TRUE) |> nflversedata::nflverse_upload("pfr_advstats")
