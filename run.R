# Config
options(encoding = "utf8")
options("stringsAsFactors" = FALSE)
options(mc.cores = parallel::detectCores())
DATADIR <- ini::read.ini("config.ini")$local$DATADIR

# Run
source("scripts/setup.R")
source("scripts/load.R")