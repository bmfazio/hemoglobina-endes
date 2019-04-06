# Config
options(encoding = "utf8")
options("stringsAsFactors" = FALSE)
#options(mc.cores = parallel::detectCores())
options(mc.cores = 2)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
packrat::set_opts(vcs.ignore.src = TRUE, vcs.ignore.lib = TRUE)
DATADIR <- ini::read.ini("config.ini")$local$DATADIR

# Run
source("scripts/setup.R")
source("scripts/load.R")
source("scripts/analysis.R")