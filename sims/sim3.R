Npercell <- 40
set.seed(6)
Sys.time()

library(tidyverse)
library(mirt)
library(here)

R.utils::sourceDirectory(here("R"))

runs_df <-
	crossing(
		n_items = 20,
		mean_easy = runif(Npercell, -2, 2),
		lnmean_disc = runif(Npercell, -0.5, 1.5),
		guess = c(0.03, 0.1, 0.25),
		n_students = c(1000, 5000, 10000)
	) %>%
	mutate(
		out = pmap(., run3),
		MRwin = out %>% map_dbl(~ which.max(.$elplMR)),
		MPwin = out %>% map_dbl(~ which.max(.$elplMP))
	)

runs_df %>% write_rds("sims/sim3.rds")
