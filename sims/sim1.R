set.seed(100)
runspercell <- 500

Sys.time()

library(tidyverse)
library(mirt)
library(here)
R.utils::sourceDirectory(here("R"))

table4 <-
	read_rds(here("data-parameters/table4.rds")) %>%
	select(a1 = a, d = b, g = c) %>%
	mutate(d = -d, u = 1)

runs_df <-
	crossing(
		true_model = c("1PL", "2PL", "3PL"),
		n_items = c(20),
		n_students = c(500, 1000),
		theta_mu = c(0)
	) %>%
	slice(rep(row_number(), runspercell)) %>% # RUNS
	arrange(true_model, n_items, n_students, theta_mu)

out <-
	runs_df %>%
	mutate(
		out = pmap(., run),
		MRwin = out %>% map_dbl(~ which.max(.$elplMR)),
		MPwin = out %>% map_dbl(~ which.max(.$elplMP))
	)

Sys.time()

out %>% write_rds("sims/sim1.rds")
