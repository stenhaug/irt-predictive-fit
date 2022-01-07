library(tidyverse)
library(mirt)
library(here)
runs <- 2000
set.seed(10)
R.utils::sourceDirectory(here("R"))

table4 <-
	read_rds(here("data-parameters/table4.rds")) %>%
	select(a1 = a, d = b, g = c) %>%
	mutate(d = -d, u = 1)

runs_df <-
	tibble(
		true_model = rep("3PL", runs),
		n_items = rep(20, runs),
		n_students = runif(runs, 100, 10000),
		theta_mu = runif(runs, -2, 2)
	) %>%
	arrange(true_model, n_items, n_students, theta_mu)

out <-
	runs_df %>%
	mutate(
		out = pmap(., run),
		MRwin = out %>% map_dbl(~ which.max(.$elplMR)),
		MPwin = out %>% map_dbl(~ which.max(.$elplMP))
	)

out %>% write_rds("sims/sim2.rds")
