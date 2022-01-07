N <- 2000
set.seed(1)

options(warn=-1)
library(tidyverse)
library(mirt)
library(here)
R.utils::sourceDirectory(here("R"))
R.utils::sourceDirectory(here("R-md"))

runs_df <-
	tibble(
		dim = 2,
		nitems = 20,
		p = runif(N, 0, 0.95),
		n_students = sample(500:10000, N, replace = TRUE),
		ncyc = 100
	) %>%
	mutate(out = pmap(., run_MD))

Sys.time()

runs_df %>% write_rds("sims/sim4.rds")
