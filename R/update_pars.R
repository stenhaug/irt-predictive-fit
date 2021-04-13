update_pars <- function(pars, true_model, n_items){
	stopifnot(true_model %in% c("1PL", "2PL", "3PL"))

	if (true_model == "1PL"){
		pars <- pars %>% mutate(a1 = 1, g = 0)
	}

	if (true_model == "2PL"){
		pars <- pars %>% mutate(g = 0)
	}

	pars %>% slice(1:n_items)
}


