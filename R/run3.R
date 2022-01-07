run3 <- function(n_items, mean_easy, lnmean_disc, guess, n_students){
	# 3 refers to the third simulation of the paper
	# run3(n_items = 20, mean_easy = 1, lnmean_disc = 0.75, guess = 0.25, n_students = 100)

	pars <- draw_item_pars(n_items, mean_easy, lnmean_disc, guess)

	sim <- generate_sim(pars, "3PL", n_students, theta_mu = 0)

	p_true <- theta_pars_to_p(sim$Theta[ , 1], pars)

	data_analysis_models <-
		tibble(
			model =
				c("Rasch", "2PL", "3PL") %>%
				map(~ mirt(sim$data, 1, ., verbose = FALSE, technical = list(NCYCLES = 500)))
		) %>%
		mutate(
			AIC = model %>% map_dbl(~ .@Fit$AIC),
			BIC = model %>% map_dbl(~ .@Fit$BIC),
			p_from_lr = map2_dbl(model, lag(model), anova_pvalue),
		) %>%
		mutate(
			elplMR = map_dbl(model, ~ elplMR(sim$data, p_true, ., "EAP", max = 1e-10)),
			elplMP = map_dbl(model, ~ elplMP(sim$data, pars, ., 0, 1))
		) %>%
		mutate(model = NULL)

	data_analysis_models
}
