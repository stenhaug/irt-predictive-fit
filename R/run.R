run <- function(n_items, n_students, theta_mu, true_model){
	# f(n_items = 20, n_students = 300, theta_mu  = -1, true_model = "3PL")
	pars <- update_pars(table4, true_model, n_items)

	sim <- generate_sim(pars, true_model, n_students, theta_mu)

	p_true <- theta_pars_to_p(sim$Theta[ , 1], pars)

	data_analysis_models <-
		tibble(
			model =
				c("Rasch", "2PL", "3PL") %>%
				map(~ mirt(sim$data, 1, ., verbose = TRUE, technical = list(NCYCLES = 500)))
		) %>%
		mutate(
			AIC = model %>% map_dbl(~ .@Fit$AIC),
			BIC = model %>% map_dbl(~ .@Fit$BIC),
			p_from_lr = map2_dbl(model, lag(model), anova_pvalue),
		) %>%
		mutate(
			elplMR = map_dbl(model, ~ elplMR(sim$data, p_true, ., "EAP", max = 1e-10)),
			elplMP = map_dbl(model, ~ elplMP(sim$data, pars, ., theta_mu, 1))
		) %>%
		mutate(model = NULL)

	data_analysis_models
}
