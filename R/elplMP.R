elplMP <- function(data, pars, model, theta_mu, theta_var){
	srvp <-
		data %>%
		as_tibble() %>%
		map(unique) %>%
		do.call(crossing, .) %>%
		arrange_all(~ desc(.))

	model_truth <-
		mirt(
			data,
			1,
			pars =
				mod2values(model) %>%
				mutate(
					value = c(pars %>% as.matrix() %>% t() %>% as.vector(), theta_mu, theta_var),
					est = FALSE
				),
		)

	table <-
		srvp %>%
		mutate(
			p =
				calc_loglik_ghq_person(
					model_truth,
					srvp %>% as.matrix(),
					100,
					6
				),

			log_lik_given_model =
				calc_loglik_ghq_person(
					model,
					srvp %>% as.matrix(),
					100,
					6
				) %>%
				log()
		)

	weighted.mean(table$log_lik_given_model, table$p)
}
