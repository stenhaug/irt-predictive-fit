elplMP_MD <- function(data, model, pars, p){

	srvp <-
		data %>%
		as_tibble() %>%
		map(unique) %>%
		do.call(crossing, .) %>%
		arrange_all(~ desc(.))

	model_truth <-
		mirt(
			data,
			ncol(pars) - 3,
			pars = as.data.frame(make_model_df(pars, p))
		)

	table <-
		srvp %>%
		mutate(
			p =
				calc_loglik_ghq_person_2D(
					model_truth,
					srvp %>% as.matrix(),
					FALSE
				)
		)

	if(model@Model$nfact == 1){
		table$log_lik_given_model <-
			calc_loglik_ghq_person(
				model,
				srvp %>% as.matrix(),
				100,
				6
			) %>%
			log()
	}

	if(model@Model$nfact == 2){
		table$log_lik_given_model <-
			calc_loglik_ghq_person_2D(
				model,
				srvp %>% as.matrix(),
				FALSE
			) %>%
			log()
	}

	weighted.mean(table$log_lik_given_model, table$p)
}
