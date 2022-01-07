calc_loglik_ghq_person <- function(model, data, quad_points, theta_span){
	theta_mu <- coef(model)$GroupPars[ , "MEAN_1"]

	theta <- matrix(seq(theta_mu - theta_span, theta_mu + theta_span, length.out = quad_points))

	prior <-
		dnorm(
			theta[, 1],
			mean = theta_mu,
			sd = sqrt(coef(model)$GroupPars[ , "COV_11"])
		)

	prior <- prior / sum(prior)

	avg_likelihood_of_each_response <-
		mirt:::Estep.mirt( # this could be made more efficient by using tab data and freq
			pars = model@ParObjects$pars,
			tabdata = make_fulldata(data),
			freq = rep(1, nrow(data)),
			CUSTOM.IND = model@Internals$CUSTOM.IND,
			Theta = theta,
			prior = prior,
			itemloc = model@Model$itemloc,
			full = FALSE,
			Etable = TRUE,
			omp_threads = 1
		)$expected

	avg_likelihood_of_each_response
}
