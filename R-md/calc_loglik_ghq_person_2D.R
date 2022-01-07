calc_loglik_ghq_person_2D <- function(model, data, only_observed){

	dim <- model@Model$nfact

	sigma <- matrix(rep(coef(model)$GroupPars[4], dim^2), ncol = dim)
	diag(sigma) <- 1

	theta <-
		crossing(
			one = seq(-6, 6, 0.25),
			two = seq(-6, 6, 0.25)
		) %>%
		as.matrix()
	prior <- mvtnorm::dmvnorm(theta, c(0, 0), sigma, log=FALSE)
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
