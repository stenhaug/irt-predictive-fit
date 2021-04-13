theta_pars_to_p <- function(theta, pars){
	n_students <- length(theta)
	n_items <- nrow(pars)

	p <- matrix(nrow = n_students, ncol = n_items)

	for (i in 1:n_students){
		for (j in 1:n_items){
			p[i, j] <- pars$g[j] + (1 - pars$g[j]) * boot::inv.logit(pars$a1[j] * theta[i] + pars$d[j])
		}
	}

	p
}
