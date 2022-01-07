generate_sim <- function(pars, true_model, n_students, theta_mu){

	stopifnot(true_model %in% c("1PL", "2PL", "3PL"))

	if (true_model == "1PL"){
		sim <-
			simdata(
				d = matrix(pars$d, nrow = nrow(pars), ncol = 1),
				a = matrix(pars$a1, nrow = nrow(pars), ncol = 1),
				N = n_students,
				mu = theta_mu,
				sigma = diag(1),
				itemtype = rep("2PL", nrow(pars)),
				returnList = TRUE
			)
	}

	if (true_model == "2PL"){
		sim <-
			simdata(
				d = matrix(pars$d, nrow = nrow(pars), ncol = 1),
				a = matrix(pars$a1, nrow = nrow(pars), ncol = 1),
				N = n_students,
				mu = theta_mu,
				sigma = diag(1),
				itemtype = rep("2PL", nrow(pars)),
				returnList = TRUE
			)
	}

	if (true_model == "3PL"){
		sim <-
			simdata(
				d = matrix(pars$d, nrow = nrow(pars), ncol = 1),
				a = matrix(pars$a1, nrow = nrow(pars), ncol = 1),
				guess = pars$g,
				N = n_students,
				mu = theta_mu,
				sigma = diag(1),
				itemtype = rep("3PL", nrow(pars)),
				returnList = TRUE
			)
	}

	sim
}
