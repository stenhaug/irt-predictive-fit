run_MD_withcv <- function(dim, nitems, p, n_students, ncyc){
	# generate from 2f --------------------------------------------------------
	sigma <- matrix(rep(p, dim^2), ncol = dim)
	diag(sigma) <- 1

	theta <- MASS::mvrnorm(n_students, mu = rep(0, dim), Sigma = sigma)
	disc <- matrix(rlnorm(dim * nitems, 0, 0.5), nrow = nitems)
	disc[nitems, 2] <- 0
	easy <- rnorm(nitems, 0, 1)

	sim <-
		simdata(
			a = disc,
			d = easy,
			itemtype = "2PL",
			Theta = theta,
			returnList = TRUE
		)

	pars <-
		cbind(disc, easy, rep(0, nitems), rep(1, nitems)) %>%
		as.data.frame() %>%
		as_tibble() %>%
		set_names(c(paste0("a", 1:dim), "d", "g", "u"))

	# fit 1f and 2f -----------------------------------------------------------
	mod1f <- mirt(sim$data, 1, "2PL", technical = list(NCYCLES = ncyc))
	mod2f <- mirt(sim$data, 2, "2PL", technical = list(NCYCLES = ncyc))
	mod2f@Options$exploratory <- FALSE

	# intermediate steps for elplMR -------------------------------------------
	p_true <-
		1:nitems %>%
		map(~ probtrace(sim$itemobjects[[.]], theta)[ , 2]) %>%
		do.call(cbind, .)

	f1 <- fscores(mod1f, method = "EAP", rotate = "none", QMC = FALSE)
	f2 <- fscores(mod2f, method = "EAP", rotate = "none", QMC = FALSE)

	# CV ----------------------------------------------------------------------
	wide <- sim$data
	folds <- make_folds(wide)
	splits <- vfold_cv(wide, v = 8, repeats = 1)

	mp_one <- mp_cv(1, "2PL", "EM", 100, splits)
	mp_two <- mp_cv(2, "2PL", "EM", 100, splits)

	mr_one <- mr_cv(1, "2PL", "EM", 100, wide, folds)
	mr_two <- mr_cv(2, "2PL", "EM", 100, wide, folds)

	# output ------------------------------------------------------------------
	list(
		f1_cv_mp_lik = sum(mp_one$log_lik),
		f2_cv_mp_lik = sum(mp_two$log_lik),

		f1_cv_mr_lik = sum(mr_one$lik),
		f2_cv_mr_lik = sum(mr_two$lik),

		f1_cv_mr_acc = mean(mr_one$acc),
		f2_cv_mr_acc = mean(mr_two$acc),


		f1_AIC = mod1f@Fit$AIC,
		f1_BIC = mod1f@Fit$BIC,
		f2_AIC = mod2f@Fit$AIC,
		f2_BIC = mod2f@Fit$BIC,
		p = anova(mod1f, mod2f)$p[2],

		f1_elplMP = elplMP_MD(sim$data, mod1f, pars, p, only_observed = nitems > 20),
		f2_elplMP = elplMP_MD(sim$data, mod2f, pars, p, only_observed = nitems > 20),

		f1_elplMR = f1 %>% fscores_to_p_model(mod1f) %>% elplMR_MD(p_true),
		f2_elplMR = f2 %>% fscores_to_p_model(mod2f) %>% elplMR_MD(p_true),

		f1_MRmad = abs(p_true - fscores_to_p_model(f1, mod1f)) %>% median(),
		f2_MRmad = abs(p_true - fscores_to_p_model(f2, mod2f)) %>% median()
	)
}
