full <- function(dim = 1, items = "Rasch", engine = "QMCEM"){
	mirt(wide, dim, itemtype = items, method = engine)
}

make_folds <- function(w){
	vector_to_fold <- function(x){
		out <- x
		notna <- which(!is.na(as.numeric(x)))
		one_to_eight <- rerun(100, sample(1:8)) %>% unlist()
		out[notna] <- one_to_eight[1:length(notna)]
		out
	}

	w %>% apply(1, vector_to_fold) %>% t()
}

model_fscores_to_p <- function(m, f){
	1:length(m@Model$itemtype) %>% map(~ probtrace(extract.item(m, .), f)[ , 2]) %>% bind_cols() %>% as.matrix()
}

make_folds <- function(w){
	vector_to_fold <- function(x){
		out <- x
		notna <- which(!is.na(as.numeric(x)))
		one_to_eight <- rerun(100, sample(1:8)) %>% unlist()
		out[notna] <- one_to_eight[1:length(notna)]
		out
	}

	w %>% apply(1, vector_to_fold) %>% t()
}

get_train <- function(the_fold, folds, partition, hold_fold){
	wide_train <- partition

	for (x in c(the_fold, hold_fold)){
		wide_train[folds == x] <- NA
	}

	wide_train
}

get_test <- function(the_fold, folds, partition){
	wide_test <- partition
	wide_test[folds != the_fold] <- NA
	wide_test
}

get_p_from_anova <- function(mod1, mod2){

	if(is.null(mod1) | is.null(mod2)){return(-999)}

	if("logical" %in% c(class(mod1), class(mod2))) {return(NA_real_)} # catches missing
	anova(mod1, mod2)$p[2]
}

get_split <- function(id, id2, splits){
	splits$splits[[which(splits$id == id & splits$id2 == id2)]]
}

mirt_calc_log_lik <- function(method, model, data, draws){
	if(method == "EM"){
		return(mirt_em_calc_log_lik_marg(model, data))
	}

	if(method == "QMCEM"){
		return(mirt_mci_calc_log_lik_marg(model, data, draws))
	}
}

mirt_em_calc_log_lik_marg <- function(model, data){
	mirt:::Estep.mirt(
		pars = model@ParObjects$pars,
		tabdata = make_fulldata(data),
		freq = rep(1, nrow(data)),
		CUSTOM.IND = model@Internals$CUSTOM.IND,
		Theta = model@Model$Theta,
		prior = model@Internals$Prior[[1]],
		itemloc = model@Model$itemloc,
		full = FALSE,
		Etable = TRUE,
		omp_threads = 1
	)$expected %>%
		log()
}

mirt_mci_calc_log_lik_marg <- function(model, data, draws){
	dim <- ncol(model@Fit$F)

	mirt:::Estep.mirt(
		pars = model@ParObjects$pars,
		tabdata = make_fulldata(data),
		freq = rep(1, nrow(data)),
		CUSTOM.IND = model@Internals$CUSTOM.IND,
		Theta = MASS::mvrnorm(n = draws, mu = rep(0, dim), Sigma = diag(dim)),
		prior = rep(1 / draws, draws),
		itemloc = model@Model$itemloc,
		full = FALSE,
		Etable = TRUE,
		omp_threads = 1
	)$expected %>%
		log()
}

make_fulldata <- function(data){
	data <- as.matrix(data)
	wrong <- 1 - data
	right <- data
	colnames(wrong) <- glue::glue("Item.{1:ncol(data)}_1")
	colnames(right) <- glue::glue("Item.{1:ncol(data)}_2")
	out <- cbind(wrong, right)[, order(c(seq(ncol(wrong)), seq(ncol(right))))]

	# I don't totally get this but I think we just make NA = 0
	# So that they are disregarded from the likelihood calculation
	out[is.na(out)] <- 0
	out
}

evaluate_p <- function(p, train, test){
	list(
		train_acc = mean((p > 0.5) == train, na.rm = TRUE),
		test_acc = mean((p > 0.5) == test, na.rm = TRUE),
		train_rmse = sum((train - p)^2, na.rm = TRUE) / sum(!is.na(train)),
		test_rmse = sum((test - p)^2, na.rm = TRUE) / sum(!is.na(test))
	)
}

get_p <- function(model, fscores){
	n_items <- length(model@Data$K)

	1:n_items %>%
		map(~ probtrace(extract.item(model, .), fscores)[ , 2]) %>%
		do.call(cbind, .)
}

mr_cv <- function(dim = 1, items = "Rasch", engine = "QMCEM", ncyc = 200){
	tibble(the_fold = 1:8) %>%
		mutate(
			model = the_fold %>% map(~ mirt(get_train(., folds, wide, 999), dim, itemtype = items, method = engine, technical = list(NCYCLES = ncyc))),
			fscores = map(model, ~ fscores(., rotate = "none", QMC = engine == "QMCEM")),
			p = map2(model, fscores, model_fscores_to_p),
			lik = map2_dbl(the_fold, p, ~ sum(log(ifelse(as.matrix(get_test(.x, folds, wide)) == 1, .y, 1 - .y)), na.rm = TRUE)),
			acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, wide)), na.rm = TRUE))
		)
}

mp_cv <- function(dim = 1, items = "Rasch", engine = "QMCEM", ncyc = 200){
	tibble(the_fold = 1:8) %>%
		mutate(
			in_model = the_fold %>% map(~ mirt(training(splits$splits[[.]]), dim, itemtype = items, method = engine, technical = list(NCYCLES = ncyc))),
			out_data = the_fold %>% map(~ testing(splits$splits[[.]])),
			log_lik =
				pmap_dbl(
					list(engine, in_model, out_data),
					~ mirt_calc_log_lik(..1, ..2, ..3, draws = 100000) %>% sum()
				)
		)
}
