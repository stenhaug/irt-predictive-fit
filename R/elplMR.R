elplMR <- function(data, p_true, model, estimation, max = 1e-10){
	insample_fscores <- fscores(model, method = estimation)[ , 1]

	p_model <- theta_pars_to_p(insample_fscores, mirt_model_coefs(model))

	p_model <- ifelse(p_model < max, max, p_model)
	p_model <- ifelse(p_model > (1 - max), 1 - max, p_model)

	p_model_correct <- p_model^p_true * (1 - p_model)^(1 - p_true)

	sum(log(p_model_correct))
}
