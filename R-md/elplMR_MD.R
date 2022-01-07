fscores_to_p_model <- function(fscores, model, max = 1e-10){
	p_model <- get_p(model, fscores)
	p_model <- ifelse(p_model < max, max, p_model)
	p_model <- ifelse(p_model > (1 - max), 1 - max, p_model)
	p_model
}

elplMR_MD <- function(p_model, p_true){
	p_model_correct <- p_model^p_true * (1 - p_model)^(1 - p_true)
	sum(log(p_model_correct))
}
