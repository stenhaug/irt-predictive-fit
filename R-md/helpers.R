fscores_to_p_model <- function(fscores, model, max = 1e-10){
	p_model <- get_p(model, fscores)
	p_model <- ifelse(p_model < max, max, p_model)
	p_model <- ifelse(p_model > (1 - max), 1 - max, p_model)
	p_model
}

get_p <- function(model, fscores){
	n_items <- length(model@Data$K)

	1:n_items %>%
		map(~ probtrace(extract.item(model, .), fscores)[ , 2]) %>%
		do.call(cbind, .)
}

dim_to_cov <- function(dim){

	if(dim == 3){
		return(paste0("COV_", c("11", "21", "31", "22", "32", "33")))
	}

	if(dim == 2){
		return(paste0("COV_", c("11", "21", "22")))
	}

	if(dim == 1){
		return(paste0("COV_", c("11")))
	}
}

make_model_df <- function(pars, p){

	nitems <- nrow(pars)
	dim <- ncol(pars) - 3

	items <-
		crossing(
			group = "all",
			item = 1:nitems,
			class = "dich",
			name = c(paste0("a", 1:dim), "d", "g", "u"),
			parnum = -1,
			value = -1,
			lbound = -Inf,
			ubound = Inf,
			est = FALSE,
			prior.type = "none",
			prior_1 = NaN,
			prior_2 = NaN
		) %>%
		mutate(
			item = rep(paste0("Item_", 1:nitems), each = dim + 3),
			value = pars %>% as.matrix() %>% t() %>% as.vector()
		)

	groups <-
		tibble(name = c(paste0("MEAN_", 1:dim), dim_to_cov(dim))) %>%
		mutate(
			group = "all",
			item = "GROUP",
			class = "GroupPars",
			parnum = -1,
			value = -1,
			lbound = -Inf,
			ubound = Inf,
			est = FALSE,
			prior.type = "none",
			prior_1 = NaN,
			prior_2 = NaN
		) %>%
		select(group, item, class, name, everything()) %>%
		mutate(value = case_when(
			str_detect(name, "^MEAN") ~ 0,
			str_sub(name, 5, 5) == str_sub(name, 6, 6) ~ 1,
			TRUE ~ p
		))

	bind_rows(items, groups) %>%
		mutate(parnum = row_number())
}
