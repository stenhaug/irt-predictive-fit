quiet <- function(x) {
	sink(tempfile())
	on.exit(sink())
	invisible(force(x))
}

mirt_model_coefs <- function(mirt_model){
	coef(mirt_model) %>%
		magrittr::extract(-length(.)) %>%
		do.call(rbind, .) %>%
		as_tibble()
}

make_fulldata <- function(data){
	wrong <- 1 - data
	right <- data
	colnames(wrong) <- glue::glue("Item.{1:ncol(data)}_1")
	colnames(right) <- glue::glue("Item.{1:ncol(data)}_2")
	cbind(wrong, right)[, order(c(seq(ncol(wrong)), seq(ncol(right))))]
}
