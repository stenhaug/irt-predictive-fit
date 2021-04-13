draw_item_pars <- function(n_items, mean_easy, lnmean_disc, guess){
	tibble(
		a1 = rlnorm(n_items, lnmean_disc, 0.5),
		d = rnorm(n_items, mean_easy, 1),
		g = guess
	) %>%
		mutate(u = 1)
}
