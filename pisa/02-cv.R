library(tidyverse)
library(rsample)
library(mirt)

source("pisa/cvhelpers.R")

dataformodel <- read_rds("data-pisa/dataformodel.rds")

wide <- dataformodel[ , -(1:9)]
folds <- make_folds(wide)
splits <- vfold_cv(wide, v = 8, repeats = 1)

out <-
	bind_rows(
		tibble(dim = 1, items = "Rasch"),
		crossing(dim = 1:3, items = str_glue("{2:3}PL"))
	) %>%
	mutate(engine = ifelse(dim >= 4, "QMCEM", "EM")) %>%
	mutate(full = pmap(., full)) %>%
	mutate(
		npars = full %>% map_dbl(~ .@Model$nestpars),
		aic = full %>% map_dbl(~ .@Fit$AIC),
		bic = full %>% map_dbl(~ .@Fit$BIC)
	) %>%
	arrange(npars) %>%
	mutate(
		mr_cv = pmap(select(., dim:engine), mr_cv),
		mp_cv = pmap(select(., dim:engine), mp_cv)
	)

out %>%
	arrange(npars) %>%
	mutate(
		anova_p = map2_dbl(full, lag(full), get_p_from_anova),
		mr_lik = mr_cv %>% map_dbl(~ sum(.$lik)),
		mr_acc = mr_cv %>% map_dbl(~ mean(.$acc)),
		mp_lik = mp_cv %>% map_dbl(~ sum(.$log_lik)),
	) %>%
	mutate(
		itemscorrect = mr_acc * 9800 * 17
	) %>%
	write_rds("pisa/cvpisa.rds")

# out %>%
# 	arrange(npars) %>%
# 	slice(-1) %>%
# 	mutate(
# 		anova_p = map2_dbl(full, lag(full), get_p_from_anova),
# 		mr_lik = mr_cv %>% map_dbl(~ sum(.$lik)),
# 		mr_acc = mr_cv %>% map_dbl(~ mean(.$acc)),
# 		mp_lik = mp_cv %>% map_dbl(~ sum(.$log_lik)),
# 	) %>%
# 	mutate(name = str_glue("{dim}F, {items} ({npars})")) %>%
# 	mutate(aic = -aic, bic = -bic) %>%
# 	select(name, npars, aic, bic, mr_lik:mp_lik) %>%
# 	gather(var, val, -name, -npars) %>%
# 	ggplot(aes(x = npars, y = val)) +
# 	geom_label(aes(label = name), size = 2) +
# 	facet_wrap(~ var, scales = "free") +
# 	labs(x = "Item parameters")
#
