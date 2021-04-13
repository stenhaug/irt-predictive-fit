library(tidyverse)
library(foreign)

data <-
	foreign::read.spss("data-raw/CY6_MS_CMB_STU_COG.sav") %>%
	as_tibble() %>%
	mutate_all(as.character)

data %>%
	select(
		CNTRYID, CNTSTUID, Region, STRATUM, SUBNATIO,
		ADMINMODE, LANGTEST_COG, BOOKID, CBASCI,
		matches("^(C|D)S.+(\\dC$|\\dS)$")
	) %>%
	write_rds("data-raw-ish/cog_science.rds")

binarize <- function(x){
	case_when(
		str_detect(x, "Full credit") ~ 1L,
		str_detect(x, "Partial credit") ~ 1L,
		str_detect(x, "No credit") ~ 0L,
		TRUE ~ NA_integer_
	)
}

cog_science <-
	read_rds("~/Desktop/cog_science.rds") %>%
	filter(ADMINMODE == "Computer") %>%
	mutate_at(vars(matches("^(C|D)S.+(\\dC$|\\dS)$")), binarize)

cog_science %>% write_rds("~/Desktop/cog_science_computer_binary.rds")
