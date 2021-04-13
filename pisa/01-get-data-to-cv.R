# just want coded responses (starts with D and ends with C)
# or scored responses (starts with C and ends with S)
# 2nd letter always S to just get science item

library(tidyverse)
library(mirt)
library(rsample)

# get going ---------------------------------------------------------------

cog <- read_rds("data-pisa/cog_science_computer_binary.rds")

# C = 126 computer scored questions, D = 57 human coded questions
names(cog)[-(1:9)] %>% str_sub(1, 1) %>% table()

cog$CNTRYID %>% table()

# see form lookup this corresponds to booklet 25 science items with atleast 90% response
# rate

cog %>%
	filter(BOOKID == "Form 91 (CBA)", CBASCI == "Science Random Number 6") %>%
	map_dbl(~ mean(is.na(.))) -> okay


# data for model ----------------------------------------------------------
dataformodel <- cog[ , okay < 0.1] %>% na.omit()

dataformodel[ , -(1:9)] %>%
	gather(var, val) %>%
	group_by(var) %>%
	summarize(mean(val))

dataformodel %>% write_rds("data-pisa/dataformodel.rds")

