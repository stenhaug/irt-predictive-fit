library(tidyverse)

# this reproduces table 4 from irt model selection methods for dich items kang and cohen
# note i dont know if these are item difficulties or easiness but i suspect difficulties because thats the term they use in the paper

# raw data using datapasta
a1 <- c(1.1005, NA, 2.2093, NA, 1.4493, NA, 0.7514, NA, 1.5789, NA, 0.6425, NA, 1.6254, NA, 1.3415, NA, 0.918, NA, 1.8027, NA, 0.8159, NA, 0.9375, NA, 0.9126, NA, 1.9395, NA, 0.3746, NA, 0.673, NA, 0.4166, NA, 1.2093, NA, 0.9486, NA, 1.4916)
b1 <- c("0.4078", NA, "0.5696", NA, "–1.0610", NA, "–0.2437", NA, "0.3206", NA, "–1.3762", NA, "–0.9800", NA, "–0.6881", NA, "–0.3526", NA, "0.2400", NA, "0.5917", NA, "1.8891", NA, "–0.2690", NA, "0.3673", NA, "–0.9681", NA, "–1.2601", NA, "0.5225", NA, "–1.3356", NA, "0.9515", NA, "0.9811")
c1 <- c(0.2228, NA, 0.2332, NA, 0.2337, NA, 0.1445, NA, 0.2581, NA, 0.2712, NA, 0.1232, NA, 0.1954, NA, 0.2709, NA, 0.2984, NA, 0.0587, NA, 0.1405, NA, 0.2339, NA, 0.2387, NA, 0.3527, NA, 0.1206, NA, 0.1244, NA, 0.1167, NA, 0.2787, NA, 0.1923)
a2 <- c(0.5659, NA, 0.6128, NA, 1.1037, NA, 1.9886, NA, 0.5691, NA, 1.0346, NA, 1.1384, NA, 3.3488, NA, 2.6306, NA, 0.6652, NA, 1.0342, NA, 1.0163, NA, 1.2945, NA, 1.6521, NA, 0.9696, NA, 1.2369, NA, 0.7812, NA, 0.7728, NA, 0.5441, NA, 1.4025)
b2 <- c("–0.1257", NA, "–0.7826", NA, "0.0615", NA, "0.4244", NA, "–0.7350", NA, "0.9836", NA, "–1.2651", NA, "–0.2252", NA, "–0.6576", NA, "1.7007", NA, "1.0805", NA, "–2.0452", NA, "0.1627", NA, "0.0573", NA, "1.2171", NA, "2.1226", NA, "0.4228", NA, "–0.1656", NA, "–0.2055", NA, "1.2841")
c2 <- c(0.3426, NA, 0.1925, NA, 0.2324, NA, 0.1396, NA, 0.2059, NA, 0.3124, NA, 0.1832, NA, 0.1811, NA, 0.2537, NA, 0.2184, NA, 0.2261, NA, 0.3464, NA, 0.1455, NA, 0.3861, NA, 0.1046, NA, 0.1656, NA, 0.2696, NA, 0.178, NA, 0.1961, NA, 0.2917)

# clean up
a <- c(a1, a2)
a <- a[!is.na(a)]
b <- c(b1, b2)
b <- as.numeric(stringr::str_replace(b, "–", "-"))
b <- b[!is.na(b)]
b <- -b
c <- c(c1, c2)
c <- c[!is.na(c)]

# save as a dataframe
table4 <- tibble(a, b, c)

table4 %>% write_rds("data-parameters/table4.rds")