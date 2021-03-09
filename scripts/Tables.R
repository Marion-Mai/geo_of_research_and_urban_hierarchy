# Scripts by Marion Maisonobe for the Tables of the chapter:
# "Geography of research and urban hierarchy", in "Centrality and hierarchy of networks and territories", coordinated by Julie Fen-Chong. © ISTE Editions 2021
# Original version in French: "Géographie de la recherche et hiérarchie urbaine", in _Centralités et hiérarchies des réseaux et des territoires_, coordinated by Julie Fen-Chong. © ISTE Editions 2021
# "Figure_1.R" and "Figure_2.R": scripts to plot the relation between academic publications, academic staff and urban areas population in France and the UK
# "Tables.R": Script to measure the spatial distribution of the scientific production by discipline in France and the UK
# Data from the French institute of statistics INSEE (DADS - 2014), from ETER and HESA 2014, and from the Web of Science 2012-2013-2014
# Data processed at the urban areas level by Marion Maisonobe for UK data and by Laurent Jégou for French data
# 2021-03-08

library(readr) # for read_csv
library(tidyverse) # for tidyverse
library(estimatr) # for lm_robust
library(kableExtra) # for kbl

######################################################################################################

# Custom functions


# perform a linear regression (lm_robust) per discipline and compute the beta estimates, the confidence intervals and R2 for each scaling laws

lmmm <- function(dataset, y, x, var) {
  y <- enquo(y)
  x <- enquo(x)
  var <- enquo(var)

  dataset  %>%
    filter((!!y) > 0 & (!!x) > 0) %>% # select positive observations only
    rename(Y = !!y, X = !!x) %>%
    nest_by(!!var) %>% # group by disciplines
    mutate(model = list(lm_robust(log(Y)~ log(X), data = data)), n = dim(data)[1]) %>% # run the linear model + count the number of observations per discipline
    summarise(tidy(model, conf.int = .95), r2 = summary(model)$r.squared, obs = n)  %>% # extract the estimates, the confidence interval and the number of observations
    mutate(term = gsub("X", as_label(x), term))
  }

# combine all the results in only one table and organize them

tabmm <- function(a, b) {
  bind_rows(a,b) %>% # gather the two tables
  filter(! term %in% "(Intercept)")%>%
  mutate(bet = paste0(round(estimate, 2)))%>% # keep only two decimals
  mutate(inter = paste0("[",round(conf.low, 2), "; ", round(conf.high, 2), "]"))%>% # display the confidence interval between square brackets
  mutate(r2 = round(r2, 2))%>% # keep only two decimals
  select(label_ds, bet, inter, r2, obs, term)
}

# choose the disciplines's order of appearance in the table, and choose the order of the columns

tabfinmm <- function(a) {

X <- levels(as.factor(a$term))[1]

  a %>%
  pivot_wider(names_from = term, values_from = c(bet, inter, r2, obs)) %>%
  filter(! label_ds %in% c("SCI", "AHSS")) %>%
  arrange(factor(label_ds, levels = c("Basic biology",
                                      "Medecine",
                                      "Applied biology",
                                      "Chemistry",
                                      "Physics",
                                      "Space & earth science",
                                      "Engineering",
                                      "Mathematics",
                                      "Arts & Humanities",
                                      "Social Science",
                                      "All Disciplines")))%>%
  relocate(paste0("inter_",X), .before = 3) %>%
  relocate(paste0("r2_",X), .before = 4) %>%
  relocate(paste0("obs_",X), .before = 5)
}

# Generate the table according to the awful graphic charter of ISTE editions

iste <- function(a, country, source, path) {

a %>%
  kbl(col.names = NULL, booktabs = T) %>%
  add_header_above(c("Discipline", "β" = 1, "CI95 %" = 1, "R2" = 1, "n" = 1, "β" = 1, "CI95 %" = 1, "R2" = 1, "n" = 1), bold = T, color = "white", background = "#002060", line = "F") %>%
  add_header_above(c(" ", "Academic Personnel" = 4, "Urban Population" = 4), bold = T, color = "white", background = "#002060") %>% #
  add_header_above(c(" ", setNames(8, country)), bold = T, color = "white", background = "#002060") %>% # , "France" = 4
  add_header_above(c(" ", "Beta estimates and R2 of OLS Regressions" = 8), bold = T, color = "white", background = "#002060", extra_css = "border-top: 2px solid white;") %>%
  kable_classic(full_width = F, html_font = "Times New Roman", font_size = 9, bootstrap_options = "bordered") %>%
  row_spec(row = 0, bold = T, color = "white", background = "#002060", extra_css = "border-bottom: 1px solid black;") %>% # , hline_after = T
  row_spec(1:11, color = "black", background = "#dbe5f1") %>%  # format last row
  row_spec(11, bold = T) %>%  # format last row
  column_spec (1:9,border_left = T, border_right = T, extra_css = "border-bottom: 1px solid black;") %>%
  column_spec(1, bold = T, color = "white", background = "#002060", extra_css = "border-right: 1px solid black;") %>% # format first column
  column_spec(c(3, 7), italic = T) %>% # format first column
  footnote(symbol = source) %>%
  save_kable(path, zoom = 4)
}

####################################################################################################################
## TABLE 1 - UK
####################################################################################################################

# Script to edit the first Table:
# Descriptors of the relationships between scientific production, academic staff and urban population by discipline and urban area in the United Kingdom

# load the dataset
data <- read_csv("data/uk/data_uk_2014.csv")

# perform a linear regression (lm_robust) per discipline and compute the beta estimates, the confidence intervals and R2 for each scaling laws

## with staff data
tabukpub <- lmmm(data, y = nb_pub, x = astaff2014, var = label_ds)

## with urban population data
tabukpop <- lmmm(data, y = nb_pub, x = pop2011, var = label_ds)

## combine the results in one table only
tab <- tabmm(tabukpub, tabukpop)

## tidy the table
tabfin <- tabfinmm(tab)

## apply the awful ISTE Editions' graphic charter and export the resulting table as an image

source_uk <- "Staff data: ETER, HESA, 2014, and multi-site universities websites retrieved in 2020;\nPublication data: Web of Science/OST-HCERES (articles, reviews and letters), 2014;\nPopulation data: 2011 Census (ONS, NISRA and Public Health Scotland). NETSCITY urban areas."

iste(tabfin, country = "UK", source = source_uk, path = "img/table_1.jpeg")


##################################################################################################################################################################################################################################################################
## TABLE 2 - FRANCE
##################################################################################################################################################################################################################################################################

# Script to edit the second Table:
# Descriptors of the relationships between scientific production, academic staff and urban population by discipline and urban area in France

data <- read_csv("data/fr/with_DADS/data_fr_2014.csv")

# perform a linear regression (lm_robust) per discipline and compute the beta estimates, the confidence intervals and R2 for each scaling laws

## with staff data
tabfrpub <- lmmm(data, y = nb_pub, x = ec_dads_etp_2014, var = label_ds)

## with urban population data
tabfrpop <- lmmm(data, y = nb_pub, x = pop2012, var = label_ds)

## combine the results in one table only
tab <- tabmm(tabfrpub, tabfrpop)

## tidy the table
tabfin <- tabfinmm(tab)

## apply the awful ISTE Editions' graphic charter and export the resulting table as an image

source_fr <- "Staff data: DADS, INSEE, 2014;\nPublication data: Web of Science/OST-HCERES (articles, reviews and letters), 2014;\nPopulation data: 2012 Census (INSEE). NETSCITY urban areas."

iste(tabfin, country = "France", source = source_fr, path = "img/table_2.jpeg")


