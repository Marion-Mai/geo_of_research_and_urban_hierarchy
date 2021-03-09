# Scripts by Marion Maisonobe for the Figures of the chapter:
# "Geography of research and urban hierarchy", in "Centrality and hierarchy of networks and territories", coordinated by Julie Fen-Chong. © ISTE Editions 2021
# Original version in French: "Géographie de la recherche et hiérarchie urbaine", in Centralités et hiérarchies des réseaux et des territoires, coordinated by Julie Fen-Chong. © ISTE Editions 2021
# "Figures.R": Script to plot the relations between academic publications, academic staff and urban areas population in France and the UK
# "Tables.R": Script to measure the spatial distribution of the scientific production by discipline in France and the UK
# Data from the French institute of statistics INSEE (DADS - 2014), from ETER and HESA 2014, and from the Web of Science 2012-2014
# Data processed at the urban areas level by Marion Maisonobe for UK data and by Laurent Jégou for French data
# 2021-03-08

################################################################################################################################
# Libraries
################################################################################################################################

library(readr) # for read_tsv
library(tidyverse) # for select
library(ggplot2) # for ggplot
library(ggrepel) # for ggrepel
library(stringr) # for str_to_title
library(estimatr) # for lm_robust
library(cowplot) # for ggdraw, add_sub and plot_grid

###########################################################################################################
## Custom functions
###########################################################################################################

###########################################################################################################
## Function to write the model equation
###########################################################################################################

## Inspired from https://stats.stackexchange.com/questions/63600/how-to-translate-the-results-from-lm-to-an-equation

model_equation <- function(model, ...) {
  format_args <- list(...)

  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_r2 <- round(summary(model)$r.squared, 2)
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 " \n R2 = ",
                                 model_r2,
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}

###########################################################################################################
## Custom functions
###########################################################################################################

# Rename "y" the Number of Academic Publications, and "x" the Academic Staff

tabfig <-  function(dataset, d, agglo, x_staff, x_pop, y, label, c) {

  agglo <- enquo(agglo)
  x_staff <- enquo(x_staff)
  x_pop <- enquo(x_pop)
  y <- enquo(y)
  label <- enquo(label)

  dataset  %>%
  filter(discipline %in% d) %>%
  select(Agglomeration = (!!agglo), x_staff = (!!x_staff), x_pop = (!!x_pop), y = (!!y), label = (!!label)) %>%
  pivot_longer(cols = starts_with("x_"), names_to = "variable", names_prefix = "x_", values_to = "x",)%>%
  filter(x > 0 & y > 0) %>%
  mutate(country = c)

}


lmfig <-  function(dataset) {

  dataset  %>%
  filter(x > 0 & y > 0) %>%
  nest_by(variable, country) %>%
  mutate(model = list(lm_robust(log(y)~ log(x), data = data))) %>%
  summarise(tidy(model, conf.int = .95), r2 = summary(model)$r.squared, eq = model_equation(model, digits = 3, trim = TRUE))  %>%
  filter(! term %in% "(Intercept)")

}

###########################################################################################
## FIGURE 1 - UK
###########################################################################################

###########################################################################################
## Load the data and run the Regression model
###########################################################################################

# Open the file with Academic Personnel and Publication data

# load the dataset
data <- read_csv("data/uk/data_uk_2014.csv")

table <- tabfig(data, d = "D12", agglo = nom_agglocomposite, x_staff = astaff2014, x_pop = pop2011, y = nb_pub, label = label, c = "UK")

tab <- lmfig(table)


# Extract the model residuals
# resid <- tibble(Agglomeration = table$Agglomeration, Residuals = lm.r$residuals)

############################################################################################################
## Plot the data with the regression line
############################################################################################################

#svg(paste("img/Figure_1.svg"), pointsize = 11, width = 12 , height = 9 ) #width = 12

plot.new()
tot_plot <- ggplot(table, aes(x = log(x), y = log(y))) +
  geom_point(color = "red") +    # Use hollow circles (shape=1)
  geom_smooth(method = lm_robust) +   # Add linear regression line
  facet_wrap(~ variable, labeller = labeller(variable =
                                               c("pop" = "Urban population",
                                                 "staff" = "Academic personnel")
  )) +
  scale_x_continuous(breaks = c(4,8,12,16), labels = c(round(exp(4),0), round(exp(8),0), round(exp(12),0),round(exp(16),0) ))+
  scale_y_continuous(breaks = c(0,2.5,5,7.5, 10, 12), labels = c(round(exp(0),0), round(exp(2.5),0), round(exp(5),0), round(exp(7.5),0), round(exp(10),0), round(exp(12),0)))+
  labs(x = "Urban population (log) and academic personnel (log)")+
  labs(y = "Number of academic publications in 2014 (log)")+
  geom_text_repel(label = str_to_title(table$label), size= 4, segment.alpha = 0.3) +
  geom_label(data = tab, inherit.aes = FALSE, aes(x = 5, y = 11, label= eq), size = 4) +
  # annotate("text" ,  4, 9, label = tab$eq, color = "blue", hjust = 1) +
  theme_bw(base_size = 14) +
  theme (axis.title.x = element_text(size = 14, angle = 0, hjust = .5, vjust = 0, face = "bold"), #color = "grey20",
         axis.title.y = element_text(size = 14, angle = 90, hjust = .5, vjust = .5, face = "bold"),
         panel.grid.major.x = element_blank(),
         strip.background = element_rect(fill = "#002060", color = "#002060"),
         strip.text = element_text(size = 14, colour = 'white'),
         plot.title = element_text(size = 14, face = "bold"),
         plot.caption = element_text(size = 10, hjust = 0, face = "italic"))# hjust = 0, to move caption to the left)

ggdraw(add_sub(tot_plot, fontface = "italic", size = 11, color = "black", x = 0, y = 0.5, hjust = 0, vjust = 0.5, fontfamily = "sans", lineheight = 1,
               label =
                 "\nPublication data: Web of Science/OST-HCERES (articles, reviews, letters), 2014. Fractionation at the level of urban areas (Maisonobe et al., 2018)\nPopulation data: 2011 Census (ONS, NISRA and Public Health Scotland). 106 urban agglomerations.\nNote: Population data are collected at the ward level and then clustered at the level of Netscity urban agglomerations.\nStaff data: ETER (based on HESA, 2014) and UK Universities websites. 55 urban agglomerations. Note: Southampton is grouped with Portsmouth.\nPersonnel data for 21 (out of 47) multi-site universities in the United Kingdom are estimated based on information available on university websites. \nIt concerns Anglia Ruskin U, Bangor U, Coventry U, Cranfield U, Heriot-Watt U, Manchester Met U, SRUC, The U of Exeter, The U of Glasgow, The U of Greenwich, \nThe U of Hull, The U of Stirling, The U of Wolverhampton, UWS, UCLAN, U of Chester, U of Cumbria, U of Derby, U of Nottingham, U of Ulster, and UWTSD.\nThe information to make these estimates for the 26 remaining multi-site universities was missing."
))

dev.off()

###########################################################################################
## FIGURE 2 - FRANCE
###########################################################################################

###########################################################################################
## Load the data and run the Regression model
###########################################################################################

# Open the file with Academic Personnel and Publication data

# load the dataset
data <- read_csv("data/fr/with_DADS/data_fr_2014.csv")

table <- tabfig(data, d = "D12", agglo = nom_agglocomposite, x_staff = ec_dads_etp_2014, x_pop = pop2012, y = nb_pub, label = city_label, c = "FR")

tab <- lmfig(table)


# Extract the model residuals
# resid <- tibble(Agglomeration = table$Agglomeration, Residuals = lm.r$residuals)

############################################################################################################
## Plot the data with the regression line
############################################################################################################

#svg(paste("img/Figure_2.svg"), pointsize = 11, width = 12 , height = 11 ) #width = 12

plot.new()
tot_plot <- ggplot(table, aes(x = log(x), y = log(y))) +
  geom_point(color = "red") +    # Use hollow circles (shape=1)
  geom_smooth(method = lm_robust) +   # Add linear regression line
  facet_wrap(~ variable, labeller = labeller(variable =
                                               c("pop" = "Urban population",
                                                 "staff" = "HER personnel")
  )) +
  scale_x_continuous(breaks = c(4,8,12,16), labels = c(round(exp(4),0), round(exp(8),0), round(exp(12),0), round(exp(16),0)))+
  scale_y_continuous(breaks = c(0,4,8, 10), labels = c(round(exp(0),0), round(exp(4),0), round(exp(8),0), round(exp(10),0)))+
  labs(x = "Urban population (log) and Higher Education and Research (HER) personnel (log)")+
  labs(y = "Number of academic publications in 2014 (log)")+
  geom_text_repel(label = str_to_title(table$label), size= 4, segment.alpha = 0.3) +
  geom_label(data = tab, inherit.aes = FALSE, aes(x = 5, y = 11, label= eq), size = 4) +
  # annotate("text" ,  4, 9, label = tab$eq, color = "blue", hjust = 1) +
  theme_bw(base_size = 14) +
  theme (axis.title.x = element_text(size = 14, angle = 0, hjust = .5, vjust = 0, face = "bold"), #color = "grey20",
         axis.title.y = element_text(size = 14, angle = 90, hjust = .5, vjust = .5, face = "bold"),
         panel.grid.major.x = element_blank(),
         strip.background = element_rect(fill = "#002060", color = "#002060"),
         strip.text = element_text(size = 14, colour = 'white'),
         plot.title = element_text(size = 14, face = "bold"),
         plot.caption = element_text(size = 10, hjust = 0, face = "italic"))# hjust = 0, to move caption to the left)

ggdraw(add_sub(tot_plot, fontface = "italic", size = 11, color = "black", x = 0, y = 0.5, hjust = 0, vjust = 0.5, fontfamily = "sans", lineheight = 1,
               label =
                 "\nPublication data: Web of Science/OST-HCERES (articles, reviews, letters), 2014. Fractionation at the level of urban areas (Maisonobe et al., 2018)\nPopulation data: 2012 Census (INSEE). 166 urban agglomerations.\nNote: Population data are collected at the municipality level and then clustered at the level of Netscity urban agglomerations.\nStaff data: DADS 2014, Insee, France. 111 urban agglomeration. Note: Staff data are missing for overseas cities."
))

dev.off()
