
# Staff Data at the level of urban areas in France are under an NDA agreement because of statistical secrecy:
# See the description of private and public jobs and wages in 2015: https://www.insee.fr/fr/statistiques/3536754. We used PCS 342B (“Professors and lecturers”); 342C (“Associate and certified teachers -secondary school teachers -in higher education”); 342D (“Temporary teaching staff in higher education”); 342F (“Directors and research officers in public research”); 342G (“Research and study engineers in public research”) and 342H (“Public research fellows”) in the “employees” file. Déclaration Annuelle de Données Sociales (DADS): Customized tabulation, INSEE [producer], ADISP [publisher].
# We thank PROGEDO-ADISP for sharing these social data on HER staff.
# These data have first been used in: Grossetti, M., Maisonobe, M., Jégou, L., Milard, B., & Cabanac, G. (2020). Spatial organisation of French research from the scholarly publication standpoint (1999-2017): Long-standing dynamics and policy-induced disorder. EPJ Web Conf., 244. https://doi.org/10.1051/epjconf/202024401005

# Remove staff data from the public file on the git
datafrnda <- read_csv("data/fr/with_DADS/data_fr_2014.csv")

datafrnda %>%
  mutate(ec_dads_etp_2014 = ifelse(is.na(ec_dads_etp_2014), NA, "Under_NDA")) %>%
  write_csv("data/fr/without_DADS/data_fr_2014.csv")
