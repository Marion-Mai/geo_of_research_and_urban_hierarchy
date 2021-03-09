# Geography of research and urban hierarchy

This git repository contains the scripts and data needed to reproduce the figures and tables of the chapter:
"Geography of research and urban hierarchy", to be published in _Centrality and hierarchy of networks and territories_, coordinated by Julie Fen-Chong. © ISTE Editions 2021

It includes: 
- The first draft and original version of the chapter in French: "Géographie de la recherche et hiérarchie urbaine" in .pdf
- "Figures.R": a script to plot the relations between academic publications, academic staff and urban areas population in France and the UK
- "Tables.R": a script to measure the spatial distribution of the scientific production by discipline in France and the UK
- Figures 1 and 2 of the chapter in .svg and .png
- Tables 1 and 2 of the chapter in .png
- The data needed to generate these figures and tables (except for the French social data concerned by statistical secrecy).

## Data sources:
- **Publication data**: Web of Science/OST-HCERES (articles, reviews, letters), 2014. Fractionation at the level of urban areas (Maisonobe et al., 2018)
- **Population data**: 2011 UK Census (ONS, NISRA and Public Health Scotland) collected at the ward level and then clustered at the level of NETSCITY urban agglomerations by M. Maisonobe; 2012 France Census (INSEE) collected at the municipality level and then clustered at the level of NETSCITY urban agglomerations by L. Jégou.
- **Staff data**: 
    - **UK**: ETER (based on HESA, 2014) and UK Universities websites. Personnel data for 21 (out of 47) multi-site universities in the United Kingdom have been estimated based on information available on university websites. It concerns Anglia Ruskin U, Bangor U, Coventry U, Cranfield U, Heriot-Watt U, Manchester Met U, SRUC, The U of Exeter, The U of Glasgow, The U of Greenwich, The U of Hull, The U of Stirling, The U of Wolverhampton, UWS, UCLAN, U of Chester, U of Cumbria, U of Derby, U of Nottingham, U of Ulster, and UWTSD.The information to make these estimates for the 26 remaining multi-site universities was missing.
    - **France**: staff Data at the level of urban areas in France are under an NDA agreement because of statistical secrecy.
Description: https://www.insee.fr/fr/statistiques/3536754. We used PCS 342B (“Professors and lecturers”); 342C (“Associate and certified teachers -secondary school teachers -in higher education”); 342D (“Temporary teaching staff in higher education”); 342F (“Directors and research officers in public research”); 342G (“Research and study engineers in public research”) and 342H (“Public research fellows”) in the “employees” file. Déclaration Annuelle de Données Sociales (DADS): Customized tabulation, INSEE [producer], ADISP [publisher].
We thank PROGEDO-ADISP for sharing these social data on Higher Education and Research staff. We were not allowed to share them publicly on this repository. 
These data have first been used in: Grossetti, M., Maisonobe, M., Jégou, L., Milard, B., & Cabanac, G. (2020). Spatial organisation of French research from the scholarly publication standpoint (1999-2017): Long-standing dynamics and policy-induced disorder. EPJ Web Conf., 244. https://doi.org/10.1051/epjconf/202024401005

Last update: 2021-03-09
