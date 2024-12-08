# Data-Analysis-Report---Football-Related-Arrests
 POL42540-App Data Wrangling and Vis.-2024/25 

Data Sources: 

Core Data: 
https://www.gov.uk/government/statistics/football-related-arrests-and-banning-orders-2022-to-2023-season/football-related-arrests-and-banning-orders-england-and-wales-2022-to-2023-season#s3  (Specific file on the website: 'Football related arrest statistics, England and Wales: detailed datasets')

Economic Indicators Data: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/monthlygrossdomesticproductbygrossvalueadded

Football Team Rankings Data: 
https://github.com/jfjelstul/englishfootball/tree/master

CVD friendly Color schemes used for the plots: 
  - viridis: Colorblind-Friendly Color Maps for R (https://cran.r-project.org/web/packages/viridis/index.html)
  - Custom colors & CVD friendly schemes - colorblind-friendly brewer palettes like RdYlBu via library(RColorBrewer) - https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/#:~:text=RdYlBu%2C%20RdYlGn%2C%20Spectral-,Show%20only%20colorblind%2Dfriendly%20brewer%20palettes,-To%20display%20only

Steps to merge the datasets: 
1) Merge core arrests data with Economic Indicators - Runi this script - [R Script] Merge Football Arrests Data with Economic Indicators Data.r
2) Merge Rankings data with the optput from step 1 listed above to get the final dataset - Run this R script - [R Script] Football_Rankings_Merged.r

Steps to reproduce the plots:
1) Football Related Arrests by season - DataVizProject-Plot3.png - Run this script - [R Script] DataVizProject - Plots 3 & 4 Final.r
2) Football Related Arrests - Home vs Away Supporters - DataVizProject-Plot4.png - Run this script - [R Script] DataVizProject - Plots 3 & 4 Final.r


