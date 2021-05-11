# Estimating crime in place: moving beyond residence location 

This is the code used in the production of the "Estimating crime in place: moving beyond residence location" paper published at xxx. This is joined work of Alexandru Cernat, David Buil-Gil, Ian Brunton-Smith, Jose Pina-Sánchez and Marta Murrià-Sangenís


## Reproduce results

We have started from the xxx data downloaded from xxx on xxxx date. The ".R" file cleans the data and prepars the csv files used in the analysis while the  "data_exploration.R" runs the analysis and produces the graphs. The Mplus folder has the sensitivity analysis done using mplus.



## Session info

```
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] MplusAutomation_0.8  ggpubr_0.4.0         viridis_0.6.0        viridisLite_0.4.0   
 [5] rstan_2.21.2         StanHeaders_2.21.0-7 blavaan_0.3-15       RcppParallel_5.1.2  
 [9] Rcpp_1.0.6           ggcorrplot_0.1.3     lavaan_0.6-8         forcats_0.5.1       
[13] stringr_1.4.0        dplyr_1.0.5          purrr_0.3.4          readr_1.4.0         
[17] tidyr_1.1.3          tibble_3.1.1         ggplot2_3.3.3        tidyverse_1.3.1     
[21] knitr_1.32          

loaded via a namespace (and not attached):
  [1] colorspace_2.0-0   ggsignif_0.6.1     ellipsis_0.3.1     rio_0.5.26        
  [5] ggridges_0.5.3     fs_1.5.0           rstudioapi_0.13    listenv_0.8.0     
  [9] MatrixModels_0.5-0 fansi_0.4.2        mvtnorm_1.1-1      lubridate_1.7.10  
 [13] xml2_1.3.2         codetools_0.2-18   mnormt_2.0.2       texreg_1.37.5     
 [17] bayesplot_1.8.0    jsonlite_1.7.2     mcmc_0.9-7         broom_0.7.6       
 [21] dbplyr_2.1.1       compiler_4.0.5     httr_1.4.2         backports_1.2.1   
 [25] assertthat_0.2.1   Matrix_1.3-2       cli_2.4.0          quantreg_5.85     
 [29] prettyunits_1.1.1  tools_4.0.5        coda_0.19-4        gtable_0.3.0      
 [33] glue_1.4.2         V8_3.4.0           carData_3.0-4      cellranger_1.1.0  
 [37] vctrs_0.3.7        conquer_1.0.2      xfun_0.22          proto_1.0.0       
 [41] globals_0.14.0     ps_1.6.0           openxlsx_4.2.3     rvest_1.0.0       
 [45] CompQuadForm_1.4.3 lifecycle_1.0.0    renv_0.13.2        rstatix_0.7.0     
 [49] future_1.21.0      MASS_7.3-53.1      zoo_1.8-9          scales_1.1.1      
 [53] hms_1.0.0          parallel_4.0.5     sandwich_3.0-0     inline_0.3.17     
 [57] SparseM_1.81       curl_4.3           gridExtra_2.3      pander_0.6.3      
 [61] loo_2.4.1          stringi_1.5.3      boot_1.3-27        pkgbuild_1.2.0    
 [65] zip_2.1.1          rlang_0.4.10       pkgconfig_2.0.3    matrixStats_0.58.0
 [69] lattice_0.20-41    rstantools_2.1.1   processx_3.5.1     tidyselect_1.1.0  
 [73] parallelly_1.24.0  plyr_1.8.6         magrittr_2.0.1     R6_2.5.0          
 [77] generics_0.1.0     DBI_1.1.1          gsubfn_0.7         pillar_1.6.0      
 [81] haven_2.4.0        foreign_0.8-81     withr_2.4.2        abind_1.4-5       
 [85] future.apply_1.7.0 modelr_0.1.8       crayon_1.4.1       car_3.0-10        
 [89] nonnest2_0.5-5     utf8_1.2.1         tmvnsim_1.0-2      grid_4.0.5        
 [93] readxl_1.3.1       data.table_1.14.0  pbivnorm_0.6.0     callr_3.6.0       
 [97] reprex_2.0.0       digest_0.6.27      xtable_1.8-4       MCMCpack_1.5-0    
[101] stats4_4.0.5       munsell_0.5.0 
```
