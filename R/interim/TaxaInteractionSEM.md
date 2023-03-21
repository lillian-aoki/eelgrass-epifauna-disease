Community Interaction SEM results
================

# SEMs comparing epifauna vs lacuna vs ampithoid vs iodteid vs richness

Try including epiphyte at the blade level. This variable was not
important before, but I’ve changed the structure of the SEMs (two years
only, meadow-scale predictors). And, the relationship with taxa might
vary (haven’t looked at that). Also, the data are now updated with
better BB data.

A few notes:  
- Large epifauna only (except at BB). - Epiphyte blades only. A handful
are missing due to no epiphytes (wrong leaf etc) - Zeros are included
(not NAs), all sites for all models.

``` r
# data ###
dis <- read_csv("data/epiphyte_SEM_data_all_large.csv")
```

    ## Rows: 1350 Columns: 48
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): Meter, SampleId, Region, SiteCode, TidalHeight, GrazingScars, Bro...
    ## dbl  (38): Transect, Blade, LongestBladeLength, LongestBladeWidth, SheathLen...
    ## date  (1): SampleDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# updating the SEM to compare the effects of using large vs all animals
dis$BladeAreaLog <- log10(dis$BladeArea)
dis$EpiphyteLog <- log10(dis$EpiphytePerAreamgcm2+0.01)
# use the full data set without subsetting because the SEM no longer includes epiphytes or grazing
dis1 <- select(dis, c(Epifauna = Epifauna_all, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
                      DensityLog, YearBinary, Year, Meadow, Region,BladeAreaLog, TidalHeightBinary, 
                      Prevalence, LesionArea, EpiphytePerAreamgcm2, EpiphyteLog,
                      Lacuna = Lacuna_all, 
                      Ampithoid = Ampithoid_all, Idoteid = Idoteid_all, Richness = Richness_all))

dis1_large <- select(dis, c(Epifauna = Epifauna_large, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
                      DensityLog, YearBinary, Year, Meadow, Region,BladeAreaLog, TidalHeightBinary, 
                      Prevalence, LesionArea, EpiphyteLog, Lacuna = Lacuna_large, 
                      Ampithoid = Ampithoid_large, Idoteid = Idoteid_large, Richness = Richness_large))
dis_large <- na.omit(dis1_large)
```

## Prevalence + Epifauna

``` r
sem_prev_epi <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
    lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + 
          Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_large,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
summary(sem_prev_epi)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_epi 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + EpiphyteLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  116.333   417.871
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1307.004     0.0042  0.9484 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1307.001     0.0169  0.8966 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 1305.003     0.0000  0.9956 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.333 with P-value = 0.999 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0051    0.0011 1256.6183    21.4678
    ##       Epifauna MonthlyMeanTemp_June  -0.1739    0.0142 1206.6412   146.4311
    ##       Epifauna         CanopyHeight  -0.5637    0.0377 1329.3983   222.2756
    ##       Epifauna           DensityLog  -0.1645    0.0353 1327.7115    21.5019
    ##       Epifauna           YearBinary  -0.0379    0.0083 1313.8766    20.6126
    ##   CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ##   CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ##     DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ##     DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ##     DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ##   BladeAreaLog             Epifauna  -0.0626    0.0426   87.7021     2.0172
    ##   BladeAreaLog         CanopyHeight   0.6842    0.0641   86.5019   107.1005
    ##   BladeAreaLog           DensityLog  -0.1456    0.0559  101.2453     6.0711
    ##   BladeAreaLog    TempAnomWarm_June  -0.0029    0.0018   68.5047     2.3280
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0058    0.0179   16.5561     0.0838
    ##   BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.5598   332.7172
    ##   BladeAreaLog           YearBinary   0.0553    0.0179  954.1424     9.3890
    ##    EpiphyteLog         BladeAreaLog  -0.1593    0.0436 1316.7196    13.3331
    ##    EpiphyteLog             Epifauna   0.1379    0.0842  255.6101     2.5751
    ##    EpiphyteLog         CanopyHeight  -0.4370    0.1296  286.6236    10.9755
    ##    EpiphyteLog           DensityLog  -0.2158    0.1117  331.2801     3.5593
    ##    EpiphyteLog    TempAnomWarm_June   0.0086    0.0037  500.9209     5.1057
    ##    EpiphyteLog MonthlyMeanTemp_June  -0.2232    0.0449  189.0115    22.1647
    ##    EpiphyteLog    TidalHeightBinary  -0.0565    0.0234 1306.7835     5.8313
    ##    EpiphyteLog           YearBinary  -0.0372    0.0297 1223.4455     1.5531
    ##     Prevalence         BladeAreaLog   1.7422    0.2963 1338.0000     5.8795
    ##     Prevalence          EpiphyteLog  -0.0142    0.1642 1338.0000    -0.0865
    ##     Prevalence             Epifauna   1.2960    0.4302 1338.0000     3.0126
    ##     Prevalence         CanopyHeight   1.7492    0.6821 1338.0000     2.5646
    ##     Prevalence           DensityLog   2.6509    0.5716 1338.0000     4.6379
    ##     Prevalence    TempAnomWarm_June   0.0252    0.0192 1338.0000     1.3135
    ##     Prevalence MonthlyMeanTemp_June   0.3921     0.225 1338.0000     1.7427
    ##     Prevalence    TidalHeightBinary   0.6429     0.143 1338.0000     4.4968
    ##     Prevalence           YearBinary   0.3695     0.172 1338.0000     2.1491
    ##   ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.1095 ***
    ##    0.0000      -0.7026 ***
    ##    0.0000      -0.4683 ***
    ##    0.0000      -0.1943 ***
    ##    0.0000      -0.0456 ***
    ##    0.0000      -0.1583 ***
    ##    0.0000      -0.2824 ***
    ##    0.0000      -0.1525 ***
    ##    0.0000      -0.3365 ***
    ##    0.0000       0.6848 ***
    ##    0.0000      -0.0497 ***
    ##    0.1591      -0.0611    
    ##    0.0000       0.5549 ***
    ##    0.0154      -0.1679   *
    ##    0.1317      -0.0616    
    ##    0.7758      -0.0227    
    ##    0.0000      -0.2851 ***
    ##    0.0022       0.0649  **
    ##    0.0003      -0.0668 ***
    ##    0.1098       0.0564    
    ##    0.0010      -0.1486  **
    ##    0.0601      -0.1044    
    ##    0.0243       0.0751   *
    ##    0.0000      -0.3692 ***
    ##    0.0159       -0.028   *
    ##    0.2129      -0.0183    
    ##    0.0000            - ***
    ##    0.9310            -    
    ##    0.0026            -  **
    ##    0.0103            -   *
    ##    0.0000            - ***
    ##    0.1890            -    
    ##    0.0814            -    
    ##    0.0000            - ***
    ##    0.0316            -   *
    ##    0.0270      -0.0527   *
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.16        0.98
    ##   CanopyHeight   none     0.14        0.96
    ##     DensityLog   none     0.20        0.99
    ##   BladeAreaLog   none     0.51        0.68
    ##    EpiphyteLog   none     0.14        0.87
    ##     Prevalence  delta     0.16        0.50

``` r
coefs(sem_prev_epi)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0051    0.0011 1256.6183    21.4678
    ## 2      Epifauna MonthlyMeanTemp_June  -0.1739    0.0142 1206.6412   146.4311
    ## 3      Epifauna         CanopyHeight  -0.5637    0.0377 1329.3983   222.2756
    ## 4      Epifauna           DensityLog  -0.1645    0.0353 1327.7115    21.5019
    ## 5      Epifauna           YearBinary  -0.0379    0.0083 1313.8766    20.6126
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ## 8  CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ## 10   DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ## 11   DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ## 12 BladeAreaLog             Epifauna  -0.0626    0.0426   87.7021     2.0172
    ## 13 BladeAreaLog         CanopyHeight   0.6842    0.0641   86.5019   107.1005
    ## 14 BladeAreaLog           DensityLog  -0.1456    0.0559  101.2453     6.0711
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0029    0.0018   68.5047     2.3280
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0058    0.0179   16.5561     0.0838
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.5598   332.7172
    ## 18 BladeAreaLog           YearBinary   0.0553    0.0179  954.1424     9.3890
    ## 19  EpiphyteLog         BladeAreaLog  -0.1593    0.0436 1316.7196    13.3331
    ## 20  EpiphyteLog             Epifauna   0.1379    0.0842  255.6101     2.5751
    ## 21  EpiphyteLog         CanopyHeight  -0.4370    0.1296  286.6236    10.9755
    ## 22  EpiphyteLog           DensityLog  -0.2158    0.1117  331.2801     3.5593
    ## 23  EpiphyteLog    TempAnomWarm_June   0.0086    0.0037  500.9209     5.1057
    ## 24  EpiphyteLog MonthlyMeanTemp_June  -0.2232    0.0449  189.0115    22.1647
    ## 25  EpiphyteLog    TidalHeightBinary  -0.0565    0.0234 1306.7835     5.8313
    ## 26  EpiphyteLog           YearBinary  -0.0372    0.0297 1223.4455     1.5531
    ## 27   Prevalence         BladeAreaLog   1.7422    0.2963 1338.0000     5.8795
    ## 28   Prevalence          EpiphyteLog  -0.0142    0.1642 1338.0000    -0.0865
    ## 29   Prevalence             Epifauna   1.2960    0.4302 1338.0000     3.0126
    ## 30   Prevalence         CanopyHeight   1.7492    0.6821 1338.0000     2.5646
    ## 31   Prevalence           DensityLog   2.6509    0.5716 1338.0000     4.6379
    ## 32   Prevalence    TempAnomWarm_June   0.0252    0.0192 1338.0000     1.3135
    ## 33   Prevalence MonthlyMeanTemp_June   0.3921     0.225 1338.0000     1.7427
    ## 34   Prevalence    TidalHeightBinary   0.6429     0.143 1338.0000     4.4968
    ## 35   Prevalence           YearBinary   0.3695     0.172 1338.0000     2.1491
    ## 36 ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.1095 ***
    ## 2   0.0000      -0.7026 ***
    ## 3   0.0000      -0.4683 ***
    ## 4   0.0000      -0.1943 ***
    ## 5   0.0000      -0.0456 ***
    ## 6   0.0000      -0.1583 ***
    ## 7   0.0000      -0.2824 ***
    ## 8   0.0000      -0.1525 ***
    ## 9   0.0000      -0.3365 ***
    ## 10  0.0000       0.6848 ***
    ## 11  0.0000      -0.0497 ***
    ## 12  0.1591      -0.0611    
    ## 13  0.0000       0.5549 ***
    ## 14  0.0154      -0.1679   *
    ## 15  0.1317      -0.0616    
    ## 16  0.7758      -0.0227    
    ## 17  0.0000      -0.2851 ***
    ## 18  0.0022       0.0649  **
    ## 19  0.0003      -0.0668 ***
    ## 20  0.1098       0.0564    
    ## 21  0.0010      -0.1486  **
    ## 22  0.0601      -0.1044    
    ## 23  0.0243       0.0751   *
    ## 24  0.0000      -0.3692 ***
    ## 25  0.0159      -0.0280   *
    ## 26  0.2129      -0.0183    
    ## 27  0.0000       0.3266 ***
    ## 28  0.9310      -0.0064    
    ## 29  0.0026       0.2371  **
    ## 30  0.0103       0.2659   *
    ## 31  0.0000       0.5731 ***
    ## 32  0.1890       0.0986    
    ## 33  0.0814       0.2899    
    ## 34  0.0000       0.1424 ***
    ## 35  0.0316       0.0813   *
    ## 36  0.0270      -0.0527   *

## Prev + Amp

``` r
sem_prev_amp <- psem(
  lmer(Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
    lmer(BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + 
          Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_large,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0107381 (tol = 0.002, component 1)

``` r
summary(sem_prev_amp)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_amp 
    ## 
    ## Call:
    ##   Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + EpiphyteLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  116.621   418.159
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1307.004     0.0042  0.9484 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1307.001     0.0169  0.8966 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 1305.002     0.0302  0.8622 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.621 with P-value = 0.996 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##      Ampithoid    TempAnomWarm_June  -0.0111     6e-04 1288.9012   312.8124
    ##      Ampithoid MonthlyMeanTemp_June   0.1686    0.0081 1251.6387   430.6287
    ##      Ampithoid         CanopyHeight   0.1823    0.0215 1316.8622    71.6754
    ##      Ampithoid           DensityLog   0.0931    0.0201 1314.2149    21.3009
    ##      Ampithoid           YearBinary  -0.1111    0.0047 1308.6985   551.2114
    ##   CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ##   CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ##     DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ##     DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ##     DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ##   BladeAreaLog            Ampithoid   0.0338    0.0448   47.3668     0.5303
    ##   BladeAreaLog         CanopyHeight   0.7419    0.0619  139.6888   131.7490
    ##   BladeAreaLog           DensityLog  -0.1044    0.0519   88.6712     3.5359
    ##   BladeAreaLog    TempAnomWarm_June  -0.0008    0.0017   51.2154     0.1807
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0192    0.0171   14.3530     1.0608
    ##   BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1306.0097   332.4250
    ##   BladeAreaLog           YearBinary   0.0651    0.0201  883.2043    10.2472
    ##    EpiphyteLog         BladeAreaLog  -0.1612    0.0435 1316.5899    13.7024
    ##    EpiphyteLog            Ampithoid  -0.2202    0.1038   58.7462     4.3120
    ##    EpiphyteLog         CanopyHeight  -0.5637    0.1219  401.1432    20.7733
    ##    EpiphyteLog           DensityLog  -0.2564    0.1087  246.9364     5.2712
    ##    EpiphyteLog    TempAnomWarm_June   0.0039    0.0038  363.9419     0.9611
    ##    EpiphyteLog MonthlyMeanTemp_June  -0.2009    0.0478  205.1647    16.0035
    ##    EpiphyteLog    TidalHeightBinary  -0.0568    0.0233 1306.9311     5.9075
    ##    EpiphyteLog           YearBinary  -0.0789    0.0336 1094.3155     5.4696
    ##     Prevalence         BladeAreaLog   1.7151    0.2962 1338.0000     5.7907
    ##     Prevalence          EpiphyteLog   0.0150    0.1639 1338.0000     0.0914
    ##     Prevalence            Ampithoid   0.1772    0.5752 1338.0000     0.3081
    ##     Prevalence         CanopyHeight   1.0038    0.6864 1338.0000     1.4624
    ##     Prevalence           DensityLog   2.1599    0.5763 1338.0000     3.7481
    ##     Prevalence    TempAnomWarm_June   0.0103    0.0196 1338.0000     0.5226
    ##     Prevalence MonthlyMeanTemp_June   0.3094    0.2284 1338.0000     1.3549
    ##     Prevalence    TidalHeightBinary   0.6391     0.143 1338.0000     4.4689
    ##     Prevalence           YearBinary   0.3455    0.2038 1338.0000     1.6949
    ##   ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2151 ***
    ##    0.0000       0.6197 ***
    ##    0.0000       0.1378 ***
    ##    0.0000       0.1001 ***
    ##    0.0000      -0.1216 ***
    ##    0.0000      -0.1583 ***
    ##    0.0000      -0.2824 ***
    ##    0.0000      -0.1525 ***
    ##    0.0000      -0.3365 ***
    ##    0.0000       0.6848 ***
    ##    0.0000      -0.0497 ***
    ##    0.4701       0.0362    
    ##    0.0000       0.6017 ***
    ##    0.0633      -0.1204    
    ##    0.6726      -0.0159    
    ##    0.3201      -0.0759    
    ##    0.0000      -0.2852 ***
    ##    0.0014       0.0764  **
    ##    0.0002      -0.0676 ***
    ##    0.0422      -0.0991   *
    ##    0.0000      -0.1917 ***
    ##    0.0225       -0.124   *
    ##    0.3276       0.0342    
    ##    0.0001      -0.3322 ***
    ##    0.0152      -0.0281   *
    ##    0.0195      -0.0389   *
    ##    0.0000            - ***
    ##    0.9272            -    
    ##    0.7580            -    
    ##    0.1436            -    
    ##    0.0002            - ***
    ##    0.6012            -    
    ##    0.1755            -    
    ##    0.0000            - ***
    ##    0.0901            -    
    ##    0.0270      -0.0527   *
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##      Ampithoid   none     0.28        0.99
    ##   CanopyHeight   none     0.14        0.96
    ##     DensityLog   none     0.20        0.99
    ##   BladeAreaLog   none     0.53        0.66
    ##    EpiphyteLog   none     0.14        0.87
    ##     Prevalence  delta     0.12        0.50

``` r
coefs(sem_prev_amp)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1     Ampithoid    TempAnomWarm_June  -0.0111     6e-04 1288.9012   312.8124
    ## 2     Ampithoid MonthlyMeanTemp_June   0.1686    0.0081 1251.6387   430.6287
    ## 3     Ampithoid         CanopyHeight   0.1823    0.0215 1316.8622    71.6754
    ## 4     Ampithoid           DensityLog   0.0931    0.0201 1314.2149    21.3009
    ## 5     Ampithoid           YearBinary  -0.1111    0.0047 1308.6985   551.2114
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ## 8  CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ## 10   DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ## 11   DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ## 12 BladeAreaLog            Ampithoid   0.0338    0.0448   47.3668     0.5303
    ## 13 BladeAreaLog         CanopyHeight   0.7419    0.0619  139.6888   131.7490
    ## 14 BladeAreaLog           DensityLog  -0.1044    0.0519   88.6712     3.5359
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0008    0.0017   51.2154     0.1807
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0192    0.0171   14.3530     1.0608
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1306.0097   332.4250
    ## 18 BladeAreaLog           YearBinary   0.0651    0.0201  883.2043    10.2472
    ## 19  EpiphyteLog         BladeAreaLog  -0.1612    0.0435 1316.5899    13.7024
    ## 20  EpiphyteLog            Ampithoid  -0.2202    0.1038   58.7462     4.3120
    ## 21  EpiphyteLog         CanopyHeight  -0.5637    0.1219  401.1432    20.7733
    ## 22  EpiphyteLog           DensityLog  -0.2564    0.1087  246.9364     5.2712
    ## 23  EpiphyteLog    TempAnomWarm_June   0.0039    0.0038  363.9419     0.9611
    ## 24  EpiphyteLog MonthlyMeanTemp_June  -0.2009    0.0478  205.1647    16.0035
    ## 25  EpiphyteLog    TidalHeightBinary  -0.0568    0.0233 1306.9311     5.9075
    ## 26  EpiphyteLog           YearBinary  -0.0789    0.0336 1094.3155     5.4696
    ## 27   Prevalence         BladeAreaLog   1.7151    0.2962 1338.0000     5.7907
    ## 28   Prevalence          EpiphyteLog   0.0150    0.1639 1338.0000     0.0914
    ## 29   Prevalence            Ampithoid   0.1772    0.5752 1338.0000     0.3081
    ## 30   Prevalence         CanopyHeight   1.0038    0.6864 1338.0000     1.4624
    ## 31   Prevalence           DensityLog   2.1599    0.5763 1338.0000     3.7481
    ## 32   Prevalence    TempAnomWarm_June   0.0103    0.0196 1338.0000     0.5226
    ## 33   Prevalence MonthlyMeanTemp_June   0.3094    0.2284 1338.0000     1.3549
    ## 34   Prevalence    TidalHeightBinary   0.6391     0.143 1338.0000     4.4689
    ## 35   Prevalence           YearBinary   0.3455    0.2038 1338.0000     1.6949
    ## 36 ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2151 ***
    ## 2   0.0000       0.6197 ***
    ## 3   0.0000       0.1378 ***
    ## 4   0.0000       0.1001 ***
    ## 5   0.0000      -0.1216 ***
    ## 6   0.0000      -0.1583 ***
    ## 7   0.0000      -0.2824 ***
    ## 8   0.0000      -0.1525 ***
    ## 9   0.0000      -0.3365 ***
    ## 10  0.0000       0.6848 ***
    ## 11  0.0000      -0.0497 ***
    ## 12  0.4701       0.0362    
    ## 13  0.0000       0.6017 ***
    ## 14  0.0633      -0.1204    
    ## 15  0.6726      -0.0159    
    ## 16  0.3201      -0.0759    
    ## 17  0.0000      -0.2852 ***
    ## 18  0.0014       0.0764  **
    ## 19  0.0002      -0.0676 ***
    ## 20  0.0422      -0.0991   *
    ## 21  0.0000      -0.1917 ***
    ## 22  0.0225      -0.1240   *
    ## 23  0.3276       0.0342    
    ## 24  0.0001      -0.3322 ***
    ## 25  0.0152      -0.0281   *
    ## 26  0.0195      -0.0389   *
    ## 27  0.0000       0.3212 ***
    ## 28  0.9272       0.0067    
    ## 29  0.7580       0.0356    
    ## 30  0.1436       0.1525    
    ## 31  0.0002       0.4665 ***
    ## 32  0.6012       0.0401    
    ## 33  0.1755       0.2286    
    ## 34  0.0000       0.1414 ***
    ## 35  0.0901       0.0760    
    ## 36  0.0270      -0.0527   *

## Prev + Lac

``` r
sem_prev_lac <- psem(
  lmer(Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
    lmer(BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + 
          Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_large,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.016154 (tol = 0.002, component 1)

``` r
summary(sem_prev_lac)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_lac 
    ## 
    ## Call:
    ##   Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + EpiphyteLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  116.509   418.047
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1307.004     0.0042  0.9484 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1307.001     0.0169  0.8966 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 1305.008     0.0122  0.9119 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.509 with P-value = 0.998 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##         Lacuna    TempAnomWarm_June   0.0230     0.002  982.2379   132.8365
    ##         Lacuna MonthlyMeanTemp_June   0.0443    0.0254  823.7249     2.9342
    ##         Lacuna         CanopyHeight   0.8748    0.0684 1330.5901   162.6092
    ##         Lacuna           DensityLog   0.3756    0.0637 1274.4733    34.1917
    ##         Lacuna           YearBinary  -0.1301    0.0152 1316.7524    73.5252
    ##   CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ##   CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ##     DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ##     DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ##     DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ##   BladeAreaLog               Lacuna  -0.0002    0.0244   97.7686     0.0001
    ##   BladeAreaLog         CanopyHeight   0.7259    0.0586   72.3600   142.3152
    ##   BladeAreaLog           DensityLog  -0.1098    0.0525   82.3809     3.8471
    ##   BladeAreaLog    TempAnomWarm_June  -0.0015    0.0017   61.5480     0.7527
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0109    0.0172   13.3636     0.3283
    ##   BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.5656   332.3803
    ##   BladeAreaLog           YearBinary   0.0586    0.0185  978.7128     9.8569
    ##    EpiphyteLog         BladeAreaLog  -0.1614    0.0435 1316.5411    13.7339
    ##    EpiphyteLog               Lacuna  -0.1002    0.0482  353.7633     4.1664
    ##    EpiphyteLog         CanopyHeight  -0.4694    0.1239  220.7511    13.7866
    ##    EpiphyteLog           DensityLog  -0.2325    0.1095  245.9877     4.2646
    ##    EpiphyteLog    TempAnomWarm_June   0.0086    0.0036  341.2032     5.2076
    ##    EpiphyteLog MonthlyMeanTemp_June  -0.2255     0.045  229.5183    22.7017
    ##    EpiphyteLog    TidalHeightBinary  -0.0568    0.0234 1306.6745     5.9187
    ##    EpiphyteLog           YearBinary  -0.0609    0.0305 1259.3148     3.9604
    ##     Prevalence         BladeAreaLog   1.7214    0.2933 1338.0000     5.8683
    ##     Prevalence          EpiphyteLog   0.0305    0.1579 1338.0000     0.1933
    ##     Prevalence               Lacuna   1.0240    0.2318 1338.0000     4.4174
    ##     Prevalence         CanopyHeight   0.5792    0.5512 1338.0000     1.0509
    ##     Prevalence           DensityLog   1.6535    0.4683 1338.0000     3.5310
    ##     Prevalence    TempAnomWarm_June  -0.0127    0.0153 1338.0000    -0.8283
    ##     Prevalence MonthlyMeanTemp_June   0.2886    0.1563 1338.0000     1.8462
    ##     Prevalence    TidalHeightBinary   0.6404    0.1427 1338.0000     4.4877
    ##     Prevalence           YearBinary   0.5069    0.1742 1338.0000     2.9103
    ##   ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##   P.Value Std.Estimate    
    ##    0.0000       0.3002 ***
    ##    0.0871       0.1094    
    ##    0.0000       0.4439 ***
    ##    0.0000       0.2711 ***
    ##    0.0000      -0.0956 ***
    ##    0.0000      -0.1583 ***
    ##    0.0000      -0.2824 ***
    ##    0.0000      -0.1525 ***
    ##    0.0000      -0.3365 ***
    ##    0.0000       0.6848 ***
    ##    0.0000      -0.0497 ***
    ##    0.9929       -4e-04    
    ##    0.0000       0.5887 ***
    ##    0.0532      -0.1266    
    ##    0.3890      -0.0319    
    ##    0.5762      -0.0428    
    ##    0.0000      -0.2852 ***
    ##    0.0017       0.0688  **
    ##    0.0002      -0.0677 ***
    ##    0.0420      -0.0672   *
    ##    0.0003      -0.1596 ***
    ##    0.0400      -0.1124   *
    ##    0.0231       0.0756   *
    ##    0.0000      -0.3729 ***
    ##    0.0151      -0.0281   *
    ##    0.0468        -0.03   *
    ##    0.0000            - ***
    ##    0.8467            -    
    ##    0.0000            - ***
    ##    0.2933            -    
    ##    0.0004            - ***
    ##    0.4075            -    
    ##    0.0649            -    
    ##    0.0000            - ***
    ##    0.0036            -  **
    ##    0.0270      -0.0527   *
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##         Lacuna   none     0.11        0.96
    ##   CanopyHeight   none     0.14        0.96
    ##     DensityLog   none     0.20        0.99
    ##   BladeAreaLog   none     0.52        0.67
    ##    EpiphyteLog   none     0.12        0.87
    ##     Prevalence  delta     0.11        0.35

``` r
coefs(sem_prev_lac)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1        Lacuna    TempAnomWarm_June   0.0230     0.002  982.2379   132.8365
    ## 2        Lacuna MonthlyMeanTemp_June   0.0443    0.0254  823.7249     2.9342
    ## 3        Lacuna         CanopyHeight   0.8748    0.0684 1330.5901   162.6092
    ## 4        Lacuna           DensityLog   0.3756    0.0637 1274.4733    34.1917
    ## 5        Lacuna           YearBinary  -0.1301    0.0152 1316.7524    73.5252
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ## 8  CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ## 10   DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ## 11   DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ## 12 BladeAreaLog               Lacuna  -0.0002    0.0244   97.7686     0.0001
    ## 13 BladeAreaLog         CanopyHeight   0.7259    0.0586   72.3600   142.3152
    ## 14 BladeAreaLog           DensityLog  -0.1098    0.0525   82.3809     3.8471
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0015    0.0017   61.5480     0.7527
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0109    0.0172   13.3636     0.3283
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.5656   332.3803
    ## 18 BladeAreaLog           YearBinary   0.0586    0.0185  978.7128     9.8569
    ## 19  EpiphyteLog         BladeAreaLog  -0.1614    0.0435 1316.5411    13.7339
    ## 20  EpiphyteLog               Lacuna  -0.1002    0.0482  353.7633     4.1664
    ## 21  EpiphyteLog         CanopyHeight  -0.4694    0.1239  220.7511    13.7866
    ## 22  EpiphyteLog           DensityLog  -0.2325    0.1095  245.9877     4.2646
    ## 23  EpiphyteLog    TempAnomWarm_June   0.0086    0.0036  341.2032     5.2076
    ## 24  EpiphyteLog MonthlyMeanTemp_June  -0.2255     0.045  229.5183    22.7017
    ## 25  EpiphyteLog    TidalHeightBinary  -0.0568    0.0234 1306.6745     5.9187
    ## 26  EpiphyteLog           YearBinary  -0.0609    0.0305 1259.3148     3.9604
    ## 27   Prevalence         BladeAreaLog   1.7214    0.2933 1338.0000     5.8683
    ## 28   Prevalence          EpiphyteLog   0.0305    0.1579 1338.0000     0.1933
    ## 29   Prevalence               Lacuna   1.0240    0.2318 1338.0000     4.4174
    ## 30   Prevalence         CanopyHeight   0.5792    0.5512 1338.0000     1.0509
    ## 31   Prevalence           DensityLog   1.6535    0.4683 1338.0000     3.5310
    ## 32   Prevalence    TempAnomWarm_June  -0.0127    0.0153 1338.0000    -0.8283
    ## 33   Prevalence MonthlyMeanTemp_June   0.2886    0.1563 1338.0000     1.8462
    ## 34   Prevalence    TidalHeightBinary   0.6404    0.1427 1338.0000     4.4877
    ## 35   Prevalence           YearBinary   0.5069    0.1742 1338.0000     2.9103
    ## 36 ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.3002 ***
    ## 2   0.0871       0.1094    
    ## 3   0.0000       0.4439 ***
    ## 4   0.0000       0.2711 ***
    ## 5   0.0000      -0.0956 ***
    ## 6   0.0000      -0.1583 ***
    ## 7   0.0000      -0.2824 ***
    ## 8   0.0000      -0.1525 ***
    ## 9   0.0000      -0.3365 ***
    ## 10  0.0000       0.6848 ***
    ## 11  0.0000      -0.0497 ***
    ## 12  0.9929      -0.0004    
    ## 13  0.0000       0.5887 ***
    ## 14  0.0532      -0.1266    
    ## 15  0.3890      -0.0319    
    ## 16  0.5762      -0.0428    
    ## 17  0.0000      -0.2852 ***
    ## 18  0.0017       0.0688  **
    ## 19  0.0002      -0.0677 ***
    ## 20  0.0420      -0.0672   *
    ## 21  0.0003      -0.1596 ***
    ## 22  0.0400      -0.1124   *
    ## 23  0.0231       0.0756   *
    ## 24  0.0000      -0.3729 ***
    ## 25  0.0151      -0.0281   *
    ## 26  0.0468      -0.0300   *
    ## 27  0.0000       0.3268 ***
    ## 28  0.8467       0.0138    
    ## 29  0.0000       0.3107 ***
    ## 30  0.2933       0.0892    
    ## 31  0.0004       0.3620 ***
    ## 32  0.4075      -0.0502    
    ## 33  0.0649       0.2161    
    ## 34  0.0000       0.1436 ***
    ## 35  0.0036       0.1130  **
    ## 36  0.0270      -0.0527   *

## Prev + Ido

``` r
sem_prev_ido <- psem(
  lmer(Idoteid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
    lmer(BladeAreaLog ~ Idoteid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + 
          Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_large,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0207277 (tol = 0.002, component 1)

``` r
summary(sem_prev_ido)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_ido 
    ## 
    ## Call:
    ##   Idoteid ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + EpiphyteLog + Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  116.407   417.945
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1307.004     0.0042  0.9484 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1307.001     0.0169  0.8966 
    ##        Idoteid ~ TidalHeightBinary + ...      coef 1305.001     0.0026  0.9593 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.407 with P-value = 0.999 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Idoteid    TempAnomWarm_June   0.0252     7e-04 1314.5137  1365.4406
    ##        Idoteid MonthlyMeanTemp_June  -0.4461    0.0089 1316.2321  2528.7489
    ##        Idoteid         CanopyHeight   0.2784    0.0233 1316.1285   142.7440
    ##        Idoteid           DensityLog   0.4204    0.0219 1317.3774   369.0443
    ##        Idoteid           YearBinary  -0.0757    0.0051 1308.3153   218.0732
    ##   CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ##   CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ##     DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ##     DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ##     DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ##   BladeAreaLog              Idoteid  -0.0413    0.0414   53.5755     0.9200
    ##   BladeAreaLog         CanopyHeight   0.7439    0.0611   92.0091   135.7817
    ##   BladeAreaLog           DensityLog  -0.0980     0.053   87.5514     3.0040
    ##   BladeAreaLog    TempAnomWarm_June  -0.0011    0.0016   39.5783     0.4400
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0181    0.0172   11.0726     0.9241
    ##   BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.8704   332.6165
    ##   BladeAreaLog           YearBinary   0.0574    0.0177  866.2975    10.2859
    ##    EpiphyteLog         BladeAreaLog  -0.1613    0.0436 1317.6898    13.6646
    ##    EpiphyteLog              Idoteid   0.0905    0.0972   63.8238     0.7908
    ##    EpiphyteLog         CanopyHeight  -0.5444    0.1248  260.4624    18.1833
    ##    EpiphyteLog           DensityLog  -0.2945    0.1112  181.2899     6.5426
    ##    EpiphyteLog    TempAnomWarm_June   0.0052     0.004  123.4027     1.5130
    ##    EpiphyteLog MonthlyMeanTemp_June  -0.2021    0.0546   67.0358    11.7318
    ##    EpiphyteLog    TidalHeightBinary  -0.0569    0.0234 1307.2814     5.9151
    ##    EpiphyteLog           YearBinary  -0.0377    0.0299  966.4577     1.5613
    ##     Prevalence         BladeAreaLog   1.7225    0.2973 1338.0000     5.7935
    ##     Prevalence          EpiphyteLog  -0.0119    0.1653 1338.0000    -0.0719
    ##     Prevalence              Idoteid   0.8490    0.4892 1338.0000     1.7354
    ##     Prevalence         CanopyHeight   0.6234    0.6776 1338.0000     0.9199
    ##     Prevalence           DensityLog   1.9035    0.5889 1338.0000     3.2322
    ##     Prevalence    TempAnomWarm_June  -0.0047    0.0246 1338.0000    -0.1912
    ##     Prevalence MonthlyMeanTemp_June   0.5655    0.3371 1338.0000     1.6776
    ##     Prevalence    TidalHeightBinary   0.6389     0.143 1338.0000     4.4674
    ##     Prevalence           YearBinary   0.3477    0.1727 1338.0000     2.0132
    ##   ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##   P.Value Std.Estimate    
    ##    0.0000       0.5682 ***
    ##    0.0000      -1.9023 ***
    ##    0.0000        0.244 ***
    ##    0.0000        0.524 ***
    ##    0.0000      -0.0961 ***
    ##    0.0000      -0.1583 ***
    ##    0.0000      -0.2824 ***
    ##    0.0000      -0.1525 ***
    ##    0.0000      -0.3365 ***
    ##    0.0000       0.6848 ***
    ##    0.0000      -0.0497 ***
    ##    0.3418      -0.0382    
    ##    0.0000       0.6033 ***
    ##    0.0866       -0.113    
    ##    0.5110      -0.0234    
    ##    0.3569      -0.0713    
    ##    0.0000      -0.2852 ***
    ##    0.0014       0.0674  **
    ##    0.0002      -0.0676 ***
    ##    0.3772       0.0351    
    ##    0.0000      -0.1851 ***
    ##    0.0114      -0.1424   *
    ##    0.2210       0.0455    
    ##    0.0011      -0.3342  **
    ##    0.0151      -0.0282   *
    ##    0.2118      -0.0186    
    ##    0.0000            - ***
    ##    0.9427            -    
    ##    0.0827            -    
    ##    0.3576            -    
    ##    0.0012            -  **
    ##    0.8484            -    
    ##    0.0934            -    
    ##    0.0000            - ***
    ##    0.0441            -   *
    ##    0.0270      -0.0527   *
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##        Idoteid   none     0.23        1.00
    ##   CanopyHeight   none     0.14        0.96
    ##     DensityLog   none     0.20        0.99
    ##   BladeAreaLog   none     0.53        0.67
    ##    EpiphyteLog   none     0.12        0.86
    ##     Prevalence  delta     0.15        0.57

``` r
coefs(sem_prev_ido)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1       Idoteid    TempAnomWarm_June   0.0252     7e-04 1314.5137  1365.4406
    ## 2       Idoteid MonthlyMeanTemp_June  -0.4461    0.0089 1316.2321  2528.7489
    ## 3       Idoteid         CanopyHeight   0.2784    0.0233 1316.1285   142.7440
    ## 4       Idoteid           DensityLog   0.4204    0.0219 1317.3774   369.0443
    ## 5       Idoteid           YearBinary  -0.0757    0.0051 1308.3153   218.0732
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ## 8  CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ## 10   DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ## 11   DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ## 12 BladeAreaLog              Idoteid  -0.0413    0.0414   53.5755     0.9200
    ## 13 BladeAreaLog         CanopyHeight   0.7439    0.0611   92.0091   135.7817
    ## 14 BladeAreaLog           DensityLog  -0.0980     0.053   87.5514     3.0040
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0011    0.0016   39.5783     0.4400
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0181    0.0172   11.0726     0.9241
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.8704   332.6165
    ## 18 BladeAreaLog           YearBinary   0.0574    0.0177  866.2975    10.2859
    ## 19  EpiphyteLog         BladeAreaLog  -0.1613    0.0436 1317.6898    13.6646
    ## 20  EpiphyteLog              Idoteid   0.0905    0.0972   63.8238     0.7908
    ## 21  EpiphyteLog         CanopyHeight  -0.5444    0.1248  260.4624    18.1833
    ## 22  EpiphyteLog           DensityLog  -0.2945    0.1112  181.2899     6.5426
    ## 23  EpiphyteLog    TempAnomWarm_June   0.0052     0.004  123.4027     1.5130
    ## 24  EpiphyteLog MonthlyMeanTemp_June  -0.2021    0.0546   67.0358    11.7318
    ## 25  EpiphyteLog    TidalHeightBinary  -0.0569    0.0234 1307.2814     5.9151
    ## 26  EpiphyteLog           YearBinary  -0.0377    0.0299  966.4577     1.5613
    ## 27   Prevalence         BladeAreaLog   1.7225    0.2973 1338.0000     5.7935
    ## 28   Prevalence          EpiphyteLog  -0.0119    0.1653 1338.0000    -0.0719
    ## 29   Prevalence              Idoteid   0.8490    0.4892 1338.0000     1.7354
    ## 30   Prevalence         CanopyHeight   0.6234    0.6776 1338.0000     0.9199
    ## 31   Prevalence           DensityLog   1.9035    0.5889 1338.0000     3.2322
    ## 32   Prevalence    TempAnomWarm_June  -0.0047    0.0246 1338.0000    -0.1912
    ## 33   Prevalence MonthlyMeanTemp_June   0.5655    0.3371 1338.0000     1.6776
    ## 34   Prevalence    TidalHeightBinary   0.6389     0.143 1338.0000     4.4674
    ## 35   Prevalence           YearBinary   0.3477    0.1727 1338.0000     2.0132
    ## 36 ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.5682 ***
    ## 2   0.0000      -1.9023 ***
    ## 3   0.0000       0.2440 ***
    ## 4   0.0000       0.5240 ***
    ## 5   0.0000      -0.0961 ***
    ## 6   0.0000      -0.1583 ***
    ## 7   0.0000      -0.2824 ***
    ## 8   0.0000      -0.1525 ***
    ## 9   0.0000      -0.3365 ***
    ## 10  0.0000       0.6848 ***
    ## 11  0.0000      -0.0497 ***
    ## 12  0.3418      -0.0382    
    ## 13  0.0000       0.6033 ***
    ## 14  0.0866      -0.1130    
    ## 15  0.5110      -0.0234    
    ## 16  0.3569      -0.0713    
    ## 17  0.0000      -0.2852 ***
    ## 18  0.0014       0.0674  **
    ## 19  0.0002      -0.0676 ***
    ## 20  0.3772       0.0351    
    ## 21  0.0000      -0.1851 ***
    ## 22  0.0114      -0.1424   *
    ## 23  0.2210       0.0455    
    ## 24  0.0011      -0.3342  **
    ## 25  0.0151      -0.0282   *
    ## 26  0.2118      -0.0186    
    ## 27  0.0000       0.3227 ***
    ## 28  0.9427      -0.0053    
    ## 29  0.0827       0.1471    
    ## 30  0.3576       0.0947    
    ## 31  0.0012       0.4112  **
    ## 32  0.8484      -0.0184    
    ## 33  0.0934       0.4178    
    ## 34  0.0000       0.1414 ***
    ## 35  0.0441       0.0765   *
    ## 36  0.0270      -0.0527   *

## Prev + Rich

``` r
sem_prev_rich <- psem(
  lmer(Richness ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_large),
    lmer(BladeAreaLog ~ Richness + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Richness + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_large),
  glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + 
          Richness + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_large,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
summary(sem_prev_rich)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_rich 
    ## 
    ## Call:
    ##   Richness ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Richness + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Richness + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + EpiphyteLog + Richness + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  116.331   417.869
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1307.004     0.0042  0.9484 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1307.001     0.0169  0.8966 
    ##       Richness ~ TidalHeightBinary + ...      coef 1305.004     0.0000  0.9967 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.331 with P-value = 0.999 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Richness    TempAnomWarm_June   0.0016     0.022 1316.1431     0.0050
    ##       Richness MonthlyMeanTemp_June  -3.5883    0.2859 1304.2450   155.6152
    ##       Richness         CanopyHeight  12.1562     0.753 1326.7089   259.4973
    ##       Richness           DensityLog  -0.7265     0.706 1330.2905     1.0518
    ##       Richness           YearBinary   0.1736    0.1668 1315.7442     1.0827
    ##   CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ##   CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ##     DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ##     DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ##     DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ##   BladeAreaLog             Richness   0.0005    0.0023  194.7444     0.0526
    ##   BladeAreaLog         CanopyHeight   0.7233    0.0593   72.1725   138.0459
    ##   BladeAreaLog           DensityLog  -0.1089    0.0525   85.9370     3.8138
    ##   BladeAreaLog    TempAnomWarm_June  -0.0014    0.0017   86.1642     0.6109
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0103    0.0173   14.3813     0.2946
    ##   BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.4970   332.4562
    ##   BladeAreaLog           YearBinary   0.0588    0.0177  932.1908    10.8055
    ##    EpiphyteLog         BladeAreaLog  -0.1661    0.0435 1318.9661    14.5505
    ##    EpiphyteLog             Richness   0.0122    0.0043  386.7837     7.5753
    ##    EpiphyteLog         CanopyHeight  -0.6109    0.1239  188.2611    23.2479
    ##    EpiphyteLog           DensityLog  -0.2497    0.1072  229.0281     5.1419
    ##    EpiphyteLog    TempAnomWarm_June   0.0081    0.0036  450.2779     4.7966
    ##    EpiphyteLog MonthlyMeanTemp_June  -0.2025    0.0459  194.5918    17.4666
    ##    EpiphyteLog    TidalHeightBinary  -0.0581    0.0233 1307.3907     6.1955
    ##    EpiphyteLog           YearBinary  -0.0415    0.0294 1209.5972     1.9703
    ##     Prevalence         BladeAreaLog   1.7348    0.2973 1338.0000     5.8342
    ##     Prevalence          EpiphyteLog   0.0284    0.1642 1338.0000     0.1730
    ##     Prevalence             Richness  -0.0362    0.0272 1338.0000    -1.3301
    ##     Prevalence         CanopyHeight   1.1573    0.7073 1338.0000     1.6362
    ##     Prevalence           DensityLog   2.1680    0.5886 1338.0000     3.6831
    ##     Prevalence    TempAnomWarm_June  -0.0008    0.0179 1338.0000    -0.0423
    ##     Prevalence MonthlyMeanTemp_June   0.3047    0.1935 1338.0000     1.5751
    ##     Prevalence    TidalHeightBinary   0.6437    0.1433 1338.0000     4.4919
    ##     Prevalence           YearBinary   0.3080    0.1731 1338.0000     1.7801
    ##   ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##   P.Value Std.Estimate    
    ##    0.9437       0.0025    
    ##    0.0000      -1.0852 ***
    ##    0.0000       0.7558 ***
    ##    0.3053      -0.0642    
    ##    0.2983       0.0156    
    ##    0.0000      -0.1583 ***
    ##    0.0000      -0.2824 ***
    ##    0.0000      -0.1525 ***
    ##    0.0000      -0.3365 ***
    ##    0.0000       0.6848 ***
    ##    0.0000      -0.0497 ***
    ##    0.8188       0.0071    
    ##    0.0000       0.5866 ***
    ##    0.0541      -0.1255    
    ##    0.4366      -0.0296    
    ##    0.5956      -0.0407    
    ##    0.0000      -0.2852 ***
    ##    0.0010       0.0691  **
    ##    0.0001      -0.0696 ***
    ##    0.0062       0.0667  **
    ##    0.0000      -0.2077 ***
    ##    0.0243      -0.1207   *
    ##    0.0290       0.0706   *
    ##    0.0000      -0.3349 ***
    ##    0.0129      -0.0288   *
    ##    0.1607      -0.0204    
    ##    0.0000            - ***
    ##    0.8626            -    
    ##    0.1835            -    
    ##    0.1018            -    
    ##    0.0002            - ***
    ##    0.9663            -    
    ##    0.1152            -    
    ##    0.0000            - ***
    ##    0.0751            -    
    ##    0.0270      -0.0527   *
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Richness   none     0.17        0.99
    ##   CanopyHeight   none     0.14        0.96
    ##     DensityLog   none     0.20        0.99
    ##   BladeAreaLog   none     0.51        0.67
    ##    EpiphyteLog   none     0.11        0.88
    ##     Prevalence  delta     0.12        0.50

``` r
coefs(sem_prev_rich)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Richness    TempAnomWarm_June   0.0016     0.022 1316.1431     0.0050
    ## 2      Richness MonthlyMeanTemp_June  -3.5883    0.2859 1304.2450   155.6152
    ## 3      Richness         CanopyHeight  12.1562     0.753 1326.7089   259.4973
    ## 4      Richness           DensityLog  -0.7265     0.706 1330.2905     1.0518
    ## 5      Richness           YearBinary   0.1736    0.1668 1315.7442     1.0827
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1076.6622   101.9145
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0088  904.9215    42.5620
    ## 8  CanopyHeight           YearBinary  -0.1053    0.0051 1314.6864   418.5992
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     7e-04 1313.0246   806.1687
    ## 10   DensityLog MonthlyMeanTemp_June   0.2002    0.0096 1308.3041   435.5327
    ## 11   DensityLog           YearBinary  -0.0488    0.0055 1310.0323    79.0300
    ## 12 BladeAreaLog             Richness   0.0005    0.0023  194.7444     0.0526
    ## 13 BladeAreaLog         CanopyHeight   0.7233    0.0593   72.1725   138.0459
    ## 14 BladeAreaLog           DensityLog  -0.1089    0.0525   85.9370     3.8138
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0014    0.0017   86.1642     0.6109
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0103    0.0173   14.3813     0.2946
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2414    0.0132 1305.4970   332.4562
    ## 18 BladeAreaLog           YearBinary   0.0588    0.0177  932.1908    10.8055
    ## 19  EpiphyteLog         BladeAreaLog  -0.1661    0.0435 1318.9661    14.5505
    ## 20  EpiphyteLog             Richness   0.0122    0.0043  386.7837     7.5753
    ## 21  EpiphyteLog         CanopyHeight  -0.6109    0.1239  188.2611    23.2479
    ## 22  EpiphyteLog           DensityLog  -0.2497    0.1072  229.0281     5.1419
    ## 23  EpiphyteLog    TempAnomWarm_June   0.0081    0.0036  450.2779     4.7966
    ## 24  EpiphyteLog MonthlyMeanTemp_June  -0.2025    0.0459  194.5918    17.4666
    ## 25  EpiphyteLog    TidalHeightBinary  -0.0581    0.0233 1307.3907     6.1955
    ## 26  EpiphyteLog           YearBinary  -0.0415    0.0294 1209.5972     1.9703
    ## 27   Prevalence         BladeAreaLog   1.7348    0.2973 1338.0000     5.8342
    ## 28   Prevalence          EpiphyteLog   0.0284    0.1642 1338.0000     0.1730
    ## 29   Prevalence             Richness  -0.0362    0.0272 1338.0000    -1.3301
    ## 30   Prevalence         CanopyHeight   1.1573    0.7073 1338.0000     1.6362
    ## 31   Prevalence           DensityLog   2.1680    0.5886 1338.0000     3.6831
    ## 32   Prevalence    TempAnomWarm_June  -0.0008    0.0179 1338.0000    -0.0423
    ## 33   Prevalence MonthlyMeanTemp_June   0.3047    0.1935 1338.0000     1.5751
    ## 34   Prevalence    TidalHeightBinary   0.6437    0.1433 1338.0000     4.4919
    ## 35   Prevalence           YearBinary   0.3080    0.1731 1338.0000     1.7801
    ## 36 ~~DensityLog       ~~CanopyHeight  -0.0527         - 1338.0000    -1.9290
    ##    P.Value Std.Estimate    
    ## 1   0.9437       0.0025    
    ## 2   0.0000      -1.0852 ***
    ## 3   0.0000       0.7558 ***
    ## 4   0.3053      -0.0642    
    ## 5   0.2983       0.0156    
    ## 6   0.0000      -0.1583 ***
    ## 7   0.0000      -0.2824 ***
    ## 8   0.0000      -0.1525 ***
    ## 9   0.0000      -0.3365 ***
    ## 10  0.0000       0.6848 ***
    ## 11  0.0000      -0.0497 ***
    ## 12  0.8188       0.0071    
    ## 13  0.0000       0.5866 ***
    ## 14  0.0541      -0.1255    
    ## 15  0.4366      -0.0296    
    ## 16  0.5956      -0.0407    
    ## 17  0.0000      -0.2852 ***
    ## 18  0.0010       0.0691  **
    ## 19  0.0001      -0.0696 ***
    ## 20  0.0062       0.0667  **
    ## 21  0.0000      -0.2077 ***
    ## 22  0.0243      -0.1207   *
    ## 23  0.0290       0.0706   *
    ## 24  0.0000      -0.3349 ***
    ## 25  0.0129      -0.0288   *
    ## 26  0.1607      -0.0204    
    ## 27  0.0000       0.3231 ***
    ## 28  0.8626       0.0126    
    ## 29  0.1835      -0.0879    
    ## 30  0.1018       0.1748    
    ## 31  0.0002       0.4657 ***
    ## 32  0.9663      -0.0029    
    ## 33  0.1152       0.2239    
    ## 34  0.0000       0.1416 ***
    ## 35  0.0751       0.0674    
    ## 36  0.0270      -0.0527   *

## Lesion + Epifauna

``` r
les_large <- subset(dis_large, LesionArea>0)
les_large$LesionAreaLog <- log10(les_large$LesionArea)
les_large <- na.omit(les_large)
```

``` r
sem_les_epi <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
    lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_epi)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_epi 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  123.630   382.753
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 566.3929     0.8091  0.3688 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 566.4446     1.5459  0.2143 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 564.3721     0.0950  0.7580 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 5.63 with P-value = 0.466 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0037    0.0018 502.0516     4.3381
    ##        Epifauna MonthlyMeanTemp_June  -0.1648    0.0217 452.1378    55.1785
    ##        Epifauna         CanopyHeight  -0.5050    0.0664 584.4397    56.9444
    ##        Epifauna           DensityLog  -0.1875    0.0542 589.9714    11.7876
    ##        Epifauna           YearBinary  -0.0255    0.0129 582.8627     3.8827
    ##    CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ##    CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ##    CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ##      DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ##      DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ##      DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ##    BladeAreaLog             Epifauna  -0.0443    0.0515  65.2667     0.6510
    ##    BladeAreaLog         CanopyHeight   0.7139    0.0834  77.6108    64.1950
    ##    BladeAreaLog           DensityLog  -0.1620    0.0638  43.6786     5.1197
    ##    BladeAreaLog    TempAnomWarm_June   0.0022    0.0021  61.3906     0.9792
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0314    0.0159   8.8266     3.4388
    ##    BladeAreaLog    TidalHeightBinary  -0.2103    0.0171 575.0568   151.3186
    ##    BladeAreaLog           YearBinary   0.0647    0.0224 469.6165     8.1521
    ##     EpiphyteLog         BladeAreaLog  -0.2476    0.0791 578.4312     9.7609
    ##     EpiphyteLog             Epifauna   0.0462    0.1239 143.5518     0.1321
    ##     EpiphyteLog         CanopyHeight  -0.3529    0.2074 150.8053     2.7667
    ##     EpiphyteLog           DensityLog  -0.3216    0.1656 172.4694     3.5236
    ##     EpiphyteLog    TempAnomWarm_June   0.0117    0.0057 129.3126     3.8214
    ##     EpiphyteLog MonthlyMeanTemp_June  -0.2773    0.0645  48.9092    15.4351
    ##     EpiphyteLog    TidalHeightBinary  -0.0575    0.0365 570.3920     2.4842
    ##     EpiphyteLog           YearBinary   0.0261    0.0453 498.6372     0.3273
    ##   LesionAreaLog         BladeAreaLog   0.5103    0.1269 583.7689    15.9390
    ##   LesionAreaLog          EpiphyteLog   0.0209    0.0624 327.1529     0.1041
    ##   LesionAreaLog             Epifauna   0.6832     0.144  49.9020    20.4009
    ##   LesionAreaLog         CanopyHeight   0.8891    0.2529  83.1886    11.3898
    ##   LesionAreaLog           DensityLog   0.2129    0.1893  51.8491     1.0726
    ##   LesionAreaLog    TempAnomWarm_June   0.0114    0.0065  52.4070     2.7785
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0842    0.0549   9.8015     1.9893
    ##   LesionAreaLog    TidalHeightBinary   0.0084    0.0589 583.3371     0.0200
    ##   LesionAreaLog           YearBinary   0.0494    0.0679 460.0441     0.5185
    ##    ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##   P.Value Std.Estimate    
    ##    0.0378      -0.0747   *
    ##    0.0000      -0.6044 ***
    ##    0.0000      -0.3847 ***
    ##    0.0006      -0.2148 ***
    ##    0.0493      -0.0301   *
    ##    0.0000      -0.3424 ***
    ##    0.2727       0.0680    
    ##    0.0000      -0.1904 ***
    ##    0.0000      -0.2264 ***
    ##    0.0000       0.4339 ***
    ##    0.0002      -0.0294 ***
    ##    0.4227      -0.0482    
    ##    0.0000       0.5919 ***
    ##    0.0287      -0.2020   *
    ##    0.3263       0.0469    
    ##    0.0973      -0.1254    
    ##    0.0000      -0.2750 ***
    ##    0.0045       0.0833  **
    ##    0.0019      -0.1089  **
    ##    0.7168       0.0221    
    ##    0.0983      -0.1287    
    ##    0.0622      -0.1763    
    ##    0.0528       0.1119    
    ##    0.0003      -0.4866 ***
    ##    0.1155      -0.0331    
    ##    0.5675       0.0148    
    ##    0.0001       0.2738 ***
    ##    0.7472       0.0255    
    ##    0.0000       0.3990 ***
    ##    0.0011       0.3955  **
    ##    0.3052       0.1424    
    ##    0.1015       0.1336    
    ##    0.1894       0.1802    
    ##    0.8875       0.0059    
    ##    0.4719       0.0341    
    ##    0.0013       0.1236  **
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.13        0.98
    ##    CanopyHeight   none     0.07        0.98
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.60        0.71
    ##     EpiphyteLog   none     0.17        0.86
    ##   LesionAreaLog   none     0.23        0.48

Passes global fit test. Epifauna is sig and pos for Lesion Area
Standardized coefs:

``` r
coefs(sem_les_epi)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1       Epifauna    TempAnomWarm_June  -0.0037    0.0018 502.0516     4.3381
    ## 2       Epifauna MonthlyMeanTemp_June  -0.1648    0.0217 452.1378    55.1785
    ## 3       Epifauna         CanopyHeight  -0.5050    0.0664 584.4397    56.9444
    ## 4       Epifauna           DensityLog  -0.1875    0.0542 589.9714    11.7876
    ## 5       Epifauna           YearBinary  -0.0255    0.0129 582.8627     3.8827
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ## 8   CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ## 9     DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ## 10    DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ## 11    DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ## 12  BladeAreaLog             Epifauna  -0.0443    0.0515  65.2667     0.6510
    ## 13  BladeAreaLog         CanopyHeight   0.7139    0.0834  77.6108    64.1950
    ## 14  BladeAreaLog           DensityLog  -0.1620    0.0638  43.6786     5.1197
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0022    0.0021  61.3906     0.9792
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0314    0.0159   8.8266     3.4388
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2103    0.0171 575.0568   151.3186
    ## 18  BladeAreaLog           YearBinary   0.0647    0.0224 469.6165     8.1521
    ## 19   EpiphyteLog         BladeAreaLog  -0.2476    0.0791 578.4312     9.7609
    ## 20   EpiphyteLog             Epifauna   0.0462    0.1239 143.5518     0.1321
    ## 21   EpiphyteLog         CanopyHeight  -0.3529    0.2074 150.8053     2.7667
    ## 22   EpiphyteLog           DensityLog  -0.3216    0.1656 172.4694     3.5236
    ## 23   EpiphyteLog    TempAnomWarm_June   0.0117    0.0057 129.3126     3.8214
    ## 24   EpiphyteLog MonthlyMeanTemp_June  -0.2773    0.0645  48.9092    15.4351
    ## 25   EpiphyteLog    TidalHeightBinary  -0.0575    0.0365 570.3920     2.4842
    ## 26   EpiphyteLog           YearBinary   0.0261    0.0453 498.6372     0.3273
    ## 27 LesionAreaLog         BladeAreaLog   0.5103    0.1269 583.7689    15.9390
    ## 28 LesionAreaLog          EpiphyteLog   0.0209    0.0624 327.1529     0.1041
    ## 29 LesionAreaLog             Epifauna   0.6832     0.144  49.9020    20.4009
    ## 30 LesionAreaLog         CanopyHeight   0.8891    0.2529  83.1886    11.3898
    ## 31 LesionAreaLog           DensityLog   0.2129    0.1893  51.8491     1.0726
    ## 32 LesionAreaLog    TempAnomWarm_June   0.0114    0.0065  52.4070     2.7785
    ## 33 LesionAreaLog MonthlyMeanTemp_June   0.0842    0.0549   9.8015     1.9893
    ## 34 LesionAreaLog    TidalHeightBinary   0.0084    0.0589 583.3371     0.0200
    ## 35 LesionAreaLog           YearBinary   0.0494    0.0679 460.0441     0.5185
    ## 36  ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##    P.Value Std.Estimate    
    ## 1   0.0378      -0.0747   *
    ## 2   0.0000      -0.6044 ***
    ## 3   0.0000      -0.3847 ***
    ## 4   0.0006      -0.2148 ***
    ## 5   0.0493      -0.0301   *
    ## 6   0.0000      -0.3424 ***
    ## 7   0.2727       0.0680    
    ## 8   0.0000      -0.1904 ***
    ## 9   0.0000      -0.2264 ***
    ## 10  0.0000       0.4339 ***
    ## 11  0.0002      -0.0294 ***
    ## 12  0.4227      -0.0482    
    ## 13  0.0000       0.5919 ***
    ## 14  0.0287      -0.2020   *
    ## 15  0.3263       0.0469    
    ## 16  0.0973      -0.1254    
    ## 17  0.0000      -0.2750 ***
    ## 18  0.0045       0.0833  **
    ## 19  0.0019      -0.1089  **
    ## 20  0.7168       0.0221    
    ## 21  0.0983      -0.1287    
    ## 22  0.0622      -0.1763    
    ## 23  0.0528       0.1119    
    ## 24  0.0003      -0.4866 ***
    ## 25  0.1155      -0.0331    
    ## 26  0.5675       0.0148    
    ## 27  0.0001       0.2738 ***
    ## 28  0.7472       0.0255    
    ## 29  0.0000       0.3990 ***
    ## 30  0.0011       0.3955  **
    ## 31  0.3052       0.1424    
    ## 32  0.1015       0.1336    
    ## 33  0.1894       0.1802    
    ## 34  0.8875       0.0059    
    ## 35  0.4719       0.0341    
    ## 36  0.0013       0.1236  **

## Lesion + Amp

``` r
sem_les_amp <- psem(
  lmer(Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
    lmer(BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_amp)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_amp 
    ## 
    ## Call:
    ##   Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  124.398   383.521
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 566.3929     0.8091  0.3688 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 566.4446     1.5459  0.2143 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 564.1430     0.4217  0.5163 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.398 with P-value = 0.38 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##       Ampithoid    TempAnomWarm_June  -0.0125     0.001 551.0471   155.7120
    ##       Ampithoid MonthlyMeanTemp_June   0.1947    0.0123 516.6767   246.2004
    ##       Ampithoid         CanopyHeight   0.1814    0.0382 582.3497    22.3968
    ##       Ampithoid           DensityLog   0.0236    0.0309 575.2430     0.5758
    ##       Ampithoid           YearBinary  -0.1174    0.0073 571.6844   257.4191
    ##    CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ##    CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ##    CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ##      DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ##      DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ##      DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ##    BladeAreaLog            Ampithoid   0.1063    0.0499  44.6374     3.8840
    ##    BladeAreaLog         CanopyHeight   0.8198    0.0755  62.1286    95.1908
    ##    BladeAreaLog           DensityLog  -0.1353    0.0559  32.1486     4.4831
    ##    BladeAreaLog    TempAnomWarm_June   0.0047     0.002  52.0192     5.0572
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0457    0.0153  10.7710     8.1105
    ##    BladeAreaLog    TidalHeightBinary  -0.2100    0.0171 576.6453   150.7860
    ##    BladeAreaLog           YearBinary   0.0946     0.025 357.5157    13.4409
    ##     EpiphyteLog         BladeAreaLog  -0.2443    0.0789 576.7283     9.5661
    ##     EpiphyteLog            Ampithoid  -0.2241    0.1473  52.6058     2.2065
    ##     EpiphyteLog         CanopyHeight  -0.4733    0.2019 221.9329     5.2668
    ##     EpiphyteLog           DensityLog  -0.3403    0.1614 148.6749     4.1370
    ##     EpiphyteLog    TempAnomWarm_June   0.0071    0.0061 102.2635     1.2215
    ##     EpiphyteLog MonthlyMeanTemp_June  -0.2423    0.0707  53.0657     9.8797
    ##     EpiphyteLog    TidalHeightBinary  -0.0571    0.0363 569.6603     2.4646
    ##     EpiphyteLog           YearBinary  -0.0197    0.0529 477.8697     0.1355
    ##   LesionAreaLog         BladeAreaLog   0.5178     0.129 584.8602    15.9695
    ##   LesionAreaLog          EpiphyteLog   0.0479    0.0647 480.2029     0.5202
    ##   LesionAreaLog            Ampithoid  -0.2860    0.1631  45.7241     2.8796
    ##   LesionAreaLog         CanopyHeight   0.1615    0.2739 152.6350     0.3223
    ##   LesionAreaLog           DensityLog  -0.1598    0.1974  59.0893     0.5811
    ##   LesionAreaLog    TempAnomWarm_June  -0.0102    0.0074  48.8374     1.6591
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1988    0.0739  14.1703     5.9088
    ##   LesionAreaLog    TidalHeightBinary   0.0129    0.0596 579.2826     0.0469
    ##   LesionAreaLog           YearBinary  -0.0747    0.0796 359.4287     0.8499
    ##    ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2374 ***
    ##    0.0000       0.6758 ***
    ##    0.0000       0.1308 ***
    ##    0.4483       0.0256    
    ##    0.0000      -0.1315 ***
    ##    0.0000      -0.3424 ***
    ##    0.2727       0.0680    
    ##    0.0000      -0.1904 ***
    ##    0.0000      -0.2264 ***
    ##    0.0000       0.4339 ***
    ##    0.0002      -0.0294 ***
    ##    0.0550       0.1222    
    ##    0.0000       0.6797 ***
    ##    0.0421      -0.1687   *
    ##    0.0288       0.1023   *
    ##    0.0162      -0.1824   *
    ##    0.0000      -0.2746 ***
    ##    0.0003       0.1218 ***
    ##    0.0021      -0.1074  **
    ##    0.1434      -0.1133    
    ##    0.0227      -0.1725   *
    ##    0.0437      -0.1866   *
    ##    0.2717       0.0684    
    ##    0.0027      -0.4251  **
    ##    0.1170      -0.0328    
    ##    0.7130      -0.0111    
    ##    0.0001       0.2778 ***
    ##    0.4711       0.0584    
    ##    0.0965      -0.1764    
    ##    0.5711       0.0718    
    ##    0.4489      -0.1069    
    ##    0.2038      -0.1192    
    ##    0.0289       0.4256   *
    ##    0.8286       0.0091    
    ##    0.3572      -0.0516    
    ##    0.0013       0.1236  **
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##       Ampithoid   none     0.28        0.99
    ##    CanopyHeight   none     0.07        0.98
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.64        0.72
    ##     EpiphyteLog   none     0.17        0.87
    ##   LesionAreaLog   none     0.14        0.56

Standardized coefs:

``` r
coefs(sem_les_amp)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1      Ampithoid    TempAnomWarm_June  -0.0125     0.001 551.0471   155.7120
    ## 2      Ampithoid MonthlyMeanTemp_June   0.1947    0.0123 516.6767   246.2004
    ## 3      Ampithoid         CanopyHeight   0.1814    0.0382 582.3497    22.3968
    ## 4      Ampithoid           DensityLog   0.0236    0.0309 575.2430     0.5758
    ## 5      Ampithoid           YearBinary  -0.1174    0.0073 571.6844   257.4191
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ## 8   CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ## 9     DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ## 10    DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ## 11    DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ## 12  BladeAreaLog            Ampithoid   0.1063    0.0499  44.6374     3.8840
    ## 13  BladeAreaLog         CanopyHeight   0.8198    0.0755  62.1286    95.1908
    ## 14  BladeAreaLog           DensityLog  -0.1353    0.0559  32.1486     4.4831
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0047     0.002  52.0192     5.0572
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0457    0.0153  10.7710     8.1105
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2100    0.0171 576.6453   150.7860
    ## 18  BladeAreaLog           YearBinary   0.0946     0.025 357.5157    13.4409
    ## 19   EpiphyteLog         BladeAreaLog  -0.2443    0.0789 576.7283     9.5661
    ## 20   EpiphyteLog            Ampithoid  -0.2241    0.1473  52.6058     2.2065
    ## 21   EpiphyteLog         CanopyHeight  -0.4733    0.2019 221.9329     5.2668
    ## 22   EpiphyteLog           DensityLog  -0.3403    0.1614 148.6749     4.1370
    ## 23   EpiphyteLog    TempAnomWarm_June   0.0071    0.0061 102.2635     1.2215
    ## 24   EpiphyteLog MonthlyMeanTemp_June  -0.2423    0.0707  53.0657     9.8797
    ## 25   EpiphyteLog    TidalHeightBinary  -0.0571    0.0363 569.6603     2.4646
    ## 26   EpiphyteLog           YearBinary  -0.0197    0.0529 477.8697     0.1355
    ## 27 LesionAreaLog         BladeAreaLog   0.5178     0.129 584.8602    15.9695
    ## 28 LesionAreaLog          EpiphyteLog   0.0479    0.0647 480.2029     0.5202
    ## 29 LesionAreaLog            Ampithoid  -0.2860    0.1631  45.7241     2.8796
    ## 30 LesionAreaLog         CanopyHeight   0.1615    0.2739 152.6350     0.3223
    ## 31 LesionAreaLog           DensityLog  -0.1598    0.1974  59.0893     0.5811
    ## 32 LesionAreaLog    TempAnomWarm_June  -0.0102    0.0074  48.8374     1.6591
    ## 33 LesionAreaLog MonthlyMeanTemp_June   0.1988    0.0739  14.1703     5.9088
    ## 34 LesionAreaLog    TidalHeightBinary   0.0129    0.0596 579.2826     0.0469
    ## 35 LesionAreaLog           YearBinary  -0.0747    0.0796 359.4287     0.8499
    ## 36  ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2374 ***
    ## 2   0.0000       0.6758 ***
    ## 3   0.0000       0.1308 ***
    ## 4   0.4483       0.0256    
    ## 5   0.0000      -0.1315 ***
    ## 6   0.0000      -0.3424 ***
    ## 7   0.2727       0.0680    
    ## 8   0.0000      -0.1904 ***
    ## 9   0.0000      -0.2264 ***
    ## 10  0.0000       0.4339 ***
    ## 11  0.0002      -0.0294 ***
    ## 12  0.0550       0.1222    
    ## 13  0.0000       0.6797 ***
    ## 14  0.0421      -0.1687   *
    ## 15  0.0288       0.1023   *
    ## 16  0.0162      -0.1824   *
    ## 17  0.0000      -0.2746 ***
    ## 18  0.0003       0.1218 ***
    ## 19  0.0021      -0.1074  **
    ## 20  0.1434      -0.1133    
    ## 21  0.0227      -0.1725   *
    ## 22  0.0437      -0.1866   *
    ## 23  0.2717       0.0684    
    ## 24  0.0027      -0.4251  **
    ## 25  0.1170      -0.0328    
    ## 26  0.7130      -0.0111    
    ## 27  0.0001       0.2778 ***
    ## 28  0.4711       0.0584    
    ## 29  0.0965      -0.1764    
    ## 30  0.5711       0.0718    
    ## 31  0.4489      -0.1069    
    ## 32  0.2038      -0.1192    
    ## 33  0.0289       0.4256   *
    ## 34  0.8286       0.0091    
    ## 35  0.3572      -0.0516    
    ## 36  0.0013       0.1236  **

## Lesion + Lac

``` r
sem_les_lac <- psem(
  lmer(Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
    lmer(BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_lac)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_lac 
    ## 
    ## Call:
    ##   Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  124.151   383.274
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 566.3929     0.8091  0.3688 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 566.4446     1.5459  0.2143 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 564.6927     0.2996  0.5843 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.151 with P-value = 0.406 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##          Lacuna    TempAnomWarm_June   0.0166    0.0035 370.6236    21.3242
    ##          Lacuna MonthlyMeanTemp_June   0.0835    0.0428 290.5015     3.5322
    ##          Lacuna         CanopyHeight   0.5470    0.1315 531.7603    16.8388
    ##          Lacuna           DensityLog   0.7898    0.1079 562.0392    52.1576
    ##          Lacuna           YearBinary  -0.1307     0.026 588.0300    25.0746
    ##    CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ##    CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ##    CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ##      DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ##      DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ##      DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ##    BladeAreaLog               Lacuna  -0.0260    0.0261  62.6004     0.8484
    ##    BladeAreaLog         CanopyHeight   0.7516    0.0704  56.4316    97.2344
    ##    BladeAreaLog           DensityLog  -0.1440    0.0576  32.4744     4.8756
    ##    BladeAreaLog    TempAnomWarm_June   0.0034    0.0019  71.6058     2.9341
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0327    0.0147   8.7126     4.3724
    ##    BladeAreaLog    TidalHeightBinary  -0.2101    0.0171 575.8800   150.5047
    ##    BladeAreaLog           YearBinary   0.0620    0.0227 463.8710     7.2157
    ##     EpiphyteLog         BladeAreaLog  -0.2506    0.0789 577.4976    10.0598
    ##     EpiphyteLog               Lacuna  -0.0856    0.0668 243.6537     1.5466
    ##     EpiphyteLog         CanopyHeight  -0.3596    0.1946 117.2524     3.2416
    ##     EpiphyteLog           DensityLog  -0.2903    0.1653 130.3670     2.8584
    ##     EpiphyteLog    TempAnomWarm_June   0.0120    0.0055 108.8510     4.2803
    ##     EpiphyteLog MonthlyMeanTemp_June  -0.2692    0.0657  48.0060    14.0622
    ##     EpiphyteLog    TidalHeightBinary  -0.0574    0.0364 570.0969     2.4824
    ##     EpiphyteLog           YearBinary   0.0090    0.0463 511.8902     0.0372
    ##   LesionAreaLog         BladeAreaLog   0.5111    0.1278 580.6367    15.8033
    ##   LesionAreaLog          EpiphyteLog   0.0561    0.0634 418.5910     0.7400
    ##   LesionAreaLog               Lacuna   0.2754     0.075  45.2485    12.1979
    ##   LesionAreaLog         CanopyHeight   0.3561    0.2156  64.9834     2.5843
    ##   LesionAreaLog           DensityLog  -0.2912    0.1706  34.4867     2.5942
    ##   LesionAreaLog    TempAnomWarm_June  -0.0089    0.0065  62.6631     1.7045
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1338    0.0675  12.7042     3.1779
    ##   LesionAreaLog    TidalHeightBinary   0.0077    0.0594 583.2117     0.0168
    ##   LesionAreaLog           YearBinary   0.0497    0.0691 451.0346     0.5073
    ##    ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##   P.Value Std.Estimate    
    ##    0.0000       0.1844 ***
    ##    0.0612       0.1701    
    ##    0.0000       0.2316 ***
    ##    0.0000       0.5028 ***
    ##    0.0000      -0.0859 ***
    ##    0.0000      -0.3424 ***
    ##    0.2727       0.0680    
    ##    0.0000      -0.1904 ***
    ##    0.0000      -0.2264 ***
    ##    0.0000       0.4339 ***
    ##    0.0002      -0.0294 ***
    ##    0.3606      -0.0510    
    ##    0.0000       0.6232 ***
    ##    0.0344      -0.1796   *
    ##    0.0911       0.0734    
    ##    0.0671      -0.1304    
    ##    0.0000      -0.2747 ***
    ##    0.0075       0.0799  **
    ##    0.0016      -0.1102  **
    ##    0.2148      -0.0737    
    ##    0.0744      -0.1311    
    ##    0.0933      -0.1592    
    ##    0.0409       0.1152   *
    ##    0.0005      -0.4725 ***
    ##    0.1157      -0.0330    
    ##    0.8471       0.0051    
    ##    0.0001       0.2742 ***
    ##    0.3902       0.0685    
    ##    0.0011       0.2894  **
    ##    0.1128       0.1584    
    ##    0.1164      -0.1948    
    ##    0.1965      -0.1039    
    ##    0.0985       0.2866    
    ##    0.8969       0.0054    
    ##    0.4767       0.0343    
    ##    0.0013       0.1236  **
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##          Lacuna   none     0.10        0.96
    ##    CanopyHeight   none     0.07        0.98
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.62        0.71
    ##     EpiphyteLog   none     0.15        0.86
    ##   LesionAreaLog   none     0.20        0.58

``` r
coefs(sem_les_lac)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1         Lacuna    TempAnomWarm_June   0.0166    0.0035 370.6236    21.3242
    ## 2         Lacuna MonthlyMeanTemp_June   0.0835    0.0428 290.5015     3.5322
    ## 3         Lacuna         CanopyHeight   0.5470    0.1315 531.7603    16.8388
    ## 4         Lacuna           DensityLog   0.7898    0.1079 562.0392    52.1576
    ## 5         Lacuna           YearBinary  -0.1307     0.026 588.0300    25.0746
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ## 8   CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ## 9     DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ## 10    DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ## 11    DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ## 12  BladeAreaLog               Lacuna  -0.0260    0.0261  62.6004     0.8484
    ## 13  BladeAreaLog         CanopyHeight   0.7516    0.0704  56.4316    97.2344
    ## 14  BladeAreaLog           DensityLog  -0.1440    0.0576  32.4744     4.8756
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0034    0.0019  71.6058     2.9341
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0327    0.0147   8.7126     4.3724
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2101    0.0171 575.8800   150.5047
    ## 18  BladeAreaLog           YearBinary   0.0620    0.0227 463.8710     7.2157
    ## 19   EpiphyteLog         BladeAreaLog  -0.2506    0.0789 577.4976    10.0598
    ## 20   EpiphyteLog               Lacuna  -0.0856    0.0668 243.6537     1.5466
    ## 21   EpiphyteLog         CanopyHeight  -0.3596    0.1946 117.2524     3.2416
    ## 22   EpiphyteLog           DensityLog  -0.2903    0.1653 130.3670     2.8584
    ## 23   EpiphyteLog    TempAnomWarm_June   0.0120    0.0055 108.8510     4.2803
    ## 24   EpiphyteLog MonthlyMeanTemp_June  -0.2692    0.0657  48.0060    14.0622
    ## 25   EpiphyteLog    TidalHeightBinary  -0.0574    0.0364 570.0969     2.4824
    ## 26   EpiphyteLog           YearBinary   0.0090    0.0463 511.8902     0.0372
    ## 27 LesionAreaLog         BladeAreaLog   0.5111    0.1278 580.6367    15.8033
    ## 28 LesionAreaLog          EpiphyteLog   0.0561    0.0634 418.5910     0.7400
    ## 29 LesionAreaLog               Lacuna   0.2754     0.075  45.2485    12.1979
    ## 30 LesionAreaLog         CanopyHeight   0.3561    0.2156  64.9834     2.5843
    ## 31 LesionAreaLog           DensityLog  -0.2912    0.1706  34.4867     2.5942
    ## 32 LesionAreaLog    TempAnomWarm_June  -0.0089    0.0065  62.6631     1.7045
    ## 33 LesionAreaLog MonthlyMeanTemp_June   0.1338    0.0675  12.7042     3.1779
    ## 34 LesionAreaLog    TidalHeightBinary   0.0077    0.0594 583.2117     0.0168
    ## 35 LesionAreaLog           YearBinary   0.0497    0.0691 451.0346     0.5073
    ## 36  ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.1844 ***
    ## 2   0.0612       0.1701    
    ## 3   0.0000       0.2316 ***
    ## 4   0.0000       0.5028 ***
    ## 5   0.0000      -0.0859 ***
    ## 6   0.0000      -0.3424 ***
    ## 7   0.2727       0.0680    
    ## 8   0.0000      -0.1904 ***
    ## 9   0.0000      -0.2264 ***
    ## 10  0.0000       0.4339 ***
    ## 11  0.0002      -0.0294 ***
    ## 12  0.3606      -0.0510    
    ## 13  0.0000       0.6232 ***
    ## 14  0.0344      -0.1796   *
    ## 15  0.0911       0.0734    
    ## 16  0.0671      -0.1304    
    ## 17  0.0000      -0.2747 ***
    ## 18  0.0075       0.0799  **
    ## 19  0.0016      -0.1102  **
    ## 20  0.2148      -0.0737    
    ## 21  0.0744      -0.1311    
    ## 22  0.0933      -0.1592    
    ## 23  0.0409       0.1152   *
    ## 24  0.0005      -0.4725 ***
    ## 25  0.1157      -0.0330    
    ## 26  0.8471       0.0051    
    ## 27  0.0001       0.2742 ***
    ## 28  0.3902       0.0685    
    ## 29  0.0011       0.2894  **
    ## 30  0.1128       0.1584    
    ## 31  0.1164      -0.1948    
    ## 32  0.1965      -0.1039    
    ## 33  0.0985       0.2866    
    ## 34  0.8969       0.0054    
    ## 35  0.4767       0.0343    
    ## 36  0.0013       0.1236  **

## Lesion + Ido

``` r
sem_les_ido <- psem(
  lmer(Idoteid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
    lmer(BladeAreaLog ~ Idoteid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_ido)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_ido 
    ## 
    ## Call:
    ##   Idoteid ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  126.126   385.249
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 566.3929     0.8091  0.3688 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 566.4446     1.5459  0.2143 
    ##        Idoteid ~ TidalHeightBinary + ...      coef 564.1055     1.5233  0.2176 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 8.126 with P-value = 0.229 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##         Idoteid    TempAnomWarm_June   0.0236    0.0012 574.9073   419.5729
    ##         Idoteid MonthlyMeanTemp_June  -0.4340    0.0143 574.6230   917.6006
    ##         Idoteid         CanopyHeight   0.2235    0.0432 582.1045    26.6334
    ##         Idoteid           DensityLog   0.4908    0.0351 579.0743   194.4424
    ##         Idoteid           YearBinary  -0.0861    0.0083 572.2059   108.5606
    ##    CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ##    CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ##    CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ##      DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ##      DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ##      DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ##    BladeAreaLog              Idoteid  -0.0341    0.0489  41.9018     0.4381
    ##    BladeAreaLog         CanopyHeight   0.7633    0.0764  70.9873    85.6867
    ##    BladeAreaLog           DensityLog  -0.1332    0.0619  41.2670     3.6678
    ##    BladeAreaLog    TempAnomWarm_June   0.0030    0.0019  60.3748     2.3782
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0339    0.0161   8.5933     3.9559
    ##    BladeAreaLog    TidalHeightBinary  -0.2106    0.0171 575.0071   151.7331
    ##    BladeAreaLog           YearBinary   0.0673     0.022 400.1263     9.0558
    ##     EpiphyteLog         BladeAreaLog  -0.2436     0.079 579.8209     9.4573
    ##     EpiphyteLog              Idoteid   0.1927    0.1292  51.9252     2.0251
    ##     EpiphyteLog         CanopyHeight  -0.4505    0.1942 128.8696     5.0755
    ##     EpiphyteLog           DensityLog  -0.3979    0.1618 114.3431     5.5594
    ##     EpiphyteLog    TempAnomWarm_June   0.0081    0.0056  56.6648     1.7887
    ##     EpiphyteLog MonthlyMeanTemp_June  -0.2254    0.0708  28.7568     8.2001
    ##     EpiphyteLog    TidalHeightBinary  -0.0550    0.0365 572.2628     2.2654
    ##     EpiphyteLog           YearBinary   0.0353    0.0451 404.2340     0.6033
    ##   LesionAreaLog         BladeAreaLog   0.5078    0.1281 582.2096    15.5696
    ##   LesionAreaLog          EpiphyteLog   0.0433    0.0654 554.6416     0.4258
    ##   LesionAreaLog              Idoteid   0.3791    0.1471  29.0564     6.1675
    ##   LesionAreaLog         CanopyHeight   0.1774    0.2556  78.0242     0.4541
    ##   LesionAreaLog           DensityLog  -0.3251    0.2059  51.4778     2.2692
    ##   LesionAreaLog    TempAnomWarm_June  -0.0135    0.0076  58.9294     2.7199
    ##   LesionAreaLog MonthlyMeanTemp_June   0.2861     0.088  24.1140     8.4192
    ##   LesionAreaLog    TidalHeightBinary   0.0141    0.0593 579.7908     0.0566
    ##   LesionAreaLog           YearBinary   0.0108    0.0682 409.7015     0.0249
    ##    ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##   P.Value Std.Estimate    
    ##    0.0000       0.4671 ***
    ##    0.0000      -1.5697 ***
    ##    0.0000       0.1679 ***
    ##    0.0000       0.5545 ***
    ##    0.0000      -0.1005 ***
    ##    0.0000      -0.3424 ***
    ##    0.2727       0.0680    
    ##    0.0000      -0.1904 ***
    ##    0.0000      -0.2264 ***
    ##    0.0000       0.4339 ***
    ##    0.0002      -0.0294 ***
    ##    0.5117      -0.0376    
    ##    0.0000       0.6329 ***
    ##    0.0624      -0.1661    
    ##    0.1283       0.0653    
    ##    0.0794      -0.1353    
    ##    0.0000      -0.2754 ***
    ##    0.0028       0.0866  **
    ##    0.0022      -0.1071  **
    ##    0.1607       0.0935    
    ##    0.0260      -0.1642   *
    ##    0.0201      -0.2181   *
    ##    0.1864       0.0780    
    ##    0.0077      -0.3955  **
    ##    0.1328      -0.0316    
    ##    0.4378       0.0200    
    ##    0.0001       0.2725 ***
    ##    0.5144       0.0528    
    ##    0.0190       0.2245   *
    ##    0.5024       0.0789    
    ##    0.1381      -0.2175    
    ##    0.1044      -0.1581    
    ##    0.0078       0.6128  **
    ##    0.8121       0.0099    
    ##    0.8748       0.0075    
    ##    0.0013       0.1236  **
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##         Idoteid   none     0.21        1.00
    ##    CanopyHeight   none     0.07        0.98
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.60        0.71
    ##     EpiphyteLog   none     0.14        0.85
    ##   LesionAreaLog   none     0.16        0.75

``` r
coefs(sem_les_ido)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1        Idoteid    TempAnomWarm_June   0.0236    0.0012 574.9073   419.5729
    ## 2        Idoteid MonthlyMeanTemp_June  -0.4340    0.0143 574.6230   917.6006
    ## 3        Idoteid         CanopyHeight   0.2235    0.0432 582.1045    26.6334
    ## 4        Idoteid           DensityLog   0.4908    0.0351 579.0743   194.4424
    ## 5        Idoteid           YearBinary  -0.0861    0.0083 572.2059   108.5606
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ## 8   CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ## 9     DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ## 10    DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ## 11    DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ## 12  BladeAreaLog              Idoteid  -0.0341    0.0489  41.9018     0.4381
    ## 13  BladeAreaLog         CanopyHeight   0.7633    0.0764  70.9873    85.6867
    ## 14  BladeAreaLog           DensityLog  -0.1332    0.0619  41.2670     3.6678
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0030    0.0019  60.3748     2.3782
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0339    0.0161   8.5933     3.9559
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2106    0.0171 575.0071   151.7331
    ## 18  BladeAreaLog           YearBinary   0.0673     0.022 400.1263     9.0558
    ## 19   EpiphyteLog         BladeAreaLog  -0.2436     0.079 579.8209     9.4573
    ## 20   EpiphyteLog              Idoteid   0.1927    0.1292  51.9252     2.0251
    ## 21   EpiphyteLog         CanopyHeight  -0.4505    0.1942 128.8696     5.0755
    ## 22   EpiphyteLog           DensityLog  -0.3979    0.1618 114.3431     5.5594
    ## 23   EpiphyteLog    TempAnomWarm_June   0.0081    0.0056  56.6648     1.7887
    ## 24   EpiphyteLog MonthlyMeanTemp_June  -0.2254    0.0708  28.7568     8.2001
    ## 25   EpiphyteLog    TidalHeightBinary  -0.0550    0.0365 572.2628     2.2654
    ## 26   EpiphyteLog           YearBinary   0.0353    0.0451 404.2340     0.6033
    ## 27 LesionAreaLog         BladeAreaLog   0.5078    0.1281 582.2096    15.5696
    ## 28 LesionAreaLog          EpiphyteLog   0.0433    0.0654 554.6416     0.4258
    ## 29 LesionAreaLog              Idoteid   0.3791    0.1471  29.0564     6.1675
    ## 30 LesionAreaLog         CanopyHeight   0.1774    0.2556  78.0242     0.4541
    ## 31 LesionAreaLog           DensityLog  -0.3251    0.2059  51.4778     2.2692
    ## 32 LesionAreaLog    TempAnomWarm_June  -0.0135    0.0076  58.9294     2.7199
    ## 33 LesionAreaLog MonthlyMeanTemp_June   0.2861     0.088  24.1140     8.4192
    ## 34 LesionAreaLog    TidalHeightBinary   0.0141    0.0593 579.7908     0.0566
    ## 35 LesionAreaLog           YearBinary   0.0108    0.0682 409.7015     0.0249
    ## 36  ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.4671 ***
    ## 2   0.0000      -1.5697 ***
    ## 3   0.0000       0.1679 ***
    ## 4   0.0000       0.5545 ***
    ## 5   0.0000      -0.1005 ***
    ## 6   0.0000      -0.3424 ***
    ## 7   0.2727       0.0680    
    ## 8   0.0000      -0.1904 ***
    ## 9   0.0000      -0.2264 ***
    ## 10  0.0000       0.4339 ***
    ## 11  0.0002      -0.0294 ***
    ## 12  0.5117      -0.0376    
    ## 13  0.0000       0.6329 ***
    ## 14  0.0624      -0.1661    
    ## 15  0.1283       0.0653    
    ## 16  0.0794      -0.1353    
    ## 17  0.0000      -0.2754 ***
    ## 18  0.0028       0.0866  **
    ## 19  0.0022      -0.1071  **
    ## 20  0.1607       0.0935    
    ## 21  0.0260      -0.1642   *
    ## 22  0.0201      -0.2181   *
    ## 23  0.1864       0.0780    
    ## 24  0.0077      -0.3955  **
    ## 25  0.1328      -0.0316    
    ## 26  0.4378       0.0200    
    ## 27  0.0001       0.2725 ***
    ## 28  0.5144       0.0528    
    ## 29  0.0190       0.2245   *
    ## 30  0.5024       0.0789    
    ## 31  0.1381      -0.2175    
    ## 32  0.1044      -0.1581    
    ## 33  0.0078       0.6128  **
    ## 34  0.8121       0.0099    
    ## 35  0.8748       0.0075    
    ## 36  0.0013       0.1236  **

## Lesion + Rich

``` r
sem_les_rich <- psem(
  lmer(Richness ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_large),
    lmer(BladeAreaLog ~ Richness + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_large),
  lmer(EpiphyteLog ~ BladeAreaLog + Richness + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Richness + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_large),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_rich)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_rich 
    ## 
    ## Call:
    ##   Richness ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Richness + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   EpiphyteLog ~ BladeAreaLog + Richness + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Richness + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  125.299   384.422
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 566.3929     0.8091  0.3688 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 566.4446     1.5459  0.2143 
    ##       Richness ~ TidalHeightBinary + ...      coef 564.3700     0.9541  0.3291 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 7.299 with P-value = 0.294 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##        Richness    TempAnomWarm_June   0.0842    0.0341 580.6171     6.0022
    ##        Richness MonthlyMeanTemp_June  -4.3257    0.4225 572.2835   102.7422
    ##        Richness         CanopyHeight  13.3775    1.2654 576.7738   110.2556
    ##        Richness           DensityLog   0.4173    1.0349 587.8288     0.1607
    ##        Richness           YearBinary   0.6545    0.2457 583.2505     7.0646
    ##    CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ##    CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ##    CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ##      DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ##      DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ##      DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ##    BladeAreaLog             Richness   0.0029    0.0031 235.8422     0.8170
    ##    BladeAreaLog         CanopyHeight   0.7438    0.0745  61.2604    85.6556
    ##    BladeAreaLog           DensityLog  -0.1410    0.0608  40.0240     4.2558
    ##    BladeAreaLog    TempAnomWarm_June   0.0039    0.0021 139.3903     3.1666
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0334    0.0157   9.4720     4.0048
    ##    BladeAreaLog    TidalHeightBinary  -0.2109    0.0171 574.7472   152.3465
    ##    BladeAreaLog           YearBinary   0.0695    0.0221 452.1796     9.5948
    ##     EpiphyteLog         BladeAreaLog  -0.2570    0.0791 581.4598    10.5043
    ##     EpiphyteLog             Richness   0.0102    0.0067 276.3115     2.1951
    ##     EpiphyteLog         CanopyHeight  -0.4262    0.1916  99.2905     4.6895
    ##     EpiphyteLog           DensityLog  -0.3400    0.1575 132.3405     4.3335
    ##     EpiphyteLog    TempAnomWarm_June   0.0126    0.0055 140.5811     4.7508
    ##     EpiphyteLog MonthlyMeanTemp_June  -0.2582    0.0655  45.3752    12.8861
    ##     EpiphyteLog    TidalHeightBinary  -0.0607    0.0365 572.8326     2.7582
    ##     EpiphyteLog           YearBinary   0.0271    0.0447 484.8036     0.3618
    ##   LesionAreaLog         BladeAreaLog   0.4940    0.1289 585.3276    14.5179
    ##   LesionAreaLog          EpiphyteLog   0.0384    0.0648 463.0509     0.3322
    ##   LesionAreaLog             Richness   0.0146    0.0096 183.5600     2.2008
    ##   LesionAreaLog         CanopyHeight   0.3637    0.2473  75.4542     2.0146
    ##   LesionAreaLog           DensityLog  -0.1040    0.1993  60.5962     0.2388
    ##   LesionAreaLog    TempAnomWarm_June   0.0015    0.0072  93.4937     0.0424
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1358    0.0664  12.2225     3.4572
    ##   LesionAreaLog    TidalHeightBinary   0.0056    0.0596 581.0095     0.0089
    ##   LesionAreaLog           YearBinary   0.0080     0.069 449.2039     0.0133
    ##    ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##   P.Value Std.Estimate    
    ##    0.0146       0.1405   *
    ##    0.0000      -1.3220 ***
    ##    0.0000       0.8493 ***
    ##    0.6886       0.0398    
    ##    0.0081       0.0646  **
    ##    0.0000      -0.3424 ***
    ##    0.2727       0.0680    
    ##    0.0000      -0.1904 ***
    ##    0.0000      -0.2264 ***
    ##    0.0000       0.4339 ***
    ##    0.0002      -0.0294 ***
    ##    0.3670       0.0380    
    ##    0.0000       0.6167 ***
    ##    0.0456      -0.1758   *
    ##    0.0773       0.0850    
    ##    0.0748      -0.1332    
    ##    0.0000      -0.2757 ***
    ##    0.0021       0.0896  **
    ##    0.0013      -0.1130  **
    ##    0.1396       0.0587    
    ##    0.0327      -0.1554   *
    ##    0.0393      -0.1864   *
    ##    0.0309       0.1208   *
    ##    0.0008      -0.4531 ***
    ##    0.0973      -0.0349    
    ##    0.5478       0.0153    
    ##    0.0002       0.2651 ***
    ##    0.5647       0.0468    
    ##    0.1397       0.1026    
    ##    0.1599       0.1618    
    ##    0.6268      -0.0696    
    ##    0.8372       0.0180    
    ##    0.0872       0.2909    
    ##    0.9251       0.0039    
    ##    0.9082       0.0056    
    ##    0.0013       0.1236  **
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Richness   none     0.14        0.99
    ##    CanopyHeight   none     0.07        0.98
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.59        0.71
    ##     EpiphyteLog   none     0.15        0.86
    ##   LesionAreaLog   none     0.14        0.53

Passes global fit test. Epifauna is sig and pos for Lesion Area
Standardized coefs:

``` r
coefs(sem_les_rich)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1       Richness    TempAnomWarm_June   0.0842    0.0341 580.6171     6.0022
    ## 2       Richness MonthlyMeanTemp_June  -4.3257    0.4225 572.2835   102.7422
    ## 3       Richness         CanopyHeight  13.3775    1.2654 576.7738   110.2556
    ## 4       Richness           DensityLog   0.4173    1.0349 587.8288     0.1607
    ## 5       Richness           YearBinary   0.6545    0.2457 583.2505     7.0646
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0130     9e-04 517.3243   218.4805
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0141    0.0126 469.3370     1.2062
    ## 8   CanopyHeight           YearBinary  -0.1226    0.0062 569.0245   393.8892
    ## 9     DensityLog    TempAnomWarm_June  -0.0129    0.0011 547.5740   141.1666
    ## 10    DensityLog MonthlyMeanTemp_June   0.1355    0.0157 520.0078    72.3305
    ## 11    DensityLog           YearBinary  -0.0284    0.0076 568.8301    13.9347
    ## 12  BladeAreaLog             Richness   0.0029    0.0031 235.8422     0.8170
    ## 13  BladeAreaLog         CanopyHeight   0.7438    0.0745  61.2604    85.6556
    ## 14  BladeAreaLog           DensityLog  -0.1410    0.0608  40.0240     4.2558
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0039    0.0021 139.3903     3.1666
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0334    0.0157   9.4720     4.0048
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2109    0.0171 574.7472   152.3465
    ## 18  BladeAreaLog           YearBinary   0.0695    0.0221 452.1796     9.5948
    ## 19   EpiphyteLog         BladeAreaLog  -0.2570    0.0791 581.4598    10.5043
    ## 20   EpiphyteLog             Richness   0.0102    0.0067 276.3115     2.1951
    ## 21   EpiphyteLog         CanopyHeight  -0.4262    0.1916  99.2905     4.6895
    ## 22   EpiphyteLog           DensityLog  -0.3400    0.1575 132.3405     4.3335
    ## 23   EpiphyteLog    TempAnomWarm_June   0.0126    0.0055 140.5811     4.7508
    ## 24   EpiphyteLog MonthlyMeanTemp_June  -0.2582    0.0655  45.3752    12.8861
    ## 25   EpiphyteLog    TidalHeightBinary  -0.0607    0.0365 572.8326     2.7582
    ## 26   EpiphyteLog           YearBinary   0.0271    0.0447 484.8036     0.3618
    ## 27 LesionAreaLog         BladeAreaLog   0.4940    0.1289 585.3276    14.5179
    ## 28 LesionAreaLog          EpiphyteLog   0.0384    0.0648 463.0509     0.3322
    ## 29 LesionAreaLog             Richness   0.0146    0.0096 183.5600     2.2008
    ## 30 LesionAreaLog         CanopyHeight   0.3637    0.2473  75.4542     2.0146
    ## 31 LesionAreaLog           DensityLog  -0.1040    0.1993  60.5962     0.2388
    ## 32 LesionAreaLog    TempAnomWarm_June   0.0015    0.0072  93.4937     0.0424
    ## 33 LesionAreaLog MonthlyMeanTemp_June   0.1358    0.0664  12.2225     3.4572
    ## 34 LesionAreaLog    TidalHeightBinary   0.0056    0.0596 581.0095     0.0089
    ## 35 LesionAreaLog           YearBinary   0.0080     0.069 449.2039     0.0133
    ## 36  ~~DensityLog       ~~CanopyHeight   0.1236         - 597.0000     3.0350
    ##    P.Value Std.Estimate    
    ## 1   0.0146       0.1405   *
    ## 2   0.0000      -1.3220 ***
    ## 3   0.0000       0.8493 ***
    ## 4   0.6886       0.0398    
    ## 5   0.0081       0.0646  **
    ## 6   0.0000      -0.3424 ***
    ## 7   0.2727       0.0680    
    ## 8   0.0000      -0.1904 ***
    ## 9   0.0000      -0.2264 ***
    ## 10  0.0000       0.4339 ***
    ## 11  0.0002      -0.0294 ***
    ## 12  0.3670       0.0380    
    ## 13  0.0000       0.6167 ***
    ## 14  0.0456      -0.1758   *
    ## 15  0.0773       0.0850    
    ## 16  0.0748      -0.1332    
    ## 17  0.0000      -0.2757 ***
    ## 18  0.0021       0.0896  **
    ## 19  0.0013      -0.1130  **
    ## 20  0.1396       0.0587    
    ## 21  0.0327      -0.1554   *
    ## 22  0.0393      -0.1864   *
    ## 23  0.0309       0.1208   *
    ## 24  0.0008      -0.4531 ***
    ## 25  0.0973      -0.0349    
    ## 26  0.5478       0.0153    
    ## 27  0.0002       0.2651 ***
    ## 28  0.5647       0.0468    
    ## 29  0.1397       0.1026    
    ## 30  0.1599       0.1618    
    ## 31  0.6268      -0.0696    
    ## 32  0.8372       0.0180    
    ## 33  0.0872       0.2909    
    ## 34  0.9251       0.0039    
    ## 35  0.9082       0.0056    
    ## 36  0.0013       0.1236  **

## Partial coefficients

### prevalence

``` r
prev_epi_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis_large,
        family="binomial")
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=T))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# will want to try and get the year/site means plotted onto this plot, but can hold off for now?
prev_lac_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis_large,
        family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.016154 (tol = 0.002, component 1)

``` r
plot(predictorEffect("Lacuna", prev_lac_1, partial.residuals=T))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
prev_amp_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis_large,
        family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0107381 (tol = 0.002, component 1)

``` r
plot(predictorEffect("Ampithoid", prev_amp_1,partial.residuals=TRUE))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
prev_ido_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis_large,
        family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0207277 (tol = 0.002, component 1)

``` r
plot(predictorEffect("Idoteid", prev_ido_1,partial.residuals=T))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
prev_rich_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Richness + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis_large,
        family="binomial")
plot(predictorEffect("Richness", prev_rich_1,partial.residuals=TRUE, main="type='link'"))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

### lesion

``` r
les_epi_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les_large)
plot(predictorEffect("Epifauna", les_epi_1, partial.residuals=TRUE))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
les_lac_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les_large)
plot(predictorEffect("Lacuna", les_lac_1, partial.residuals=TRUE))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
les_amp_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les_large)
plot(predictorEffect("Ampithoid", les_amp_1, partial.residuals=TRUE))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
les_ido_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les_large)
plot(predictorEffect("Idoteid", les_ido_1, partial.residuals=TRUE))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
les_rich_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Richness + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les_large)
plot(predictorEffect("Richness", les_rich_1, partial.residuals=T, type="rescale"))
```

![](TaxaInteractionSEM_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->

``` r
# plot(ggpredict(les_rich_1, terms = "Richness"), rawdata = TRUE, labels = scales::log10_trans())
```
