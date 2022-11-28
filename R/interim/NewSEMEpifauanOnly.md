Community Interaction SEM results
================

# SEMs comparing epifauna vs lacuna vs ampithoid

``` r
# data ###
dis <- read_csv("data/epiphyte_SEM_data.csv")
```

    ## Rows: 1350 Columns: 41
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): Meter, SampleId, Region, SiteCode, TidalHeight, GrazingScars, Bro...
    ## dbl  (31): Transect, Blade, LongestBladeLength, LongestBladeWidth, SheathLen...
    ## date  (1): SampleDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dis$BladeAreaLog <- log10(dis$BladeArea)
# use the full data set without subsetting because the SEM no longer includes epiphytes or grazing
dis1 <- select(dis, c(Epifauna, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, DensityLog, YearBinary, Year, Meadow, Region,
                      BladeAreaLog, TidalHeightBinary, Prevalence, LesionArea, Lacuna, Ampithoid))
```

## Prevalence + Epifauna

``` r
dis2 <- select(dis1, -c(Lacuna, Ampithoid))
dis2 <- na.omit(dis2)
sem_prev_epi <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=dis2),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis2),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis2),
    lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
  glmer(Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0115979 (tol = 0.002, component 1)

``` r
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
    ##   Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.002   324.289
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.003          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9997 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 1315.001          0  0.9996 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.002 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0008     0.001 1312.3270     0.5817
    ##       Epifauna MonthlyMeanTemp_June  -0.2312    0.0136 1294.2583   286.3904
    ##       Epifauna         CanopyHeight  -0.7317    0.0358 1336.7859   416.3266
    ##       Epifauna           DensityLog  -0.1237    0.0336 1340.0409    13.4913
    ##       Epifauna           YearBinary  -0.1217    0.0079 1321.9541   236.8469
    ##   CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1086.0640   103.0575
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0087  912.8932    42.7841
    ##   CanopyHeight           YearBinary  -0.1050    0.0051 1324.7492   419.2073
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.1216   819.5334
    ##     DensityLog MonthlyMeanTemp_June   0.2009    0.0095 1318.5233   441.8346
    ##     DensityLog           YearBinary  -0.0493    0.0055 1320.0539    81.4583
    ##   BladeAreaLog             Epifauna  -0.0716    0.0411   67.7684     2.8473
    ##   BladeAreaLog         CanopyHeight   0.6683    0.0658   76.6556    97.6408
    ##   BladeAreaLog           DensityLog  -0.1525    0.0557   98.5308     6.6716
    ##   BladeAreaLog    TempAnomWarm_June  -0.0028    0.0018   70.1305     2.3474
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0097    0.0172   14.6998     0.2583
    ##   BladeAreaLog    TidalHeightBinary  -0.2393    0.0133 1315.5925   325.7586
    ##   BladeAreaLog           YearBinary   0.0489    0.0186  781.6036     6.7513
    ##     Prevalence         BladeAreaLog   1.7020    0.2899 1348.0000     5.8711
    ##     Prevalence             Epifauna   1.2531    0.4193 1348.0000     2.9884
    ##     Prevalence         CanopyHeight   1.9777    0.7049 1348.0000     2.8058
    ##     Prevalence           DensityLog   2.6765    0.5727 1348.0000     4.6737
    ##     Prevalence    TempAnomWarm_June   0.0233    0.0194 1348.0000     1.1972
    ##     Prevalence MonthlyMeanTemp_June   0.4138    0.2327 1348.0000     1.7783
    ##     Prevalence    TidalHeightBinary   0.6159    0.1416 1348.0000     4.3484
    ##     Prevalence           YearBinary   0.4954    0.1796 1348.0000     2.7591
    ##   ~~DensityLog       ~~CanopyHeight  -0.0520         - 1348.0000    -1.9091
    ##   P.Value Std.Estimate    
    ##    0.4458      -0.0151    
    ##    0.0000      -0.8273 ***
    ##    0.0000      -0.5377 ***
    ##    0.0002      -0.1288 ***
    ##    0.0000      -0.1292 ***
    ##    0.0000      -0.1577 ***
    ##    0.0000      -0.2825 ***
    ##    0.0000      -0.1517 ***
    ##    0.0000       -0.337 ***
    ##    0.0000       0.6902 ***
    ##    0.0000      -0.0502 ***
    ##    0.0961      -0.0792    
    ##    0.0000       0.5429 ***
    ##    0.0113      -0.1756   *
    ##    0.1300       -0.059    
    ##    0.6189      -0.0382    
    ##    0.0000      -0.2826 ***
    ##    0.0095       0.0573  **
    ##    0.0000            - ***
    ##    0.0028            -  **
    ##    0.0050            -  **
    ##    0.0000            - ***
    ##    0.2312            -    
    ##    0.0754            -    
    ##    0.0000            - ***
    ##    0.0058            -  **
    ##    0.0282       -0.052   *
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.20        0.99
    ##   CanopyHeight   none     0.14        0.96
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.53        0.68
    ##     Prevalence  delta     0.15        0.50

Model passes global fit. Epifauna is positive and significant predictor
for Prevalence.

``` r
coefs(sem_prev_epi)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0008     0.001 1312.3270     0.5817
    ## 2      Epifauna MonthlyMeanTemp_June  -0.2312    0.0136 1294.2583   286.3904
    ## 3      Epifauna         CanopyHeight  -0.7317    0.0358 1336.7859   416.3266
    ## 4      Epifauna           DensityLog  -0.1237    0.0336 1340.0409    13.4913
    ## 5      Epifauna           YearBinary  -0.1217    0.0079 1321.9541   236.8469
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0061     6e-04 1086.0640   103.0575
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0580    0.0087  912.8932    42.7841
    ## 8  CanopyHeight           YearBinary  -0.1050    0.0051 1324.7492   419.2073
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.1216   819.5334
    ## 10   DensityLog MonthlyMeanTemp_June   0.2009    0.0095 1318.5233   441.8346
    ## 11   DensityLog           YearBinary  -0.0493    0.0055 1320.0539    81.4583
    ## 12 BladeAreaLog             Epifauna  -0.0716    0.0411   67.7684     2.8473
    ## 13 BladeAreaLog         CanopyHeight   0.6683    0.0658   76.6556    97.6408
    ## 14 BladeAreaLog           DensityLog  -0.1525    0.0557   98.5308     6.6716
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0028    0.0018   70.1305     2.3474
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0097    0.0172   14.6998     0.2583
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2393    0.0133 1315.5925   325.7586
    ## 18 BladeAreaLog           YearBinary   0.0489    0.0186  781.6036     6.7513
    ## 19   Prevalence         BladeAreaLog   1.7020    0.2899 1348.0000     5.8711
    ## 20   Prevalence             Epifauna   1.2531    0.4193 1348.0000     2.9884
    ## 21   Prevalence         CanopyHeight   1.9777    0.7049 1348.0000     2.8058
    ## 22   Prevalence           DensityLog   2.6765    0.5727 1348.0000     4.6737
    ## 23   Prevalence    TempAnomWarm_June   0.0233    0.0194 1348.0000     1.1972
    ## 24   Prevalence MonthlyMeanTemp_June   0.4138    0.2327 1348.0000     1.7783
    ## 25   Prevalence    TidalHeightBinary   0.6159    0.1416 1348.0000     4.3484
    ## 26   Prevalence           YearBinary   0.4954    0.1796 1348.0000     2.7591
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0520         - 1348.0000    -1.9091
    ##    P.Value Std.Estimate    
    ## 1   0.4458      -0.0151    
    ## 2   0.0000      -0.8273 ***
    ## 3   0.0000      -0.5377 ***
    ## 4   0.0002      -0.1288 ***
    ## 5   0.0000      -0.1292 ***
    ## 6   0.0000      -0.1577 ***
    ## 7   0.0000      -0.2825 ***
    ## 8   0.0000      -0.1517 ***
    ## 9   0.0000      -0.3370 ***
    ## 10  0.0000       0.6902 ***
    ## 11  0.0000      -0.0502 ***
    ## 12  0.0961      -0.0792    
    ## 13  0.0000       0.5429 ***
    ## 14  0.0113      -0.1756   *
    ## 15  0.1300      -0.0590    
    ## 16  0.6189      -0.0382    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0095       0.0573  **
    ## 19  0.0000       0.3189 ***
    ## 20  0.0028       0.2596  **
    ## 21  0.0050       0.3010  **
    ## 22  0.0000       0.5774 ***
    ## 23  0.2312       0.0908    
    ## 24  0.0754       0.3067    
    ## 25  0.0000       0.1363 ***
    ## 26  0.0058       0.1089  **
    ## 27  0.0282      -0.0520   *

## prev + lac

``` r
dis3 <- select(dis1, -c(Epifauna, Ampithoid))
dis3 <- dis3[-which(is.na(dis3$Lacuna)),] # omit missing Lacuna values
dis3 <- na.omit(dis3) # remove any other NAs (missing blade area for one)

sem_prev_lac <- psem(
  lmer(Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis3),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis3),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis3),
  lmer(BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis3),
  glmer(Prevalence ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis3,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
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
    ##   Prevalence ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.002   320.13
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1202.000          0  0.9999 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1202.000          0  0.9997 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 1200.002          0  0.9994 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.002 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##         Lacuna    TempAnomWarm_June   0.0057    0.0018 1219.2758    10.2331
    ##         Lacuna MonthlyMeanTemp_June   0.0280     0.023 1222.4586     1.4656
    ##         Lacuna         CanopyHeight   0.0450    0.0604 1222.1436     0.5499
    ##         Lacuna           DensityLog  -0.2039    0.0564 1213.1028    12.8914
    ##         Lacuna           YearBinary  -0.1649    0.0134 1206.3208   152.0434
    ##   CanopyHeight    TempAnomWarm_June  -0.0062     6e-04 1210.2163    94.4785
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0565    0.0094 1215.3719    36.1283
    ##   CanopyHeight           YearBinary  -0.1049    0.0054 1203.7702   381.0934
    ##     DensityLog    TempAnomWarm_June  -0.0187     7e-04 1207.3574   755.3749
    ##     DensityLog MonthlyMeanTemp_June   0.2028      0.01 1210.8641   409.8036
    ##     DensityLog           YearBinary  -0.0490    0.0057 1203.4361    73.5206
    ##   BladeAreaLog               Lacuna  -0.0581    0.0264   75.4723     4.4329
    ##   BladeAreaLog         CanopyHeight   0.7014    0.0626  113.4589   115.7070
    ##   BladeAreaLog           DensityLog  -0.1686    0.0478   63.8432    10.9511
    ##   BladeAreaLog    TempAnomWarm_June  -0.0050    0.0021  335.5089     5.7756
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0432    0.0242  153.1222     2.8935
    ##   BladeAreaLog    TidalHeightBinary  -0.2535    0.0141 1200.5388   325.2617
    ##   BladeAreaLog           YearBinary   0.0528    0.0196  927.8572     7.1184
    ##     Prevalence         BladeAreaLog   1.7476    0.3043 1229.0000     5.7427
    ##     Prevalence               Lacuna   1.0734     0.284 1229.0000     3.7800
    ##     Prevalence         CanopyHeight   0.8478    0.6754 1229.0000     1.2553
    ##     Prevalence           DensityLog   1.8593    0.5156 1229.0000     3.6062
    ##     Prevalence    TempAnomWarm_June  -0.0234    0.0211 1229.0000    -1.1101
    ##     Prevalence MonthlyMeanTemp_June   0.6679    0.2534 1229.0000     2.6356
    ##     Prevalence    TidalHeightBinary   0.6776    0.1515 1229.0000     4.4719
    ##     Prevalence           YearBinary   0.5018    0.1849 1229.0000     2.7145
    ##   ~~DensityLog       ~~CanopyHeight  -0.0520         - 1229.0000    -1.8242
    ##   P.Value Std.Estimate    
    ##    0.0014       0.0815  **
    ##    0.2263       0.0375    
    ##    0.4585       0.0243    
    ##    0.0003      -0.1683 ***
    ##    0.0000       -0.133 ***
    ##    0.0000      -0.1658 ***
    ##    0.0000      -0.1406 ***
    ##    0.0000      -0.1569 ***
    ##    0.0000      -0.3262 ***
    ##    0.0000       0.3296 ***
    ##    0.0000      -0.0479 ***
    ##    0.0386      -0.0866   *
    ##    0.0000       0.5644 ***
    ##    0.0015      -0.2075  **
    ##    0.0168      -0.1082   *
    ##    0.0910       0.0863    
    ##    0.0000      -0.3074 ***
    ##    0.0078       0.0635  **
    ##    0.0000            - ***
    ##    0.0002            - ***
    ##    0.2094            -    
    ##    0.0003            - ***
    ##    0.2670            -    
    ##    0.0084            -  **
    ##    0.0000            - ***
    ##    0.0066            -  **
    ##    0.0342       -0.052   *
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##         Lacuna   none     0.04        0.95
    ##   CanopyHeight   none     0.10        0.95
    ##     DensityLog   none     0.08        0.97
    ##   BladeAreaLog   none     0.59        0.66
    ##     Prevalence  delta     0.11        0.45

Passes global fit test Lacuna effect on Prevalence is sig and positive

``` r
coefs(sem_prev_lac)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1        Lacuna    TempAnomWarm_June   0.0057    0.0018 1219.2758    10.2331
    ## 2        Lacuna MonthlyMeanTemp_June   0.0280     0.023 1222.4586     1.4656
    ## 3        Lacuna         CanopyHeight   0.0450    0.0604 1222.1436     0.5499
    ## 4        Lacuna           DensityLog  -0.2039    0.0564 1213.1028    12.8914
    ## 5        Lacuna           YearBinary  -0.1649    0.0134 1206.3208   152.0434
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0062     6e-04 1210.2163    94.4785
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0565    0.0094 1215.3719    36.1283
    ## 8  CanopyHeight           YearBinary  -0.1049    0.0054 1203.7702   381.0934
    ## 9    DensityLog    TempAnomWarm_June  -0.0187     7e-04 1207.3574   755.3749
    ## 10   DensityLog MonthlyMeanTemp_June   0.2028      0.01 1210.8641   409.8036
    ## 11   DensityLog           YearBinary  -0.0490    0.0057 1203.4361    73.5206
    ## 12 BladeAreaLog               Lacuna  -0.0581    0.0264   75.4723     4.4329
    ## 13 BladeAreaLog         CanopyHeight   0.7014    0.0626  113.4589   115.7070
    ## 14 BladeAreaLog           DensityLog  -0.1686    0.0478   63.8432    10.9511
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0050    0.0021  335.5089     5.7756
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0432    0.0242  153.1222     2.8935
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2535    0.0141 1200.5388   325.2617
    ## 18 BladeAreaLog           YearBinary   0.0528    0.0196  927.8572     7.1184
    ## 19   Prevalence         BladeAreaLog   1.7476    0.3043 1229.0000     5.7427
    ## 20   Prevalence               Lacuna   1.0734     0.284 1229.0000     3.7800
    ## 21   Prevalence         CanopyHeight   0.8478    0.6754 1229.0000     1.2553
    ## 22   Prevalence           DensityLog   1.8593    0.5156 1229.0000     3.6062
    ## 23   Prevalence    TempAnomWarm_June  -0.0234    0.0211 1229.0000    -1.1101
    ## 24   Prevalence MonthlyMeanTemp_June   0.6679    0.2534 1229.0000     2.6356
    ## 25   Prevalence    TidalHeightBinary   0.6776    0.1515 1229.0000     4.4719
    ## 26   Prevalence           YearBinary   0.5018    0.1849 1229.0000     2.7145
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0520         - 1229.0000    -1.8242
    ##    P.Value Std.Estimate    
    ## 1   0.0014       0.0815  **
    ## 2   0.2263       0.0375    
    ## 3   0.4585       0.0243    
    ## 4   0.0003      -0.1683 ***
    ## 5   0.0000      -0.1330 ***
    ## 6   0.0000      -0.1658 ***
    ## 7   0.0000      -0.1406 ***
    ## 8   0.0000      -0.1569 ***
    ## 9   0.0000      -0.3262 ***
    ## 10  0.0000       0.3296 ***
    ## 11  0.0000      -0.0479 ***
    ## 12  0.0386      -0.0866   *
    ## 13  0.0000       0.5644 ***
    ## 14  0.0015      -0.2075  **
    ## 15  0.0168      -0.1082   *
    ## 16  0.0910       0.0863    
    ## 17  0.0000      -0.3074 ***
    ## 18  0.0078       0.0635  **
    ## 19  0.0000       0.3161 ***
    ## 20  0.0002       0.2896 ***
    ## 21  0.2094       0.1234    
    ## 22  0.0003       0.4141 ***
    ## 23  0.2670      -0.0908    
    ## 24  0.0084       0.2417  **
    ## 25  0.0000       0.1486 ***
    ## 26  0.0066       0.1093  **
    ## 27  0.0342      -0.0520   *

## prev + amp

``` r
dis4 <- select(dis1, -c(Epifauna, Lacuna))
dis4 <- dis4[-which(is.na(dis4$Ampithoid)),] # omit missing Ampithoid values
dis4 <- na.omit(dis4) # remove any other NAs (missing blade area for one)

sem_prev_amp <- psem(
  lmer(Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis4),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis4),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis4),
  lmer(BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis4),
  glmer(Prevalence ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis4,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00467413 (tol = 0.002, component 1)

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
    ##   Prevalence ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.001   320.092
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1198.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1198.000          0  0.9998 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 1196.007          0  0.9999 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.001 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##      Ampithoid    TempAnomWarm_June  -0.0033    0.0012  982.5863     6.8676
    ##      Ampithoid MonthlyMeanTemp_June  -0.0957    0.0151  888.4719    38.9145
    ##      Ampithoid         CanopyHeight  -1.2152    0.0371 1218.5197  1069.2525
    ##      Ampithoid           DensityLog  -0.4611    0.0468 1107.7121    94.8639
    ##      Ampithoid           YearBinary  -0.3059    0.0104 1214.0844   863.1626
    ##   CanopyHeight    TempAnomWarm_June  -0.0048     7e-04  989.8316    45.8644
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0633    0.0092  830.0430    46.3315
    ##   CanopyHeight           YearBinary  -0.1229    0.0066 1206.9242   342.1794
    ##     DensityLog    TempAnomWarm_June  -0.0178     5e-04 1206.1373  1044.0810
    ##     DensityLog MonthlyMeanTemp_June   0.2029    0.0073 1207.3382   769.3327
    ##     DensityLog           YearBinary  -0.0674    0.0052 1201.3623   170.8185
    ##   BladeAreaLog            Ampithoid   0.0098    0.0478   92.9600     0.0383
    ##   BladeAreaLog         CanopyHeight   0.7004    0.0817   75.3802    65.2143
    ##   BladeAreaLog           DensityLog  -0.1535    0.0616   50.1031     5.2914
    ##   BladeAreaLog    TempAnomWarm_June  -0.0006    0.0017   53.7581     0.1107
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0103    0.0165   12.0127     0.3252
    ##   BladeAreaLog    TidalHeightBinary  -0.2566    0.0137 1196.6691   353.3991
    ##   BladeAreaLog           YearBinary   0.0290    0.0251  222.0879     1.2503
    ##     Prevalence         BladeAreaLog   1.8418    0.3171 1228.0000     5.8082
    ##     Prevalence            Ampithoid  -1.2583    0.5524 1228.0000    -2.2778
    ##     Prevalence         CanopyHeight  -0.2154    0.9884 1228.0000    -0.2180
    ##     Prevalence           DensityLog   2.1282    0.7965 1228.0000     2.6721
    ##     Prevalence    TempAnomWarm_June  -0.0060    0.0182 1228.0000    -0.3272
    ##     Prevalence MonthlyMeanTemp_June   0.2617    0.1861 1228.0000     1.4057
    ##     Prevalence    TidalHeightBinary   0.5336    0.1539 1228.0000     3.4676
    ##     Prevalence           YearBinary   0.2305    0.2932 1228.0000     0.7861
    ##   ~~DensityLog       ~~CanopyHeight  -0.1390         - 1228.0000    -4.9119
    ##   P.Value Std.Estimate    
    ##    0.0089      -0.0762  **
    ##    0.0000      -0.4254 ***
    ##    0.0000      -1.1171 ***
    ##    0.0000      -0.6076 ***
    ##    0.0000      -0.3946 ***
    ##    0.0000      -0.1188 ***
    ##    0.0000      -0.3061 ***
    ##    0.0000      -0.1724 ***
    ##    0.0000      -0.3097 ***
    ##    0.0000       0.6841 ***
    ##    0.0000       -0.066 ***
    ##    0.8452       0.0086    
    ##    0.0000       0.5667 ***
    ##    0.0256      -0.1781   *
    ##    0.7406      -0.0124    
    ##    0.5790      -0.0404    
    ##    0.0000      -0.2958 ***
    ##    0.2647        0.033    
    ##    0.0000            - ***
    ##    0.0227            -   *
    ##    0.8274            -    
    ##    0.0075            -  **
    ##    0.7435            -    
    ##    0.1598            -    
    ##    0.0005            - ***
    ##    0.4318            -    
    ##    0.0000       -0.139 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##      Ampithoid   none     0.30        0.98
    ##   CanopyHeight   none     0.16        0.96
    ##     DensityLog   none     0.22        0.99
    ##   BladeAreaLog   none     0.55        0.68
    ##     Prevalence  delta     0.14        0.49

Passes global fit test. Ampithoid is negative for prevalence

``` r
coefs(sem_prev_amp)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1     Ampithoid    TempAnomWarm_June  -0.0033    0.0012  982.5863     6.8676
    ## 2     Ampithoid MonthlyMeanTemp_June  -0.0957    0.0151  888.4719    38.9145
    ## 3     Ampithoid         CanopyHeight  -1.2152    0.0371 1218.5197  1069.2525
    ## 4     Ampithoid           DensityLog  -0.4611    0.0468 1107.7121    94.8639
    ## 5     Ampithoid           YearBinary  -0.3059    0.0104 1214.0844   863.1626
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0048     7e-04  989.8316    45.8644
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0633    0.0092  830.0430    46.3315
    ## 8  CanopyHeight           YearBinary  -0.1229    0.0066 1206.9242   342.1794
    ## 9    DensityLog    TempAnomWarm_June  -0.0178     5e-04 1206.1373  1044.0810
    ## 10   DensityLog MonthlyMeanTemp_June   0.2029    0.0073 1207.3382   769.3327
    ## 11   DensityLog           YearBinary  -0.0674    0.0052 1201.3623   170.8185
    ## 12 BladeAreaLog            Ampithoid   0.0098    0.0478   92.9600     0.0383
    ## 13 BladeAreaLog         CanopyHeight   0.7004    0.0817   75.3802    65.2143
    ## 14 BladeAreaLog           DensityLog  -0.1535    0.0616   50.1031     5.2914
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0006    0.0017   53.7581     0.1107
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0103    0.0165   12.0127     0.3252
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2566    0.0137 1196.6691   353.3991
    ## 18 BladeAreaLog           YearBinary   0.0290    0.0251  222.0879     1.2503
    ## 19   Prevalence         BladeAreaLog   1.8418    0.3171 1228.0000     5.8082
    ## 20   Prevalence            Ampithoid  -1.2583    0.5524 1228.0000    -2.2778
    ## 21   Prevalence         CanopyHeight  -0.2154    0.9884 1228.0000    -0.2180
    ## 22   Prevalence           DensityLog   2.1282    0.7965 1228.0000     2.6721
    ## 23   Prevalence    TempAnomWarm_June  -0.0060    0.0182 1228.0000    -0.3272
    ## 24   Prevalence MonthlyMeanTemp_June   0.2617    0.1861 1228.0000     1.4057
    ## 25   Prevalence    TidalHeightBinary   0.5336    0.1539 1228.0000     3.4676
    ## 26   Prevalence           YearBinary   0.2305    0.2932 1228.0000     0.7861
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.1390         - 1228.0000    -4.9119
    ##    P.Value Std.Estimate    
    ## 1   0.0089      -0.0762  **
    ## 2   0.0000      -0.4254 ***
    ## 3   0.0000      -1.1171 ***
    ## 4   0.0000      -0.6076 ***
    ## 5   0.0000      -0.3946 ***
    ## 6   0.0000      -0.1188 ***
    ## 7   0.0000      -0.3061 ***
    ## 8   0.0000      -0.1724 ***
    ## 9   0.0000      -0.3097 ***
    ## 10  0.0000       0.6841 ***
    ## 11  0.0000      -0.0660 ***
    ## 12  0.8452       0.0086    
    ## 13  0.0000       0.5667 ***
    ## 14  0.0256      -0.1781   *
    ## 15  0.7406      -0.0124    
    ## 16  0.5790      -0.0404    
    ## 17  0.0000      -0.2958 ***
    ## 18  0.2647       0.0330    
    ## 19  0.0000       0.3449 ***
    ## 20  0.0227      -0.2074   *
    ## 21  0.8274      -0.0326    
    ## 22  0.0075       0.4622  **
    ## 23  0.7435      -0.0226    
    ## 24  0.1598       0.1916    
    ## 25  0.0005       0.1152 ***
    ## 26  0.4318       0.0490    
    ## 27  0.0000      -0.1390 ***

## Limit to complete cases (sites with lacuna and ampithoids)

### epifauna

``` r
dis_cc <- na.omit(dis1) # remove missing Epifauna, Lacuna, and Ampithoid values, plus any other NAs (missing blade area for one)

sem_prev_epi_cc <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_cc),
    lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_cc),
  glmer(Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_cc,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
summary(sem_prev_epi_cc)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_epi_cc 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.002   315.507
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1083.000          0  0.9999 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1083.000          0  0.9998 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 1081.005          0  0.9995 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.002 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0076    0.0013 1062.6810    32.6466
    ##       Epifauna MonthlyMeanTemp_June  -0.1671    0.0159 1040.2012   107.5675
    ##       Epifauna         CanopyHeight  -0.7580    0.0386 1086.4620   381.3828
    ##       Epifauna           DensityLog  -0.4125    0.0485  785.6579    69.1221
    ##       Epifauna           YearBinary  -0.1263    0.0108 1089.8760   135.7832
    ##   CanopyHeight    TempAnomWarm_June  -0.0048     7e-04 1090.7377    41.4886
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0623    0.0099 1095.4087    39.4932
    ##   CanopyHeight           YearBinary  -0.1228     0.007 1086.2998   307.8459
    ##     DensityLog    TempAnomWarm_June  -0.0178     6e-04 1086.1680   950.3157
    ##     DensityLog MonthlyMeanTemp_June   0.2040    0.0077 1087.8941   702.0699
    ##     DensityLog           YearBinary  -0.0672    0.0054 1084.7054   153.2339
    ##   BladeAreaLog             Epifauna  -0.1200    0.0395   43.1083     8.4145
    ##   BladeAreaLog         CanopyHeight   0.5998    0.0702   62.8515    65.3199
    ##   BladeAreaLog           DensityLog  -0.2670    0.0575   40.2838    18.4454
    ##   BladeAreaLog    TempAnomWarm_June  -0.0064    0.0023  235.5936     7.1245
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0399    0.0239  106.4089     2.5848
    ##   BladeAreaLog    TidalHeightBinary  -0.2742    0.0145 1081.7518   358.5008
    ##   BladeAreaLog           YearBinary   0.0218    0.0229  442.4917     0.8610
    ##     Prevalence         BladeAreaLog   1.9426     0.339 1109.0000     5.7303
    ##     Prevalence             Epifauna   1.7095    0.5067 1109.0000     3.3737
    ##     Prevalence         CanopyHeight   2.3123    0.9042 1109.0000     2.5572
    ##     Prevalence           DensityLog   3.4744    0.8754 1109.0000     3.9691
    ##     Prevalence    TempAnomWarm_June  -0.0104    0.0261 1109.0000    -0.3978
    ##     Prevalence MonthlyMeanTemp_June   0.8761    0.2889 1109.0000     3.0323
    ##     Prevalence    TidalHeightBinary   0.5930    0.1662 1109.0000     3.5674
    ##     Prevalence           YearBinary   0.8934    0.2503 1109.0000     3.5690
    ##   ~~DensityLog       ~~CanopyHeight  -0.1390         - 1109.0000    -4.6680
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.1468 ***
    ##    0.0000      -0.2932 ***
    ##    0.0000      -0.5627 ***
    ##    0.0000      -0.4802 ***
    ##    0.0000      -0.1374 ***
    ##    0.0000      -0.1254 ***
    ##    0.0000      -0.1473 ***
    ##    0.0000      -0.1799 ***
    ##    0.0000      -0.2969 ***
    ##    0.0000       0.3075 ***
    ##    0.0000      -0.0628 ***
    ##    0.0058      -0.1283  **
    ##    0.0000       0.4761 ***
    ##    0.0001      -0.3324 ***
    ##    0.0081      -0.1322  **
    ##    0.1109       0.0748    
    ##    0.0000      -0.3247 ***
    ##    0.3540       0.0253    
    ##    0.0000            - ***
    ##    0.0007            - ***
    ##    0.0106            -   *
    ##    0.0001            - ***
    ##    0.6908            -    
    ##    0.0024            -  **
    ##    0.0004            - ***
    ##    0.0004            - ***
    ##    0.0000       -0.139 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.36        0.95
    ##   CanopyHeight   none     0.09        0.95
    ##     DensityLog   none     0.09        0.99
    ##   BladeAreaLog   none     0.61        0.70
    ##     Prevalence  delta     0.22        0.61

Passes global fit test. Epifauna is positive and significant for
prevalence.

Standardized coefs of sem:

``` r
coefs(sem_prev_epi_cc)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0076    0.0013 1062.6810    32.6466
    ## 2      Epifauna MonthlyMeanTemp_June  -0.1671    0.0159 1040.2012   107.5675
    ## 3      Epifauna         CanopyHeight  -0.7580    0.0386 1086.4620   381.3828
    ## 4      Epifauna           DensityLog  -0.4125    0.0485  785.6579    69.1221
    ## 5      Epifauna           YearBinary  -0.1263    0.0108 1089.8760   135.7832
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0048     7e-04 1090.7377    41.4886
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0623    0.0099 1095.4087    39.4932
    ## 8  CanopyHeight           YearBinary  -0.1228     0.007 1086.2998   307.8459
    ## 9    DensityLog    TempAnomWarm_June  -0.0178     6e-04 1086.1680   950.3157
    ## 10   DensityLog MonthlyMeanTemp_June   0.2040    0.0077 1087.8941   702.0699
    ## 11   DensityLog           YearBinary  -0.0672    0.0054 1084.7054   153.2339
    ## 12 BladeAreaLog             Epifauna  -0.1200    0.0395   43.1083     8.4145
    ## 13 BladeAreaLog         CanopyHeight   0.5998    0.0702   62.8515    65.3199
    ## 14 BladeAreaLog           DensityLog  -0.2670    0.0575   40.2838    18.4454
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0064    0.0023  235.5936     7.1245
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0399    0.0239  106.4089     2.5848
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2742    0.0145 1081.7518   358.5008
    ## 18 BladeAreaLog           YearBinary   0.0218    0.0229  442.4917     0.8610
    ## 19   Prevalence         BladeAreaLog   1.9426     0.339 1109.0000     5.7303
    ## 20   Prevalence             Epifauna   1.7095    0.5067 1109.0000     3.3737
    ## 21   Prevalence         CanopyHeight   2.3123    0.9042 1109.0000     2.5572
    ## 22   Prevalence           DensityLog   3.4744    0.8754 1109.0000     3.9691
    ## 23   Prevalence    TempAnomWarm_June  -0.0104    0.0261 1109.0000    -0.3978
    ## 24   Prevalence MonthlyMeanTemp_June   0.8761    0.2889 1109.0000     3.0323
    ## 25   Prevalence    TidalHeightBinary   0.5930    0.1662 1109.0000     3.5674
    ## 26   Prevalence           YearBinary   0.8934    0.2503 1109.0000     3.5690
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.1390         - 1109.0000    -4.6680
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.1468 ***
    ## 2   0.0000      -0.2932 ***
    ## 3   0.0000      -0.5627 ***
    ## 4   0.0000      -0.4802 ***
    ## 5   0.0000      -0.1374 ***
    ## 6   0.0000      -0.1254 ***
    ## 7   0.0000      -0.1473 ***
    ## 8   0.0000      -0.1799 ***
    ## 9   0.0000      -0.2969 ***
    ## 10  0.0000       0.3075 ***
    ## 11  0.0000      -0.0628 ***
    ## 12  0.0058      -0.1283  **
    ## 13  0.0000       0.4761 ***
    ## 14  0.0001      -0.3324 ***
    ## 15  0.0081      -0.1322  **
    ## 16  0.1109       0.0748    
    ## 17  0.0000      -0.3247 ***
    ## 18  0.3540       0.0253    
    ## 19  0.0000       0.3423 ***
    ## 20  0.0007       0.3220 ***
    ## 21  0.0106       0.3234   *
    ## 22  0.0001       0.7618 ***
    ## 23  0.6908      -0.0379    
    ## 24  0.0024       0.2895  **
    ## 25  0.0004       0.1238 ***
    ## 26  0.0004       0.1831 ***
    ## 27  0.0000      -0.1390 ***

### lacuna

``` r
sem_prev_lac_cc <- psem(
  lmer(Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_cc),
  glmer(Prevalence ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_cc,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00375867 (tol = 0.002, component 1)

``` r
summary(sem_prev_lac_cc)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_lac_cc 
    ## 
    ## Call:
    ##   Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.002   315.507
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1083.000          0  0.9999 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1083.000          0  0.9998 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 1081.004          0  0.9995 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.002 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##         Lacuna    TempAnomWarm_June   0.0023    0.0021 1101.2610     1.2045
    ##         Lacuna MonthlyMeanTemp_June   0.0838    0.0254 1097.1570    10.7964
    ##         Lacuna         CanopyHeight  -0.0219    0.0613 1100.7360     0.1267
    ##         Lacuna           DensityLog  -0.6131    0.0776 1040.0115    61.1352
    ##         Lacuna           YearBinary  -0.2572    0.0172 1093.2541   224.2331
    ##   CanopyHeight    TempAnomWarm_June  -0.0048     7e-04 1090.7377    41.4886
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0623    0.0099 1095.4087    39.4932
    ##   CanopyHeight           YearBinary  -0.1228     0.007 1086.2998   307.8459
    ##     DensityLog    TempAnomWarm_June  -0.0178     6e-04 1086.1680   950.3157
    ##     DensityLog MonthlyMeanTemp_June   0.2040    0.0077 1087.8941   702.0699
    ##     DensityLog           YearBinary  -0.0672    0.0054 1084.7054   153.2339
    ##   BladeAreaLog               Lacuna  -0.0765    0.0283   83.0066     6.6062
    ##   BladeAreaLog         CanopyHeight   0.6430    0.0649  100.9065    89.0493
    ##   BladeAreaLog           DensityLog  -0.2228    0.0519   39.2815    15.8506
    ##   BladeAreaLog    TempAnomWarm_June  -0.0045    0.0022  243.5208     3.9317
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0480    0.0243  127.3887     3.5123
    ##   BladeAreaLog    TidalHeightBinary  -0.2741    0.0145 1081.6686   357.5668
    ##   BladeAreaLog           YearBinary   0.0141    0.0243  564.4479     0.3209
    ##     Prevalence         BladeAreaLog   1.9264    0.3375 1109.0000     5.7070
    ##     Prevalence               Lacuna   1.3264    0.3343 1109.0000     3.9676
    ##     Prevalence         CanopyHeight   1.2511    0.7436 1109.0000     1.6825
    ##     Prevalence           DensityLog   2.5054    0.6961 1109.0000     3.5992
    ##     Prevalence    TempAnomWarm_June  -0.0427    0.0245 1109.0000    -1.7410
    ##     Prevalence MonthlyMeanTemp_June   0.6968    0.2693 1109.0000     2.5875
    ##     Prevalence    TidalHeightBinary   0.5918    0.1662 1109.0000     3.5613
    ##     Prevalence           YearBinary   1.0039    0.2581 1109.0000     3.8904
    ##   ~~DensityLog       ~~CanopyHeight  -0.1390         - 1109.0000    -4.6680
    ##   P.Value Std.Estimate    
    ##    0.2727       0.0329    
    ##    0.0010       0.1082  **
    ##    0.7220       -0.012    
    ##    0.0000      -0.5252 ***
    ##    0.0000       -0.206 ***
    ##    0.0000      -0.1254 ***
    ##    0.0000      -0.1473 ***
    ##    0.0000      -0.1799 ***
    ##    0.0000      -0.2969 ***
    ##    0.0000       0.3075 ***
    ##    0.0000      -0.0628 ***
    ##    0.0119      -0.1111   *
    ##    0.0000       0.5105 ***
    ##    0.0003      -0.2773 ***
    ##    0.0485       -0.093   *
    ##    0.0632         0.09    
    ##    0.0000      -0.3247 ***
    ##    0.5713       0.0164    
    ##    0.0000            - ***
    ##    0.0001            - ***
    ##    0.0925            -    
    ##    0.0003            - ***
    ##    0.0817            -    
    ##    0.0097            -  **
    ##    0.0004            - ***
    ##    0.0001            - ***
    ##    0.0000       -0.139 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##         Lacuna   none     0.17        0.96
    ##   CanopyHeight   none     0.09        0.95
    ##     DensityLog   none     0.09        0.99
    ##   BladeAreaLog   none     0.61        0.68
    ##     Prevalence  delta     0.16        0.52

Passes global fit test. Lacuna is positive and sig for prevalence.

``` r
coefs(sem_prev_lac_cc)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1        Lacuna    TempAnomWarm_June   0.0023    0.0021 1101.2610     1.2045
    ## 2        Lacuna MonthlyMeanTemp_June   0.0838    0.0254 1097.1570    10.7964
    ## 3        Lacuna         CanopyHeight  -0.0219    0.0613 1100.7360     0.1267
    ## 4        Lacuna           DensityLog  -0.6131    0.0776 1040.0115    61.1352
    ## 5        Lacuna           YearBinary  -0.2572    0.0172 1093.2541   224.2331
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0048     7e-04 1090.7377    41.4886
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0623    0.0099 1095.4087    39.4932
    ## 8  CanopyHeight           YearBinary  -0.1228     0.007 1086.2998   307.8459
    ## 9    DensityLog    TempAnomWarm_June  -0.0178     6e-04 1086.1680   950.3157
    ## 10   DensityLog MonthlyMeanTemp_June   0.2040    0.0077 1087.8941   702.0699
    ## 11   DensityLog           YearBinary  -0.0672    0.0054 1084.7054   153.2339
    ## 12 BladeAreaLog               Lacuna  -0.0765    0.0283   83.0066     6.6062
    ## 13 BladeAreaLog         CanopyHeight   0.6430    0.0649  100.9065    89.0493
    ## 14 BladeAreaLog           DensityLog  -0.2228    0.0519   39.2815    15.8506
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0045    0.0022  243.5208     3.9317
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0480    0.0243  127.3887     3.5123
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2741    0.0145 1081.6686   357.5668
    ## 18 BladeAreaLog           YearBinary   0.0141    0.0243  564.4479     0.3209
    ## 19   Prevalence         BladeAreaLog   1.9264    0.3375 1109.0000     5.7070
    ## 20   Prevalence               Lacuna   1.3264    0.3343 1109.0000     3.9676
    ## 21   Prevalence         CanopyHeight   1.2511    0.7436 1109.0000     1.6825
    ## 22   Prevalence           DensityLog   2.5054    0.6961 1109.0000     3.5992
    ## 23   Prevalence    TempAnomWarm_June  -0.0427    0.0245 1109.0000    -1.7410
    ## 24   Prevalence MonthlyMeanTemp_June   0.6968    0.2693 1109.0000     2.5875
    ## 25   Prevalence    TidalHeightBinary   0.5918    0.1662 1109.0000     3.5613
    ## 26   Prevalence           YearBinary   1.0039    0.2581 1109.0000     3.8904
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.1390         - 1109.0000    -4.6680
    ##    P.Value Std.Estimate    
    ## 1   0.2727       0.0329    
    ## 2   0.0010       0.1082  **
    ## 3   0.7220      -0.0120    
    ## 4   0.0000      -0.5252 ***
    ## 5   0.0000      -0.2060 ***
    ## 6   0.0000      -0.1254 ***
    ## 7   0.0000      -0.1473 ***
    ## 8   0.0000      -0.1799 ***
    ## 9   0.0000      -0.2969 ***
    ## 10  0.0000       0.3075 ***
    ## 11  0.0000      -0.0628 ***
    ## 12  0.0119      -0.1111   *
    ## 13  0.0000       0.5105 ***
    ## 14  0.0003      -0.2773 ***
    ## 15  0.0485      -0.0930   *
    ## 16  0.0632       0.0900    
    ## 17  0.0000      -0.3247 ***
    ## 18  0.5713       0.0164    
    ## 19  0.0000       0.3450 ***
    ## 20  0.0001       0.3452 ***
    ## 21  0.0925       0.1779    
    ## 22  0.0003       0.5585 ***
    ## 23  0.0817      -0.1585    
    ## 24  0.0097       0.2341  **
    ## 25  0.0004       0.1256 ***
    ## 26  0.0001       0.2092 ***
    ## 27  0.0000      -0.1390 ***

### ampithoid

``` r
sem_prev_amp_cc <- psem(
  lmer(Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis_cc),
  lmer(BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis_cc),
  glmer(Prevalence ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_cc,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.005539 (tol = 0.002, component 1)

``` r
summary(sem_prev_amp_cc)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_amp_cc 
    ## 
    ## Call:
    ##   Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.001   315.506
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1083.000          0  0.9999 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1083.000          0  0.9998 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 1081.006          0  0.9999 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.001 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##      Ampithoid    TempAnomWarm_June  -0.0027    0.0013 1093.7087     4.2140
    ##      Ampithoid MonthlyMeanTemp_June  -0.1019    0.0161 1082.5664    39.4343
    ##      Ampithoid         CanopyHeight  -1.2020    0.0391 1102.5962   938.8096
    ##      Ampithoid           DensityLog  -0.4446    0.0491  976.6281    79.7032
    ##      Ampithoid           YearBinary  -0.3038    0.0109 1095.8696   767.9414
    ##   CanopyHeight    TempAnomWarm_June  -0.0048     7e-04 1090.7377    41.4886
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0623    0.0099 1095.4087    39.4932
    ##   CanopyHeight           YearBinary  -0.1228     0.007 1086.2998   307.8459
    ##     DensityLog    TempAnomWarm_June  -0.0178     6e-04 1086.1680   950.3157
    ##     DensityLog MonthlyMeanTemp_June   0.2040    0.0077 1087.8941   702.0699
    ##     DensityLog           YearBinary  -0.0672    0.0054 1084.7054   153.2339
    ##   BladeAreaLog            Ampithoid   0.0044    0.0507  125.2893     0.0069
    ##   BladeAreaLog         CanopyHeight   0.6995    0.0765   53.0834    70.1855
    ##   BladeAreaLog           DensityLog  -0.1809    0.0567   35.9925     8.2954
    ##   BladeAreaLog    TempAnomWarm_June  -0.0040    0.0023  263.2724     2.8288
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0344    0.0262  132.1926     1.5206
    ##   BladeAreaLog    TidalHeightBinary  -0.2742    0.0145 1081.2707   357.8496
    ##   BladeAreaLog           YearBinary   0.0407    0.0249  206.4244     2.4021
    ##     Prevalence         BladeAreaLog   1.8745    0.3369 1109.0000     5.5647
    ##     Prevalence            Ampithoid  -1.1279    0.6009 1109.0000    -1.8771
    ##     Prevalence         CanopyHeight  -0.1381     1.016 1109.0000    -0.1360
    ##     Prevalence           DensityLog   2.0417    0.8464 1109.0000     2.4121
    ##     Prevalence    TempAnomWarm_June  -0.0237    0.0262 1109.0000    -0.9050
    ##     Prevalence MonthlyMeanTemp_June   0.5256    0.3062 1109.0000     1.7165
    ##     Prevalence    TidalHeightBinary   0.5834    0.1663 1109.0000     3.5085
    ##     Prevalence           YearBinary   0.2989    0.2983 1109.0000     1.0020
    ##   ~~DensityLog       ~~CanopyHeight  -0.1390         - 1109.0000    -4.6680
    ##   P.Value Std.Estimate    
    ##    0.0403      -0.0869   *
    ##    0.0000      -0.2919 ***
    ##    0.0000      -1.4565 ***
    ##    0.0000      -0.8446 ***
    ##    0.0000      -0.5395 ***
    ##    0.0000      -0.1254 ***
    ##    0.0000      -0.1473 ***
    ##    0.0000      -0.1799 ***
    ##    0.0000      -0.2969 ***
    ##    0.0000       0.3075 ***
    ##    0.0000      -0.0628 ***
    ##    0.9338       0.0029    
    ##    0.0000       0.5553 ***
    ##    0.0067      -0.2251  **
    ##    0.0938      -0.0826    
    ##    0.2197       0.0645    
    ##    0.0000      -0.3248 ***
    ##    0.1227       0.0473    
    ##    0.0000            - ***
    ##    0.0605            -    
    ##    0.8919            -    
    ##    0.0159            -   *
    ##    0.3655            -    
    ##    0.0861            -    
    ##    0.0005            - ***
    ##    0.3164            -    
    ##    0.0000       -0.139 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##      Ampithoid   none     0.43        0.96
    ##   CanopyHeight   none     0.09        0.95
    ##     DensityLog   none     0.09        0.99
    ##   BladeAreaLog   none     0.59        0.67
    ##     Prevalence  delta     0.15        0.53

Passes global fit test. Ampithoid is negative for prevalence. Epiphyte
presence not sig for prevalence

``` r
coefs(sem_prev_amp_cc)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1     Ampithoid    TempAnomWarm_June  -0.0027    0.0013 1093.7087     4.2140
    ## 2     Ampithoid MonthlyMeanTemp_June  -0.1019    0.0161 1082.5664    39.4343
    ## 3     Ampithoid         CanopyHeight  -1.2020    0.0391 1102.5962   938.8096
    ## 4     Ampithoid           DensityLog  -0.4446    0.0491  976.6281    79.7032
    ## 5     Ampithoid           YearBinary  -0.3038    0.0109 1095.8696   767.9414
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0048     7e-04 1090.7377    41.4886
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0623    0.0099 1095.4087    39.4932
    ## 8  CanopyHeight           YearBinary  -0.1228     0.007 1086.2998   307.8459
    ## 9    DensityLog    TempAnomWarm_June  -0.0178     6e-04 1086.1680   950.3157
    ## 10   DensityLog MonthlyMeanTemp_June   0.2040    0.0077 1087.8941   702.0699
    ## 11   DensityLog           YearBinary  -0.0672    0.0054 1084.7054   153.2339
    ## 12 BladeAreaLog            Ampithoid   0.0044    0.0507  125.2893     0.0069
    ## 13 BladeAreaLog         CanopyHeight   0.6995    0.0765   53.0834    70.1855
    ## 14 BladeAreaLog           DensityLog  -0.1809    0.0567   35.9925     8.2954
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0040    0.0023  263.2724     2.8288
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0344    0.0262  132.1926     1.5206
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2742    0.0145 1081.2707   357.8496
    ## 18 BladeAreaLog           YearBinary   0.0407    0.0249  206.4244     2.4021
    ## 19   Prevalence         BladeAreaLog   1.8745    0.3369 1109.0000     5.5647
    ## 20   Prevalence            Ampithoid  -1.1279    0.6009 1109.0000    -1.8771
    ## 21   Prevalence         CanopyHeight  -0.1381     1.016 1109.0000    -0.1360
    ## 22   Prevalence           DensityLog   2.0417    0.8464 1109.0000     2.4121
    ## 23   Prevalence    TempAnomWarm_June  -0.0237    0.0262 1109.0000    -0.9050
    ## 24   Prevalence MonthlyMeanTemp_June   0.5256    0.3062 1109.0000     1.7165
    ## 25   Prevalence    TidalHeightBinary   0.5834    0.1663 1109.0000     3.5085
    ## 26   Prevalence           YearBinary   0.2989    0.2983 1109.0000     1.0020
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.1390         - 1109.0000    -4.6680
    ##    P.Value Std.Estimate    
    ## 1   0.0403      -0.0869   *
    ## 2   0.0000      -0.2919 ***
    ## 3   0.0000      -1.4565 ***
    ## 4   0.0000      -0.8446 ***
    ## 5   0.0000      -0.5395 ***
    ## 6   0.0000      -0.1254 ***
    ## 7   0.0000      -0.1473 ***
    ## 8   0.0000      -0.1799 ***
    ## 9   0.0000      -0.2969 ***
    ## 10  0.0000       0.3075 ***
    ## 11  0.0000      -0.0628 ***
    ## 12  0.9338       0.0029    
    ## 13  0.0000       0.5553 ***
    ## 14  0.0067      -0.2251  **
    ## 15  0.0938      -0.0826    
    ## 16  0.2197       0.0645    
    ## 17  0.0000      -0.3248 ***
    ## 18  0.1227       0.0473    
    ## 19  0.0000       0.3350 ***
    ## 20  0.0605      -0.1320    
    ## 21  0.8919      -0.0196    
    ## 22  0.0159       0.4541   *
    ## 23  0.3655      -0.0879    
    ## 24  0.0861       0.1762    
    ## 25  0.0005       0.1235 ***
    ## 26  0.3164       0.0621    
    ## 27  0.0000      -0.1390 ***

## Lesion + Epifauna

``` r
les1 <- subset(dis1, LesionArea>0)
les1$LesionAreaLog <- log10(les1$LesionArea)
```

``` r
les2 <- select(les1, -c(Lacuna, Ampithoid))
les2 <- na.omit(les2)
sem_les_epi <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les2),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les2),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les2),
    lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les2),
  lmer(LesionAreaLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les2),
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
    ##   LesionAreaLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  98.126   300.385
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.4042     0.9568  0.3284 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4553     1.6499  0.1995 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 567.3072     0.1350  0.7134 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.126 with P-value = 0.409 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0018    0.0017 534.9572     1.0287
    ##        Epifauna MonthlyMeanTemp_June  -0.2096    0.0214 499.0907    92.5250
    ##        Epifauna         CanopyHeight  -0.7557    0.0652 592.1047   132.4142
    ##        Epifauna           DensityLog  -0.1129    0.0532 593.8627     4.4401
    ##        Epifauna           YearBinary  -0.1201    0.0126 583.5582    90.2675
    ##    CanopyHeight    TempAnomWarm_June  -0.0131     9e-04 520.6583   220.3874
    ##    CanopyHeight MonthlyMeanTemp_June   0.0146    0.0126 472.7833     1.2956
    ##    CanopyHeight           YearBinary  -0.1225    0.0062 572.0349   394.7215
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 550.7350   142.5710
    ##      DensityLog MonthlyMeanTemp_June   0.1360    0.0157 523.2563    73.1489
    ##      DensityLog           YearBinary  -0.0285    0.0076 571.8406    14.0916
    ##    BladeAreaLog             Epifauna  -0.0551    0.0506  55.2344     1.0779
    ##    BladeAreaLog         CanopyHeight   0.6933    0.0888  75.6990    55.1033
    ##    BladeAreaLog           DensityLog  -0.1677    0.0644  41.8594     5.3834
    ##    BladeAreaLog    TempAnomWarm_June   0.0019    0.0021  80.2093     0.8076
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0322    0.0155   8.9798     3.7926
    ##    BladeAreaLog    TidalHeightBinary  -0.2061    0.0174 578.6334   139.4463
    ##    BladeAreaLog           YearBinary   0.0591     0.024 429.0780     5.9299
    ##   LesionAreaLog         BladeAreaLog   0.5312    0.1228 588.4673    18.4695
    ##   LesionAreaLog             Epifauna   0.6477     0.135  41.1493    21.2292
    ##   LesionAreaLog         CanopyHeight   0.9455     0.257  77.3613    12.6451
    ##   LesionAreaLog           DensityLog   0.2287    0.1841  49.7287     1.2757
    ##   LesionAreaLog    TempAnomWarm_June   0.0115    0.0063  64.9282     3.1037
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0828    0.0498   9.0867     2.3827
    ##   LesionAreaLog    TidalHeightBinary   0.0122    0.0581 586.9815     0.0437
    ##   LesionAreaLog           YearBinary   0.1029    0.0706 417.9472     2.0858
    ##    ~~DensityLog       ~~CanopyHeight   0.1254         - 600.0000     3.0889
    ##   P.Value Std.Estimate    
    ##    0.3109      -0.0312    
    ##    0.0000      -0.6720 ***
    ##    0.0000      -0.5037 ***
    ##    0.0355      -0.1132   *
    ##    0.0000      -0.1245 ***
    ##    0.0000      -0.3433 ***
    ##    0.2556       0.0704    
    ##    0.0000      -0.1905 ***
    ##    0.0000      -0.2269 ***
    ##    0.0000       0.4350 ***
    ##    0.0002      -0.0295 ***
    ##    0.3037      -0.0684    
    ##    0.0000       0.5736 ***
    ##    0.0253      -0.2086   *
    ##    0.3715       0.0424    
    ##    0.0834      -0.1282    
    ##    0.0000      -0.2692 ***
    ##    0.0153       0.0760   *
    ##    0.0000       0.2856 ***
    ##    0.0000       0.4322 ***
    ##    0.0006       0.4205 ***
    ##    0.2641       0.1530    
    ##    0.0828       0.1343    
    ##    0.1568       0.1772    
    ##    0.8346       0.0086    
    ##    0.1494       0.0712    
    ##    0.0011       0.1254  **
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.19        0.99
    ##    CanopyHeight   none     0.07        0.98
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.60        0.70
    ##   LesionAreaLog   none     0.18        0.41

Passes global fit test. Epifauna is sig and pos for Lesion Area
Standardized coefs:

``` r
coefs(sem_les_epi)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1       Epifauna    TempAnomWarm_June  -0.0018    0.0017 534.9572     1.0287
    ## 2       Epifauna MonthlyMeanTemp_June  -0.2096    0.0214 499.0907    92.5250
    ## 3       Epifauna         CanopyHeight  -0.7557    0.0652 592.1047   132.4142
    ## 4       Epifauna           DensityLog  -0.1129    0.0532 593.8627     4.4401
    ## 5       Epifauna           YearBinary  -0.1201    0.0126 583.5582    90.2675
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0131     9e-04 520.6583   220.3874
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0146    0.0126 472.7833     1.2956
    ## 8   CanopyHeight           YearBinary  -0.1225    0.0062 572.0349   394.7215
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 550.7350   142.5710
    ## 10    DensityLog MonthlyMeanTemp_June   0.1360    0.0157 523.2563    73.1489
    ## 11    DensityLog           YearBinary  -0.0285    0.0076 571.8406    14.0916
    ## 12  BladeAreaLog             Epifauna  -0.0551    0.0506  55.2344     1.0779
    ## 13  BladeAreaLog         CanopyHeight   0.6933    0.0888  75.6990    55.1033
    ## 14  BladeAreaLog           DensityLog  -0.1677    0.0644  41.8594     5.3834
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0019    0.0021  80.2093     0.8076
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0322    0.0155   8.9798     3.7926
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2061    0.0174 578.6334   139.4463
    ## 18  BladeAreaLog           YearBinary   0.0591     0.024 429.0780     5.9299
    ## 19 LesionAreaLog         BladeAreaLog   0.5312    0.1228 588.4673    18.4695
    ## 20 LesionAreaLog             Epifauna   0.6477     0.135  41.1493    21.2292
    ## 21 LesionAreaLog         CanopyHeight   0.9455     0.257  77.3613    12.6451
    ## 22 LesionAreaLog           DensityLog   0.2287    0.1841  49.7287     1.2757
    ## 23 LesionAreaLog    TempAnomWarm_June   0.0115    0.0063  64.9282     3.1037
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.0828    0.0498   9.0867     2.3827
    ## 25 LesionAreaLog    TidalHeightBinary   0.0122    0.0581 586.9815     0.0437
    ## 26 LesionAreaLog           YearBinary   0.1029    0.0706 417.9472     2.0858
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1254         - 600.0000     3.0889
    ##    P.Value Std.Estimate    
    ## 1   0.3109      -0.0312    
    ## 2   0.0000      -0.6720 ***
    ## 3   0.0000      -0.5037 ***
    ## 4   0.0355      -0.1132   *
    ## 5   0.0000      -0.1245 ***
    ## 6   0.0000      -0.3433 ***
    ## 7   0.2556       0.0704    
    ## 8   0.0000      -0.1905 ***
    ## 9   0.0000      -0.2269 ***
    ## 10  0.0000       0.4350 ***
    ## 11  0.0002      -0.0295 ***
    ## 12  0.3037      -0.0684    
    ## 13  0.0000       0.5736 ***
    ## 14  0.0253      -0.2086   *
    ## 15  0.3715       0.0424    
    ## 16  0.0834      -0.1282    
    ## 17  0.0000      -0.2692 ***
    ## 18  0.0153       0.0760   *
    ## 19  0.0000       0.2856 ***
    ## 20  0.0000       0.4322 ***
    ## 21  0.0006       0.4205 ***
    ## 22  0.2641       0.1530    
    ## 23  0.0828       0.1343    
    ## 24  0.1568       0.1772    
    ## 25  0.8346       0.0086    
    ## 26  0.1494       0.0712    
    ## 27  0.0011       0.1254  **

## Lesion + Lacuna

``` r
les3 <- select(les1, -c(Epifauna, Ampithoid))
les3 <- na.omit(les3)
sem_les_lac <- psem(
  lmer(Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les3),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les3),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les3),
    lmer(BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les3),
  lmer(LesionAreaLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les3),
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
    ##   LesionAreaLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  97.488   296.243
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 529.3012     0.8726  0.3507 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 529.2657     1.5981  0.2067 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 527.5299     0.0201  0.8873 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 5.488 with P-value = 0.483 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##          Lacuna    TempAnomWarm_June  -0.0002     0.003 543.7943     0.0064
    ##          Lacuna MonthlyMeanTemp_June   0.0675    0.0368 547.2699     3.3265
    ##          Lacuna         CanopyHeight  -0.2277     0.109 536.6630     4.2637
    ##          Lacuna           DensityLog   0.1883     0.088 525.0401     4.4418
    ##          Lacuna           YearBinary  -0.1708    0.0213 545.3483    64.1790
    ##    CanopyHeight    TempAnomWarm_June  -0.0134     9e-04 537.4520   212.5417
    ##    CanopyHeight MonthlyMeanTemp_June   0.0210    0.0135 540.6726     2.4310
    ##    CanopyHeight           YearBinary  -0.1220    0.0064 530.7982   364.1040
    ##      DensityLog    TempAnomWarm_June  -0.0133    0.0011 536.1532   137.4236
    ##      DensityLog MonthlyMeanTemp_June   0.1412    0.0166 539.0729    72.2965
    ##      DensityLog           YearBinary  -0.0281    0.0079 530.6907    12.7446
    ##    BladeAreaLog               Lacuna  -0.0755    0.0294  62.1153     5.8209
    ##    BladeAreaLog         CanopyHeight   0.7322    0.0848 153.7608    67.7954
    ##    BladeAreaLog           DensityLog  -0.2043    0.0502  35.3252    13.6359
    ##    BladeAreaLog    TempAnomWarm_June   0.0001    0.0028 209.8931     0.0006
    ##    BladeAreaLog MonthlyMeanTemp_June   0.0115    0.0311  71.6711     0.1115
    ##    BladeAreaLog    TidalHeightBinary  -0.2149    0.0178 542.8959   144.4681
    ##    BladeAreaLog           YearBinary   0.0584    0.0242 408.6318     5.6672
    ##   LesionAreaLog         BladeAreaLog   0.7161    0.1301 543.7814    30.0718
    ##   LesionAreaLog               Lacuna   0.2674    0.1002  68.7680     6.5253
    ##   LesionAreaLog         CanopyHeight   0.0769    0.2906 194.3613     0.0664
    ##   LesionAreaLog           DensityLog  -0.2294    0.1791  45.1174     1.4410
    ##   LesionAreaLog    TempAnomWarm_June  -0.0155     0.009 260.3935     2.8142
    ##   LesionAreaLog MonthlyMeanTemp_June   0.2610    0.1055 133.9725     5.4931
    ##   LesionAreaLog    TidalHeightBinary   0.0969     0.061 540.6024     2.5125
    ##   LesionAreaLog           YearBinary   0.0235    0.0752 436.0389     0.0957
    ##    ~~DensityLog       ~~CanopyHeight   0.1250         - 556.0000     2.9618
    ##   P.Value Std.Estimate    
    ##    0.9361      -0.0028    
    ##    0.0687       0.0757    
    ##    0.0394      -0.0991   *
    ##    0.0355       0.1316   *
    ##    0.0000      -0.1192 ***
    ##    0.0000      -0.3652 ***
    ##    0.1195       0.0542    
    ##    0.0000      -0.1956 ***
    ##    0.0000      -0.2253 ***
    ##    0.0000       0.2269 ***
    ##    0.0004      -0.0281 ***
    ##    0.0188      -0.1432   *
    ##    0.0000       0.6041 ***
    ##    0.0007      -0.2706 ***
    ##    0.9800       0.0017    
    ##    0.7394       0.0244    
    ##    0.0000      -0.2891 ***
    ##    0.0177       0.0772   *
    ##    0.0000       0.3761 ***
    ##    0.0129       0.2662   *
    ##    0.7970       0.0333    
    ##    0.2362      -0.1595    
    ##    0.0946      -0.1828    
    ##    0.0206       0.2916   *
    ##    0.1135       0.0684    
    ##    0.7572       0.0163    
    ##    0.0016       0.1250  **
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##          Lacuna   none     0.03        0.95
    ##    CanopyHeight   none     0.10        0.97
    ##      DensityLog   none     0.03        0.98
    ##    BladeAreaLog   none     0.67        0.72
    ##   LesionAreaLog   none     0.18        0.51

Passes global fit test. Lacuna is sig and pos for Lesion Area
Standardized coefs:

``` r
coefs(sem_les_lac)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1         Lacuna    TempAnomWarm_June  -0.0002     0.003 543.7943     0.0064
    ## 2         Lacuna MonthlyMeanTemp_June   0.0675    0.0368 547.2699     3.3265
    ## 3         Lacuna         CanopyHeight  -0.2277     0.109 536.6630     4.2637
    ## 4         Lacuna           DensityLog   0.1883     0.088 525.0401     4.4418
    ## 5         Lacuna           YearBinary  -0.1708    0.0213 545.3483    64.1790
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0134     9e-04 537.4520   212.5417
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0210    0.0135 540.6726     2.4310
    ## 8   CanopyHeight           YearBinary  -0.1220    0.0064 530.7982   364.1040
    ## 9     DensityLog    TempAnomWarm_June  -0.0133    0.0011 536.1532   137.4236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1412    0.0166 539.0729    72.2965
    ## 11    DensityLog           YearBinary  -0.0281    0.0079 530.6907    12.7446
    ## 12  BladeAreaLog               Lacuna  -0.0755    0.0294  62.1153     5.8209
    ## 13  BladeAreaLog         CanopyHeight   0.7322    0.0848 153.7608    67.7954
    ## 14  BladeAreaLog           DensityLog  -0.2043    0.0502  35.3252    13.6359
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0001    0.0028 209.8931     0.0006
    ## 16  BladeAreaLog MonthlyMeanTemp_June   0.0115    0.0311  71.6711     0.1115
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2149    0.0178 542.8959   144.4681
    ## 18  BladeAreaLog           YearBinary   0.0584    0.0242 408.6318     5.6672
    ## 19 LesionAreaLog         BladeAreaLog   0.7161    0.1301 543.7814    30.0718
    ## 20 LesionAreaLog               Lacuna   0.2674    0.1002  68.7680     6.5253
    ## 21 LesionAreaLog         CanopyHeight   0.0769    0.2906 194.3613     0.0664
    ## 22 LesionAreaLog           DensityLog  -0.2294    0.1791  45.1174     1.4410
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0155     0.009 260.3935     2.8142
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.2610    0.1055 133.9725     5.4931
    ## 25 LesionAreaLog    TidalHeightBinary   0.0969     0.061 540.6024     2.5125
    ## 26 LesionAreaLog           YearBinary   0.0235    0.0752 436.0389     0.0957
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1250         - 556.0000     2.9618
    ##    P.Value Std.Estimate    
    ## 1   0.9361      -0.0028    
    ## 2   0.0687       0.0757    
    ## 3   0.0394      -0.0991   *
    ## 4   0.0355       0.1316   *
    ## 5   0.0000      -0.1192 ***
    ## 6   0.0000      -0.3652 ***
    ## 7   0.1195       0.0542    
    ## 8   0.0000      -0.1956 ***
    ## 9   0.0000      -0.2253 ***
    ## 10  0.0000       0.2269 ***
    ## 11  0.0004      -0.0281 ***
    ## 12  0.0188      -0.1432   *
    ## 13  0.0000       0.6041 ***
    ## 14  0.0007      -0.2706 ***
    ## 15  0.9800       0.0017    
    ## 16  0.7394       0.0244    
    ## 17  0.0000      -0.2891 ***
    ## 18  0.0177       0.0772   *
    ## 19  0.0000       0.3761 ***
    ## 20  0.0129       0.2662   *
    ## 21  0.7970       0.0333    
    ## 22  0.2362      -0.1595    
    ## 23  0.0946      -0.1828    
    ## 24  0.0206       0.2916   *
    ## 25  0.1135       0.0684    
    ## 26  0.7572       0.0163    
    ## 27  0.0016       0.1250  **

## Lesion + Ampithoid

``` r
les4 <- select(les1, -c(Epifauna, Lacuna))
les4 <- na.omit(les4)
sem_les_amp <- psem(
  lmer(Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les4),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les4),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les4),
    lmer(BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les4),
  lmer(LesionAreaLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les4),
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
    ##   LesionAreaLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  98.669   295.91
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 508.4514     1.7154  0.1909 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 508.1616     1.0479  0.3065 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 506.4977     0.2618  0.6091 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.669 with P-value = 0.353 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##       Ampithoid    TempAnomWarm_June  -0.0012    0.0018 418.4607     0.4301
    ##       Ampithoid MonthlyMeanTemp_June  -0.1374    0.0221 369.0752    36.5042
    ##       Ampithoid         CanopyHeight  -1.3182    0.0615 527.0343   451.9375
    ##       Ampithoid           DensityLog   0.0351    0.0821 441.4433     0.1750
    ##       Ampithoid           YearBinary  -0.2768    0.0143 522.6992   371.3792
    ##    CanopyHeight    TempAnomWarm_June  -0.0120     0.001 455.4525   146.9677
    ##    CanopyHeight MonthlyMeanTemp_June   0.0104    0.0132 406.1511     0.5858
    ##    CanopyHeight           YearBinary  -0.1339    0.0077 513.5161   303.5212
    ##      DensityLog    TempAnomWarm_June  -0.0121     7e-04 516.1132   283.7415
    ##      DensityLog MonthlyMeanTemp_June   0.1424    0.0098 515.8248   209.6389
    ##      DensityLog           YearBinary  -0.0518    0.0055 510.4593    88.3492
    ##    BladeAreaLog            Ampithoid   0.1099    0.0619  83.3874     2.7249
    ##    BladeAreaLog         CanopyHeight   0.8415    0.1017  44.9169    53.7819
    ##    BladeAreaLog           DensityLog  -0.1418    0.0682  20.7372     3.1840
    ##    BladeAreaLog    TempAnomWarm_June   0.0055    0.0021  59.5879     6.2698
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0315    0.0152   9.4380     3.8452
    ##    BladeAreaLog    TidalHeightBinary  -0.2276    0.0181 516.2463   157.9031
    ##    BladeAreaLog           YearBinary   0.0605    0.0299 158.8057     3.6613
    ##   LesionAreaLog         BladeAreaLog   0.5715    0.1319 525.8048    18.6125
    ##   LesionAreaLog            Ampithoid  -0.5775    0.1907  77.5760     8.4661
    ##   LesionAreaLog         CanopyHeight  -0.4185    0.3417  79.2557     1.3798
    ##   LesionAreaLog           DensityLog  -0.5473    0.2313  34.0924     4.8631
    ##   LesionAreaLog    TempAnomWarm_June  -0.0144    0.0074  56.5995     3.3583
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1631    0.0709  13.2730     4.2836
    ##   LesionAreaLog    TidalHeightBinary   0.0260    0.0626 521.0909     0.1718
    ##   LesionAreaLog           YearBinary  -0.1534    0.0926 145.4093     2.5829
    ##    ~~DensityLog       ~~CanopyHeight   0.0605         - 538.0000     1.4017
    ##   P.Value Std.Estimate    
    ##    0.5123      -0.0279    
    ##    0.0000      -0.5850 ***
    ##    0.0000      -1.1734 ***
    ##    0.6759       0.0476    
    ##    0.0000      -0.3661 ***
    ##    0.0000      -0.3037 ***
    ##    0.4445       0.0495    
    ##    0.0000      -0.1990 ***
    ##    0.0000      -0.2007 ***
    ##    0.0000       0.4473 ***
    ##    0.0000      -0.0506 ***
    ##    0.1026       0.1013    
    ##    0.0000       0.6905 ***
    ##    0.0890      -0.1771    
    ##    0.0150       0.1131   *
    ##    0.0800      -0.1235    
    ##    0.0000      -0.2890 ***
    ##    0.0575       0.0738    
    ##    0.0000       0.3138 ***
    ##    0.0047      -0.2923  **
    ##    0.2437      -0.1885    
    ##    0.0343      -0.3753   *
    ##    0.0721      -0.1642    
    ##    0.0585       0.3514    
    ##    0.6787       0.0181    
    ##    0.1102      -0.1027    
    ##    0.0808       0.0605    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##       Ampithoid   none     0.33        0.99
    ##    CanopyHeight   none     0.07        0.98
    ##      DensityLog   none     0.13        0.99
    ##    BladeAreaLog   none     0.63        0.73
    ##   LesionAreaLog   none     0.17        0.61

Passes global fit test. Ampithoid is significant and negative for lesion
area Standarized coefs:

``` r
coefs(sem_les_amp)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1      Ampithoid    TempAnomWarm_June  -0.0012    0.0018 418.4607     0.4301
    ## 2      Ampithoid MonthlyMeanTemp_June  -0.1374    0.0221 369.0752    36.5042
    ## 3      Ampithoid         CanopyHeight  -1.3182    0.0615 527.0343   451.9375
    ## 4      Ampithoid           DensityLog   0.0351    0.0821 441.4433     0.1750
    ## 5      Ampithoid           YearBinary  -0.2768    0.0143 522.6992   371.3792
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0120     0.001 455.4525   146.9677
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0104    0.0132 406.1511     0.5858
    ## 8   CanopyHeight           YearBinary  -0.1339    0.0077 513.5161   303.5212
    ## 9     DensityLog    TempAnomWarm_June  -0.0121     7e-04 516.1132   283.7415
    ## 10    DensityLog MonthlyMeanTemp_June   0.1424    0.0098 515.8248   209.6389
    ## 11    DensityLog           YearBinary  -0.0518    0.0055 510.4593    88.3492
    ## 12  BladeAreaLog            Ampithoid   0.1099    0.0619  83.3874     2.7249
    ## 13  BladeAreaLog         CanopyHeight   0.8415    0.1017  44.9169    53.7819
    ## 14  BladeAreaLog           DensityLog  -0.1418    0.0682  20.7372     3.1840
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0055    0.0021  59.5879     6.2698
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0315    0.0152   9.4380     3.8452
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2276    0.0181 516.2463   157.9031
    ## 18  BladeAreaLog           YearBinary   0.0605    0.0299 158.8057     3.6613
    ## 19 LesionAreaLog         BladeAreaLog   0.5715    0.1319 525.8048    18.6125
    ## 20 LesionAreaLog            Ampithoid  -0.5775    0.1907  77.5760     8.4661
    ## 21 LesionAreaLog         CanopyHeight  -0.4185    0.3417  79.2557     1.3798
    ## 22 LesionAreaLog           DensityLog  -0.5473    0.2313  34.0924     4.8631
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0144    0.0074  56.5995     3.3583
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.1631    0.0709  13.2730     4.2836
    ## 25 LesionAreaLog    TidalHeightBinary   0.0260    0.0626 521.0909     0.1718
    ## 26 LesionAreaLog           YearBinary  -0.1534    0.0926 145.4093     2.5829
    ## 27  ~~DensityLog       ~~CanopyHeight   0.0605         - 538.0000     1.4017
    ##    P.Value Std.Estimate    
    ## 1   0.5123      -0.0279    
    ## 2   0.0000      -0.5850 ***
    ## 3   0.0000      -1.1734 ***
    ## 4   0.6759       0.0476    
    ## 5   0.0000      -0.3661 ***
    ## 6   0.0000      -0.3037 ***
    ## 7   0.4445       0.0495    
    ## 8   0.0000      -0.1990 ***
    ## 9   0.0000      -0.2007 ***
    ## 10  0.0000       0.4473 ***
    ## 11  0.0000      -0.0506 ***
    ## 12  0.1026       0.1013    
    ## 13  0.0000       0.6905 ***
    ## 14  0.0890      -0.1771    
    ## 15  0.0150       0.1131   *
    ## 16  0.0800      -0.1235    
    ## 17  0.0000      -0.2890 ***
    ## 18  0.0575       0.0738    
    ## 19  0.0000       0.3138 ***
    ## 20  0.0047      -0.2923  **
    ## 21  0.2437      -0.1885    
    ## 22  0.0343      -0.3753   *
    ## 23  0.0721      -0.1642    
    ## 24  0.0585       0.3514    
    ## 25  0.6787       0.0181    
    ## 26  0.1102      -0.1027    
    ## 27  0.0808       0.0605

## Complete cases

``` r
# limit to complete cases of lesion data
les_cc <- subset(dis_cc, LesionArea>0)
les_cc$LesionAreaLog <- log10(les_cc$LesionArea)
```

### CC lesion + Epifauna

``` r
sem_les_epi_cc <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_cc),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_cc),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_cc),
    lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_cc),
  lmer(LesionAreaLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_cc),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_epi_cc)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_epi_cc 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  98.384   291.701
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 468.3158     1.5890  0.2081 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 468.0902     1.0195  0.3132 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 466.5800     0.2318  0.6304 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.384 with P-value = 0.382 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0093    0.0021 485.9594    19.5951
    ##        Epifauna MonthlyMeanTemp_June  -0.1311     0.025 479.2381    26.6828
    ##        Epifauna         CanopyHeight  -0.7516    0.0679 482.3260   119.3357
    ##        Epifauna           DensityLog  -0.6305     0.088 267.6909    46.5645
    ##        Epifauna           YearBinary  -0.1314    0.0158 478.4685    68.7300
    ##    CanopyHeight    TempAnomWarm_June  -0.0124     0.001 475.9865   141.8059
    ##    CanopyHeight MonthlyMeanTemp_June   0.0172    0.0143 478.9416     1.4536
    ##    CanopyHeight           YearBinary  -0.1329     0.008 470.5157   275.2855
    ##      DensityLog    TempAnomWarm_June  -0.0122     7e-04 470.9773   265.8758
    ##      DensityLog MonthlyMeanTemp_June   0.1444    0.0103 471.9632   197.6298
    ##      DensityLog           YearBinary  -0.0515    0.0057 469.4208    80.5362
    ##    BladeAreaLog             Epifauna  -0.0897    0.0513  43.8996     2.6679
    ##    BladeAreaLog         CanopyHeight   0.6720    0.1027  73.8946    35.3482
    ##    BladeAreaLog           DensityLog  -0.2250      0.07  20.9302     7.4913
    ##    BladeAreaLog    TempAnomWarm_June   0.0017    0.0032 130.4975     0.2525
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0133    0.0329  72.5996     0.1384
    ##    BladeAreaLog    TidalHeightBinary  -0.2367    0.0185 477.5006   163.2917
    ##    BladeAreaLog           YearBinary   0.0287    0.0278 253.0492     0.9852
    ##   LesionAreaLog         BladeAreaLog   0.7977     0.137 483.7919    33.3868
    ##   LesionAreaLog             Epifauna   0.6747     0.149  41.2388    18.1636
    ##   LesionAreaLog         CanopyHeight   0.3776    0.3202 105.9022     1.2275
    ##   LesionAreaLog           DensityLog   0.0570    0.2131  26.8920     0.0556
    ##   LesionAreaLog    TempAnomWarm_June  -0.0113    0.0097 159.7913     1.1965
    ##   LesionAreaLog MonthlyMeanTemp_June   0.3111    0.0979  79.7968     8.7288
    ##   LesionAreaLog    TidalHeightBinary   0.1122    0.0646 482.5266     2.9852
    ##   LesionAreaLog           YearBinary   0.0891    0.0832 220.1265     1.0742
    ##    ~~DensityLog       ~~CanopyHeight   0.0602         - 494.0000     1.3358
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.1612 ***
    ##    0.0000      -0.2109 ***
    ##    0.0000      -0.4941 ***
    ##    0.0000      -0.6821 ***
    ##    0.0000      -0.1333 ***
    ##    0.0000      -0.3266 ***
    ##    0.2285       0.0422    
    ##    0.0000      -0.2051 ***
    ##    0.0000      -0.1950 ***
    ##    0.0000       0.2147 ***
    ##    0.0000      -0.0483 ***
    ##    0.1095      -0.1107    
    ##    0.0000       0.5452 ***
    ##    0.0124      -0.3004   *
    ##    0.6161       0.0372    
    ##    0.7110      -0.0265    
    ##    0.0000      -0.3101 ***
    ##    0.3219       0.0360    
    ##    0.0000       0.4274 ***
    ##    0.0001       0.4461 ***
    ##    0.2704       0.1641    
    ##    0.8154       0.0408    
    ##    0.2757      -0.1290    
    ##    0.0041       0.3310  **
    ##    0.0847       0.0787    
    ##    0.3011       0.0598    
    ##    0.0911       0.0602    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.34        0.96
    ##    CanopyHeight   none     0.10        0.97
    ##      DensityLog   none     0.03        0.99
    ##    BladeAreaLog   none     0.64        0.72
    ##   LesionAreaLog   none     0.23        0.45

Passes global fit test. Epifauna is sig and pos for Lesion area

``` r
coefs(sem_les_epi_cc)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1       Epifauna    TempAnomWarm_June  -0.0093    0.0021 485.9594    19.5951
    ## 2       Epifauna MonthlyMeanTemp_June  -0.1311     0.025 479.2381    26.6828
    ## 3       Epifauna         CanopyHeight  -0.7516    0.0679 482.3260   119.3357
    ## 4       Epifauna           DensityLog  -0.6305     0.088 267.6909    46.5645
    ## 5       Epifauna           YearBinary  -0.1314    0.0158 478.4685    68.7300
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0124     0.001 475.9865   141.8059
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0172    0.0143 478.9416     1.4536
    ## 8   CanopyHeight           YearBinary  -0.1329     0.008 470.5157   275.2855
    ## 9     DensityLog    TempAnomWarm_June  -0.0122     7e-04 470.9773   265.8758
    ## 10    DensityLog MonthlyMeanTemp_June   0.1444    0.0103 471.9632   197.6298
    ## 11    DensityLog           YearBinary  -0.0515    0.0057 469.4208    80.5362
    ## 12  BladeAreaLog             Epifauna  -0.0897    0.0513  43.8996     2.6679
    ## 13  BladeAreaLog         CanopyHeight   0.6720    0.1027  73.8946    35.3482
    ## 14  BladeAreaLog           DensityLog  -0.2250      0.07  20.9302     7.4913
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0017    0.0032 130.4975     0.2525
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0133    0.0329  72.5996     0.1384
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2367    0.0185 477.5006   163.2917
    ## 18  BladeAreaLog           YearBinary   0.0287    0.0278 253.0492     0.9852
    ## 19 LesionAreaLog         BladeAreaLog   0.7977     0.137 483.7919    33.3868
    ## 20 LesionAreaLog             Epifauna   0.6747     0.149  41.2388    18.1636
    ## 21 LesionAreaLog         CanopyHeight   0.3776    0.3202 105.9022     1.2275
    ## 22 LesionAreaLog           DensityLog   0.0570    0.2131  26.8920     0.0556
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0113    0.0097 159.7913     1.1965
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.3111    0.0979  79.7968     8.7288
    ## 25 LesionAreaLog    TidalHeightBinary   0.1122    0.0646 482.5266     2.9852
    ## 26 LesionAreaLog           YearBinary   0.0891    0.0832 220.1265     1.0742
    ## 27  ~~DensityLog       ~~CanopyHeight   0.0602         - 494.0000     1.3358
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.1612 ***
    ## 2   0.0000      -0.2109 ***
    ## 3   0.0000      -0.4941 ***
    ## 4   0.0000      -0.6821 ***
    ## 5   0.0000      -0.1333 ***
    ## 6   0.0000      -0.3266 ***
    ## 7   0.2285       0.0422    
    ## 8   0.0000      -0.2051 ***
    ## 9   0.0000      -0.1950 ***
    ## 10  0.0000       0.2147 ***
    ## 11  0.0000      -0.0483 ***
    ## 12  0.1095      -0.1107    
    ## 13  0.0000       0.5452 ***
    ## 14  0.0124      -0.3004   *
    ## 15  0.6161       0.0372    
    ## 16  0.7110      -0.0265    
    ## 17  0.0000      -0.3101 ***
    ## 18  0.3219       0.0360    
    ## 19  0.0000       0.4274 ***
    ## 20  0.0001       0.4461 ***
    ## 21  0.2704       0.1641    
    ## 22  0.8154       0.0408    
    ## 23  0.2757      -0.1290    
    ## 24  0.0041       0.3310  **
    ## 25  0.0847       0.0787    
    ## 26  0.3011       0.0598    
    ## 27  0.0911       0.0602

### CC lesion + Lacuna

``` r
sem_les_lac_cc <- psem(
  lmer(Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_cc),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_cc),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_cc),
    lmer(BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_cc),
  lmer(LesionAreaLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_cc),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_lac_cc)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_lac_cc 
    ## 
    ## Call:
    ##   Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  98.910   292.227
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 468.3158     1.5890  0.2081 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 468.0902     1.0195  0.3132 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 466.7610     0.4890  0.4847 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.91 with P-value = 0.329 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##          Lacuna    TempAnomWarm_June   0.0014    0.0035 487.4348     0.1662
    ##          Lacuna MonthlyMeanTemp_June   0.0599    0.0421 478.3879     1.9802
    ##          Lacuna         CanopyHeight  -0.1893    0.1142 481.3243     2.6874
    ##          Lacuna           DensityLog   0.1113    0.1463 283.7543     0.5372
    ##          Lacuna           YearBinary  -0.1963    0.0267 481.5867    53.6080
    ##    CanopyHeight    TempAnomWarm_June  -0.0124     0.001 475.9865   141.8059
    ##    CanopyHeight MonthlyMeanTemp_June   0.0172    0.0143 478.9416     1.4536
    ##    CanopyHeight           YearBinary  -0.1329     0.008 470.5157   275.2855
    ##      DensityLog    TempAnomWarm_June  -0.0122     7e-04 470.9773   265.8758
    ##      DensityLog MonthlyMeanTemp_June   0.1444    0.0103 471.9632   197.6298
    ##      DensityLog           YearBinary  -0.0515    0.0057 469.4208    80.5362
    ##    BladeAreaLog               Lacuna  -0.0884    0.0328  66.1803     6.3132
    ##    BladeAreaLog         CanopyHeight   0.6845    0.0901 126.5885    51.8346
    ##    BladeAreaLog           DensityLog  -0.2230    0.0578  25.9594    11.7064
    ##    BladeAreaLog    TempAnomWarm_June   0.0015     0.003 150.6410     0.2317
    ##    BladeAreaLog MonthlyMeanTemp_June   0.0118    0.0324  66.3319     0.1085
    ##    BladeAreaLog    TidalHeightBinary  -0.2362    0.0185 479.7737   162.1011
    ##    BladeAreaLog           YearBinary   0.0188    0.0284 244.9015     0.4124
    ##   LesionAreaLog         BladeAreaLog   0.7810     0.139 482.0159    31.3265
    ##   LesionAreaLog               Lacuna   0.2430    0.1094  74.3632     4.5207
    ##   LesionAreaLog         CanopyHeight  -0.0965    0.3048 154.5015     0.0946
    ##   LesionAreaLog           DensityLog  -0.3906     0.207  25.6864     3.1295
    ##   LesionAreaLog    TempAnomWarm_June  -0.0242    0.0096 181.0051     6.0018
    ##   LesionAreaLog MonthlyMeanTemp_June   0.3149    0.1092 128.3877     7.5389
    ##   LesionAreaLog    TidalHeightBinary   0.1112    0.0654 480.2181     2.8713
    ##   LesionAreaLog           YearBinary   0.0375    0.0886 266.3657     0.1724
    ##    ~~DensityLog       ~~CanopyHeight   0.0602         - 494.0000     1.3358
    ##   P.Value Std.Estimate    
    ##    0.6837       0.0163    
    ##    0.1600       0.0629    
    ##    0.1018      -0.0812    
    ##    0.4642       0.0786    
    ##    0.0000      -0.1300 ***
    ##    0.0000      -0.3266 ***
    ##    0.2285       0.0422    
    ##    0.0000      -0.2051 ***
    ##    0.0000      -0.1950 ***
    ##    0.0000       0.2147 ***
    ##    0.0000      -0.0483 ***
    ##    0.0144      -0.1672   *
    ##    0.0000       0.5553 ***
    ##    0.0021      -0.2977  **
    ##    0.6310       0.0326    
    ##    0.7429       0.0235    
    ##    0.0000      -0.3094 ***
    ##    0.5213       0.0235    
    ##    0.0000       0.4185 ***
    ##    0.0368       0.2462   *
    ##    0.7589      -0.0419    
    ##    0.0888      -0.2795    
    ##    0.0152      -0.2766   *
    ##    0.0069       0.3350  **
    ##    0.0908       0.0780    
    ##    0.6783       0.0252    
    ##    0.0911       0.0602    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##          Lacuna   none     0.03        0.95
    ##    CanopyHeight   none     0.10        0.97
    ##      DensityLog   none     0.03        0.99
    ##    BladeAreaLog   none     0.68        0.73
    ##   LesionAreaLog   none     0.21        0.58

Passes global fit test. Lacuana is sig and positive for lesion area
Standardized coefs:

``` r
coefs(sem_les_lac_cc)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1         Lacuna    TempAnomWarm_June   0.0014    0.0035 487.4348     0.1662
    ## 2         Lacuna MonthlyMeanTemp_June   0.0599    0.0421 478.3879     1.9802
    ## 3         Lacuna         CanopyHeight  -0.1893    0.1142 481.3243     2.6874
    ## 4         Lacuna           DensityLog   0.1113    0.1463 283.7543     0.5372
    ## 5         Lacuna           YearBinary  -0.1963    0.0267 481.5867    53.6080
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0124     0.001 475.9865   141.8059
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0172    0.0143 478.9416     1.4536
    ## 8   CanopyHeight           YearBinary  -0.1329     0.008 470.5157   275.2855
    ## 9     DensityLog    TempAnomWarm_June  -0.0122     7e-04 470.9773   265.8758
    ## 10    DensityLog MonthlyMeanTemp_June   0.1444    0.0103 471.9632   197.6298
    ## 11    DensityLog           YearBinary  -0.0515    0.0057 469.4208    80.5362
    ## 12  BladeAreaLog               Lacuna  -0.0884    0.0328  66.1803     6.3132
    ## 13  BladeAreaLog         CanopyHeight   0.6845    0.0901 126.5885    51.8346
    ## 14  BladeAreaLog           DensityLog  -0.2230    0.0578  25.9594    11.7064
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0015     0.003 150.6410     0.2317
    ## 16  BladeAreaLog MonthlyMeanTemp_June   0.0118    0.0324  66.3319     0.1085
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2362    0.0185 479.7737   162.1011
    ## 18  BladeAreaLog           YearBinary   0.0188    0.0284 244.9015     0.4124
    ## 19 LesionAreaLog         BladeAreaLog   0.7810     0.139 482.0159    31.3265
    ## 20 LesionAreaLog               Lacuna   0.2430    0.1094  74.3632     4.5207
    ## 21 LesionAreaLog         CanopyHeight  -0.0965    0.3048 154.5015     0.0946
    ## 22 LesionAreaLog           DensityLog  -0.3906     0.207  25.6864     3.1295
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0242    0.0096 181.0051     6.0018
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.3149    0.1092 128.3877     7.5389
    ## 25 LesionAreaLog    TidalHeightBinary   0.1112    0.0654 480.2181     2.8713
    ## 26 LesionAreaLog           YearBinary   0.0375    0.0886 266.3657     0.1724
    ## 27  ~~DensityLog       ~~CanopyHeight   0.0602         - 494.0000     1.3358
    ##    P.Value Std.Estimate    
    ## 1   0.6837       0.0163    
    ## 2   0.1600       0.0629    
    ## 3   0.1018      -0.0812    
    ## 4   0.4642       0.0786    
    ## 5   0.0000      -0.1300 ***
    ## 6   0.0000      -0.3266 ***
    ## 7   0.2285       0.0422    
    ## 8   0.0000      -0.2051 ***
    ## 9   0.0000      -0.1950 ***
    ## 10  0.0000       0.2147 ***
    ## 11  0.0000      -0.0483 ***
    ## 12  0.0144      -0.1672   *
    ## 13  0.0000       0.5553 ***
    ## 14  0.0021      -0.2977  **
    ## 15  0.6310       0.0326    
    ## 16  0.7429       0.0235    
    ## 17  0.0000      -0.3094 ***
    ## 18  0.5213       0.0235    
    ## 19  0.0000       0.4185 ***
    ## 20  0.0368       0.2462   *
    ## 21  0.7589      -0.0419    
    ## 22  0.0888      -0.2795    
    ## 23  0.0152      -0.2766   *
    ## 24  0.0069       0.3350  **
    ## 25  0.0908       0.0780    
    ## 26  0.6783       0.0252    
    ## 27  0.0911       0.0602

### CC lesion + ampithoid

``` r
sem_les_amp_cc <- psem(
  lmer(Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les_cc),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_cc),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les_cc),
    lmer(BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_cc),
  lmer(LesionAreaLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_cc),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_amp_cc)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_amp_cc 
    ## 
    ## Call:
    ##   Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  98.473   291.79
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 468.3158     1.5890  0.2081 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 468.0902     1.0195  0.3132 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 466.5483     0.2706  0.6032 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.473 with P-value = 0.372 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##       Ampithoid    TempAnomWarm_June  -0.0003     0.002 487.3606     0.0157
    ##       Ampithoid MonthlyMeanTemp_June  -0.1469    0.0238 486.1273    37.4325
    ##       Ampithoid         CanopyHeight  -1.2824    0.0645 487.3462   388.2213
    ##       Ampithoid           DensityLog   0.0373    0.0843 346.4276     0.1830
    ##       Ampithoid           YearBinary  -0.2730     0.015 478.3483   330.2837
    ##    CanopyHeight    TempAnomWarm_June  -0.0124     0.001 475.9865   141.8059
    ##    CanopyHeight MonthlyMeanTemp_June   0.0172    0.0143 478.9416     1.4536
    ##    CanopyHeight           YearBinary  -0.1329     0.008 470.5157   275.2855
    ##      DensityLog    TempAnomWarm_June  -0.0122     7e-04 470.9773   265.8758
    ##      DensityLog MonthlyMeanTemp_June   0.1444    0.0103 471.9632   197.6298
    ##      DensityLog           YearBinary  -0.0515    0.0057 469.4208    80.5362
    ##    BladeAreaLog            Ampithoid   0.0747    0.0678 143.4303     1.0781
    ##    BladeAreaLog         CanopyHeight   0.8297    0.0945  38.7978    60.0340
    ##    BladeAreaLog           DensityLog  -0.1546    0.0619  18.9537     4.5097
    ##    BladeAreaLog    TempAnomWarm_June   0.0039     0.003 124.5016     1.4408
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0148    0.0328  59.4434     0.1661
    ##    BladeAreaLog    TidalHeightBinary  -0.2387    0.0186 475.6081   164.6257
    ##    BladeAreaLog           YearBinary   0.0630    0.0284 128.9959     4.3340
    ##   LesionAreaLog         BladeAreaLog   0.7739    0.1383 482.5479    31.0340
    ##   LesionAreaLog            Ampithoid  -0.4857    0.2159 148.0764     4.7218
    ##   LesionAreaLog         CanopyHeight  -0.6548    0.3415  72.1869     3.3064
    ##   LesionAreaLog           DensityLog  -0.5657     0.227  28.1271     5.2485
    ##   LesionAreaLog    TempAnomWarm_June  -0.0289    0.0096 143.3481     8.4218
    ##   LesionAreaLog MonthlyMeanTemp_June   0.3519    0.1085  81.9863     9.3917
    ##   LesionAreaLog    TidalHeightBinary   0.1207    0.0654 476.7950     3.3919
    ##   LesionAreaLog           YearBinary  -0.1380    0.0918 134.4646     2.0891
    ##    ~~DensityLog       ~~CanopyHeight   0.0602         - 494.0000     1.3358
    ##   P.Value Std.Estimate    
    ##    0.9004      -0.0088    
    ##    0.0000      -0.4796 ***
    ##    0.0000      -1.7108 ***
    ##    0.6691       0.0819    
    ##    0.0000      -0.5619 ***
    ##    0.0000      -0.3266 ***
    ##    0.2285       0.0422    
    ##    0.0000      -0.2051 ***
    ##    0.0000      -0.1950 ***
    ##    0.0000       0.2147 ***
    ##    0.0000      -0.0483 ***
    ##    0.3009       0.0454    
    ##    0.0000       0.6731 ***
    ##    0.0471      -0.2065   *
    ##    0.2323       0.0822    
    ##    0.6850      -0.0294    
    ##    0.0000      -0.3126 ***
    ##    0.0393       0.0789   *
    ##    0.0000       0.4147 ***
    ##    0.0314      -0.1583   *
    ##    0.0732      -0.2846    
    ##    0.0297      -0.4047   *
    ##    0.0043      -0.3294  **
    ##    0.0030       0.3743  **
    ##    0.0661       0.0847    
    ##    0.1507      -0.0925    
    ##    0.0911       0.0602    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##       Ampithoid   none     0.45        0.98
    ##    CanopyHeight   none     0.10        0.97
    ##      DensityLog   none     0.03        0.99
    ##    BladeAreaLog   none     0.64        0.72
    ##   LesionAreaLog   none     0.20        0.58

Passes global fit test. Ampithoid is sig and negative for lesion area.

``` r
coefs(sem_les_amp_cc)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1      Ampithoid    TempAnomWarm_June  -0.0003     0.002 487.3606     0.0157
    ## 2      Ampithoid MonthlyMeanTemp_June  -0.1469    0.0238 486.1273    37.4325
    ## 3      Ampithoid         CanopyHeight  -1.2824    0.0645 487.3462   388.2213
    ## 4      Ampithoid           DensityLog   0.0373    0.0843 346.4276     0.1830
    ## 5      Ampithoid           YearBinary  -0.2730     0.015 478.3483   330.2837
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0124     0.001 475.9865   141.8059
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.0172    0.0143 478.9416     1.4536
    ## 8   CanopyHeight           YearBinary  -0.1329     0.008 470.5157   275.2855
    ## 9     DensityLog    TempAnomWarm_June  -0.0122     7e-04 470.9773   265.8758
    ## 10    DensityLog MonthlyMeanTemp_June   0.1444    0.0103 471.9632   197.6298
    ## 11    DensityLog           YearBinary  -0.0515    0.0057 469.4208    80.5362
    ## 12  BladeAreaLog            Ampithoid   0.0747    0.0678 143.4303     1.0781
    ## 13  BladeAreaLog         CanopyHeight   0.8297    0.0945  38.7978    60.0340
    ## 14  BladeAreaLog           DensityLog  -0.1546    0.0619  18.9537     4.5097
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0039     0.003 124.5016     1.4408
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0148    0.0328  59.4434     0.1661
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2387    0.0186 475.6081   164.6257
    ## 18  BladeAreaLog           YearBinary   0.0630    0.0284 128.9959     4.3340
    ## 19 LesionAreaLog         BladeAreaLog   0.7739    0.1383 482.5479    31.0340
    ## 20 LesionAreaLog            Ampithoid  -0.4857    0.2159 148.0764     4.7218
    ## 21 LesionAreaLog         CanopyHeight  -0.6548    0.3415  72.1869     3.3064
    ## 22 LesionAreaLog           DensityLog  -0.5657     0.227  28.1271     5.2485
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0289    0.0096 143.3481     8.4218
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.3519    0.1085  81.9863     9.3917
    ## 25 LesionAreaLog    TidalHeightBinary   0.1207    0.0654 476.7950     3.3919
    ## 26 LesionAreaLog           YearBinary  -0.1380    0.0918 134.4646     2.0891
    ## 27  ~~DensityLog       ~~CanopyHeight   0.0602         - 494.0000     1.3358
    ##    P.Value Std.Estimate    
    ## 1   0.9004      -0.0088    
    ## 2   0.0000      -0.4796 ***
    ## 3   0.0000      -1.7108 ***
    ## 4   0.6691       0.0819    
    ## 5   0.0000      -0.5619 ***
    ## 6   0.0000      -0.3266 ***
    ## 7   0.2285       0.0422    
    ## 8   0.0000      -0.2051 ***
    ## 9   0.0000      -0.1950 ***
    ## 10  0.0000       0.2147 ***
    ## 11  0.0000      -0.0483 ***
    ## 12  0.3009       0.0454    
    ## 13  0.0000       0.6731 ***
    ## 14  0.0471      -0.2065   *
    ## 15  0.2323       0.0822    
    ## 16  0.6850      -0.0294    
    ## 17  0.0000      -0.3126 ***
    ## 18  0.0393       0.0789   *
    ## 19  0.0000       0.4147 ***
    ## 20  0.0314      -0.1583   *
    ## 21  0.0732      -0.2846    
    ## 22  0.0297      -0.4047   *
    ## 23  0.0043      -0.3294  **
    ## 24  0.0030       0.3743  **
    ## 25  0.0661       0.0847    
    ## 26  0.1507      -0.0925    
    ## 27  0.0911       0.0602

## SEM Conclusions

-   Epifauna abundance influences disease  
-   Lacuna and ampithoid abundance show contrasting effects, could be
    explained by contrasting mechanisms  
-   Limited mediation of temperature effects

# Partial Residuals

Use models that include all cases (not complete case data), plot partial
residuals of epifauna, lacuana, and ampithoid for disease response
variables (prevalence and lesion area)

## Prevalence

``` r
# approach from Duffy 2015 - doesn't work for binomial glmm
# create list of models for prev ~ epifauna
# pre_epi_model <- list(glm(Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
#           TempAnomWarm_June + MonthlyMeanTemp_June + 
#           TidalHeightBinary + YearBinary, 
#           # random=~1|Region/Meadow,
#         data=dis2,
#         family="binomial"))
# try <- partialResid(Prevalence ~ Epifauna, pre_epi_model)
# PrevalenceResidY <- resid(glmer(Prevalence ~ BladeAreaLog + CanopyHeight + DensityLog + 
#           TempAnomWarm_June + MonthlyMeanTemp_June + 
#           TidalHeightBinary + YearBinary +
#             (1|Region) +(1|Meadow), 
#         data=dis2,
#         family="binomial"))
# EpifaunaResidX <- resid(lmer(Epifauna ~ BladeAreaLog + CanopyHeight + DensityLog + 
#           TempAnomWarm_June + MonthlyMeanTemp_June + 
#           TidalHeightBinary + YearBinary +
#             (1|Region) +(1|Meadow), 
#         data=dis2))
# plot(exp(PrevalenceResidY)~EpifaunaResidX)
# alternative is using the effects package
prev_epi_1 <- glmer(Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis2,
        family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0115979 (tol = 0.002, component 1)

``` r
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
# will want to try and get the year/site means plotted onto this plot, but can hold off for now?
prev_lac_1 <- glmer(Prevalence ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis3,
        family="binomial")
plot(predictorEffect("Lacuna", prev_lac_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

``` r
prev_amp_1 <- glmer(Prevalence ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis4,
        family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00467413 (tol = 0.002, component 1)

``` r
plot(predictorEffect("Ampithoid", prev_amp_1,partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->

## Lesion

### Epifauna

``` r
# have to re-specify the model using lme() because partialResid() doens't work with lmer()
les_epi_model <- list(lesion=lme(LesionAreaLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary, 
          random=~1|Region/Meadow,
        data=les2))
les_epi_resid <- partialResid(LesionAreaLog ~ Epifauna, modelList=les_epi_model)
les_epi_resid_df <- cbind(les2, EpifaunaResidX=les_epi_resid$xresid, LesionResidY=les_epi_resid$yresid)
les_epi_resid_summ <- les_epi_resid_df %>%
  group_by(Year, Meadow)%>%
  summarise(EpifaunaResidX_Meadow=mean(EpifaunaResidX),
            LesionResidY_Meadow=mean(LesionResidY))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

``` r
ggplot()+
  geom_point(data=les_epi_resid,aes(x=xresid,y=yresid),size=2.5,alpha=0.75,col="grey50")+
  stat_smooth(data=les_epi_resid,aes(x=xresid,y=yresid),method="lm",se=F,lwd=1,col="black")+
  geom_point(data=les_epi_resid_summ, aes(x=EpifaunaResidX_Meadow, y=LesionResidY_Meadow, color=as.factor(Year)),
             size=3)+
  scale_color_discrete(name="Year")+
  labs(x="Log Epifauna | others",y="Log Lesion Area | others",nrow=2)+
  theme_bw(base_size=18)+
  guides(col=guide_legend(ncol=2))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
```

    ## `geom_smooth()` using formula 'y ~ x'

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
# coefs(les_epi_1)
# plot(les_epi_resid)
# les_epi_resid <- partialResid(LesionAreaLog ~ Epifauna + (1|Meadow) + (1|Region), modelList = sem_les_epi)
# les_yresid <- resid(lmer(LesionAreaLog ~ BladeAreaLog + CanopyHeight + DensityLog + 
#           TempAnomWarm_June + MonthlyMeanTemp_June + 
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=les2))
# les_xresid <- resid(lmer(Epifauna ~ BladeAreaLog + CanopyHeight + DensityLog + 
#           TempAnomWarm_June + MonthlyMeanTemp_June + 
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=les2))
# plot(les_xresid, les_yresid)
```

``` r
# re-set sem as all lme()
sem_les_epi_1 <- psem(
  lme(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary,
      random=~1|Region/Meadow,
       data=les2),
  lme(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary,
      random=~1|Region/Meadow,
       data=les2),
  lme(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary,
      random=~1|Region/Meadow,
       data=les2),
    lme(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary,
      random=~1|Region/Meadow,
       data=les2),
  lme(LesionAreaLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary,  
      random=~1|Region/Meadow,
        data=les2),
  DensityLog%~~%CanopyHeight
)

les_epi_resid_1 <- partialResid(LesionAreaLog ~ Epifauna, modelList=sem_les_epi_1)
les_epi_resid1_df <- cbind(les2, EpifaunaResidX=les_epi_resid_1$xresid, LesionResidY=les_epi_resid_1$yresid)
les_epi_resid1_summ <- les_epi_resid1_df %>%
  group_by(Year, Meadow)%>%
  summarise(EpifaunaResidX_Meadow=mean(EpifaunaResidX),
            LesionResidY_Meadow=mean(LesionResidY))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

``` r
ggplot()+
  geom_point(data=les_epi_resid_1,aes(x=xresid,y=yresid),size=2.5,alpha=0.75,col="grey50")+
  stat_smooth(data=les_epi_resid_1,aes(x=xresid,y=yresid),method="lm",se=F,lwd=1,col="black")+
  geom_point(data=les_epi_resid1_summ, aes(x=EpifaunaResidX_Meadow, y=LesionResidY_Meadow, color=as.factor(Year)),
             size=3)+
  scale_color_discrete(name="Year")+
  labs(x="Log Epifauna | others",y="Log Lesion Area | others",nrow=2)+
  theme_bw(base_size=18)+
  guides(col=guide_legend(ncol=2))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
```

    ## `geom_smooth()` using formula 'y ~ x'

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Lacuna

``` r
# have to re-specify the model using lme() because partialResid() doens't work with lmer()
les_lac_model <- list(lesion=lme(LesionAreaLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary, 
          random=~1|Region/Meadow,
        data=les3))
les_lac_resid <- partialResid(LesionAreaLog ~ Lacuna, modelList=les_lac_model)
les_lac_resid_df <- cbind(les3, LacunaResidX=les_lac_resid$xresid, LesionResidY=les_lac_resid$yresid)
les_lac_resid_summ <- les_lac_resid_df %>%
  group_by(Year, Meadow)%>%
  summarise(LacunaResidX_Meadow=mean(LacunaResidX),
            LesionResidY_Meadow=mean(LesionResidY))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

``` r
ggplot()+
  geom_point(data=les_lac_resid,aes(x=xresid,y=yresid),size=2.5,alpha=0.75,col="grey50")+
  stat_smooth(data=les_lac_resid,aes(x=xresid,y=yresid),method="lm",se=F,lwd=1,col="black")+
  geom_point(data=les_lac_resid_summ, aes(x=LacunaResidX_Meadow, y=LesionResidY_Meadow, color=as.factor(Year)),
             size=3)+
  scale_color_discrete(name="Year")+
  labs(x="Log Lacuna | others",y="Log Lesion Area | others",nrow=2)+
  theme_bw(base_size=18)+
  guides(col=guide_legend(ncol=2))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
```

    ## `geom_smooth()` using formula 'y ~ x'

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### Ampithoid

``` r
# have to re-specify the model using lme() because partialResid() doens't work with lmer()
les_amp_model <- list(lesion=lme(LesionAreaLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary, 
          random=~1|Region/Meadow,
        data=les4))
les_amp_resid <- partialResid(LesionAreaLog ~ Ampithoid, modelList=les_amp_model)
les_amp_resid_df <- cbind(les4, AmpithoidResidX=les_amp_resid$xresid, LesionResidY=les_amp_resid$yresid)
les_amp_resid_summ <- les_amp_resid_df %>%
  group_by(Year, Meadow)%>%
  summarise(AmpithoidResidX_Meadow=mean(AmpithoidResidX),
            LesionResidY_Meadow=mean(LesionResidY))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

``` r
ggplot()+
  geom_point(data=les_amp_resid,aes(x=xresid,y=yresid),size=2.5,alpha=0.75,col="grey50")+
  stat_smooth(data=les_amp_resid,aes(x=xresid,y=yresid),method="lm",se=F,lwd=1,col="black")+
  geom_point(data=les_amp_resid_summ, aes(x=AmpithoidResidX_Meadow, y=LesionResidY_Meadow, color=as.factor(Year)),
             size=3)+
  scale_color_discrete(name="Year")+
  labs(x="Log Ampithoid | others",y="Log Lesion Area | others",nrow=2)+
  theme_bw(base_size=18)+
  guides(col=guide_legend(ncol=2))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
```

    ## `geom_smooth()` using formula 'y ~ x'

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

Alternatively, use the effects package and look at partial correlations
using effect plot

``` r
les_epi_1 <- lmer(LesionAreaLog ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les2)
plot(predictorEffect("Epifauna", les_epi_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
les_lac_1 <- lmer(LesionAreaLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les3)
plot(predictorEffect("Lacuna", les_lac_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->

``` r
les_amp_1 <- lmer(LesionAreaLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les4)
plot(predictorEffect("Ampithoid", les_amp_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->
