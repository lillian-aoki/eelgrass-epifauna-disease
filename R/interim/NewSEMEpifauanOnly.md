Community Interaction SEM results
================

# SEMs comparing epifauna vs lacuna vs ampithoid

``` r
# data ###
dis <- read_csv("data/epiphyte_SEM_data.csv")
```

    ## Rows: 1350 Columns: 43
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): Meter, SampleId, Region, SiteCode, TidalHeight, GrazingScars, Bro...
    ## dbl  (33): Transect, Blade, LongestBladeLength, LongestBladeWidth, SheathLen...
    ## date  (1): SampleDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dis$BladeAreaLog <- log10(dis$BladeArea)
# use the full data set without subsetting because the SEM no longer includes epiphytes or grazing
dis1 <- select(dis, c(Epifauna, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
                      DensityLog, YearBinary, Year, Meadow, Region,BladeAreaLog, TidalHeightBinary, 
                      Prevalence, LesionArea, Lacuna, Ampithoid, Idoteid, Richness))
```

## Prevalence + Epifauna

``` r
dis2 <- select(dis1, -c(Lacuna, Ampithoid, Idoteid))
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
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
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
    ##       Epifauna    TempAnomWarm_June  -0.0012     0.001 1313.4403     1.4527
    ##       Epifauna MonthlyMeanTemp_June  -0.2175    0.0129 1296.1566   278.6897
    ##       Epifauna         CanopyHeight  -0.6985    0.0319 1334.7635   478.3563
    ##       Epifauna           DensityLog  -0.1249    0.0317 1339.6234    15.4467
    ##       Epifauna           YearBinary  -0.1237    0.0074 1320.7258   278.1730
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog             Epifauna  -0.0779    0.0423   61.2130     3.1848
    ##   BladeAreaLog         CanopyHeight   0.6381    0.0645   84.7570    92.9980
    ##   BladeAreaLog           DensityLog  -0.1653    0.0565  102.1447     7.7730
    ##   BladeAreaLog    TempAnomWarm_June  -0.0038    0.0018   83.0439     3.9361
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0023     0.019   19.8256     0.0116
    ##   BladeAreaLog    TidalHeightBinary  -0.2393    0.0133 1315.4916   325.0699
    ##   BladeAreaLog           YearBinary   0.0470    0.0188  799.0265     6.1270
    ##     Prevalence         BladeAreaLog   1.7127    0.2897 1348.0000     5.9126
    ##     Prevalence             Epifauna   1.2961    0.4339 1348.0000     2.9871
    ##     Prevalence         CanopyHeight   1.8753    0.6847 1348.0000     2.7389
    ##     Prevalence           DensityLog   2.6773    0.5724 1348.0000     4.6770
    ##     Prevalence    TempAnomWarm_June   0.0229    0.0195 1348.0000     1.1779
    ##     Prevalence MonthlyMeanTemp_June   0.4077    0.2356 1348.0000     1.7302
    ##     Prevalence    TidalHeightBinary   0.6186    0.1416 1348.0000     4.3671
    ##     Prevalence           YearBinary   0.5034    0.1807 1348.0000     2.7854
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.2283      -0.0222    
    ##    0.0000      -0.7788 ***
    ##    0.0000      -0.5219 ***
    ##    0.0001      -0.1308 ***
    ##    0.0000      -0.1314 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.0793       -0.086    
    ##    0.0000       0.5266 ***
    ##    0.0063      -0.1911  **
    ##    0.0506        -0.08    
    ##    0.9153        0.009    
    ##    0.0000      -0.2826 ***
    ##    0.0135       0.0552   *
    ##    0.0000            - ***
    ##    0.0028            -  **
    ##    0.0062            -  **
    ##    0.0000            - ***
    ##    0.2388            -    
    ##    0.0836            -    
    ##    0.0000            - ***
    ##    0.0053            -  **
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.19        0.99
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.49        0.69
    ##     Prevalence  delta     0.14        0.50

Model passes global fit. Epifauna is positive and significant predictor
for Prevalence.

``` r
coefs(sem_prev_epi)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0012     0.001 1313.4403     1.4527
    ## 2      Epifauna MonthlyMeanTemp_June  -0.2175    0.0129 1296.1566   278.6897
    ## 3      Epifauna         CanopyHeight  -0.6985    0.0319 1334.7635   478.3563
    ## 4      Epifauna           DensityLog  -0.1249    0.0317 1339.6234    15.4467
    ## 5      Epifauna           YearBinary  -0.1237    0.0074 1320.7258   278.1730
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog             Epifauna  -0.0779    0.0423   61.2130     3.1848
    ## 13 BladeAreaLog         CanopyHeight   0.6381    0.0645   84.7570    92.9980
    ## 14 BladeAreaLog           DensityLog  -0.1653    0.0565  102.1447     7.7730
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0038    0.0018   83.0439     3.9361
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0023     0.019   19.8256     0.0116
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2393    0.0133 1315.4916   325.0699
    ## 18 BladeAreaLog           YearBinary   0.0470    0.0188  799.0265     6.1270
    ## 19   Prevalence         BladeAreaLog   1.7127    0.2897 1348.0000     5.9126
    ## 20   Prevalence             Epifauna   1.2961    0.4339 1348.0000     2.9871
    ## 21   Prevalence         CanopyHeight   1.8753    0.6847 1348.0000     2.7389
    ## 22   Prevalence           DensityLog   2.6773    0.5724 1348.0000     4.6770
    ## 23   Prevalence    TempAnomWarm_June   0.0229    0.0195 1348.0000     1.1779
    ## 24   Prevalence MonthlyMeanTemp_June   0.4077    0.2356 1348.0000     1.7302
    ## 25   Prevalence    TidalHeightBinary   0.6186    0.1416 1348.0000     4.3671
    ## 26   Prevalence           YearBinary   0.5034    0.1807 1348.0000     2.7854
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.2283      -0.0222    
    ## 2   0.0000      -0.7788 ***
    ## 3   0.0000      -0.5219 ***
    ## 4   0.0001      -0.1308 ***
    ## 5   0.0000      -0.1314 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.0793      -0.0860    
    ## 13  0.0000       0.5266 ***
    ## 14  0.0063      -0.1911  **
    ## 15  0.0506      -0.0800    
    ## 16  0.9153       0.0090    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0135       0.0552   *
    ## 19  0.0000       0.3208 ***
    ## 20  0.0028       0.2682  **
    ## 21  0.0062       0.2899  **
    ## 22  0.0000       0.5798 ***
    ## 23  0.2388       0.0896    
    ## 24  0.0836       0.3021    
    ## 25  0.0000       0.1368 ***
    ## 26  0.0053       0.1107  **
    ## 27  0.2902      -0.0151

## prev + lac

``` r
dis3 <- select(dis1, -c(Epifauna, Ampithoid, Idoteid))
# dis3 <- dis3[-which(is.na(dis3$Lacuna)),] # omit missing Lacuna values
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
    ##  90.002   324.289
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 1315.003          0  0.9994 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.002 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##         Lacuna    TempAnomWarm_June   0.0096    0.0019 1222.5897    25.3195
    ##         Lacuna MonthlyMeanTemp_June   0.1455    0.0252 1144.0136    32.6768
    ##         Lacuna         CanopyHeight   0.7276    0.0624 1339.7614   135.4265
    ##         Lacuna           DensityLog   0.0297    0.0618 1331.2031     0.2284
    ##         Lacuna           YearBinary  -0.1131    0.0145 1324.0281    60.3761
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog               Lacuna  -0.0071    0.0244   85.0615     0.0781
    ##   BladeAreaLog         CanopyHeight   0.6953    0.0574   83.9902   137.8932
    ##   BladeAreaLog           DensityLog  -0.1198    0.0532   86.4885     4.5951
    ##   BladeAreaLog    TempAnomWarm_June  -0.0025    0.0017   66.8550     1.8969
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0030    0.0194   17.6580     0.0194
    ##   BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.3605   324.8111
    ##   BladeAreaLog           YearBinary   0.0569    0.0185 1019.7124     9.3474
    ##     Prevalence         BladeAreaLog   1.7002    0.2874 1348.0000     5.9156
    ##     Prevalence               Lacuna   1.0583    0.2137 1348.0000     4.9529
    ##     Prevalence         CanopyHeight   0.7064    0.5374 1348.0000     1.3144
    ##     Prevalence           DensityLog   1.7893    0.4612 1348.0000     3.8792
    ##     Prevalence    TempAnomWarm_June  -0.0059    0.0147 1348.0000    -0.4036
    ##     Prevalence MonthlyMeanTemp_June   0.2833    0.1493 1348.0000     1.8979
    ##     Prevalence    TidalHeightBinary   0.6204    0.1417 1348.0000     4.3773
    ##     Prevalence           YearBinary   0.5117    0.1708 1348.0000     2.9957
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.0000        0.107 ***
    ##    0.0000       0.3069 ***
    ##    0.0000       0.3202 ***
    ##    0.6328       0.0183    
    ##    0.0000      -0.0707 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.7806      -0.0134    
    ##    0.0000       0.5738 ***
    ##    0.0349      -0.1385   *
    ##    0.1730      -0.0525    
    ##    0.8909       0.0119    
    ##    0.0000      -0.2826 ***
    ##    0.0023       0.0668  **
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.1887            -    
    ##    0.0001            - ***
    ##    0.6865            -    
    ##    0.0577            -    
    ##    0.0000            - ***
    ##    0.0027            -  **
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##         Lacuna   none     0.08        0.97
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.47        0.68
    ##     Prevalence  delta     0.12        0.37

Passes global fit test Lacuna effect on Prevalence is sig and positive

``` r
coefs(sem_prev_lac)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1        Lacuna    TempAnomWarm_June   0.0096    0.0019 1222.5897    25.3195
    ## 2        Lacuna MonthlyMeanTemp_June   0.1455    0.0252 1144.0136    32.6768
    ## 3        Lacuna         CanopyHeight   0.7276    0.0624 1339.7614   135.4265
    ## 4        Lacuna           DensityLog   0.0297    0.0618 1331.2031     0.2284
    ## 5        Lacuna           YearBinary  -0.1131    0.0145 1324.0281    60.3761
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog               Lacuna  -0.0071    0.0244   85.0615     0.0781
    ## 13 BladeAreaLog         CanopyHeight   0.6953    0.0574   83.9902   137.8932
    ## 14 BladeAreaLog           DensityLog  -0.1198    0.0532   86.4885     4.5951
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0025    0.0017   66.8550     1.8969
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0030    0.0194   17.6580     0.0194
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.3605   324.8111
    ## 18 BladeAreaLog           YearBinary   0.0569    0.0185 1019.7124     9.3474
    ## 19   Prevalence         BladeAreaLog   1.7002    0.2874 1348.0000     5.9156
    ## 20   Prevalence               Lacuna   1.0583    0.2137 1348.0000     4.9529
    ## 21   Prevalence         CanopyHeight   0.7064    0.5374 1348.0000     1.3144
    ## 22   Prevalence           DensityLog   1.7893    0.4612 1348.0000     3.8792
    ## 23   Prevalence    TempAnomWarm_June  -0.0059    0.0147 1348.0000    -0.4036
    ## 24   Prevalence MonthlyMeanTemp_June   0.2833    0.1493 1348.0000     1.8979
    ## 25   Prevalence    TidalHeightBinary   0.6204    0.1417 1348.0000     4.3773
    ## 26   Prevalence           YearBinary   0.5117    0.1708 1348.0000     2.9957
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.1070 ***
    ## 2   0.0000       0.3069 ***
    ## 3   0.0000       0.3202 ***
    ## 4   0.6328       0.0183    
    ## 5   0.0000      -0.0707 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.7806      -0.0134    
    ## 13  0.0000       0.5738 ***
    ## 14  0.0349      -0.1385   *
    ## 15  0.1730      -0.0525    
    ## 16  0.8909       0.0119    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0023       0.0668  **
    ## 19  0.0000       0.3219 ***
    ## 20  0.0000       0.3758 ***
    ## 21  0.1887       0.1104    
    ## 22  0.0001       0.3917 ***
    ## 23  0.6865      -0.0234    
    ## 24  0.0577       0.2122    
    ## 25  0.0000       0.1387 ***
    ## 26  0.0027       0.1137  **
    ## 27  0.2902      -0.0151

## prev + amp

``` r
dis4 <- select(dis1, -c(Epifauna, Lacuna, Idoteid))
# dis4 <- dis4[-which(is.na(dis4$Ampithoid)),] # omit missing Ampithoid values
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
    ## Model failed to converge with max|grad| = 0.0127603 (tol = 0.002, component 1)

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
    ##  90.001   324.288
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 1315.001          0  0.9999 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.001 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##      Ampithoid    TempAnomWarm_June  -0.0106     7e-04 1274.2497   260.4888
    ##      Ampithoid MonthlyMeanTemp_June   0.1831    0.0087 1218.9434   438.1374
    ##      Ampithoid         CanopyHeight   0.0403    0.0216 1327.8117     3.4724
    ##      Ampithoid           DensityLog   0.1695    0.0214 1321.3693    62.1928
    ##      Ampithoid           YearBinary  -0.1428     0.005 1319.0629   812.6462
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog            Ampithoid   0.0424    0.0463   47.8428     0.7897
    ##   BladeAreaLog         CanopyHeight   0.7135    0.0616  161.6226   124.1445
    ##   BladeAreaLog           DensityLog  -0.1110    0.0528   93.0618     3.9206
    ##   BladeAreaLog    TempAnomWarm_June  -0.0013    0.0017   52.3421     0.5295
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0141    0.0185   15.9420     0.4823
    ##   BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.7831   324.7746
    ##   BladeAreaLog           YearBinary   0.0664    0.0208  799.2452     9.9883
    ##     Prevalence         BladeAreaLog   1.6693    0.2888 1348.0000     5.7795
    ##     Prevalence            Ampithoid   0.1411    0.5824 1348.0000     0.2422
    ##     Prevalence         CanopyHeight   0.9853     0.664 1348.0000     1.4837
    ##     Prevalence           DensityLog   2.1427    0.5672 1348.0000     3.7776
    ##     Prevalence    TempAnomWarm_June   0.0093    0.0195 1348.0000     0.4769
    ##     Prevalence MonthlyMeanTemp_June   0.3091    0.2365 1348.0000     1.3071
    ##     Prevalence    TidalHeightBinary   0.6103    0.1415 1348.0000     4.3121
    ##     Prevalence           YearBinary   0.3629     0.212 1348.0000     1.7117
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2072 ***
    ##    0.0000       0.6783 ***
    ##    0.0626       0.0311    
    ##    0.0000       0.1835 ***
    ##    0.0000       -0.157 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.3786       0.0453    
    ##    0.0000       0.5888 ***
    ##    0.0507      -0.1283    
    ##    0.4701      -0.0278    
    ##    0.4974      -0.0558    
    ##    0.0000      -0.2826 ***
    ##    0.0016        0.078  **
    ##    0.0000            - ***
    ##    0.8086            -    
    ##    0.1379            -    
    ##    0.0002            - ***
    ##    0.6334            -    
    ##    0.1912            -    
    ##    0.0000            - ***
    ##    0.0869            -    
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##      Ampithoid   none     0.35        0.99
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.51        0.66
    ##     Prevalence  delta     0.12        0.50

Passes global fit test. Ampithoid is negative for prevalence

``` r
coefs(sem_prev_amp)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1     Ampithoid    TempAnomWarm_June  -0.0106     7e-04 1274.2497   260.4888
    ## 2     Ampithoid MonthlyMeanTemp_June   0.1831    0.0087 1218.9434   438.1374
    ## 3     Ampithoid         CanopyHeight   0.0403    0.0216 1327.8117     3.4724
    ## 4     Ampithoid           DensityLog   0.1695    0.0214 1321.3693    62.1928
    ## 5     Ampithoid           YearBinary  -0.1428     0.005 1319.0629   812.6462
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog            Ampithoid   0.0424    0.0463   47.8428     0.7897
    ## 13 BladeAreaLog         CanopyHeight   0.7135    0.0616  161.6226   124.1445
    ## 14 BladeAreaLog           DensityLog  -0.1110    0.0528   93.0618     3.9206
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0013    0.0017   52.3421     0.5295
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0141    0.0185   15.9420     0.4823
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.7831   324.7746
    ## 18 BladeAreaLog           YearBinary   0.0664    0.0208  799.2452     9.9883
    ## 19   Prevalence         BladeAreaLog   1.6693    0.2888 1348.0000     5.7795
    ## 20   Prevalence            Ampithoid   0.1411    0.5824 1348.0000     0.2422
    ## 21   Prevalence         CanopyHeight   0.9853     0.664 1348.0000     1.4837
    ## 22   Prevalence           DensityLog   2.1427    0.5672 1348.0000     3.7776
    ## 23   Prevalence    TempAnomWarm_June   0.0093    0.0195 1348.0000     0.4769
    ## 24   Prevalence MonthlyMeanTemp_June   0.3091    0.2365 1348.0000     1.3071
    ## 25   Prevalence    TidalHeightBinary   0.6103    0.1415 1348.0000     4.3121
    ## 26   Prevalence           YearBinary   0.3629     0.212 1348.0000     1.7117
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2072 ***
    ## 2   0.0000       0.6783 ***
    ## 3   0.0626       0.0311    
    ## 4   0.0000       0.1835 ***
    ## 5   0.0000      -0.1570 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.3786       0.0453    
    ## 13  0.0000       0.5888 ***
    ## 14  0.0507      -0.1283    
    ## 15  0.4701      -0.0278    
    ## 16  0.4974      -0.0558    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0016       0.0780  **
    ## 19  0.0000       0.3128 ***
    ## 20  0.8086       0.0282    
    ## 21  0.1379       0.1523    
    ## 22  0.0002       0.4642 ***
    ## 23  0.6334       0.0362    
    ## 24  0.1912       0.2291    
    ## 25  0.0000       0.1350 ***
    ## 26  0.0869       0.0798    
    ## 27  0.2902      -0.0151

## prev + idoteid

``` r
dis5 <- select(dis1, -c(Epifauna, Lacuna, Ampithoid))
# dis5 <- dis5[-which(is.na(dis5$Idoteid)),] # omit missing Ampithoid values
dis5 <- na.omit(dis5) # remove any other NAs (missing blade area for one)

sem_prev_ido <- psem(
  lmer(Idoteid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis5),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis5),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis5),
  lmer(BladeAreaLog ~ Idoteid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis5),
  glmer(Prevalence ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis5,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00426657 (tol = 0.002, component 1)

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
    ##   Prevalence ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.001   324.288
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
    ##        Idoteid ~ TidalHeightBinary + ...      coef 1315.000          0  0.9999 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.001 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Idoteid    TempAnomWarm_June   0.0255     7e-04 1325.2482  1333.5838
    ##        Idoteid MonthlyMeanTemp_June  -0.4383    0.0093 1327.3111  2216.4454
    ##        Idoteid         CanopyHeight   0.2218    0.0228 1325.8625    94.6786
    ##        Idoteid           DensityLog   0.4712    0.0227 1328.5017   430.5861
    ##        Idoteid           YearBinary  -0.0771    0.0053 1318.0398   213.0106
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog              Idoteid  -0.0171    0.0423   51.7343     0.1514
    ##   BladeAreaLog         CanopyHeight   0.7008    0.0604  105.1779   124.7151
    ##   BladeAreaLog           DensityLog  -0.1109    0.0543   91.2916     3.7222
    ##   BladeAreaLog    TempAnomWarm_June  -0.0021    0.0017   40.7949     1.3003
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0059     0.019   13.1639     0.0778
    ##   BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.6565   324.7969
    ##   BladeAreaLog           YearBinary   0.0570    0.0179  911.0685     9.9699
    ##     Prevalence         BladeAreaLog   1.6736    0.2895 1348.0000     5.7808
    ##     Prevalence              Idoteid   0.6837    0.4696 1348.0000     1.4560
    ##     Prevalence         CanopyHeight   0.7009    0.6424 1348.0000     1.0911
    ##     Prevalence           DensityLog   1.9384    0.5754 1348.0000     3.3689
    ##     Prevalence    TempAnomWarm_June  -0.0021    0.0221 1348.0000    -0.0955
    ##     Prevalence MonthlyMeanTemp_June   0.5006    0.2988 1348.0000     1.6756
    ##     Prevalence    TidalHeightBinary   0.6095    0.1415 1348.0000     4.3085
    ##     Prevalence           YearBinary   0.3608    0.1719 1348.0000     2.0995
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.0000       0.5745 ***
    ##    0.0000      -1.8733 ***
    ##    0.0000       0.1978 ***
    ##    0.0000       0.5885 ***
    ##    0.0000      -0.0978 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.6988      -0.0158    
    ##    0.0000       0.5784 ***
    ##    0.0568      -0.1282    
    ##    0.2608      -0.0428    
    ##    0.7847      -0.0232    
    ##    0.0000      -0.2826 ***
    ##    0.0016       0.0669  **
    ##    0.0000            - ***
    ##    0.1454            -    
    ##    0.2752            -    
    ##    0.0008            - ***
    ##    0.9239            -    
    ##    0.0938            -    
    ##    0.0000            - ***
    ##    0.0358            -   *
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##        Idoteid   none     0.23        1.00
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.49        0.67
    ##     Prevalence  delta     0.14        0.55

Passes global fit test. Idoteid is not significant for prevalence
(coefficient is positive).

``` r
coefs(sem_prev_ido)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1       Idoteid    TempAnomWarm_June   0.0255     7e-04 1325.2482  1333.5838
    ## 2       Idoteid MonthlyMeanTemp_June  -0.4383    0.0093 1327.3111  2216.4454
    ## 3       Idoteid         CanopyHeight   0.2218    0.0228 1325.8625    94.6786
    ## 4       Idoteid           DensityLog   0.4712    0.0227 1328.5017   430.5861
    ## 5       Idoteid           YearBinary  -0.0771    0.0053 1318.0398   213.0106
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog              Idoteid  -0.0171    0.0423   51.7343     0.1514
    ## 13 BladeAreaLog         CanopyHeight   0.7008    0.0604  105.1779   124.7151
    ## 14 BladeAreaLog           DensityLog  -0.1109    0.0543   91.2916     3.7222
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0021    0.0017   40.7949     1.3003
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0059     0.019   13.1639     0.0778
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.6565   324.7969
    ## 18 BladeAreaLog           YearBinary   0.0570    0.0179  911.0685     9.9699
    ## 19   Prevalence         BladeAreaLog   1.6736    0.2895 1348.0000     5.7808
    ## 20   Prevalence              Idoteid   0.6837    0.4696 1348.0000     1.4560
    ## 21   Prevalence         CanopyHeight   0.7009    0.6424 1348.0000     1.0911
    ## 22   Prevalence           DensityLog   1.9384    0.5754 1348.0000     3.3689
    ## 23   Prevalence    TempAnomWarm_June  -0.0021    0.0221 1348.0000    -0.0955
    ## 24   Prevalence MonthlyMeanTemp_June   0.5006    0.2988 1348.0000     1.6756
    ## 25   Prevalence    TidalHeightBinary   0.6095    0.1415 1348.0000     4.3085
    ## 26   Prevalence           YearBinary   0.3608    0.1719 1348.0000     2.0995
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.5745 ***
    ## 2   0.0000      -1.8733 ***
    ## 3   0.0000       0.1978 ***
    ## 4   0.0000       0.5885 ***
    ## 5   0.0000      -0.0978 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.6988      -0.0158    
    ## 13  0.0000       0.5784 ***
    ## 14  0.0568      -0.1282    
    ## 15  0.2608      -0.0428    
    ## 16  0.7847      -0.0232    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0016       0.0669  **
    ## 19  0.0000       0.3138 ***
    ## 20  0.1454       0.1187    
    ## 21  0.2752       0.1085    
    ## 22  0.0008       0.4202 ***
    ## 23  0.9239      -0.0083    
    ## 24  0.0938       0.3713    
    ## 25  0.0000       0.1349 ***
    ## 26  0.0358       0.0794   *
    ## 27  0.2902      -0.0151

``` r
# multigroup(sem_prev_ido, group = "Region")
```

## prev + richness

``` r
dis1$RichnessLog <- log10(dis1$Richness)
dis1 <- na.omit(dis1)
sem_prev_rich <- psem(
  lmer(RichnessLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis1),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis1),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis1),
  lmer(BladeAreaLog ~ RichnessLog + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis1),
  glmer(Prevalence ~ BladeAreaLog + RichnessLog + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis1,
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
    ##   RichnessLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ RichnessLog + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + RichnessLog + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  90.001   324.288
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
    ##    RichnessLog ~ TidalHeightBinary + ...      coef 1315.002          0  0.9999 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.001 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##    RichnessLog    TempAnomWarm_June   0.0006     4e-04 1301.2331     1.8174
    ##    RichnessLog MonthlyMeanTemp_June  -0.0517     0.006 1269.0429    74.1990
    ##    RichnessLog         CanopyHeight   0.2453    0.0147 1337.3156   278.1037
    ##    RichnessLog           DensityLog   0.0098    0.0146 1336.4851     0.4474
    ##    RichnessLog           YearBinary  -0.0171    0.0034 1325.1089    24.8935
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog          RichnessLog  -0.0214     0.115  177.6912     0.0329
    ##   BladeAreaLog         CanopyHeight   0.6969     0.059   78.2697   129.6904
    ##   BladeAreaLog           DensityLog  -0.1181     0.053   89.2088     4.4544
    ##   BladeAreaLog    TempAnomWarm_June  -0.0024    0.0017   75.5785     1.7183
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0016    0.0186   17.6724     0.0057
    ##   BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.3042   324.7047
    ##   BladeAreaLog           YearBinary   0.0572     0.018  982.0797     9.8398
    ##     Prevalence         BladeAreaLog   1.6697    0.2887 1348.0000     5.7843
    ##     Prevalence          RichnessLog   0.1962    1.3461 1348.0000     0.1458
    ##     Prevalence         CanopyHeight   0.9115     0.652 1348.0000     1.3979
    ##     Prevalence           DensityLog   2.1345    0.5644 1348.0000     3.7816
    ##     Prevalence    TempAnomWarm_June   0.0077    0.0179 1348.0000     0.4268
    ##     Prevalence MonthlyMeanTemp_June   0.3398    0.2091 1348.0000     1.6253
    ##     Prevalence    TidalHeightBinary   0.6104    0.1415 1348.0000     4.3148
    ##     Prevalence           YearBinary   0.3369    0.1739 1348.0000     1.9377
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.1779       0.0456    
    ##    0.0000      -0.7357 ***
    ##    0.0000       0.7278 ***
    ##    0.5037       0.0407    
    ##    0.0000      -0.0721 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.8563       -0.006    
    ##    0.0000       0.5751 ***
    ##    0.0376      -0.1365   *
    ##    0.1939        -0.05    
    ##    0.9407      -0.0061    
    ##    0.0000      -0.2826 ***
    ##    0.0018       0.0671  **
    ##    0.0000            - ***
    ##    0.8841            -    
    ##    0.1621            -    
    ##    0.0002            - ***
    ##    0.6695            -    
    ##    0.1041            -    
    ##    0.0000            - ***
    ##    0.0527            -    
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##    RichnessLog   none     0.16        0.98
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.48        0.67
    ##     Prevalence  delta     0.12        0.50

Passes global fit but richness is not significant for disease
prevalence.

``` r
coefs(sem_prev_rich)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1   RichnessLog    TempAnomWarm_June   0.0006     4e-04 1301.2331     1.8174
    ## 2   RichnessLog MonthlyMeanTemp_June  -0.0517     0.006 1269.0429    74.1990
    ## 3   RichnessLog         CanopyHeight   0.2453    0.0147 1337.3156   278.1037
    ## 4   RichnessLog           DensityLog   0.0098    0.0146 1336.4851     0.4474
    ## 5   RichnessLog           YearBinary  -0.0171    0.0034 1325.1089    24.8935
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog          RichnessLog  -0.0214     0.115  177.6912     0.0329
    ## 13 BladeAreaLog         CanopyHeight   0.6969     0.059   78.2697   129.6904
    ## 14 BladeAreaLog           DensityLog  -0.1181     0.053   89.2088     4.4544
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0024    0.0017   75.5785     1.7183
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0016    0.0186   17.6724     0.0057
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.3042   324.7047
    ## 18 BladeAreaLog           YearBinary   0.0572     0.018  982.0797     9.8398
    ## 19   Prevalence         BladeAreaLog   1.6697    0.2887 1348.0000     5.7843
    ## 20   Prevalence          RichnessLog   0.1962    1.3461 1348.0000     0.1458
    ## 21   Prevalence         CanopyHeight   0.9115     0.652 1348.0000     1.3979
    ## 22   Prevalence           DensityLog   2.1345    0.5644 1348.0000     3.7816
    ## 23   Prevalence    TempAnomWarm_June   0.0077    0.0179 1348.0000     0.4268
    ## 24   Prevalence MonthlyMeanTemp_June   0.3398    0.2091 1348.0000     1.6253
    ## 25   Prevalence    TidalHeightBinary   0.6104    0.1415 1348.0000     4.3148
    ## 26   Prevalence           YearBinary   0.3369    0.1739 1348.0000     1.9377
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.1779       0.0456    
    ## 2   0.0000      -0.7357 ***
    ## 3   0.0000       0.7278 ***
    ## 4   0.5037       0.0407    
    ## 5   0.0000      -0.0721 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.8563      -0.0060    
    ## 13  0.0000       0.5751 ***
    ## 14  0.0376      -0.1365   *
    ## 15  0.1939      -0.0500    
    ## 16  0.9407      -0.0061    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0018       0.0671  **
    ## 19  0.0000       0.3133 ***
    ## 20  0.8841       0.0102    
    ## 21  0.1621       0.1411    
    ## 22  0.0002       0.4630 ***
    ## 23  0.6695       0.0300    
    ## 24  0.1041       0.2522    
    ## 25  0.0000       0.1352 ***
    ## 26  0.0527       0.0742    
    ## 27  0.2902      -0.0151

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
    ##  90.002   324.289
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
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
    ##       Epifauna    TempAnomWarm_June  -0.0012     0.001 1313.4403     1.4527
    ##       Epifauna MonthlyMeanTemp_June  -0.2175    0.0129 1296.1566   278.6897
    ##       Epifauna         CanopyHeight  -0.6985    0.0319 1334.7635   478.3563
    ##       Epifauna           DensityLog  -0.1249    0.0317 1339.6234    15.4467
    ##       Epifauna           YearBinary  -0.1237    0.0074 1320.7258   278.1730
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog             Epifauna  -0.0779    0.0423   61.2130     3.1848
    ##   BladeAreaLog         CanopyHeight   0.6381    0.0645   84.7570    92.9980
    ##   BladeAreaLog           DensityLog  -0.1653    0.0565  102.1447     7.7730
    ##   BladeAreaLog    TempAnomWarm_June  -0.0038    0.0018   83.0439     3.9361
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0023     0.019   19.8256     0.0116
    ##   BladeAreaLog    TidalHeightBinary  -0.2393    0.0133 1315.4916   325.0699
    ##   BladeAreaLog           YearBinary   0.0470    0.0188  799.0265     6.1270
    ##     Prevalence         BladeAreaLog   1.7127    0.2897 1348.0000     5.9126
    ##     Prevalence             Epifauna   1.2961    0.4339 1348.0000     2.9871
    ##     Prevalence         CanopyHeight   1.8753    0.6847 1348.0000     2.7389
    ##     Prevalence           DensityLog   2.6773    0.5724 1348.0000     4.6770
    ##     Prevalence    TempAnomWarm_June   0.0229    0.0195 1348.0000     1.1779
    ##     Prevalence MonthlyMeanTemp_June   0.4077    0.2356 1348.0000     1.7302
    ##     Prevalence    TidalHeightBinary   0.6186    0.1416 1348.0000     4.3671
    ##     Prevalence           YearBinary   0.5034    0.1807 1348.0000     2.7854
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.2283      -0.0222    
    ##    0.0000      -0.7788 ***
    ##    0.0000      -0.5219 ***
    ##    0.0001      -0.1308 ***
    ##    0.0000      -0.1314 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.0793       -0.086    
    ##    0.0000       0.5266 ***
    ##    0.0063      -0.1911  **
    ##    0.0506        -0.08    
    ##    0.9153        0.009    
    ##    0.0000      -0.2826 ***
    ##    0.0135       0.0552   *
    ##    0.0000            - ***
    ##    0.0028            -  **
    ##    0.0062            -  **
    ##    0.0000            - ***
    ##    0.2388            -    
    ##    0.0836            -    
    ##    0.0000            - ***
    ##    0.0053            -  **
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.19        0.99
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.49        0.69
    ##     Prevalence  delta     0.14        0.50

Passes global fit test. Epifauna is positive and significant for
prevalence.

Standardized coefs of sem:

``` r
coefs(sem_prev_epi_cc)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0012     0.001 1313.4403     1.4527
    ## 2      Epifauna MonthlyMeanTemp_June  -0.2175    0.0129 1296.1566   278.6897
    ## 3      Epifauna         CanopyHeight  -0.6985    0.0319 1334.7635   478.3563
    ## 4      Epifauna           DensityLog  -0.1249    0.0317 1339.6234    15.4467
    ## 5      Epifauna           YearBinary  -0.1237    0.0074 1320.7258   278.1730
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog             Epifauna  -0.0779    0.0423   61.2130     3.1848
    ## 13 BladeAreaLog         CanopyHeight   0.6381    0.0645   84.7570    92.9980
    ## 14 BladeAreaLog           DensityLog  -0.1653    0.0565  102.1447     7.7730
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0038    0.0018   83.0439     3.9361
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0023     0.019   19.8256     0.0116
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2393    0.0133 1315.4916   325.0699
    ## 18 BladeAreaLog           YearBinary   0.0470    0.0188  799.0265     6.1270
    ## 19   Prevalence         BladeAreaLog   1.7127    0.2897 1348.0000     5.9126
    ## 20   Prevalence             Epifauna   1.2961    0.4339 1348.0000     2.9871
    ## 21   Prevalence         CanopyHeight   1.8753    0.6847 1348.0000     2.7389
    ## 22   Prevalence           DensityLog   2.6773    0.5724 1348.0000     4.6770
    ## 23   Prevalence    TempAnomWarm_June   0.0229    0.0195 1348.0000     1.1779
    ## 24   Prevalence MonthlyMeanTemp_June   0.4077    0.2356 1348.0000     1.7302
    ## 25   Prevalence    TidalHeightBinary   0.6186    0.1416 1348.0000     4.3671
    ## 26   Prevalence           YearBinary   0.5034    0.1807 1348.0000     2.7854
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.2283      -0.0222    
    ## 2   0.0000      -0.7788 ***
    ## 3   0.0000      -0.5219 ***
    ## 4   0.0001      -0.1308 ***
    ## 5   0.0000      -0.1314 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.0793      -0.0860    
    ## 13  0.0000       0.5266 ***
    ## 14  0.0063      -0.1911  **
    ## 15  0.0506      -0.0800    
    ## 16  0.9153       0.0090    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0135       0.0552   *
    ## 19  0.0000       0.3208 ***
    ## 20  0.0028       0.2682  **
    ## 21  0.0062       0.2899  **
    ## 22  0.0000       0.5798 ***
    ## 23  0.2388       0.0896    
    ## 24  0.0836       0.3021    
    ## 25  0.0000       0.1368 ***
    ## 26  0.0053       0.1107  **
    ## 27  0.2902      -0.0151

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
    ##  90.002   324.289
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 1315.003          0  0.9994 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.002 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##         Lacuna    TempAnomWarm_June   0.0096    0.0019 1222.5897    25.3195
    ##         Lacuna MonthlyMeanTemp_June   0.1455    0.0252 1144.0136    32.6768
    ##         Lacuna         CanopyHeight   0.7276    0.0624 1339.7614   135.4265
    ##         Lacuna           DensityLog   0.0297    0.0618 1331.2031     0.2284
    ##         Lacuna           YearBinary  -0.1131    0.0145 1324.0281    60.3761
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog               Lacuna  -0.0071    0.0244   85.0615     0.0781
    ##   BladeAreaLog         CanopyHeight   0.6953    0.0574   83.9902   137.8932
    ##   BladeAreaLog           DensityLog  -0.1198    0.0532   86.4885     4.5951
    ##   BladeAreaLog    TempAnomWarm_June  -0.0025    0.0017   66.8550     1.8969
    ##   BladeAreaLog MonthlyMeanTemp_June   0.0030    0.0194   17.6580     0.0194
    ##   BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.3605   324.8111
    ##   BladeAreaLog           YearBinary   0.0569    0.0185 1019.7124     9.3474
    ##     Prevalence         BladeAreaLog   1.7002    0.2874 1348.0000     5.9156
    ##     Prevalence               Lacuna   1.0583    0.2137 1348.0000     4.9529
    ##     Prevalence         CanopyHeight   0.7064    0.5374 1348.0000     1.3144
    ##     Prevalence           DensityLog   1.7893    0.4612 1348.0000     3.8792
    ##     Prevalence    TempAnomWarm_June  -0.0059    0.0147 1348.0000    -0.4036
    ##     Prevalence MonthlyMeanTemp_June   0.2833    0.1493 1348.0000     1.8979
    ##     Prevalence    TidalHeightBinary   0.6204    0.1417 1348.0000     4.3773
    ##     Prevalence           YearBinary   0.5117    0.1708 1348.0000     2.9957
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.0000        0.107 ***
    ##    0.0000       0.3069 ***
    ##    0.0000       0.3202 ***
    ##    0.6328       0.0183    
    ##    0.0000      -0.0707 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.7806      -0.0134    
    ##    0.0000       0.5738 ***
    ##    0.0349      -0.1385   *
    ##    0.1730      -0.0525    
    ##    0.8909       0.0119    
    ##    0.0000      -0.2826 ***
    ##    0.0023       0.0668  **
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.1887            -    
    ##    0.0001            - ***
    ##    0.6865            -    
    ##    0.0577            -    
    ##    0.0000            - ***
    ##    0.0027            -  **
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##         Lacuna   none     0.08        0.97
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.47        0.68
    ##     Prevalence  delta     0.12        0.37

Passes global fit test. Lacuna is positive and sig for prevalence.

``` r
coefs(sem_prev_lac_cc)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1        Lacuna    TempAnomWarm_June   0.0096    0.0019 1222.5897    25.3195
    ## 2        Lacuna MonthlyMeanTemp_June   0.1455    0.0252 1144.0136    32.6768
    ## 3        Lacuna         CanopyHeight   0.7276    0.0624 1339.7614   135.4265
    ## 4        Lacuna           DensityLog   0.0297    0.0618 1331.2031     0.2284
    ## 5        Lacuna           YearBinary  -0.1131    0.0145 1324.0281    60.3761
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog               Lacuna  -0.0071    0.0244   85.0615     0.0781
    ## 13 BladeAreaLog         CanopyHeight   0.6953    0.0574   83.9902   137.8932
    ## 14 BladeAreaLog           DensityLog  -0.1198    0.0532   86.4885     4.5951
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0025    0.0017   66.8550     1.8969
    ## 16 BladeAreaLog MonthlyMeanTemp_June   0.0030    0.0194   17.6580     0.0194
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.3605   324.8111
    ## 18 BladeAreaLog           YearBinary   0.0569    0.0185 1019.7124     9.3474
    ## 19   Prevalence         BladeAreaLog   1.7002    0.2874 1348.0000     5.9156
    ## 20   Prevalence               Lacuna   1.0583    0.2137 1348.0000     4.9529
    ## 21   Prevalence         CanopyHeight   0.7064    0.5374 1348.0000     1.3144
    ## 22   Prevalence           DensityLog   1.7893    0.4612 1348.0000     3.8792
    ## 23   Prevalence    TempAnomWarm_June  -0.0059    0.0147 1348.0000    -0.4036
    ## 24   Prevalence MonthlyMeanTemp_June   0.2833    0.1493 1348.0000     1.8979
    ## 25   Prevalence    TidalHeightBinary   0.6204    0.1417 1348.0000     4.3773
    ## 26   Prevalence           YearBinary   0.5117    0.1708 1348.0000     2.9957
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.1070 ***
    ## 2   0.0000       0.3069 ***
    ## 3   0.0000       0.3202 ***
    ## 4   0.6328       0.0183    
    ## 5   0.0000      -0.0707 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.7806      -0.0134    
    ## 13  0.0000       0.5738 ***
    ## 14  0.0349      -0.1385   *
    ## 15  0.1730      -0.0525    
    ## 16  0.8909       0.0119    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0023       0.0668  **
    ## 19  0.0000       0.3219 ***
    ## 20  0.0000       0.3758 ***
    ## 21  0.1887       0.1104    
    ## 22  0.0001       0.3917 ***
    ## 23  0.6865      -0.0234    
    ## 24  0.0577       0.2122    
    ## 25  0.0000       0.1387 ***
    ## 26  0.0027       0.1137  **
    ## 27  0.2902      -0.0151

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
    ## Model failed to converge with max|grad| = 0.0127603 (tol = 0.002, component 1)

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
    ##  90.001   324.288
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1317.004          0  0.9998 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1317.001          0  0.9998 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 1315.001          0  0.9999 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.001 with P-value = 1 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##      Ampithoid    TempAnomWarm_June  -0.0106     7e-04 1274.2497   260.4888
    ##      Ampithoid MonthlyMeanTemp_June   0.1831    0.0087 1218.9434   438.1374
    ##      Ampithoid         CanopyHeight   0.0403    0.0216 1327.8117     3.4724
    ##      Ampithoid           DensityLog   0.1695    0.0214 1321.3693    62.1928
    ##      Ampithoid           YearBinary  -0.1428     0.005 1319.0629   812.6462
    ##   CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ##   CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ##   CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ##     DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ##     DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ##     DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ##   BladeAreaLog            Ampithoid   0.0424    0.0463   47.8428     0.7897
    ##   BladeAreaLog         CanopyHeight   0.7135    0.0616  161.6226   124.1445
    ##   BladeAreaLog           DensityLog  -0.1110    0.0528   93.0618     3.9206
    ##   BladeAreaLog    TempAnomWarm_June  -0.0013    0.0017   52.3421     0.5295
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0141    0.0185   15.9420     0.4823
    ##   BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.7831   324.7746
    ##   BladeAreaLog           YearBinary   0.0664    0.0208  799.2452     9.9883
    ##     Prevalence         BladeAreaLog   1.6693    0.2888 1348.0000     5.7795
    ##     Prevalence            Ampithoid   0.1411    0.5824 1348.0000     0.2422
    ##     Prevalence         CanopyHeight   0.9853     0.664 1348.0000     1.4837
    ##     Prevalence           DensityLog   2.1427    0.5672 1348.0000     3.7776
    ##     Prevalence    TempAnomWarm_June   0.0093    0.0195 1348.0000     0.4769
    ##     Prevalence MonthlyMeanTemp_June   0.3091    0.2365 1348.0000     1.3071
    ##     Prevalence    TidalHeightBinary   0.6103    0.1415 1348.0000     4.3121
    ##     Prevalence           YearBinary   0.3629     0.212 1348.0000     1.7117
    ##   ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2072 ***
    ##    0.0000       0.6783 ***
    ##    0.0626       0.0311    
    ##    0.0000       0.1835 ***
    ##    0.0000       -0.157 ***
    ##    0.0000      -0.1375 ***
    ##    0.0000      -0.3512 ***
    ##    0.0000       -0.154 ***
    ##    0.0000      -0.3359 ***
    ##    0.0000        0.692 ***
    ##    0.0000      -0.0525 ***
    ##    0.3786       0.0453    
    ##    0.0000       0.5888 ***
    ##    0.0507      -0.1283    
    ##    0.4701      -0.0278    
    ##    0.4974      -0.0558    
    ##    0.0000      -0.2826 ***
    ##    0.0016        0.078  **
    ##    0.0000            - ***
    ##    0.8086            -    
    ##    0.1379            -    
    ##    0.0002            - ***
    ##    0.6334            -    
    ##    0.1912            -    
    ##    0.0000            - ***
    ##    0.0869            -    
    ##    0.2902      -0.0151    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##      Ampithoid   none     0.35        0.99
    ##   CanopyHeight   none     0.18        0.95
    ##     DensityLog   none     0.21        0.99
    ##   BladeAreaLog   none     0.51        0.66
    ##     Prevalence  delta     0.12        0.50

Passes global fit test. Ampithoid is negative for prevalence. Epiphyte
presence not sig for prevalence

``` r
coefs(sem_prev_amp_cc)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1     Ampithoid    TempAnomWarm_June  -0.0106     7e-04 1274.2497   260.4888
    ## 2     Ampithoid MonthlyMeanTemp_June   0.1831    0.0087 1218.9434   438.1374
    ## 3     Ampithoid         CanopyHeight   0.0403    0.0216 1327.8117     3.4724
    ## 4     Ampithoid           DensityLog   0.1695    0.0214 1321.3693    62.1928
    ## 5     Ampithoid           YearBinary  -0.1428     0.005 1319.0629   812.6462
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0054     6e-04 1041.9015    71.9211
    ## 7  CanopyHeight MonthlyMeanTemp_June  -0.0733    0.0092  852.5245    60.7839
    ## 8  CanopyHeight           YearBinary  -0.1083    0.0054 1324.9595   396.4469
    ## 9    DensityLog    TempAnomWarm_June  -0.0186     6e-04 1323.2448   820.5161
    ## 10   DensityLog MonthlyMeanTemp_June   0.2022    0.0095 1318.8341   447.5065
    ## 11   DensityLog           YearBinary  -0.0518    0.0055 1320.0373    89.7838
    ## 12 BladeAreaLog            Ampithoid   0.0424    0.0463   47.8428     0.7897
    ## 13 BladeAreaLog         CanopyHeight   0.7135    0.0616  161.6226   124.1445
    ## 14 BladeAreaLog           DensityLog  -0.1110    0.0528   93.0618     3.9206
    ## 15 BladeAreaLog    TempAnomWarm_June  -0.0013    0.0017   52.3421     0.5295
    ## 16 BladeAreaLog MonthlyMeanTemp_June  -0.0141    0.0185   15.9420     0.4823
    ## 17 BladeAreaLog    TidalHeightBinary  -0.2394    0.0133 1315.7831   324.7746
    ## 18 BladeAreaLog           YearBinary   0.0664    0.0208  799.2452     9.9883
    ## 19   Prevalence         BladeAreaLog   1.6693    0.2888 1348.0000     5.7795
    ## 20   Prevalence            Ampithoid   0.1411    0.5824 1348.0000     0.2422
    ## 21   Prevalence         CanopyHeight   0.9853     0.664 1348.0000     1.4837
    ## 22   Prevalence           DensityLog   2.1427    0.5672 1348.0000     3.7776
    ## 23   Prevalence    TempAnomWarm_June   0.0093    0.0195 1348.0000     0.4769
    ## 24   Prevalence MonthlyMeanTemp_June   0.3091    0.2365 1348.0000     1.3071
    ## 25   Prevalence    TidalHeightBinary   0.6103    0.1415 1348.0000     4.3121
    ## 26   Prevalence           YearBinary   0.3629     0.212 1348.0000     1.7117
    ## 27 ~~DensityLog       ~~CanopyHeight  -0.0151         - 1348.0000    -0.5529
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2072 ***
    ## 2   0.0000       0.6783 ***
    ## 3   0.0626       0.0311    
    ## 4   0.0000       0.1835 ***
    ## 5   0.0000      -0.1570 ***
    ## 6   0.0000      -0.1375 ***
    ## 7   0.0000      -0.3512 ***
    ## 8   0.0000      -0.1540 ***
    ## 9   0.0000      -0.3359 ***
    ## 10  0.0000       0.6920 ***
    ## 11  0.0000      -0.0525 ***
    ## 12  0.3786       0.0453    
    ## 13  0.0000       0.5888 ***
    ## 14  0.0507      -0.1283    
    ## 15  0.4701      -0.0278    
    ## 16  0.4974      -0.0558    
    ## 17  0.0000      -0.2826 ***
    ## 18  0.0016       0.0780  **
    ## 19  0.0000       0.3128 ***
    ## 20  0.8086       0.0282    
    ## 21  0.1379       0.1523    
    ## 22  0.0002       0.4642 ***
    ## 23  0.6334       0.0362    
    ## 24  0.1912       0.2291    
    ## 25  0.0000       0.1350 ***
    ## 26  0.0869       0.0798    
    ## 27  0.2902      -0.0151

## Lesion + Epifauna

``` r
les1 <- subset(dis1, LesionArea>0)
les1$LesionAreaLog <- log10(les1$LesionArea)
```

``` r
les2 <- select(les1, -c(Lacuna, Ampithoid, Idoteid))
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
    ##  98.562   300.821
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 567.2953     0.0906  0.7635 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.562 with P-value = 0.363 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0023    0.0016 540.4637     2.0307
    ##        Epifauna MonthlyMeanTemp_June  -0.1958    0.0202 505.8535    90.4971
    ##        Epifauna         CanopyHeight  -0.7243    0.0573 593.0545   158.0290
    ##        Epifauna           DensityLog  -0.1151    0.0504 593.6084     5.1493
    ##        Epifauna           YearBinary  -0.1215    0.0116 579.8888   108.4739
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog             Epifauna  -0.0630    0.0516  50.6690     1.3556
    ##    BladeAreaLog         CanopyHeight   0.6709    0.0875  83.5281    53.6990
    ##    BladeAreaLog           DensityLog  -0.1721     0.065  46.6631     5.6214
    ##    BladeAreaLog    TempAnomWarm_June   0.0017    0.0021  81.7597     0.5782
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0302     0.016   8.8969     3.1388
    ##    BladeAreaLog    TidalHeightBinary  -0.2056    0.0174 578.7837   138.4939
    ##    BladeAreaLog           YearBinary   0.0561    0.0242 438.1424     5.2875
    ##   LesionAreaLog         BladeAreaLog   0.5365    0.1228 589.1552    18.8290
    ##   LesionAreaLog             Epifauna   0.6527    0.1383  39.3382    20.5208
    ##   LesionAreaLog         CanopyHeight   0.9197    0.2553  86.2193    12.1420
    ##   LesionAreaLog           DensityLog   0.2383    0.1855  51.5505     1.3659
    ##   LesionAreaLog    TempAnomWarm_June   0.0115    0.0063  68.6726     3.0993
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0831    0.0498   9.0287     2.3960
    ##   LesionAreaLog    TidalHeightBinary   0.0147    0.0581 587.1264     0.0638
    ##   LesionAreaLog           YearBinary   0.1048    0.0712 423.6041     2.1272
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.1547      -0.0402    
    ##    0.0000      -0.6295 ***
    ##    0.0000      -0.4882 ***
    ##    0.0236      -0.1162   *
    ##    0.0000      -0.1262 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.2498      -0.0780    
    ##    0.0000       0.5598 ***
    ##    0.0219      -0.2151   *
    ##    0.4492       0.0362    
    ##    0.1106      -0.1201    
    ##    0.0000      -0.2686 ***
    ##    0.0219       0.0721   *
    ##    0.0000       0.2884 ***
    ##    0.0001       0.4344 ***
    ##    0.0008       0.4126 ***
    ##    0.2479       0.1601    
    ##    0.0828       0.1347    
    ##    0.1559       0.1778    
    ##    0.8007       0.0103    
    ##    0.1454       0.0725    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.18        0.99
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.60        0.70
    ##   LesionAreaLog   none     0.18        0.40

Passes global fit test. Epifauna is sig and pos for Lesion Area
Standardized coefs:

``` r
coefs(sem_les_epi)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1       Epifauna    TempAnomWarm_June  -0.0023    0.0016 540.4637     2.0307
    ## 2       Epifauna MonthlyMeanTemp_June  -0.1958    0.0202 505.8535    90.4971
    ## 3       Epifauna         CanopyHeight  -0.7243    0.0573 593.0545   158.0290
    ## 4       Epifauna           DensityLog  -0.1151    0.0504 593.6084     5.1493
    ## 5       Epifauna           YearBinary  -0.1215    0.0116 579.8888   108.4739
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog             Epifauna  -0.0630    0.0516  50.6690     1.3556
    ## 13  BladeAreaLog         CanopyHeight   0.6709    0.0875  83.5281    53.6990
    ## 14  BladeAreaLog           DensityLog  -0.1721     0.065  46.6631     5.6214
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0017    0.0021  81.7597     0.5782
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0302     0.016   8.8969     3.1388
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2056    0.0174 578.7837   138.4939
    ## 18  BladeAreaLog           YearBinary   0.0561    0.0242 438.1424     5.2875
    ## 19 LesionAreaLog         BladeAreaLog   0.5365    0.1228 589.1552    18.8290
    ## 20 LesionAreaLog             Epifauna   0.6527    0.1383  39.3382    20.5208
    ## 21 LesionAreaLog         CanopyHeight   0.9197    0.2553  86.2193    12.1420
    ## 22 LesionAreaLog           DensityLog   0.2383    0.1855  51.5505     1.3659
    ## 23 LesionAreaLog    TempAnomWarm_June   0.0115    0.0063  68.6726     3.0993
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.0831    0.0498   9.0287     2.3960
    ## 25 LesionAreaLog    TidalHeightBinary   0.0147    0.0581 587.1264     0.0638
    ## 26 LesionAreaLog           YearBinary   0.1048    0.0712 423.6041     2.1272
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.1547      -0.0402    
    ## 2   0.0000      -0.6295 ***
    ## 3   0.0000      -0.4882 ***
    ## 4   0.0236      -0.1162   *
    ## 5   0.0000      -0.1262 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.2498      -0.0780    
    ## 13  0.0000       0.5598 ***
    ## 14  0.0219      -0.2151   *
    ## 15  0.4492       0.0362    
    ## 16  0.1106      -0.1201    
    ## 17  0.0000      -0.2686 ***
    ## 18  0.0219       0.0721   *
    ## 19  0.0000       0.2884 ***
    ## 20  0.0001       0.4344 ***
    ## 21  0.0008       0.4126 ***
    ## 22  0.2479       0.1601    
    ## 23  0.0828       0.1347    
    ## 24  0.1559       0.1778    
    ## 25  0.8007       0.0103    
    ## 26  0.1454       0.0725    
    ## 27  0.0002       0.1420 ***

## Lesion + Lacuna

``` r
les3 <- select(les1, -c(Epifauna, Ampithoid, Idoteid))
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
    ##  98.071   300.33
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 567.6534     0.0009  0.9760 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.071 with P-value = 0.415 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##          Lacuna    TempAnomWarm_June   0.0016    0.0033 515.1231     0.2323
    ##          Lacuna MonthlyMeanTemp_June   0.2040    0.0425 463.2957    22.0373
    ##          Lacuna         CanopyHeight   0.4096    0.1188 556.8522    11.6755
    ##          Lacuna           DensityLog   0.3664    0.1047 572.1062    11.9879
    ##          Lacuna           YearBinary  -0.1228    0.0246 588.1051    24.8606
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog               Lacuna  -0.0325    0.0253  48.9872     1.3804
    ##    BladeAreaLog         CanopyHeight   0.7278    0.0698  65.1074    94.0149
    ##    BladeAreaLog           DensityLog  -0.1481    0.0575  35.0466     5.1989
    ##    BladeAreaLog    TempAnomWarm_June   0.0032    0.0019  72.9795     2.6375
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0322    0.0149   9.0132     4.2156
    ##    BladeAreaLog    TidalHeightBinary  -0.2057    0.0175 579.8624   138.0314
    ##    BladeAreaLog           YearBinary   0.0592    0.0231 465.4139     6.3245
    ##   LesionAreaLog         BladeAreaLog   0.5274    0.1237 587.5344    18.0010
    ##   LesionAreaLog               Lacuna   0.2687    0.0714  38.1973    12.7391
    ##   LesionAreaLog         CanopyHeight   0.3649    0.2116  75.0831     2.8053
    ##   LesionAreaLog           DensityLog  -0.2333    0.1667  37.7205     1.7149
    ##   LesionAreaLog    TempAnomWarm_June  -0.0059    0.0062  57.3642     0.8305
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1204    0.0618  10.3677     3.1389
    ##   LesionAreaLog    TidalHeightBinary   0.0153    0.0586 587.0726     0.0675
    ##   LesionAreaLog           YearBinary   0.0499    0.0688 452.2508     0.5149
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.6300       0.0160    
    ##    0.0000       0.3647 ***
    ##    0.0007       0.1536 ***
    ##    0.0006       0.2057 ***
    ##    0.0000      -0.0710 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.2457      -0.0724    
    ##    0.0000       0.6072 ***
    ##    0.0288      -0.1851   *
    ##    0.1087       0.0689    
    ##    0.0702      -0.1280    
    ##    0.0000      -0.2687 ***
    ##    0.0122       0.0761   *
    ##    0.0000       0.2836 ***
    ##    0.0010       0.3215 ***
    ##    0.0981       0.1637    
    ##    0.1983      -0.1568    
    ##    0.3659      -0.0691    
    ##    0.1058       0.2576    
    ##    0.7951       0.0107    
    ##    0.4734       0.0345    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##          Lacuna   none     0.06        0.98
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.61        0.70
    ##   LesionAreaLog   none     0.16        0.51

Passes global fit test. Lacuna is sig and pos for Lesion Area
Standardized coefs:

``` r
coefs(sem_les_lac)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1         Lacuna    TempAnomWarm_June   0.0016    0.0033 515.1231     0.2323
    ## 2         Lacuna MonthlyMeanTemp_June   0.2040    0.0425 463.2957    22.0373
    ## 3         Lacuna         CanopyHeight   0.4096    0.1188 556.8522    11.6755
    ## 4         Lacuna           DensityLog   0.3664    0.1047 572.1062    11.9879
    ## 5         Lacuna           YearBinary  -0.1228    0.0246 588.1051    24.8606
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog               Lacuna  -0.0325    0.0253  48.9872     1.3804
    ## 13  BladeAreaLog         CanopyHeight   0.7278    0.0698  65.1074    94.0149
    ## 14  BladeAreaLog           DensityLog  -0.1481    0.0575  35.0466     5.1989
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0032    0.0019  72.9795     2.6375
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0322    0.0149   9.0132     4.2156
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2057    0.0175 579.8624   138.0314
    ## 18  BladeAreaLog           YearBinary   0.0592    0.0231 465.4139     6.3245
    ## 19 LesionAreaLog         BladeAreaLog   0.5274    0.1237 587.5344    18.0010
    ## 20 LesionAreaLog               Lacuna   0.2687    0.0714  38.1973    12.7391
    ## 21 LesionAreaLog         CanopyHeight   0.3649    0.2116  75.0831     2.8053
    ## 22 LesionAreaLog           DensityLog  -0.2333    0.1667  37.7205     1.7149
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0059    0.0062  57.3642     0.8305
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.1204    0.0618  10.3677     3.1389
    ## 25 LesionAreaLog    TidalHeightBinary   0.0153    0.0586 587.0726     0.0675
    ## 26 LesionAreaLog           YearBinary   0.0499    0.0688 452.2508     0.5149
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.6300       0.0160    
    ## 2   0.0000       0.3647 ***
    ## 3   0.0007       0.1536 ***
    ## 4   0.0006       0.2057 ***
    ## 5   0.0000      -0.0710 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.2457      -0.0724    
    ## 13  0.0000       0.6072 ***
    ## 14  0.0288      -0.1851   *
    ## 15  0.1087       0.0689    
    ## 16  0.0702      -0.1280    
    ## 17  0.0000      -0.2687 ***
    ## 18  0.0122       0.0761   *
    ## 19  0.0000       0.2836 ***
    ## 20  0.0010       0.3215 ***
    ## 21  0.0981       0.1637    
    ## 22  0.1983      -0.1568    
    ## 23  0.3659      -0.0691    
    ## 24  0.1058       0.2576    
    ## 25  0.7951       0.0107    
    ## 26  0.4734       0.0345    
    ## 27  0.0002       0.1420 ***

## Lesion + Ampithoid

``` r
les4 <- select(les1, -c(Epifauna, Lacuna, Idoteid))
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
    ##  98.804   301.063
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 567.1570     0.1742  0.6766 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.804 with P-value = 0.339 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##       Ampithoid    TempAnomWarm_June  -0.0138     0.001 546.3592   182.1256
    ##       Ampithoid MonthlyMeanTemp_June   0.2209    0.0129 509.5827   285.6415
    ##       Ampithoid         CanopyHeight   0.0023     0.037 585.3407     0.0040
    ##       Ampithoid           DensityLog   0.1287    0.0324 582.2064    15.5437
    ##       Ampithoid           YearBinary  -0.1568    0.0075 573.6570   440.6576
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog            Ampithoid   0.1185    0.0498  44.4971     4.8862
    ##    BladeAreaLog         CanopyHeight   0.8164    0.0749  64.0333    95.8879
    ##    BladeAreaLog           DensityLog  -0.1336    0.0555  32.2813     4.4572
    ##    BladeAreaLog    TempAnomWarm_June   0.0047     0.002  57.5013     5.1027
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0463    0.0153  11.0061     8.2907
    ##    BladeAreaLog    TidalHeightBinary  -0.2052    0.0174 580.6981   137.7862
    ##    BladeAreaLog           YearBinary   0.0999    0.0259 323.4394    13.8876
    ##   LesionAreaLog         BladeAreaLog   0.5357    0.1246 588.4338    18.3438
    ##   LesionAreaLog            Ampithoid  -0.3354     0.164  45.2397     3.9066
    ##   LesionAreaLog         CanopyHeight   0.0824    0.2676 174.5461     0.0875
    ##   LesionAreaLog           DensityLog  -0.1504    0.1939  61.6035     0.5225
    ##   LesionAreaLog    TempAnomWarm_June  -0.0090     0.007  48.9528     1.4570
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1794    0.0686  12.1088     5.7021
    ##   LesionAreaLog    TidalHeightBinary   0.0176    0.0587 582.6416     0.0895
    ##   LesionAreaLog           YearBinary  -0.0937    0.0813 341.0070     1.2789
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2616 ***
    ##    0.0000       0.7671 ***
    ##    0.9499       0.0017    
    ##    0.0001       0.1404 ***
    ##    0.0000      -0.1761 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.0323       0.1358   *
    ##    0.0000       0.6812 ***
    ##    0.0426      -0.1670   *
    ##    0.0277       0.1017   *
    ##    0.0150      -0.1843   *
    ##    0.0000      -0.2681 ***
    ##    0.0002       0.1285 ***
    ##    0.0000       0.2880 ***
    ##    0.0542      -0.2066    
    ##    0.7677       0.0370    
    ##    0.4725      -0.1011    
    ##    0.2332      -0.1050    
    ##    0.0341       0.3838   *
    ##    0.7650       0.0124    
    ##    0.2589      -0.0648    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##       Ampithoid   none     0.34        0.99
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.64        0.71
    ##   LesionAreaLog   none     0.12        0.51

Passes global fit test. Ampithoid is significant and negative for lesion
area Standarized coefs:

``` r
coefs(sem_les_amp)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1      Ampithoid    TempAnomWarm_June  -0.0138     0.001 546.3592   182.1256
    ## 2      Ampithoid MonthlyMeanTemp_June   0.2209    0.0129 509.5827   285.6415
    ## 3      Ampithoid         CanopyHeight   0.0023     0.037 585.3407     0.0040
    ## 4      Ampithoid           DensityLog   0.1287    0.0324 582.2064    15.5437
    ## 5      Ampithoid           YearBinary  -0.1568    0.0075 573.6570   440.6576
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog            Ampithoid   0.1185    0.0498  44.4971     4.8862
    ## 13  BladeAreaLog         CanopyHeight   0.8164    0.0749  64.0333    95.8879
    ## 14  BladeAreaLog           DensityLog  -0.1336    0.0555  32.2813     4.4572
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0047     0.002  57.5013     5.1027
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0463    0.0153  11.0061     8.2907
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2052    0.0174 580.6981   137.7862
    ## 18  BladeAreaLog           YearBinary   0.0999    0.0259 323.4394    13.8876
    ## 19 LesionAreaLog         BladeAreaLog   0.5357    0.1246 588.4338    18.3438
    ## 20 LesionAreaLog            Ampithoid  -0.3354     0.164  45.2397     3.9066
    ## 21 LesionAreaLog         CanopyHeight   0.0824    0.2676 174.5461     0.0875
    ## 22 LesionAreaLog           DensityLog  -0.1504    0.1939  61.6035     0.5225
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0090     0.007  48.9528     1.4570
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.1794    0.0686  12.1088     5.7021
    ## 25 LesionAreaLog    TidalHeightBinary   0.0176    0.0587 582.6416     0.0895
    ## 26 LesionAreaLog           YearBinary  -0.0937    0.0813 341.0070     1.2789
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2616 ***
    ## 2   0.0000       0.7671 ***
    ## 3   0.9499       0.0017    
    ## 4   0.0001       0.1404 ***
    ## 5   0.0000      -0.1761 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.0323       0.1358   *
    ## 13  0.0000       0.6812 ***
    ## 14  0.0426      -0.1670   *
    ## 15  0.0277       0.1017   *
    ## 16  0.0150      -0.1843   *
    ## 17  0.0000      -0.2681 ***
    ## 18  0.0002       0.1285 ***
    ## 19  0.0000       0.2880 ***
    ## 20  0.0542      -0.2066    
    ## 21  0.7677       0.0370    
    ## 22  0.4725      -0.1011    
    ## 23  0.2332      -0.1050    
    ## 24  0.0341       0.3838   *
    ## 25  0.7650       0.0124    
    ## 26  0.2589      -0.0648    
    ## 27  0.0002       0.1420 ***

## Lesion + Idoteid

``` r
les5 <- select(les1, -c(Epifauna, Lacuna, Ampithoid))
les5 <- na.omit(les5)
sem_les_ido <- psem(
  lmer(Idoteid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les5),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les5),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les5),
    lmer(BladeAreaLog ~ Idoteid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les5),
  lmer(LesionAreaLog ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les5),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
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
    ##   LesionAreaLog ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  100.276   302.535
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##        Idoteid ~ TidalHeightBinary + ...      coef 567.1152     0.9746  0.3240 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 8.276 with P-value = 0.219 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##         Idoteid    TempAnomWarm_June   0.0237    0.0011 577.1501   435.0279
    ##         Idoteid MonthlyMeanTemp_June  -0.4220    0.0146 578.1087   834.4502
    ##         Idoteid         CanopyHeight   0.1730    0.0408 583.6690    17.9560
    ##         Idoteid           DensityLog   0.5565    0.0358 582.6796   240.2133
    ##         Idoteid           YearBinary  -0.0856    0.0082 573.7859   108.7218
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog              Idoteid  -0.0150     0.049  39.5792     0.0838
    ##    BladeAreaLog         CanopyHeight   0.7373    0.0758  74.0071    80.5287
    ##    BladeAreaLog           DensityLog  -0.1368    0.0622  38.5705     3.8070
    ##    BladeAreaLog    TempAnomWarm_June   0.0028    0.0019  64.3387     2.1007
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0314    0.0159   8.5522     3.4567
    ##    BladeAreaLog    TidalHeightBinary  -0.2059    0.0175 578.6424   138.5703
    ##    BladeAreaLog           YearBinary   0.0666    0.0225 417.0428     8.5166
    ##   LesionAreaLog         BladeAreaLog   0.5135    0.1237 586.9501    17.0868
    ##   LesionAreaLog              Idoteid   0.3644    0.1444  27.8896     5.9206
    ##   LesionAreaLog         CanopyHeight   0.1576    0.2474  86.4686     0.3836
    ##   LesionAreaLog           DensityLog  -0.3347    0.2033  51.3543     2.4622
    ##   LesionAreaLog    TempAnomWarm_June  -0.0131    0.0075  58.6722     2.6445
    ##   LesionAreaLog MonthlyMeanTemp_June   0.2710     0.086  22.6549     7.8986
    ##   LesionAreaLog    TidalHeightBinary   0.0161    0.0584 582.9938     0.0752
    ##   LesionAreaLog           YearBinary   0.0104    0.0679 423.0294     0.0231
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.0000       0.4663 ***
    ##    0.0000      -1.5175 ***
    ##    0.0000       0.1304 ***
    ##    0.0000       0.6285 ***
    ##    0.0000      -0.0995 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.7737      -0.0166    
    ##    0.0000       0.6152 ***
    ##    0.0583      -0.1710    
    ##    0.1521       0.0619    
    ##    0.0977      -0.1249    
    ##    0.0000      -0.2689 ***
    ##    0.0037       0.0856  **
    ##    0.0000       0.2761 ***
    ##    0.0216       0.2168   *
    ##    0.5373       0.0707    
    ##    0.1228      -0.2249    
    ##    0.1093      -0.1532    
    ##    0.0100       0.5798   *
    ##    0.7840       0.0113    
    ##    0.8792       0.0072    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##         Idoteid   none     0.21        1.00
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.59        0.70
    ##   LesionAreaLog   none     0.16        0.74

Passes global fit test. Idoteid is NOT significant for Lesion Area,
though coefficient is positive.

``` r
coefs(sem_les_ido)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1        Idoteid    TempAnomWarm_June   0.0237    0.0011 577.1501   435.0279
    ## 2        Idoteid MonthlyMeanTemp_June  -0.4220    0.0146 578.1087   834.4502
    ## 3        Idoteid         CanopyHeight   0.1730    0.0408 583.6690    17.9560
    ## 4        Idoteid           DensityLog   0.5565    0.0358 582.6796   240.2133
    ## 5        Idoteid           YearBinary  -0.0856    0.0082 573.7859   108.7218
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog              Idoteid  -0.0150     0.049  39.5792     0.0838
    ## 13  BladeAreaLog         CanopyHeight   0.7373    0.0758  74.0071    80.5287
    ## 14  BladeAreaLog           DensityLog  -0.1368    0.0622  38.5705     3.8070
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0028    0.0019  64.3387     2.1007
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0314    0.0159   8.5522     3.4567
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2059    0.0175 578.6424   138.5703
    ## 18  BladeAreaLog           YearBinary   0.0666    0.0225 417.0428     8.5166
    ## 19 LesionAreaLog         BladeAreaLog   0.5135    0.1237 586.9501    17.0868
    ## 20 LesionAreaLog              Idoteid   0.3644    0.1444  27.8896     5.9206
    ## 21 LesionAreaLog         CanopyHeight   0.1576    0.2474  86.4686     0.3836
    ## 22 LesionAreaLog           DensityLog  -0.3347    0.2033  51.3543     2.4622
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0131    0.0075  58.6722     2.6445
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.2710     0.086  22.6549     7.8986
    ## 25 LesionAreaLog    TidalHeightBinary   0.0161    0.0584 582.9938     0.0752
    ## 26 LesionAreaLog           YearBinary   0.0104    0.0679 423.0294     0.0231
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.0000       0.4663 ***
    ## 2   0.0000      -1.5175 ***
    ## 3   0.0000       0.1304 ***
    ## 4   0.0000       0.6285 ***
    ## 5   0.0000      -0.0995 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.7737      -0.0166    
    ## 13  0.0000       0.6152 ***
    ## 14  0.0583      -0.1710    
    ## 15  0.1521       0.0619    
    ## 16  0.0977      -0.1249    
    ## 17  0.0000      -0.2689 ***
    ## 18  0.0037       0.0856  **
    ## 19  0.0000       0.2761 ***
    ## 20  0.0216       0.2168   *
    ## 21  0.5373       0.0707    
    ## 22  0.1228      -0.2249    
    ## 23  0.1093      -0.1532    
    ## 24  0.0100       0.5798   *
    ## 25  0.7840       0.0113    
    ## 26  0.8792       0.0072    
    ## 27  0.0002       0.1420 ***

### Lesion + Richness

``` r
sem_les_rich <- psem(
  lmer(RichnessLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog +
         YearBinary +
         (1|Meadow) + (1|Region),
       data=les1),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les1),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=les1),
    lmer(BladeAreaLog ~ RichnessLog + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les1),
  lmer(LesionAreaLog ~ BladeAreaLog + RichnessLog + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les1),
  DensityLog%~~%CanopyHeight
)
summary(sem_les_rich)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_rich 
    ## 
    ## Call:
    ##   RichnessLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ RichnessLog + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + RichnessLog + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  98.222   300.481
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##    RichnessLog ~ TidalHeightBinary + ...      coef 567.4611     0.0143  0.9049 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.222 with P-value = 0.399 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##     RichnessLog    TempAnomWarm_June   0.0000     6e-04 553.6887     0.0045
    ##     RichnessLog MonthlyMeanTemp_June  -0.0525     0.008 524.7858    41.7502
    ##     RichnessLog         CanopyHeight   0.1871    0.0223 582.1061    69.1267
    ##     RichnessLog           DensityLog   0.0639    0.0197 589.3102    10.3710
    ##     RichnessLog           YearBinary  -0.0196    0.0046 584.8371    18.3335
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog          RichnessLog  -0.1019    0.1626 135.0933     0.3654
    ##    BladeAreaLog         CanopyHeight   0.7354    0.0714  61.0067    90.8407
    ##    BladeAreaLog           DensityLog  -0.1417    0.0591  37.1416     4.5098
    ##    BladeAreaLog    TempAnomWarm_June   0.0022    0.0021 136.4873     1.0309
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0294    0.0154   9.2707     3.1797
    ##    BladeAreaLog    TidalHeightBinary  -0.2057    0.0175 578.9133   138.1072
    ##    BladeAreaLog           YearBinary   0.0632    0.0232 484.5841     7.2307
    ##   LesionAreaLog         BladeAreaLog   0.5240    0.1237 589.4612    17.7721
    ##   LesionAreaLog          RichnessLog   1.4940    0.4898 121.6266     8.7842
    ##   LesionAreaLog         CanopyHeight   0.2678    0.2381  82.6812     1.1707
    ##   LesionAreaLog           DensityLog  -0.1299    0.1923  57.7515     0.3907
    ##   LesionAreaLog    TempAnomWarm_June   0.0065    0.0069  86.0566     0.8441
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0979    0.0612  10.9524     2.1404
    ##   LesionAreaLog    TidalHeightBinary   0.0152    0.0584 583.3459     0.0678
    ##   LesionAreaLog           YearBinary   0.0531    0.0704 485.7175     0.5580
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.9465      -0.0034    
    ##    0.0000      -0.7678 ***
    ##    0.0000       0.5742 ***
    ##    0.0014       0.2936  **
    ##    0.0000      -0.0929 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.5465      -0.0277    
    ##    0.0000       0.6136 ***
    ##    0.0404      -0.1771   *
    ##    0.3117       0.0479    
    ##    0.1073      -0.1169    
    ##    0.0000      -0.2687 ***
    ##    0.0074       0.0813  **
    ##    0.0000       0.2817 ***
    ##    0.0037       0.2184  **
    ##    0.2824       0.1201    
    ##    0.5344      -0.0873    
    ##    0.3608       0.0765    
    ##    0.1716       0.2094    
    ##    0.7947       0.0107    
    ##    0.4554       0.0367    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##     RichnessLog   none     0.12        0.99
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.60        0.70
    ##   LesionAreaLog   none     0.12        0.48

Passes global fit test. Richness does ppositively affect lesion area.
Interesting.

``` r
coefs(sem_les_rich)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1    RichnessLog    TempAnomWarm_June   0.0000     6e-04 553.6887     0.0045
    ## 2    RichnessLog MonthlyMeanTemp_June  -0.0525     0.008 524.7858    41.7502
    ## 3    RichnessLog         CanopyHeight   0.1871    0.0223 582.1061    69.1267
    ## 4    RichnessLog           DensityLog   0.0639    0.0197 589.3102    10.3710
    ## 5    RichnessLog           YearBinary  -0.0196    0.0046 584.8371    18.3335
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog          RichnessLog  -0.1019    0.1626 135.0933     0.3654
    ## 13  BladeAreaLog         CanopyHeight   0.7354    0.0714  61.0067    90.8407
    ## 14  BladeAreaLog           DensityLog  -0.1417    0.0591  37.1416     4.5098
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0022    0.0021 136.4873     1.0309
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0294    0.0154   9.2707     3.1797
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2057    0.0175 578.9133   138.1072
    ## 18  BladeAreaLog           YearBinary   0.0632    0.0232 484.5841     7.2307
    ## 19 LesionAreaLog         BladeAreaLog   0.5240    0.1237 589.4612    17.7721
    ## 20 LesionAreaLog          RichnessLog   1.4940    0.4898 121.6266     8.7842
    ## 21 LesionAreaLog         CanopyHeight   0.2678    0.2381  82.6812     1.1707
    ## 22 LesionAreaLog           DensityLog  -0.1299    0.1923  57.7515     0.3907
    ## 23 LesionAreaLog    TempAnomWarm_June   0.0065    0.0069  86.0566     0.8441
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.0979    0.0612  10.9524     2.1404
    ## 25 LesionAreaLog    TidalHeightBinary   0.0152    0.0584 583.3459     0.0678
    ## 26 LesionAreaLog           YearBinary   0.0531    0.0704 485.7175     0.5580
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.9465      -0.0034    
    ## 2   0.0000      -0.7678 ***
    ## 3   0.0000       0.5742 ***
    ## 4   0.0014       0.2936  **
    ## 5   0.0000      -0.0929 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.5465      -0.0277    
    ## 13  0.0000       0.6136 ***
    ## 14  0.0404      -0.1771   *
    ## 15  0.3117       0.0479    
    ## 16  0.1073      -0.1169    
    ## 17  0.0000      -0.2687 ***
    ## 18  0.0074       0.0813  **
    ## 19  0.0000       0.2817 ***
    ## 20  0.0037       0.2184  **
    ## 21  0.2824       0.1201    
    ## 22  0.5344      -0.0873    
    ## 23  0.3608       0.0765    
    ## 24  0.1716       0.2094    
    ## 25  0.7947       0.0107    
    ## 26  0.4554       0.0367    
    ## 27  0.0002       0.1420 ***

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
    ##  98.562   300.821
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 567.2953     0.0906  0.7635 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.562 with P-value = 0.363 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0023    0.0016 540.4637     2.0307
    ##        Epifauna MonthlyMeanTemp_June  -0.1958    0.0202 505.8535    90.4971
    ##        Epifauna         CanopyHeight  -0.7243    0.0573 593.0545   158.0290
    ##        Epifauna           DensityLog  -0.1151    0.0504 593.6084     5.1493
    ##        Epifauna           YearBinary  -0.1215    0.0116 579.8888   108.4739
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog             Epifauna  -0.0630    0.0516  50.6690     1.3556
    ##    BladeAreaLog         CanopyHeight   0.6709    0.0875  83.5281    53.6990
    ##    BladeAreaLog           DensityLog  -0.1721     0.065  46.6631     5.6214
    ##    BladeAreaLog    TempAnomWarm_June   0.0017    0.0021  81.7597     0.5782
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0302     0.016   8.8969     3.1388
    ##    BladeAreaLog    TidalHeightBinary  -0.2056    0.0174 578.7837   138.4939
    ##    BladeAreaLog           YearBinary   0.0561    0.0242 438.1424     5.2875
    ##   LesionAreaLog         BladeAreaLog   0.5365    0.1228 589.1552    18.8290
    ##   LesionAreaLog             Epifauna   0.6527    0.1383  39.3382    20.5208
    ##   LesionAreaLog         CanopyHeight   0.9197    0.2553  86.2193    12.1420
    ##   LesionAreaLog           DensityLog   0.2383    0.1855  51.5505     1.3659
    ##   LesionAreaLog    TempAnomWarm_June   0.0115    0.0063  68.6726     3.0993
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0831    0.0498   9.0287     2.3960
    ##   LesionAreaLog    TidalHeightBinary   0.0147    0.0581 587.1264     0.0638
    ##   LesionAreaLog           YearBinary   0.1048    0.0712 423.6041     2.1272
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.1547      -0.0402    
    ##    0.0000      -0.6295 ***
    ##    0.0000      -0.4882 ***
    ##    0.0236      -0.1162   *
    ##    0.0000      -0.1262 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.2498      -0.0780    
    ##    0.0000       0.5598 ***
    ##    0.0219      -0.2151   *
    ##    0.4492       0.0362    
    ##    0.1106      -0.1201    
    ##    0.0000      -0.2686 ***
    ##    0.0219       0.0721   *
    ##    0.0000       0.2884 ***
    ##    0.0001       0.4344 ***
    ##    0.0008       0.4126 ***
    ##    0.2479       0.1601    
    ##    0.0828       0.1347    
    ##    0.1559       0.1778    
    ##    0.8007       0.0103    
    ##    0.1454       0.0725    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.18        0.99
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.60        0.70
    ##   LesionAreaLog   none     0.18        0.40

Passes global fit test. Epifauna is sig and pos for Lesion area

``` r
coefs(sem_les_epi_cc)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1       Epifauna    TempAnomWarm_June  -0.0023    0.0016 540.4637     2.0307
    ## 2       Epifauna MonthlyMeanTemp_June  -0.1958    0.0202 505.8535    90.4971
    ## 3       Epifauna         CanopyHeight  -0.7243    0.0573 593.0545   158.0290
    ## 4       Epifauna           DensityLog  -0.1151    0.0504 593.6084     5.1493
    ## 5       Epifauna           YearBinary  -0.1215    0.0116 579.8888   108.4739
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog             Epifauna  -0.0630    0.0516  50.6690     1.3556
    ## 13  BladeAreaLog         CanopyHeight   0.6709    0.0875  83.5281    53.6990
    ## 14  BladeAreaLog           DensityLog  -0.1721     0.065  46.6631     5.6214
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0017    0.0021  81.7597     0.5782
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0302     0.016   8.8969     3.1388
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2056    0.0174 578.7837   138.4939
    ## 18  BladeAreaLog           YearBinary   0.0561    0.0242 438.1424     5.2875
    ## 19 LesionAreaLog         BladeAreaLog   0.5365    0.1228 589.1552    18.8290
    ## 20 LesionAreaLog             Epifauna   0.6527    0.1383  39.3382    20.5208
    ## 21 LesionAreaLog         CanopyHeight   0.9197    0.2553  86.2193    12.1420
    ## 22 LesionAreaLog           DensityLog   0.2383    0.1855  51.5505     1.3659
    ## 23 LesionAreaLog    TempAnomWarm_June   0.0115    0.0063  68.6726     3.0993
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.0831    0.0498   9.0287     2.3960
    ## 25 LesionAreaLog    TidalHeightBinary   0.0147    0.0581 587.1264     0.0638
    ## 26 LesionAreaLog           YearBinary   0.1048    0.0712 423.6041     2.1272
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.1547      -0.0402    
    ## 2   0.0000      -0.6295 ***
    ## 3   0.0000      -0.4882 ***
    ## 4   0.0236      -0.1162   *
    ## 5   0.0000      -0.1262 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.2498      -0.0780    
    ## 13  0.0000       0.5598 ***
    ## 14  0.0219      -0.2151   *
    ## 15  0.4492       0.0362    
    ## 16  0.1106      -0.1201    
    ## 17  0.0000      -0.2686 ***
    ## 18  0.0219       0.0721   *
    ## 19  0.0000       0.2884 ***
    ## 20  0.0001       0.4344 ***
    ## 21  0.0008       0.4126 ***
    ## 22  0.2479       0.1601    
    ## 23  0.0828       0.1347    
    ## 24  0.1559       0.1778    
    ## 25  0.8007       0.0103    
    ## 26  0.1454       0.0725    
    ## 27  0.0002       0.1420 ***

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
    ##  98.071   300.33
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 567.6534     0.0009  0.9760 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.071 with P-value = 0.415 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##          Lacuna    TempAnomWarm_June   0.0016    0.0033 515.1231     0.2323
    ##          Lacuna MonthlyMeanTemp_June   0.2040    0.0425 463.2957    22.0373
    ##          Lacuna         CanopyHeight   0.4096    0.1188 556.8522    11.6755
    ##          Lacuna           DensityLog   0.3664    0.1047 572.1062    11.9879
    ##          Lacuna           YearBinary  -0.1228    0.0246 588.1051    24.8606
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog               Lacuna  -0.0325    0.0253  48.9872     1.3804
    ##    BladeAreaLog         CanopyHeight   0.7278    0.0698  65.1074    94.0149
    ##    BladeAreaLog           DensityLog  -0.1481    0.0575  35.0466     5.1989
    ##    BladeAreaLog    TempAnomWarm_June   0.0032    0.0019  72.9795     2.6375
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0322    0.0149   9.0132     4.2156
    ##    BladeAreaLog    TidalHeightBinary  -0.2057    0.0175 579.8624   138.0314
    ##    BladeAreaLog           YearBinary   0.0592    0.0231 465.4139     6.3245
    ##   LesionAreaLog         BladeAreaLog   0.5274    0.1237 587.5344    18.0010
    ##   LesionAreaLog               Lacuna   0.2687    0.0714  38.1973    12.7391
    ##   LesionAreaLog         CanopyHeight   0.3649    0.2116  75.0831     2.8053
    ##   LesionAreaLog           DensityLog  -0.2333    0.1667  37.7205     1.7149
    ##   LesionAreaLog    TempAnomWarm_June  -0.0059    0.0062  57.3642     0.8305
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1204    0.0618  10.3677     3.1389
    ##   LesionAreaLog    TidalHeightBinary   0.0153    0.0586 587.0726     0.0675
    ##   LesionAreaLog           YearBinary   0.0499    0.0688 452.2508     0.5149
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.6300       0.0160    
    ##    0.0000       0.3647 ***
    ##    0.0007       0.1536 ***
    ##    0.0006       0.2057 ***
    ##    0.0000      -0.0710 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.2457      -0.0724    
    ##    0.0000       0.6072 ***
    ##    0.0288      -0.1851   *
    ##    0.1087       0.0689    
    ##    0.0702      -0.1280    
    ##    0.0000      -0.2687 ***
    ##    0.0122       0.0761   *
    ##    0.0000       0.2836 ***
    ##    0.0010       0.3215 ***
    ##    0.0981       0.1637    
    ##    0.1983      -0.1568    
    ##    0.3659      -0.0691    
    ##    0.1058       0.2576    
    ##    0.7951       0.0107    
    ##    0.4734       0.0345    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##          Lacuna   none     0.06        0.98
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.61        0.70
    ##   LesionAreaLog   none     0.16        0.51

Passes global fit test. Lacuana is sig and positive for lesion area
Standardized coefs:

``` r
coefs(sem_les_lac_cc)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1         Lacuna    TempAnomWarm_June   0.0016    0.0033 515.1231     0.2323
    ## 2         Lacuna MonthlyMeanTemp_June   0.2040    0.0425 463.2957    22.0373
    ## 3         Lacuna         CanopyHeight   0.4096    0.1188 556.8522    11.6755
    ## 4         Lacuna           DensityLog   0.3664    0.1047 572.1062    11.9879
    ## 5         Lacuna           YearBinary  -0.1228    0.0246 588.1051    24.8606
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog               Lacuna  -0.0325    0.0253  48.9872     1.3804
    ## 13  BladeAreaLog         CanopyHeight   0.7278    0.0698  65.1074    94.0149
    ## 14  BladeAreaLog           DensityLog  -0.1481    0.0575  35.0466     5.1989
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0032    0.0019  72.9795     2.6375
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0322    0.0149   9.0132     4.2156
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2057    0.0175 579.8624   138.0314
    ## 18  BladeAreaLog           YearBinary   0.0592    0.0231 465.4139     6.3245
    ## 19 LesionAreaLog         BladeAreaLog   0.5274    0.1237 587.5344    18.0010
    ## 20 LesionAreaLog               Lacuna   0.2687    0.0714  38.1973    12.7391
    ## 21 LesionAreaLog         CanopyHeight   0.3649    0.2116  75.0831     2.8053
    ## 22 LesionAreaLog           DensityLog  -0.2333    0.1667  37.7205     1.7149
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0059    0.0062  57.3642     0.8305
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.1204    0.0618  10.3677     3.1389
    ## 25 LesionAreaLog    TidalHeightBinary   0.0153    0.0586 587.0726     0.0675
    ## 26 LesionAreaLog           YearBinary   0.0499    0.0688 452.2508     0.5149
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.6300       0.0160    
    ## 2   0.0000       0.3647 ***
    ## 3   0.0007       0.1536 ***
    ## 4   0.0006       0.2057 ***
    ## 5   0.0000      -0.0710 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.2457      -0.0724    
    ## 13  0.0000       0.6072 ***
    ## 14  0.0288      -0.1851   *
    ## 15  0.1087       0.0689    
    ## 16  0.0702      -0.1280    
    ## 17  0.0000      -0.2687 ***
    ## 18  0.0122       0.0761   *
    ## 19  0.0000       0.2836 ***
    ## 20  0.0010       0.3215 ***
    ## 21  0.0981       0.1637    
    ## 22  0.1983      -0.1568    
    ## 23  0.3659      -0.0691    
    ## 24  0.1058       0.2576    
    ## 25  0.7951       0.0107    
    ## 26  0.4734       0.0345    
    ## 27  0.0002       0.1420 ***

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
    ##  98.804   301.063
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 569.5099     1.2850  0.2575 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 569.4507     1.7121  0.1912 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 567.1570     0.1742  0.6766 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 6.804 with P-value = 0.339 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error       DF Crit.Value
    ##       Ampithoid    TempAnomWarm_June  -0.0138     0.001 546.3592   182.1256
    ##       Ampithoid MonthlyMeanTemp_June   0.2209    0.0129 509.5827   285.6415
    ##       Ampithoid         CanopyHeight   0.0023     0.037 585.3407     0.0040
    ##       Ampithoid           DensityLog   0.1287    0.0324 582.2064    15.5437
    ##       Ampithoid           YearBinary  -0.1568    0.0075 573.6570   440.6576
    ##    CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ##    CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ##    CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ##      DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ##      DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ##      DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ##    BladeAreaLog            Ampithoid   0.1185    0.0498  44.4971     4.8862
    ##    BladeAreaLog         CanopyHeight   0.8164    0.0749  64.0333    95.8879
    ##    BladeAreaLog           DensityLog  -0.1336    0.0555  32.2813     4.4572
    ##    BladeAreaLog    TempAnomWarm_June   0.0047     0.002  57.5013     5.1027
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0463    0.0153  11.0061     8.2907
    ##    BladeAreaLog    TidalHeightBinary  -0.2052    0.0174 580.6981   137.7862
    ##    BladeAreaLog           YearBinary   0.0999    0.0259 323.4394    13.8876
    ##   LesionAreaLog         BladeAreaLog   0.5357    0.1246 588.4338    18.3438
    ##   LesionAreaLog            Ampithoid  -0.3354     0.164  45.2397     3.9066
    ##   LesionAreaLog         CanopyHeight   0.0824    0.2676 174.5461     0.0875
    ##   LesionAreaLog           DensityLog  -0.1504    0.1939  61.6035     0.5225
    ##   LesionAreaLog    TempAnomWarm_June  -0.0090     0.007  48.9528     1.4570
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1794    0.0686  12.1088     5.7021
    ##   LesionAreaLog    TidalHeightBinary   0.0176    0.0587 582.6416     0.0895
    ##   LesionAreaLog           YearBinary  -0.0937    0.0813 341.0070     1.2789
    ##    ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2616 ***
    ##    0.0000       0.7671 ***
    ##    0.9499       0.0017    
    ##    0.0001       0.1404 ***
    ##    0.0000      -0.1761 ***
    ##    0.0000      -0.3148 ***
    ##    0.7254      -0.0231    
    ##    0.0000      -0.1924 ***
    ##    0.0000      -0.2270 ***
    ##    0.0000       0.4379 ***
    ##    0.0001      -0.0316 ***
    ##    0.0323       0.1358   *
    ##    0.0000       0.6812 ***
    ##    0.0426      -0.1670   *
    ##    0.0277       0.1017   *
    ##    0.0150      -0.1843   *
    ##    0.0000      -0.2681 ***
    ##    0.0002       0.1285 ***
    ##    0.0000       0.2880 ***
    ##    0.0542      -0.2066    
    ##    0.7677       0.0370    
    ##    0.4725      -0.1011    
    ##    0.2332      -0.1050    
    ##    0.0341       0.3838   *
    ##    0.7650       0.0124    
    ##    0.2589      -0.0648    
    ##    0.0002       0.1420 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##       Ampithoid   none     0.34        0.99
    ##    CanopyHeight   none     0.07        0.97
    ##      DensityLog   none     0.12        0.99
    ##    BladeAreaLog   none     0.64        0.71
    ##   LesionAreaLog   none     0.12        0.51

Passes global fit test. Ampithoid is sig and negative for lesion area.

``` r
coefs(sem_les_amp_cc)
```

    ##         Response            Predictor Estimate Std.Error       DF Crit.Value
    ## 1      Ampithoid    TempAnomWarm_June  -0.0138     0.001 546.3592   182.1256
    ## 2      Ampithoid MonthlyMeanTemp_June   0.2209    0.0129 509.5827   285.6415
    ## 3      Ampithoid         CanopyHeight   0.0023     0.037 585.3407     0.0040
    ## 4      Ampithoid           DensityLog   0.1287    0.0324 582.2064    15.5437
    ## 5      Ampithoid           YearBinary  -0.1568    0.0075 573.6570   440.6576
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0121     9e-04 469.5770   162.9264
    ## 7   CanopyHeight MonthlyMeanTemp_June  -0.0049    0.0135 397.2305     0.1235
    ## 8   CanopyHeight           YearBinary  -0.1248    0.0067 572.6183   350.6094
    ## 9     DensityLog    TempAnomWarm_June  -0.0130    0.0011 551.6986   144.2236
    ## 10    DensityLog MonthlyMeanTemp_June   0.1375    0.0157 524.9647    74.8784
    ## 11    DensityLog           YearBinary  -0.0307    0.0076 571.8212    16.3799
    ## 12  BladeAreaLog            Ampithoid   0.1185    0.0498  44.4971     4.8862
    ## 13  BladeAreaLog         CanopyHeight   0.8164    0.0749  64.0333    95.8879
    ## 14  BladeAreaLog           DensityLog  -0.1336    0.0555  32.2813     4.4572
    ## 15  BladeAreaLog    TempAnomWarm_June   0.0047     0.002  57.5013     5.1027
    ## 16  BladeAreaLog MonthlyMeanTemp_June  -0.0463    0.0153  11.0061     8.2907
    ## 17  BladeAreaLog    TidalHeightBinary  -0.2052    0.0174 580.6981   137.7862
    ## 18  BladeAreaLog           YearBinary   0.0999    0.0259 323.4394    13.8876
    ## 19 LesionAreaLog         BladeAreaLog   0.5357    0.1246 588.4338    18.3438
    ## 20 LesionAreaLog            Ampithoid  -0.3354     0.164  45.2397     3.9066
    ## 21 LesionAreaLog         CanopyHeight   0.0824    0.2676 174.5461     0.0875
    ## 22 LesionAreaLog           DensityLog  -0.1504    0.1939  61.6035     0.5225
    ## 23 LesionAreaLog    TempAnomWarm_June  -0.0090     0.007  48.9528     1.4570
    ## 24 LesionAreaLog MonthlyMeanTemp_June   0.1794    0.0686  12.1088     5.7021
    ## 25 LesionAreaLog    TidalHeightBinary   0.0176    0.0587 582.6416     0.0895
    ## 26 LesionAreaLog           YearBinary  -0.0937    0.0813 341.0070     1.2789
    ## 27  ~~DensityLog       ~~CanopyHeight   0.1420         - 600.0000     3.5053
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2616 ***
    ## 2   0.0000       0.7671 ***
    ## 3   0.9499       0.0017    
    ## 4   0.0001       0.1404 ***
    ## 5   0.0000      -0.1761 ***
    ## 6   0.0000      -0.3148 ***
    ## 7   0.7254      -0.0231    
    ## 8   0.0000      -0.1924 ***
    ## 9   0.0000      -0.2270 ***
    ## 10  0.0000       0.4379 ***
    ## 11  0.0001      -0.0316 ***
    ## 12  0.0323       0.1358   *
    ## 13  0.0000       0.6812 ***
    ## 14  0.0426      -0.1670   *
    ## 15  0.0277       0.1017   *
    ## 16  0.0150      -0.1843   *
    ## 17  0.0000      -0.2681 ***
    ## 18  0.0002       0.1285 ***
    ## 19  0.0000       0.2880 ***
    ## 20  0.0542      -0.2066    
    ## 21  0.7677       0.0370    
    ## 22  0.4725      -0.1011    
    ## 23  0.2332      -0.1050    
    ## 24  0.0341       0.3838   *
    ## 25  0.7650       0.0124    
    ## 26  0.2589      -0.0648    
    ## 27  0.0002       0.1420 ***

## SEM Conclusions

- Epifauna abundance influences disease  
- Lacuna and ampithoid abundance show contrasting effects, could be
  explained by contrasting mechanisms  
- Limited mediation of temperature effects

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
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

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

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

``` r
prev_amp_1 <- glmer(Prevalence ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis4,
        family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0127603 (tol = 0.002, component 1)

``` r
plot(predictorEffect("Ampithoid", prev_amp_1,partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->

``` r
prev_ido_1 <- glmer(Prevalence ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis5,
        family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00426657 (tol = 0.002, component 1)

``` r
plot(predictorEffect("Idoteid", prev_ido_1,partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->

``` r
prev_rich_1 <- glmer(Prevalence ~ BladeAreaLog + RichnessLog + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=dis1,
        family="binomial")
plot(predictorEffect("RichnessLog", prev_rich_1,partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-32-5.png)<!-- -->

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

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

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

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

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

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

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

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

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

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
les_lac_1 <- lmer(LesionAreaLog ~ BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les3)
plot(predictorEffect("Lacuna", les_lac_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-37-2.png)<!-- -->

``` r
les_amp_1 <- lmer(LesionAreaLog ~ BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les4)
plot(predictorEffect("Ampithoid", les_amp_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-37-3.png)<!-- -->

``` r
les_ido_1 <- lmer(LesionAreaLog ~ BladeAreaLog + Idoteid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les5)
plot(predictorEffect("Idoteid", les_ido_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-37-4.png)<!-- -->

``` r
les_rich_1 <- lmer(LesionAreaLog ~ BladeAreaLog + RichnessLog + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary +
            (1|Region) +(1|Meadow), 
        data=les1)
plot(predictorEffect("RichnessLog", les_rich_1, partial.residuals=TRUE))
```

![](NewSEMEpifauanOnly_files/figure-gfm/unnamed-chunk-37-5.png)<!-- -->

# Distributions

``` r
dis2$Region <- ordered(dis2$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(dis2, aes(x=Epifauna, fill=Region))+geom_histogram(bins=30)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-prev-1.png)<!-- -->

``` r
dis3$Region <- ordered(dis3$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(dis3, aes(x=Lacuna, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-prev-2.png)<!-- -->

``` r
dis4$Region <- ordered(dis4$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(dis4, aes(x=Ampithoid, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-prev-3.png)<!-- -->

``` r
dis_cc$Region <- ordered(dis_cc$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(dis_cc, aes(x=Epifauna, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-prev-4.png)<!-- -->

``` r
dis1$Region <- ordered(dis1$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(dis1, aes(x=Idoteid, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-prev-5.png)<!-- -->

``` r
les2$Region <- ordered(les2$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(les2, aes(x=Epifauna, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-les-1.png)<!-- -->

``` r
les3$Region <- ordered(les3$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(les3, aes(x=Lacuna, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-les-2.png)<!-- -->

``` r
les4$Region <- ordered(les4$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(les4, aes(x=Ampithoid, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-les-3.png)<!-- -->

``` r
les_cc$Region <- ordered(les_cc$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(les_cc, aes(x=Epifauna, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-les-4.png)<!-- -->

``` r
les5$Region <- ordered(les1$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))
ggplot(les5, aes(x=Idoteid, fill=Region))+geom_histogram(bins=15)
```

![](NewSEMEpifauanOnly_files/figure-gfm/histograms-les-5.png)<!-- -->
