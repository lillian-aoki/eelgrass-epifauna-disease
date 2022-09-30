Blade-level SEMs with meadow-scale predictors
================

SEMs to assess temperature, grazing, disease relationships. All data
(2019 and 2021) from sites where available.

SEMs are set up with meadow-scale predictors (e.g. epifauna abundance,
canopy height) and modeled at the blade scale for prevalence, grazing
scars, and blade area.

Some relationships have been eliminated through the LMM/GLMM modeling
(e.g. epifauna does not predict grazing)

Different SEMs compared to answer specific questions:  
- does temperature afffect prevalence (disease) directly or
indirectly?  
- is temperature a driver of the relationship between grazing and
prevalence?  
- does grazing drive disease or does disease drive grazing
(i.e. increased grazing increases disease versus diseaed plants
accumulating more grazing scars)

``` r
# data ###
dis <- read_csv("data/meadow_predictors_blade_SEM_data.csv")
```

    ## Rows: 3713 Columns: 27
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (7): SampleId, Region, SiteCode, TidalHeight, BrokenTip, Meter, Meadow
    ## dbl  (19): Transect, Blade, Year, Prevalence, LesionArea, HealthyArea, Blade...
    ## date  (1): SampleDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dis$BladeAreaLog <- log10(dis$BladeArea)
dis$BladeArea_cm2 <- dis$BladeArea/100
dis$fTransect <- as.factor(dis$Transect)
dis$sLatitude <- scale(dis$Latitude)
site <- select(dis, c(Epifauna, CanopyHeight, DensityLog, TempAnomWarm_May, Latitude, sLatitude,
                      TempAnomWarm_April, TempAnomWarm_June, MonthlyMeanTemp_June, YearBinary, Meadow, Region))
site <- distinct(site)
```

## Prevalence SEMs

#### full model

``` r
sem1 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis),
  glmer(Prevalence ~ GrazingScars+ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
summary(sem1)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem1 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ GrazingScars + BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  117.810   466.107
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 3683.005     0.2028  0.6525 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 3683.002     0.1617  0.6876 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 3681.007     0.0516  0.8203 
    ##            GrazingScars ~ Epifauna + ...      coef 3713.000    -1.4439  0.1488 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 5.81 with P-value = 0.669 and on 8 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ##       Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ##       Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ##       Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ##       Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ##   CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ##   CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ##   CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ##     DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ##     DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ##     DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ##   GrazingScars         CanopyHeight   0.9155    0.4872 3713.0000     1.8791
    ##   GrazingScars           DensityLog  -0.4680    0.4383 3713.0000    -1.0678
    ##   GrazingScars         BladeAreaLog   1.0896     0.182 3713.0000     5.9881
    ##   GrazingScars    TempAnomWarm_June  -0.0270    0.0176 3713.0000    -1.5356
    ##   GrazingScars MonthlyMeanTemp_June   1.7978     0.202 3713.0000     8.9005
    ##   GrazingScars    TidalHeightBinary  -0.1015    0.0878 3713.0000    -1.1550
    ##   GrazingScars           YearBinary  -1.1452    0.1109 3713.0000   -10.3236
    ##   BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ##   BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ##   BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ##   BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ##   BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ##   BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ##     Prevalence         GrazingScars   0.8425    0.0905 3713.0000     9.3137
    ##     Prevalence         BladeAreaLog   1.2570    0.1708 3713.0000     7.3606
    ##     Prevalence             Epifauna   1.0430    0.3099 3713.0000     3.3654
    ##     Prevalence         CanopyHeight   0.9271    0.5341 3713.0000     1.7360
    ##     Prevalence           DensityLog   1.2724    0.3852 3713.0000     3.3034
    ##     Prevalence    TempAnomWarm_June   0.0015    0.0131 3713.0000     0.1130
    ##     Prevalence MonthlyMeanTemp_June   0.1256    0.1236 3713.0000     1.0164
    ##     Prevalence    TidalHeightBinary   0.2600     0.082 3713.0000     3.1723
    ##     Prevalence           YearBinary   0.2636    0.1169 3713.0000     2.2555
    ##   ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2227 ***
    ##    0.0000      -0.4991 ***
    ##    0.0000      -0.5268 ***
    ##    0.0000      -0.3181 ***
    ##    0.0000      -0.0947 ***
    ##    0.0000      -0.4038 ***
    ##    0.0000       0.4462 ***
    ##    0.0000      -0.1744 ***
    ##    0.0000      -0.4666 ***
    ##    0.0000        1.269 ***
    ##    0.0000      -0.0566 ***
    ##    0.0602            -    
    ##    0.2856            -    
    ##    0.0000            - ***
    ##    0.1246            -    
    ##    0.0000            - ***
    ##    0.2481            -    
    ##    0.0000            - ***
    ##    0.1268       0.0599    
    ##    0.0000       0.5653 ***
    ##    0.4136       0.0425    
    ##    0.0007       0.1193 ***
    ##    0.0529      -0.1609    
    ##    0.0000      -0.2451 ***
    ##    0.0269       0.0304   *
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.0008            - ***
    ##    0.0826            -    
    ##    0.0010            - ***
    ##    0.9101            -    
    ##    0.3094            -    
    ##    0.0015            -  **
    ##    0.0241            -   *
    ##    0.0000       0.1798 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.27        0.96
    ##   CanopyHeight   none     0.15        0.99
    ##     DensityLog   none     0.36        1.00
    ##   GrazingScars  delta     0.44        0.88
    ##   BladeAreaLog   none     0.53        0.66
    ##     Prevalence  delta     0.09        0.33

``` r
coefs(sem1)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ## 2      Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ## 3      Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ## 4      Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ## 5      Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ## 7  CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ## 8  CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ## 9    DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ## 10   DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ## 11   DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ## 12 GrazingScars         CanopyHeight   0.9155    0.4872 3713.0000     1.8791
    ## 13 GrazingScars           DensityLog  -0.4680    0.4383 3713.0000    -1.0678
    ## 14 GrazingScars         BladeAreaLog   1.0896     0.182 3713.0000     5.9881
    ## 15 GrazingScars    TempAnomWarm_June  -0.0270    0.0176 3713.0000    -1.5356
    ## 16 GrazingScars MonthlyMeanTemp_June   1.7978     0.202 3713.0000     8.9005
    ## 17 GrazingScars    TidalHeightBinary  -0.1015    0.0878 3713.0000    -1.1550
    ## 18 GrazingScars           YearBinary  -1.1452    0.1109 3713.0000   -10.3236
    ## 19 BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ## 20 BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ## 21 BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ## 22 BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ## 23 BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ## 24 BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ## 25 BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ## 26   Prevalence         GrazingScars   0.8425    0.0905 3713.0000     9.3137
    ## 27   Prevalence         BladeAreaLog   1.2570    0.1708 3713.0000     7.3606
    ## 28   Prevalence             Epifauna   1.0430    0.3099 3713.0000     3.3654
    ## 29   Prevalence         CanopyHeight   0.9271    0.5341 3713.0000     1.7360
    ## 30   Prevalence           DensityLog   1.2724    0.3852 3713.0000     3.3034
    ## 31   Prevalence    TempAnomWarm_June   0.0015    0.0131 3713.0000     0.1130
    ## 32   Prevalence MonthlyMeanTemp_June   0.1256    0.1236 3713.0000     1.0164
    ## 33   Prevalence    TidalHeightBinary   0.2600     0.082 3713.0000     3.1723
    ## 34   Prevalence           YearBinary   0.2636    0.1169 3713.0000     2.2555
    ## 35 ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2227 ***
    ## 2   0.0000      -0.4991 ***
    ## 3   0.0000      -0.5268 ***
    ## 4   0.0000      -0.3181 ***
    ## 5   0.0000      -0.0947 ***
    ## 6   0.0000      -0.4038 ***
    ## 7   0.0000       0.4462 ***
    ## 8   0.0000      -0.1744 ***
    ## 9   0.0000      -0.4666 ***
    ## 10  0.0000       1.2690 ***
    ## 11  0.0000      -0.0566 ***
    ## 12  0.0602       0.1262    
    ## 13  0.2856      -0.1042    
    ## 14  0.0000       0.2067 ***
    ## 15  0.1246      -0.1186    
    ## 16  0.0000       1.8258 ***
    ## 17  0.2481      -0.0225    
    ## 18  0.0000      -0.2446 ***
    ## 19  0.1268       0.0599    
    ## 20  0.0000       0.5653 ***
    ## 21  0.4136       0.0425    
    ## 22  0.0007       0.1193 ***
    ## 23  0.0529      -0.1609    
    ## 24  0.0000      -0.2451 ***
    ## 25  0.0269       0.0304   *
    ## 26  0.0000       0.1747 ***
    ## 27  0.0000       0.2393 ***
    ## 28  0.0008       0.2301 ***
    ## 29  0.0826       0.1283    
    ## 30  0.0010       0.2842 ***
    ## 31  0.9101       0.0065    
    ## 32  0.3094       0.1280    
    ## 33  0.0015       0.0579  **
    ## 34  0.0241       0.0565   *
    ## 35  0.0000       0.1798 ***

This full model fits the data well (Fisher C p-valueu = 0.669). Direct
temperature effects on prevalence are not significant. AIC is 117.8

### alternative model: no temp effects on prev

``` r
sem2 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis),
  glmer(Prevalence ~ GrazingScars+ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
summary(sem2)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem2 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ GrazingScars + BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  118.310   454.168
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                            Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##      Prevalence ~ TempAnomWarm_June + ...      coef 3713.000     0.7010  0.4833 
    ##   Prevalence ~ MonthlyMeanTemp_June + ...      coef 3713.000     1.2316  0.2181 
    ##    CanopyHeight ~ TidalHeightBinary + ...      coef 3683.005     0.2028  0.6525 
    ##      DensityLog ~ TidalHeightBinary + ...      coef 3683.002     0.1617  0.6876 
    ##        Epifauna ~ TidalHeightBinary + ...      coef 3681.007     0.0516  0.8203 
    ##             GrazingScars ~ Epifauna + ...      coef 3713.000    -1.4439  0.1488 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 10.31 with P-value = 0.589 and on 12 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ##       Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ##       Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ##       Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ##       Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ##   CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ##   CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ##   CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ##     DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ##     DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ##     DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ##   GrazingScars         CanopyHeight   0.9155    0.4872 3713.0000     1.8791
    ##   GrazingScars           DensityLog  -0.4680    0.4383 3713.0000    -1.0678
    ##   GrazingScars         BladeAreaLog   1.0896     0.182 3713.0000     5.9881
    ##   GrazingScars    TempAnomWarm_June  -0.0270    0.0176 3713.0000    -1.5356
    ##   GrazingScars MonthlyMeanTemp_June   1.7978     0.202 3713.0000     8.9005
    ##   GrazingScars    TidalHeightBinary  -0.1015    0.0878 3713.0000    -1.1550
    ##   GrazingScars           YearBinary  -1.1452    0.1109 3713.0000   -10.3236
    ##   BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ##   BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ##   BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ##   BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ##   BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ##   BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ##     Prevalence         GrazingScars   0.8630     0.089 3713.0000     9.6943
    ##     Prevalence         BladeAreaLog   1.2536    0.1703 3713.0000     7.3598
    ##     Prevalence             Epifauna   0.9011    0.2644 3713.0000     3.4078
    ##     Prevalence         CanopyHeight   0.6399    0.4245 3713.0000     1.5077
    ##     Prevalence           DensityLog   1.2937    0.3368 3713.0000     3.8412
    ##     Prevalence    TidalHeightBinary   0.2600    0.0819 3713.0000     3.1737
    ##     Prevalence           YearBinary   0.2490    0.1153 3713.0000     2.1588
    ##   ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2227 ***
    ##    0.0000      -0.4991 ***
    ##    0.0000      -0.5268 ***
    ##    0.0000      -0.3181 ***
    ##    0.0000      -0.0947 ***
    ##    0.0000      -0.4038 ***
    ##    0.0000       0.4462 ***
    ##    0.0000      -0.1744 ***
    ##    0.0000      -0.4666 ***
    ##    0.0000        1.269 ***
    ##    0.0000      -0.0566 ***
    ##    0.0602            -    
    ##    0.2856            -    
    ##    0.0000            - ***
    ##    0.1246            -    
    ##    0.0000            - ***
    ##    0.2481            -    
    ##    0.0000            - ***
    ##    0.1268       0.0599    
    ##    0.0000       0.5653 ***
    ##    0.4136       0.0425    
    ##    0.0007       0.1193 ***
    ##    0.0529      -0.1609    
    ##    0.0000      -0.2451 ***
    ##    0.0269       0.0304   *
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.0007            - ***
    ##    0.1316            -    
    ##    0.0001            - ***
    ##    0.0015            -  **
    ##    0.0309            -   *
    ##    0.0000       0.1798 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.27        0.96
    ##   CanopyHeight   none     0.15        0.99
    ##     DensityLog   none     0.36        1.00
    ##   GrazingScars  delta     0.44        0.88
    ##   BladeAreaLog   none     0.53        0.66
    ##     Prevalence  delta     0.08        0.33

This model fits the data well. Global fit test p=0.589. AIC=118.3. No
missing paths from d-sep. So, little evidence for direct temperature
effects on prevalence.

Was going to test each temperature effect separately, but I don’t think
it’s necessary since the paths are not significnat in d-sep above. Skip
next two alternative models.

### alternative model: June Mean only effect on prev

``` r
# sem3 <- psem(
#   lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
#          CanopyHeight + DensityLog 
#        + YearBinary +
#          (1|Meadow) + (1|Region),
#        data=dis),
#   lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
#          YearBinary + 
#          (1|Meadow) + (1|Region),
#        data=dis),
#   lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
#          YearBinary + 
#          (1|Meadow) + (1|Region),
#        data=dis),
#   glmer(GrazingScars ~ CanopyHeight + DensityLog + 
#           BladeAreaLog +  
#           TempAnomWarm_June + MonthlyMeanTemp_June + 
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=dis,
#         family = "binomial"),
#   lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
#          TempAnomWarm_June + MonthlyMeanTemp_June +
#          TidalHeightBinary + YearBinary +
#          (1|Region) + (1|Meadow),
#        data=dis),
#   glmer(Prevalence ~ GrazingScars+ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
#           MonthlyMeanTemp_June +
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=dis,
#         family = "binomial"),
#   DensityLog%~~%CanopyHeight
# )
# summary(sem3)
```

### alternative model: June Anom only effect on prev

``` r
# sem4 <- psem(
#   lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
#          CanopyHeight + DensityLog 
#        + YearBinary +
#          (1|Meadow) + (1|Region),
#        data=dis),
#   lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
#          YearBinary + 
#          (1|Meadow) + (1|Region),
#        data=dis),
#   lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
#          YearBinary + 
#          (1|Meadow) + (1|Region),
#        data=dis),
#   glmer(GrazingScars ~ CanopyHeight + DensityLog + 
#           BladeAreaLog +  
#           TempAnomWarm_June + MonthlyMeanTemp_June + 
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=dis,
#         family = "binomial"),
#   lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
#          TempAnomWarm_June + MonthlyMeanTemp_June +
#          TidalHeightBinary + YearBinary +
#          (1|Region) + (1|Meadow),
#        data=dis),
#   glmer(Prevalence ~ GrazingScars+ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
#           TempAnomWarm_June + 
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=dis,
#         family = "binomial"),
#   DensityLog%~~%CanopyHeight
# )
# summary(sem4)
```

### alternative model: reverse gz-prev direction

``` r
sem5 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  Prevalence + 
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis),
  glmer(Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0032231 (tol = 0.002, component 1)

``` r
summary(sem5)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem5 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + Prevalence + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  119.337   467.634
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 3683.005     0.2028  0.6525 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 3683.002     0.1617  0.6876 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 3681.007     0.0516  0.8203 
    ##            GrazingScars ~ Epifauna + ...      coef 3713.000    -1.8163  0.0693 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 7.337 with P-value = 0.501 and on 8 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ##       Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ##       Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ##       Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ##       Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ##   CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ##   CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ##   CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ##     DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ##     DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ##     DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ##   GrazingScars         CanopyHeight   0.8846    0.4878 3713.0000     1.8136
    ##   GrazingScars           DensityLog  -0.5502    0.4388 3713.0000    -1.2536
    ##   GrazingScars         BladeAreaLog   0.8910    0.1849 3713.0000     4.8179
    ##   GrazingScars           Prevalence   0.8108    0.0906 3713.0000     8.9472
    ##   GrazingScars    TempAnomWarm_June  -0.0224    0.0179 3713.0000    -1.2511
    ##   GrazingScars MonthlyMeanTemp_June   1.7400    0.2069 3713.0000     8.4098
    ##   GrazingScars    TidalHeightBinary  -0.1392    0.0892 3713.0000    -1.5608
    ##   GrazingScars           YearBinary  -1.1693    0.1123 3713.0000   -10.4125
    ##   BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ##   BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ##   BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ##   BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ##   BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ##   BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ##     Prevalence         BladeAreaLog   1.3676    0.1688 3713.0000     8.0993
    ##     Prevalence             Epifauna   0.9604    0.3087 3713.0000     3.1113
    ##     Prevalence         CanopyHeight   0.8854    0.5422 3713.0000     1.6328
    ##     Prevalence           DensityLog   1.1972    0.4091 3713.0000     2.9264
    ##     Prevalence    TempAnomWarm_June  -0.0008    0.0151 3713.0000    -0.0525
    ##     Prevalence MonthlyMeanTemp_June   0.3443    0.1593 3713.0000     2.1609
    ##     Prevalence    TidalHeightBinary   0.2346    0.0807 3713.0000     2.9049
    ##     Prevalence           YearBinary   0.0818    0.1145 3713.0000     0.7148
    ##   ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2227 ***
    ##    0.0000      -0.4991 ***
    ##    0.0000      -0.5268 ***
    ##    0.0000      -0.3181 ***
    ##    0.0000      -0.0947 ***
    ##    0.0000      -0.4038 ***
    ##    0.0000       0.4462 ***
    ##    0.0000      -0.1744 ***
    ##    0.0000      -0.4666 ***
    ##    0.0000        1.269 ***
    ##    0.0000      -0.0566 ***
    ##    0.0697            -    
    ##    0.2100            -    
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.2109            -    
    ##    0.0000            - ***
    ##    0.1186            -    
    ##    0.0000            - ***
    ##    0.1268       0.0599    
    ##    0.0000       0.5653 ***
    ##    0.4136       0.0425    
    ##    0.0007       0.1193 ***
    ##    0.0529      -0.1609    
    ##    0.0000      -0.2451 ***
    ##    0.0269       0.0304   *
    ##    0.0000            - ***
    ##    0.0019            -  **
    ##    0.1025            -    
    ##    0.0034            -  **
    ##    0.9582            -    
    ##    0.0307            -   *
    ##    0.0037            -  **
    ##    0.4747            -    
    ##    0.0000       0.1798 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.27        0.96
    ##   CanopyHeight   none     0.15        0.99
    ##     DensityLog   none     0.36        1.00
    ##   GrazingScars  delta     0.44        0.87
    ##   BladeAreaLog   none     0.53        0.66
    ##     Prevalence  delta     0.10        0.41

``` r
coefs(sem5)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ## 2      Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ## 3      Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ## 4      Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ## 5      Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ## 7  CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ## 8  CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ## 9    DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ## 10   DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ## 11   DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ## 12 GrazingScars         CanopyHeight   0.8846    0.4878 3713.0000     1.8136
    ## 13 GrazingScars           DensityLog  -0.5502    0.4388 3713.0000    -1.2536
    ## 14 GrazingScars         BladeAreaLog   0.8910    0.1849 3713.0000     4.8179
    ## 15 GrazingScars           Prevalence   0.8108    0.0906 3713.0000     8.9472
    ## 16 GrazingScars    TempAnomWarm_June  -0.0224    0.0179 3713.0000    -1.2511
    ## 17 GrazingScars MonthlyMeanTemp_June   1.7400    0.2069 3713.0000     8.4098
    ## 18 GrazingScars    TidalHeightBinary  -0.1392    0.0892 3713.0000    -1.5608
    ## 19 GrazingScars           YearBinary  -1.1693    0.1123 3713.0000   -10.4125
    ## 20 BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ## 21 BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ## 22 BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ## 23 BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ## 24 BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ## 25 BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ## 26 BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ## 27   Prevalence         BladeAreaLog   1.3676    0.1688 3713.0000     8.0993
    ## 28   Prevalence             Epifauna   0.9604    0.3087 3713.0000     3.1113
    ## 29   Prevalence         CanopyHeight   0.8854    0.5422 3713.0000     1.6328
    ## 30   Prevalence           DensityLog   1.1972    0.4091 3713.0000     2.9264
    ## 31   Prevalence    TempAnomWarm_June  -0.0008    0.0151 3713.0000    -0.0525
    ## 32   Prevalence MonthlyMeanTemp_June   0.3443    0.1593 3713.0000     2.1609
    ## 33   Prevalence    TidalHeightBinary   0.2346    0.0807 3713.0000     2.9049
    ## 34   Prevalence           YearBinary   0.0818    0.1145 3713.0000     0.7148
    ## 35 ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2227 ***
    ## 2   0.0000      -0.4991 ***
    ## 3   0.0000      -0.5268 ***
    ## 4   0.0000      -0.3181 ***
    ## 5   0.0000      -0.0947 ***
    ## 6   0.0000      -0.4038 ***
    ## 7   0.0000       0.4462 ***
    ## 8   0.0000      -0.1744 ***
    ## 9   0.0000      -0.4666 ***
    ## 10  0.0000       1.2690 ***
    ## 11  0.0000      -0.0566 ***
    ## 12  0.0697       0.1199    
    ## 13  0.2100      -0.1203    
    ## 14  0.0000       0.1661 ***
    ## 15  0.0000       0.1760 ***
    ## 16  0.2109      -0.0967    
    ## 17  0.0000       1.7370 ***
    ## 18  0.1186      -0.0304    
    ## 19  0.0000      -0.2455 ***
    ## 20  0.1268       0.0599    
    ## 21  0.0000       0.5653 ***
    ## 22  0.4136       0.0425    
    ## 23  0.0007       0.1193 ***
    ## 24  0.0529      -0.1609    
    ## 25  0.0000      -0.2451 ***
    ## 26  0.0269       0.0304   *
    ## 27  0.0000       0.2646 ***
    ## 28  0.0019       0.2153  **
    ## 29  0.1025       0.1245    
    ## 30  0.0034       0.2718  **
    ## 31  0.9582      -0.0035    
    ## 32  0.0307       0.3567   *
    ## 33  0.0037       0.0531  **
    ## 34  0.4747       0.0178    
    ## 35  0.0000       0.1798 ***

AIC is 119.3 (slightly higher than for opposite direction). Passes
global fit test (p=0.501). Not immediately clear if this direction is
better… look at coefs in rendered version.

### alternative model: no temperature effects on gz

``` r
sem6 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis),
  glmer(Prevalence ~ GrazingScars+ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
summary(sem6)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem6 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TidalHeightBinary + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ GrazingScars + BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  360.076   695.934
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                              Independ.Claim Test.Type       DF Crit.Value
    ##      GrazingScars ~ TempAnomWarm_June + ...      coef 3713.000     8.8190
    ##   GrazingScars ~ MonthlyMeanTemp_June + ...      coef 3713.000    12.5751
    ##      CanopyHeight ~ TidalHeightBinary + ...      coef 3683.005     0.2028
    ##        DensityLog ~ TidalHeightBinary + ...      coef 3683.002     0.1617
    ##          Epifauna ~ TidalHeightBinary + ...      coef 3681.007     0.0516
    ##               GrazingScars ~ Epifauna + ...      coef 3713.000    -1.4439
    ##   P.Value    
    ##    0.0000 ***
    ##    0.0000 ***
    ##    0.6525    
    ##    0.6876    
    ##    0.8203    
    ##    0.1488    
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 252.076 with P-value = 0 and on 12 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ##       Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ##       Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ##       Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ##       Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ##   CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ##   CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ##   CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ##     DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ##     DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ##     DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ##   GrazingScars         CanopyHeight  -1.2671    0.4594 3713.0000    -2.7585
    ##   GrazingScars           DensityLog   0.7465    0.4141 3713.0000     1.8026
    ##   GrazingScars         BladeAreaLog   1.0256    0.1765 3713.0000     5.8121
    ##   GrazingScars    TidalHeightBinary  -0.1039    0.0857 3713.0000    -1.2115
    ##   GrazingScars           YearBinary  -1.0013    0.1083 3713.0000    -9.2452
    ##   BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ##   BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ##   BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ##   BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ##   BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ##   BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ##     Prevalence         GrazingScars   0.8425    0.0905 3713.0000     9.3137
    ##     Prevalence         BladeAreaLog   1.2570    0.1708 3713.0000     7.3606
    ##     Prevalence             Epifauna   1.0430    0.3099 3713.0000     3.3654
    ##     Prevalence         CanopyHeight   0.9271    0.5341 3713.0000     1.7360
    ##     Prevalence           DensityLog   1.2724    0.3852 3713.0000     3.3034
    ##     Prevalence    TempAnomWarm_June   0.0015    0.0131 3713.0000     0.1130
    ##     Prevalence MonthlyMeanTemp_June   0.1256    0.1236 3713.0000     1.0164
    ##     Prevalence    TidalHeightBinary   0.2600     0.082 3713.0000     3.1723
    ##     Prevalence           YearBinary   0.2636    0.1169 3713.0000     2.2555
    ##   ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2227 ***
    ##    0.0000      -0.4991 ***
    ##    0.0000      -0.5268 ***
    ##    0.0000      -0.3181 ***
    ##    0.0000      -0.0947 ***
    ##    0.0000      -0.4038 ***
    ##    0.0000       0.4462 ***
    ##    0.0000      -0.1744 ***
    ##    0.0000      -0.4666 ***
    ##    0.0000        1.269 ***
    ##    0.0000      -0.0566 ***
    ##    0.0058            -  **
    ##    0.0715            -    
    ##    0.0000            - ***
    ##    0.2257            -    
    ##    0.0000            - ***
    ##    0.1268       0.0599    
    ##    0.0000       0.5653 ***
    ##    0.4136       0.0425    
    ##    0.0007       0.1193 ***
    ##    0.0529      -0.1609    
    ##    0.0000      -0.2451 ***
    ##    0.0269       0.0304   *
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.0008            - ***
    ##    0.0826            -    
    ##    0.0010            - ***
    ##    0.9101            -    
    ##    0.3094            -    
    ##    0.0015            -  **
    ##    0.0241            -   *
    ##    0.0000       0.1798 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.27        0.96
    ##   CanopyHeight   none     0.15        0.99
    ##     DensityLog   none     0.36        1.00
    ##   GrazingScars  delta     0.06        0.31
    ##   BladeAreaLog   none     0.53        0.66
    ##     Prevalence  delta     0.09        0.33

Fails global fit (p=0) therefor need the temperature effects on grazing.

Based on this, I don’t think we need the below either \### alternative
model: remove temp paths to gz and prev

``` r
# sem7 <- psem(
#   lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
#          CanopyHeight + DensityLog 
#        + YearBinary +
#          (1|Meadow) + (1|Region),
#        data=dis),
#   lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
#          YearBinary + 
#          (1|Meadow) + (1|Region),
#        data=dis),
#   lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
#          YearBinary + 
#          (1|Meadow) + (1|Region),
#        data=dis),
#   glmer(GrazingScars ~ CanopyHeight + DensityLog + 
#           BladeAreaLog +  
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=dis,
#         family = "binomial"),
#   lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
#          TempAnomWarm_June + MonthlyMeanTemp_June +
#          TidalHeightBinary + YearBinary +
#          (1|Region) + (1|Meadow),
#        data=dis),
#   glmer(Prevalence ~ GrazingScars+ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
#           TidalHeightBinary + YearBinary + 
#           (1|Region) + (1|Meadow),
#         data=dis,
#         family = "binomial"),
#   DensityLog%~~%CanopyHeight
# )
# summary(sem7)
```

### alternative model: prev and gz are correlated errors (missing driver)

``` r
sem8 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary + 
         (1|Meadow) + (1|Region),
       data=dis),
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=dis),
  glmer(Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis,
        family = "binomial"),
  DensityLog%~~%CanopyHeight,
  Prevalence%~~%GrazingScars
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0032231 (tol = 0.002, component 1)

``` r
summary(sem8)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem8 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   Prevalence ~ BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ##   Prevalence ~~ GrazingScars
    ## 
    ##     AIC      BIC
    ##  115.810   457.888
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 3683.005     0.2028  0.6525 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 3683.002     0.1617  0.6876 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 3681.007     0.0516  0.8203 
    ##            GrazingScars ~ Epifauna + ...      coef 3713.000    -1.4439  0.1488 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 5.81 with P-value = 0.669 and on 8 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response            Predictor Estimate Std.Error        DF Crit.Value
    ##       Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ##       Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ##       Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ##       Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ##       Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ##   CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ##   CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ##   CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ##     DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ##     DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ##     DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ##   GrazingScars         CanopyHeight   0.9155    0.4872 3713.0000     1.8791
    ##   GrazingScars           DensityLog  -0.4680    0.4383 3713.0000    -1.0678
    ##   GrazingScars         BladeAreaLog   1.0896     0.182 3713.0000     5.9881
    ##   GrazingScars    TempAnomWarm_June  -0.0270    0.0176 3713.0000    -1.5356
    ##   GrazingScars MonthlyMeanTemp_June   1.7978     0.202 3713.0000     8.9005
    ##   GrazingScars    TidalHeightBinary  -0.1015    0.0878 3713.0000    -1.1550
    ##   GrazingScars           YearBinary  -1.1452    0.1109 3713.0000   -10.3236
    ##   BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ##   BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ##   BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ##   BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ##   BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ##   BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ##   BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ##     Prevalence         BladeAreaLog   1.3676    0.1688 3713.0000     8.0993
    ##     Prevalence             Epifauna   0.9604    0.3087 3713.0000     3.1113
    ##     Prevalence         CanopyHeight   0.8854    0.5422 3713.0000     1.6328
    ##     Prevalence           DensityLog   1.1972    0.4091 3713.0000     2.9264
    ##     Prevalence    TempAnomWarm_June  -0.0008    0.0151 3713.0000    -0.0525
    ##     Prevalence MonthlyMeanTemp_June   0.3443    0.1593 3713.0000     2.1609
    ##     Prevalence    TidalHeightBinary   0.2346    0.0807 3713.0000     2.9049
    ##     Prevalence           YearBinary   0.0818    0.1145 3713.0000     0.7148
    ##   ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ##   ~~Prevalence       ~~GrazingScars   0.1546         - 3713.0000     9.5297
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.2227 ***
    ##    0.0000      -0.4991 ***
    ##    0.0000      -0.5268 ***
    ##    0.0000      -0.3181 ***
    ##    0.0000      -0.0947 ***
    ##    0.0000      -0.4038 ***
    ##    0.0000       0.4462 ***
    ##    0.0000      -0.1744 ***
    ##    0.0000      -0.4666 ***
    ##    0.0000        1.269 ***
    ##    0.0000      -0.0566 ***
    ##    0.0602            -    
    ##    0.2856            -    
    ##    0.0000            - ***
    ##    0.1246            -    
    ##    0.0000            - ***
    ##    0.2481            -    
    ##    0.0000            - ***
    ##    0.1268       0.0599    
    ##    0.0000       0.5653 ***
    ##    0.4136       0.0425    
    ##    0.0007       0.1193 ***
    ##    0.0529      -0.1609    
    ##    0.0000      -0.2451 ***
    ##    0.0269       0.0304   *
    ##    0.0000            - ***
    ##    0.0019            -  **
    ##    0.1025            -    
    ##    0.0034            -  **
    ##    0.9582            -    
    ##    0.0307            -   *
    ##    0.0037            -  **
    ##    0.4747            -    
    ##    0.0000       0.1798 ***
    ##    0.0000       0.1546 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##       Epifauna   none     0.27        0.96
    ##   CanopyHeight   none     0.15        0.99
    ##     DensityLog   none     0.36        1.00
    ##   GrazingScars  delta     0.44        0.88
    ##   BladeAreaLog   none     0.53        0.66
    ##     Prevalence  delta     0.10        0.41

``` r
coefs(sem8)
```

    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1      Epifauna    TempAnomWarm_June  -0.0112     8e-04 2538.5113   199.8682
    ## 2      Epifauna MonthlyMeanTemp_June  -0.1080    0.0095 2148.0529   127.3461
    ## 3      Epifauna         CanopyHeight  -0.8399    0.0248 3676.4211  1145.7689
    ## 4      Epifauna           DensityLog  -0.3142    0.0237 3045.4441   172.9067
    ## 5      Epifauna           YearBinary  -0.0974    0.0052 3698.1442   350.0907
    ## 6  CanopyHeight    TempAnomWarm_June  -0.0127     4e-04 3687.7176  1297.8447
    ## 7  CanopyHeight MonthlyMeanTemp_June   0.0606    0.0048 3674.6530   161.6830
    ## 8  CanopyHeight           YearBinary  -0.1125    0.0029 3685.4072  1496.0890
    ## 9    DensityLog    TempAnomWarm_June  -0.0237     4e-04 3689.7396  4221.6149
    ## 10   DensityLog MonthlyMeanTemp_June   0.2781    0.0049 3694.8913  3184.5687
    ## 11   DensityLog           YearBinary  -0.0590     0.003 3684.5785   384.7701
    ## 12 GrazingScars         CanopyHeight   0.9155    0.4872 3713.0000     1.8791
    ## 13 GrazingScars           DensityLog  -0.4680    0.4383 3713.0000    -1.0678
    ## 14 GrazingScars         BladeAreaLog   1.0896     0.182 3713.0000     5.9881
    ## 15 GrazingScars    TempAnomWarm_June  -0.0270    0.0176 3713.0000    -1.5356
    ## 16 GrazingScars MonthlyMeanTemp_June   1.7978     0.202 3713.0000     8.9005
    ## 17 GrazingScars    TidalHeightBinary  -0.1015    0.0878 3713.0000    -1.1550
    ## 18 GrazingScars           YearBinary  -1.1452    0.1109 3713.0000   -10.3236
    ## 19 BladeAreaLog             Epifauna   0.0517    0.0329  314.8792     2.3438
    ## 20 BladeAreaLog         CanopyHeight   0.7779    0.0536  223.0180   198.2248
    ## 21 BladeAreaLog           DensityLog   0.0362    0.0414  108.6548     0.6734
    ## 22 BladeAreaLog    TempAnomWarm_June   0.0052    0.0014   63.2957    12.6022
    ## 23 BladeAreaLog MonthlyMeanTemp_June  -0.0301    0.0133   18.0266     4.2936
    ## 24 BladeAreaLog    TidalHeightBinary  -0.2095    0.0077 3681.0385   740.6943
    ## 25 BladeAreaLog           YearBinary   0.0270    0.0121 2387.6198     4.9057
    ## 26   Prevalence         BladeAreaLog   1.3676    0.1688 3713.0000     8.0993
    ## 27   Prevalence             Epifauna   0.9604    0.3087 3713.0000     3.1113
    ## 28   Prevalence         CanopyHeight   0.8854    0.5422 3713.0000     1.6328
    ## 29   Prevalence           DensityLog   1.1972    0.4091 3713.0000     2.9264
    ## 30   Prevalence    TempAnomWarm_June  -0.0008    0.0151 3713.0000    -0.0525
    ## 31   Prevalence MonthlyMeanTemp_June   0.3443    0.1593 3713.0000     2.1609
    ## 32   Prevalence    TidalHeightBinary   0.2346    0.0807 3713.0000     2.9049
    ## 33   Prevalence           YearBinary   0.0818    0.1145 3713.0000     0.7148
    ## 34 ~~DensityLog       ~~CanopyHeight   0.1798         - 3713.0000    11.1315
    ## 35 ~~Prevalence       ~~GrazingScars   0.1546         - 3713.0000     9.5297
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.2227 ***
    ## 2   0.0000      -0.4991 ***
    ## 3   0.0000      -0.5268 ***
    ## 4   0.0000      -0.3181 ***
    ## 5   0.0000      -0.0947 ***
    ## 6   0.0000      -0.4038 ***
    ## 7   0.0000       0.4462 ***
    ## 8   0.0000      -0.1744 ***
    ## 9   0.0000      -0.4666 ***
    ## 10  0.0000       1.2690 ***
    ## 11  0.0000      -0.0566 ***
    ## 12  0.0602       0.1262    
    ## 13  0.2856      -0.1042    
    ## 14  0.0000       0.2067 ***
    ## 15  0.1246      -0.1186    
    ## 16  0.0000       1.8258 ***
    ## 17  0.2481      -0.0225    
    ## 18  0.0000      -0.2446 ***
    ## 19  0.1268       0.0599    
    ## 20  0.0000       0.5653 ***
    ## 21  0.4136       0.0425    
    ## 22  0.0007       0.1193 ***
    ## 23  0.0529      -0.1609    
    ## 24  0.0000      -0.2451 ***
    ## 25  0.0269       0.0304   *
    ## 26  0.0000       0.2646 ***
    ## 27  0.0019       0.2153  **
    ## 28  0.1025       0.1245    
    ## 29  0.0034       0.2718  **
    ## 30  0.9582      -0.0035    
    ## 31  0.0307       0.3567   *
    ## 32  0.0037       0.0531  **
    ## 33  0.4747       0.0178    
    ## 34  0.0000       0.1798 ***
    ## 35  0.0000       0.1546 ***

Passes global fit (p=0.669), AIC=115.8, which is 2 lower than for other
SEM with grazing driving prevalence. Need to look at coefs in rendered
version but seems like prevalence and grazing could both be responding
to another driver. Could be environmental (salinity?) Or could be
something simple like proportion lacunae or other grazer species…

This is tough. Not very straight forward.  
- Temperature does NOT directly influence prevalence  
- Grazing and prevalence are related  
- Possibly an alternative covariate is affecting both grazing scar
presence and prevalence
