Untitled
================

# SEMs comparing epifauna vs lacuna vs ampithoid

``` r
# data ###
dis <- read_csv("data/epiphyte_SEM_data.csv")
```

    ## Rows: 1724 Columns: 41
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
# dis$BladeArea_cm2 <- dis$BladeArea/100 # already in cm2 previously
dis$YearFactor <- gsub("2019", "1", dis$YearFactor)
dis$YearFactor <- gsub("2020", "2", dis$YearFactor)
dis$YearFactor <- gsub("2021", "3", dis$YearFactor)
dis2 <- dis[-c(which(is.na(dis$EpiphyteDryMass))),] # need this because epiphytes missing e.g. OR 2020
dis2$EpiphyteDetected <- ifelse(dis2$EpiphyteDryMass==0, 0, 1)
```

## Prevalence + Epifauna

``` r
# dat <- select(dis2, c(Epifauna, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, DensityLog, 
#                       YearFactor, Meadow, Region, EpiphyteDetected, BladeAreaLog, Epifauna, 
#                       TidalHeightBinary, Prevalence, LesionArea, Lacuna, Ampithoid))
sem_prev_epi <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearFactor +
         (1|Meadow) + (1|Region),
       data=dis2),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearFactor + 
         (1|Meadow) + (1|Region),
       data=dis2),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearFactor + 
         (1|Meadow) + (1|Region),
       data=dis2),
  glmer(EpiphyteDetected ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  Epifauna +
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearFactor + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearFactor +
         (1|Region) + (1|Meadow),
       data=dis2),
  glmer(Prevalence ~ EpiphyteDetected + BladeAreaLog + Epifauna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearFactor + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0657611 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0161975 (tol = 0.002, component 1)

``` r
summary(sem_prev_epi)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning: Categorical variables detected. Please refer to documentation for
    ## interpretation of Estimates!

    ## 
    ## Structural Equation Model of sem_prev_epi 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearFactor
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearFactor
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearFactor
    ##   EpiphyteDetected ~ CanopyHeight + DensityLog + BladeAreaLog + Epifauna + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   Prevalence ~ EpiphyteDetected + BladeAreaLog + Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  126.998   464.484
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1535.017     0.0034  0.9537 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1535.106     0.0078  0.9295 
    ##       Epifauna ~ TidalHeightBinary + ...      coef 1533.122     0.1647  0.6850 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.998 with P-value = 0.986 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##           Response            Predictor Estimate Std.Error        DF
    ##           Epifauna    TempAnomWarm_June   0.0076    0.0018  246.4741
    ##           Epifauna MonthlyMeanTemp_June  -0.1672    0.0383  164.7210
    ##           Epifauna         CanopyHeight  -1.0209    0.0755 1197.6759
    ##           Epifauna           DensityLog   0.1023    0.0365 1544.0928
    ##           Epifauna           YearFactor        -         -    2.0000
    ##           Epifauna       YearFactor = 2   0.0833    0.2704    4.7657
    ##           Epifauna       YearFactor = 3   0.2242    0.2696    4.7211
    ##           Epifauna       YearFactor = 1   0.4133    0.2706    4.7616
    ##       CanopyHeight    TempAnomWarm_June  -0.0098     6e-04 1360.0976
    ##       CanopyHeight MonthlyMeanTemp_June   0.0898    0.0138 1272.8499
    ##       CanopyHeight           YearFactor        -         -    2.0000
    ##       CanopyHeight       YearFactor = 2   0.6234     0.225    4.9542
    ##       CanopyHeight       YearFactor = 3   0.7028    0.2249    4.9465
    ##       CanopyHeight       YearFactor = 1   0.7667    0.2251    4.9571
    ##         DensityLog    TempAnomWarm_June  -0.0084    0.0012  250.3098
    ##         DensityLog MonthlyMeanTemp_June   0.0147    0.0269  166.2420
    ##         DensityLog           YearFactor        -         -    2.0000
    ##         DensityLog       YearFactor = 2   2.0512    0.1853    4.7780
    ##         DensityLog       YearFactor = 3   2.1011    0.1848    4.7390
    ##         DensityLog       YearFactor = 1   2.2633    0.1855    4.7790
    ##   EpiphyteDetected         CanopyHeight   5.7974    0.0011 1567.0000
    ##   EpiphyteDetected           DensityLog   1.8519    0.0011 1567.0000
    ##   EpiphyteDetected         BladeAreaLog   2.3888    0.0011 1567.0000
    ##   EpiphyteDetected             Epifauna   4.8779    0.0011 1567.0000
    ##   EpiphyteDetected    TempAnomWarm_June   0.0738    0.0011 1567.0000
    ##   EpiphyteDetected MonthlyMeanTemp_June  -1.3749    0.0011 1567.0000
    ##   EpiphyteDetected    TidalHeightBinary  -0.6078    0.0011 1567.0000
    ##   EpiphyteDetected           YearFactor        -         -    2.0000
    ##   EpiphyteDetected       YearFactor = 1   4.3925    0.0187       Inf
    ##   EpiphyteDetected       YearFactor = 3   6.0315    0.0187       Inf
    ##   EpiphyteDetected       YearFactor = 2  10.2206    0.5321       Inf
    ##       BladeAreaLog             Epifauna  -0.0246    0.0304  482.0897
    ##       BladeAreaLog         CanopyHeight   0.8339    0.0691   75.9310
    ##       BladeAreaLog           DensityLog    0.004    0.0402  238.5568
    ##       BladeAreaLog    TempAnomWarm_June   -3e-04    0.0014   64.6209
    ##       BladeAreaLog MonthlyMeanTemp_June  -0.0426    0.0189    7.8407
    ##       BladeAreaLog    TidalHeightBinary  -0.2286    0.0123 1533.7234
    ##       BladeAreaLog           YearFactor        -         -    2.0000
    ##       BladeAreaLog       YearFactor = 1   1.3262    0.0589    4.0531
    ##       BladeAreaLog       YearFactor = 2   1.3681    0.0613    4.9671
    ##       BladeAreaLog       YearFactor = 3   1.3898     0.058    4.1132
    ##         Prevalence     EpiphyteDetected  -0.1833    0.3477 1567.0000
    ##         Prevalence         BladeAreaLog   1.6183    0.2694 1567.0000
    ##         Prevalence             Epifauna   0.7509    0.2985 1567.0000
    ##         Prevalence         CanopyHeight   1.4105    0.6036 1567.0000
    ##         Prevalence           DensityLog   1.8738    0.3765 1567.0000
    ##         Prevalence    TempAnomWarm_June   0.0186    0.0143 1567.0000
    ##         Prevalence MonthlyMeanTemp_June   0.3272    0.2198 1567.0000
    ##         Prevalence    TidalHeightBinary   0.5209    0.1285 1567.0000
    ##         Prevalence           YearFactor        -         -    2.0000
    ##         Prevalence       YearFactor = 1  -0.7206    0.5459       Inf
    ##         Prevalence       YearFactor = 3  -0.4066    0.5344       Inf
    ##         Prevalence       YearFactor = 2  -0.2057    0.5979       Inf
    ##       ~~DensityLog       ~~CanopyHeight   0.2613         - 1567.0000
    ##     Crit.Value P.Value Std.Estimate    
    ##        15.6162  0.0001       0.1317 ***
    ##        17.1740  0.0001      -0.6558 ***
    ##       178.3480  0.0000      -0.6826 ***
    ##         7.7784  0.0054       0.1022  **
    ##       115.2180  0.0000            - ***
    ##         0.3081  0.7710            -    
    ##         0.8317  0.4456            -    
    ##         1.5274  0.1901            -    
    ##       246.5592  0.0000      -0.2525 ***
    ##        41.5060  0.0000       0.5266 ***
    ##       259.4728  0.0000            - ***
    ##         2.7702  0.0397            -   *
    ##         3.1245  0.0265            -   *
    ##         3.4060  0.0194            -   *
    ##        43.5916  0.0000      -0.1459 ***
    ##         0.2680  0.6054       0.0576    
    ##       156.6666  0.0000            - ***
    ##        11.0695  0.0001            - ***
    ##        11.3713  0.0001            - ***
    ##        12.1984  0.0001            - ***
    ##      5320.1332  0.0000            - ***
    ##      1699.4100  0.0000            - ***
    ##      2192.1179  0.0000            - ***
    ##      4476.6411  0.0000            - ***
    ##        67.7624  0.0000            - ***
    ##     -1260.9076  0.0000            - ***
    ##      -557.7514  0.0000            - ***
    ##   2262887.1009  0.0000            - ***
    ##       235.0693  0.0000            - ***
    ##       322.2390  0.0000            - ***
    ##        19.2069  0.0000            - ***
    ##         0.6308  0.4274      -0.0288    
    ##       132.7153  0.0000       0.6505 ***
    ##         0.0088  0.9252       0.0046    
    ##         0.0332  0.8560      -0.0052    
    ##         4.5211  0.0669      -0.1949    
    ##       345.2749  0.0000      -0.2568 ***
    ##         5.2616  0.0054            -  **
    ##        22.5344  0.0000            - ***
    ##        22.3221  0.0000            - ***
    ##        23.9777  0.0000            - ***
    ##        -0.5271  0.5981            -    
    ##         6.0079  0.0000            - ***
    ##         2.5160  0.0119            -   *
    ##         2.3367  0.0195            -   *
    ##         4.9767  0.0000            - ***
    ##         1.3010  0.1933            -    
    ##         1.4886  0.1366            -    
    ##         4.0543  0.0001            - ***
    ##         3.4067  0.1821            -    
    ##        -1.3200  0.1868            -    
    ##        -0.7609  0.4467            -    
    ##        -0.3440  0.7308            -    
    ##        10.7074  0.0000       0.2613 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##           Response method Marginal Conditional
    ##           Epifauna   none     0.25        0.96
    ##       CanopyHeight   none     0.09        0.99
    ##         DensityLog   none     0.04        0.94
    ##   EpiphyteDetected  delta     0.26        0.78
    ##       BladeAreaLog   none     0.58        0.71
    ##         Prevalence  delta     0.16        0.42

Model passes global fit. Epifauna is positive and significant predictor
for Prevalence

``` r
coefs(sem_prev_epi)
```

    ## Warning: Categorical or non-linear variables detected. Please refer to
    ## documentation for interpretation of Estimates!

    ##            Response            Predictor Estimate Std.Error        DF
    ## 1          Epifauna    TempAnomWarm_June   0.0076    0.0018  246.4741
    ## 2          Epifauna MonthlyMeanTemp_June  -0.1672    0.0383  164.7210
    ## 3          Epifauna         CanopyHeight  -1.0209    0.0755 1197.6759
    ## 4          Epifauna           DensityLog   0.1023    0.0365 1544.0928
    ## 5          Epifauna           YearFactor        -         -    2.0000
    ## 6          Epifauna       YearFactor = 2   0.0833    0.2704    4.7657
    ## 7          Epifauna       YearFactor = 3   0.2242    0.2696    4.7211
    ## 8          Epifauna       YearFactor = 1   0.4133    0.2706    4.7616
    ## 9      CanopyHeight    TempAnomWarm_June  -0.0098     6e-04 1360.0976
    ## 10     CanopyHeight MonthlyMeanTemp_June   0.0898    0.0138 1272.8499
    ## 11     CanopyHeight           YearFactor        -         -    2.0000
    ## 12     CanopyHeight       YearFactor = 2   0.6234     0.225    4.9542
    ## 13     CanopyHeight       YearFactor = 3   0.7028    0.2249    4.9465
    ## 14     CanopyHeight       YearFactor = 1   0.7667    0.2251    4.9571
    ## 15       DensityLog    TempAnomWarm_June  -0.0084    0.0012  250.3098
    ## 16       DensityLog MonthlyMeanTemp_June   0.0147    0.0269  166.2420
    ## 17       DensityLog           YearFactor        -         -    2.0000
    ## 18       DensityLog       YearFactor = 2   2.0512    0.1853    4.7780
    ## 19       DensityLog       YearFactor = 3   2.1011    0.1848    4.7390
    ## 20       DensityLog       YearFactor = 1   2.2633    0.1855    4.7790
    ## 21 EpiphyteDetected         CanopyHeight   5.7974    0.0011 1567.0000
    ## 22 EpiphyteDetected           DensityLog   1.8519    0.0011 1567.0000
    ## 23 EpiphyteDetected         BladeAreaLog   2.3888    0.0011 1567.0000
    ## 24 EpiphyteDetected             Epifauna   4.8779    0.0011 1567.0000
    ## 25 EpiphyteDetected    TempAnomWarm_June   0.0738    0.0011 1567.0000
    ## 26 EpiphyteDetected MonthlyMeanTemp_June  -1.3749    0.0011 1567.0000
    ## 27 EpiphyteDetected    TidalHeightBinary  -0.6078    0.0011 1567.0000
    ## 28 EpiphyteDetected           YearFactor        -         -    2.0000
    ## 29 EpiphyteDetected       YearFactor = 1   4.3925    0.0187       Inf
    ## 30 EpiphyteDetected       YearFactor = 3   6.0315    0.0187       Inf
    ## 31 EpiphyteDetected       YearFactor = 2  10.2206    0.5321       Inf
    ## 32     BladeAreaLog             Epifauna  -0.0246    0.0304  482.0897
    ## 33     BladeAreaLog         CanopyHeight   0.8339    0.0691   75.9310
    ## 34     BladeAreaLog           DensityLog    0.004    0.0402  238.5568
    ## 35     BladeAreaLog    TempAnomWarm_June   -3e-04    0.0014   64.6209
    ## 36     BladeAreaLog MonthlyMeanTemp_June  -0.0426    0.0189    7.8407
    ## 37     BladeAreaLog    TidalHeightBinary  -0.2286    0.0123 1533.7234
    ## 38     BladeAreaLog           YearFactor        -         -    2.0000
    ## 39     BladeAreaLog       YearFactor = 1   1.3262    0.0589    4.0531
    ## 40     BladeAreaLog       YearFactor = 2   1.3681    0.0613    4.9671
    ## 41     BladeAreaLog       YearFactor = 3   1.3898     0.058    4.1132
    ## 42       Prevalence     EpiphyteDetected  -0.1833    0.3477 1567.0000
    ## 43       Prevalence         BladeAreaLog   1.6183    0.2694 1567.0000
    ## 44       Prevalence             Epifauna   0.7509    0.2985 1567.0000
    ## 45       Prevalence         CanopyHeight   1.4105    0.6036 1567.0000
    ## 46       Prevalence           DensityLog   1.8738    0.3765 1567.0000
    ## 47       Prevalence    TempAnomWarm_June   0.0186    0.0143 1567.0000
    ## 48       Prevalence MonthlyMeanTemp_June   0.3272    0.2198 1567.0000
    ## 49       Prevalence    TidalHeightBinary   0.5209    0.1285 1567.0000
    ## 50       Prevalence           YearFactor        -         -    2.0000
    ## 51       Prevalence       YearFactor = 1  -0.7206    0.5459       Inf
    ## 52       Prevalence       YearFactor = 3  -0.4066    0.5344       Inf
    ## 53       Prevalence       YearFactor = 2  -0.2057    0.5979       Inf
    ## 54     ~~DensityLog       ~~CanopyHeight   0.2613         - 1567.0000
    ##      Crit.Value P.Value Std.Estimate    
    ## 1       15.6162  0.0001            - ***
    ## 2       17.1740  0.0001            - ***
    ## 3      178.3480  0.0000            - ***
    ## 4        7.7784  0.0054            -  **
    ## 5      115.2180  0.0000            - ***
    ## 6        0.3081  0.7710            -    
    ## 7        0.8317  0.4456            -    
    ## 8        1.5274  0.1901            -    
    ## 9      246.5592  0.0000            - ***
    ## 10      41.5060  0.0000            - ***
    ## 11     259.4728  0.0000            - ***
    ## 12       2.7702  0.0397            -   *
    ## 13       3.1245  0.0265            -   *
    ## 14       3.4060  0.0194            -   *
    ## 15      43.5916  0.0000            - ***
    ## 16       0.2680  0.6054            -    
    ## 17     156.6666  0.0000            - ***
    ## 18      11.0695  0.0001            - ***
    ## 19      11.3713  0.0001            - ***
    ## 20      12.1984  0.0001            - ***
    ## 21    5320.1332  0.0000            - ***
    ## 22    1699.4100  0.0000            - ***
    ## 23    2192.1179  0.0000            - ***
    ## 24    4476.6411  0.0000            - ***
    ## 25      67.7624  0.0000            - ***
    ## 26   -1260.9076  0.0000            - ***
    ## 27    -557.7514  0.0000            - ***
    ## 28 2262887.1009  0.0000            - ***
    ## 29     235.0693  0.0000            - ***
    ## 30     322.2390  0.0000            - ***
    ## 31      19.2069  0.0000            - ***
    ## 32       0.6308  0.4274            -    
    ## 33     132.7153  0.0000            - ***
    ## 34       0.0088  0.9252            -    
    ## 35       0.0332  0.8560            -    
    ## 36       4.5211  0.0669            -    
    ## 37     345.2749  0.0000            - ***
    ## 38       5.2616  0.0054            -  **
    ## 39      22.5344  0.0000            - ***
    ## 40      22.3221  0.0000            - ***
    ## 41      23.9777  0.0000            - ***
    ## 42      -0.5271  0.5981            -    
    ## 43       6.0079  0.0000            - ***
    ## 44       2.5160  0.0119            -   *
    ## 45       2.3367  0.0195            -   *
    ## 46       4.9767  0.0000            - ***
    ## 47       1.3010  0.1933            -    
    ## 48       1.4886  0.1366            -    
    ## 49       4.0543  0.0001            - ***
    ## 50       3.4067  0.1821            -    
    ## 51      -1.3200  0.1868            -    
    ## 52      -0.7609  0.4467            -    
    ## 53      -0.3440  0.7308            -    
    ## 54      10.7074  0.0000       0.2613 ***

## prev + lac

``` r
dis3 <- subset(dis2, Lacuna>0)
dis3 <- dis2[-which(is.na(dis2$Lacuna)),]
sem_prev_lac <- psem(
  lmer(Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearFactor +
         (1|Meadow) + (1|Region),
       data=dis3),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearFactor + 
         (1|Meadow) + (1|Region),
       data=dis3),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearFactor + 
         (1|Meadow) + (1|Region),
       data=dis3),
  glmer(EpiphyteDetected ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  Lacuna +
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearFactor + 
          (1|Region) + (1|Meadow),
        data=dis3,
        family = "binomial"),
  lmer(BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearFactor +
         (1|Region) + (1|Meadow),
       data=dis3),
  glmer(Prevalence ~ EpiphyteDetected + BladeAreaLog + Lacuna + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearFactor + 
          (1|Region) + (1|Meadow),
        data=dis3,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0444122 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00290066 (tol = 0.002, component 1)

``` r
summary(sem_prev_lac)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning: Categorical variables detected. Please refer to documentation for
    ## interpretation of Estimates!

    ## 
    ## Structural Equation Model of sem_prev_lac 
    ## 
    ## Call:
    ##   Lacuna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearFactor
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearFactor
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearFactor
    ##   EpiphyteDetected ~ CanopyHeight + DensityLog + BladeAreaLog + Lacuna + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   BladeAreaLog ~ Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   Prevalence ~ EpiphyteDetected + BladeAreaLog + Lacuna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  126.338   456.589
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1369.002     0.0066  0.9350 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1369.004     0.0037  0.9514 
    ##         Lacuna ~ TidalHeightBinary + ...      coef 1367.007     0.0040  0.9494 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.338 with P-value = 0.999 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##           Response            Predictor Estimate Std.Error        DF Crit.Value
    ##             Lacuna    TempAnomWarm_June  -0.0282    0.0017 1386.8368   287.6230
    ##             Lacuna MonthlyMeanTemp_June   0.8755    0.0311 1385.5983   790.0767
    ##             Lacuna         CanopyHeight  -0.8863    0.0561 1329.6019   247.6103
    ##             Lacuna           DensityLog   -3e-04    0.0343 1377.0695     0.0001
    ##             Lacuna           YearFactor        -         -    2.0000    64.6152
    ##             Lacuna       YearFactor = 2  -0.8507    0.3971    4.0045    -2.1422
    ##             Lacuna       YearFactor = 1  -0.6397    0.3969    3.9953    -1.6117
    ##             Lacuna       YearFactor = 3  -0.6393    0.3969    3.9968    -1.6107
    ##       CanopyHeight    TempAnomWarm_June  -0.0125     7e-04 1384.9429   337.5495
    ##       CanopyHeight MonthlyMeanTemp_June   0.1247    0.0145 1387.6152    73.5429
    ##       CanopyHeight           YearFactor        -         -    2.0000   298.8005
    ##       CanopyHeight       YearFactor = 2   0.7913    0.1303    4.0040     6.0710
    ##       CanopyHeight       YearFactor = 3   0.9185    0.1303    3.9941     7.0513
    ##       CanopyHeight       YearFactor = 1    0.963    0.1302    3.9875     7.3958
    ##         DensityLog    TempAnomWarm_June  -0.0184    0.0011 1389.9360   271.8225
    ##         DensityLog MonthlyMeanTemp_June   0.1198    0.0238 1391.7813    25.0994
    ##         DensityLog           YearFactor        -         -    2.0000   429.3625
    ##         DensityLog       YearFactor = 2   1.8743    0.2077    4.0098     9.0237
    ##         DensityLog       YearFactor = 3     2.12    0.2076    3.9991    10.2139
    ##         DensityLog       YearFactor = 1   2.2129    0.2075    3.9921    10.6662
    ##   EpiphyteDetected         CanopyHeight    3.815    4.3208 1397.0000     0.8829
    ##   EpiphyteDetected           DensityLog  -7.9327    3.0357 1397.0000    -2.6131
    ##   EpiphyteDetected         BladeAreaLog   2.4485    0.7528 1397.0000     3.2527
    ##   EpiphyteDetected               Lacuna   3.9937    1.9475 1397.0000     2.0507
    ##   EpiphyteDetected    TempAnomWarm_June  -0.1577    0.1533 1397.0000    -1.0287
    ##   EpiphyteDetected MonthlyMeanTemp_June  -4.4884    2.2652 1397.0000    -1.9815
    ##   EpiphyteDetected    TidalHeightBinary  -0.6059    0.4033 1397.0000    -1.5022
    ##   EpiphyteDetected           YearFactor        -         -    2.0000     1.5669
    ##   EpiphyteDetected       YearFactor = 2   5.4744    1.9149       Inf     2.8588
    ##   EpiphyteDetected       YearFactor = 1   6.9289    2.1098       Inf     3.2841
    ##   EpiphyteDetected       YearFactor = 3   8.3387    2.6764       Inf     3.1157
    ##       BladeAreaLog               Lacuna   0.0057    0.0358   39.8959     0.0202
    ##       BladeAreaLog         CanopyHeight   0.9331    0.0784   87.5623   120.1280
    ##       BladeAreaLog           DensityLog  -0.0031    0.0488  119.2581     0.0034
    ##       BladeAreaLog    TempAnomWarm_June   0.0024    0.0027  109.6574     0.7031
    ##       BladeAreaLog MonthlyMeanTemp_June  -0.1125    0.0474   20.9562     4.1848
    ##       BladeAreaLog    TidalHeightBinary   -0.248    0.0132 1367.1925   354.6272
    ##       BladeAreaLog           YearFactor        -         -    2.0000     3.3241
    ##       BladeAreaLog       YearFactor = 1   1.3545    0.0444    3.0495    30.5152
    ##       BladeAreaLog       YearFactor = 2   1.3925    0.0518    5.6341    26.8739
    ##       BladeAreaLog       YearFactor = 3   1.4129    0.0452    3.1936    31.2393
    ##         Prevalence     EpiphyteDetected  -0.4089    0.4213 1397.0000    -0.9707
    ##         Prevalence         BladeAreaLog   1.7424    0.2894 1397.0000     6.0203
    ##         Prevalence               Lacuna   0.7969    0.3358 1397.0000     2.3732
    ##         Prevalence         CanopyHeight   0.4854     0.712 1397.0000     0.6817
    ##         Prevalence           DensityLog   1.4305    0.4228 1397.0000     3.3835
    ##         Prevalence    TempAnomWarm_June  -0.0335     0.027 1397.0000    -1.2423
    ##         Prevalence MonthlyMeanTemp_June   0.9348    0.5268 1397.0000     1.7744
    ##         Prevalence    TidalHeightBinary   0.6154    0.1406 1397.0000     4.3774
    ##         Prevalence           YearFactor        -         -    2.0000     6.7387
    ##         Prevalence       YearFactor = 1  -0.3658    0.7168       Inf    -0.5103
    ##         Prevalence       YearFactor = 2  -0.2837    0.7627       Inf    -0.3720
    ##         Prevalence       YearFactor = 3   0.1399    0.7264       Inf     0.1925
    ##       ~~DensityLog       ~~CanopyHeight   0.1691         - 1397.0000     6.4056
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.4292 ***
    ##    0.0000       1.1593 ***
    ##    0.0000      -0.4896 ***
    ##    0.9919       -3e-04    
    ##    0.0000            - ***
    ##    0.0988            -    
    ##    0.1824            -    
    ##    0.1826            -    
    ##    0.0000      -0.3448 ***
    ##    0.0000        0.299 ***
    ##    0.0000            - ***
    ##    0.0037            -  **
    ##    0.0021            -  **
    ##    0.0018            -  **
    ##    0.0000      -0.3179 ***
    ##    0.0000       0.1794 ***
    ##    0.0000            - ***
    ##    0.0008            - ***
    ##    0.0005            - ***
    ##    0.0004            - ***
    ##    0.3773            -    
    ##    0.0090            -  **
    ##    0.0011            -  **
    ##    0.0403            -   *
    ##    0.3036            -    
    ##    0.0475            -   *
    ##    0.1330            -    
    ##    0.4568            -    
    ##    0.0043            -  **
    ##    0.0010            -  **
    ##    0.0018            -  **
    ##    0.8876       0.0079    
    ##    0.0000       0.7163 ***
    ##    0.9534      -0.0038    
    ##    0.4036        0.051    
    ##    0.0535      -0.2071    
    ##    0.0000      -0.2868 ***
    ##    0.0366            -   *
    ##    0.0001            - ***
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.3317            -    
    ##    0.0000            - ***
    ##    0.0176            -   *
    ##    0.4954            -    
    ##    0.0007            - ***
    ##    0.2141            -    
    ##    0.0760            -    
    ##    0.0000            - ***
    ##    0.0344            -   *
    ##    0.6098            -    
    ##    0.7099            -    
    ##    0.8473            -    
    ##    0.0000       0.1691 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##           Response method Marginal Conditional
    ##             Lacuna   none     0.31        0.99
    ##       CanopyHeight   none     0.08        0.97
    ##         DensityLog   none     0.08        0.97
    ##   EpiphyteDetected  delta     0.39        0.52
    ##       BladeAreaLog   none     0.59        0.67
    ##         Prevalence  delta     0.15        0.48

``` r
coefs(sem_prev_lac)
```

    ## Warning: Categorical or non-linear variables detected. Please refer to
    ## documentation for interpretation of Estimates!

    ##            Response            Predictor Estimate Std.Error        DF
    ## 1            Lacuna    TempAnomWarm_June  -0.0282    0.0017 1386.8368
    ## 2            Lacuna MonthlyMeanTemp_June   0.8755    0.0311 1385.5983
    ## 3            Lacuna         CanopyHeight  -0.8863    0.0561 1329.6019
    ## 4            Lacuna           DensityLog   -3e-04    0.0343 1377.0695
    ## 5            Lacuna           YearFactor        -         -    2.0000
    ## 6            Lacuna       YearFactor = 2  -0.8507    0.3971    4.0045
    ## 7            Lacuna       YearFactor = 1  -0.6397    0.3969    3.9953
    ## 8            Lacuna       YearFactor = 3  -0.6393    0.3969    3.9968
    ## 9      CanopyHeight    TempAnomWarm_June  -0.0125     7e-04 1384.9429
    ## 10     CanopyHeight MonthlyMeanTemp_June   0.1247    0.0145 1387.6152
    ## 11     CanopyHeight           YearFactor        -         -    2.0000
    ## 12     CanopyHeight       YearFactor = 2   0.7913    0.1303    4.0040
    ## 13     CanopyHeight       YearFactor = 3   0.9185    0.1303    3.9941
    ## 14     CanopyHeight       YearFactor = 1    0.963    0.1302    3.9875
    ## 15       DensityLog    TempAnomWarm_June  -0.0184    0.0011 1389.9360
    ## 16       DensityLog MonthlyMeanTemp_June   0.1198    0.0238 1391.7813
    ## 17       DensityLog           YearFactor        -         -    2.0000
    ## 18       DensityLog       YearFactor = 2   1.8743    0.2077    4.0098
    ## 19       DensityLog       YearFactor = 3     2.12    0.2076    3.9991
    ## 20       DensityLog       YearFactor = 1   2.2129    0.2075    3.9921
    ## 21 EpiphyteDetected         CanopyHeight    3.815    4.3208 1397.0000
    ## 22 EpiphyteDetected           DensityLog  -7.9327    3.0357 1397.0000
    ## 23 EpiphyteDetected         BladeAreaLog   2.4485    0.7528 1397.0000
    ## 24 EpiphyteDetected               Lacuna   3.9937    1.9475 1397.0000
    ## 25 EpiphyteDetected    TempAnomWarm_June  -0.1577    0.1533 1397.0000
    ## 26 EpiphyteDetected MonthlyMeanTemp_June  -4.4884    2.2652 1397.0000
    ## 27 EpiphyteDetected    TidalHeightBinary  -0.6059    0.4033 1397.0000
    ## 28 EpiphyteDetected           YearFactor        -         -    2.0000
    ## 29 EpiphyteDetected       YearFactor = 2   5.4744    1.9149       Inf
    ## 30 EpiphyteDetected       YearFactor = 1   6.9289    2.1098       Inf
    ## 31 EpiphyteDetected       YearFactor = 3   8.3387    2.6764       Inf
    ## 32     BladeAreaLog               Lacuna   0.0057    0.0358   39.8959
    ## 33     BladeAreaLog         CanopyHeight   0.9331    0.0784   87.5623
    ## 34     BladeAreaLog           DensityLog  -0.0031    0.0488  119.2581
    ## 35     BladeAreaLog    TempAnomWarm_June   0.0024    0.0027  109.6574
    ## 36     BladeAreaLog MonthlyMeanTemp_June  -0.1125    0.0474   20.9562
    ## 37     BladeAreaLog    TidalHeightBinary   -0.248    0.0132 1367.1925
    ## 38     BladeAreaLog           YearFactor        -         -    2.0000
    ## 39     BladeAreaLog       YearFactor = 1   1.3545    0.0444    3.0495
    ## 40     BladeAreaLog       YearFactor = 2   1.3925    0.0518    5.6341
    ## 41     BladeAreaLog       YearFactor = 3   1.4129    0.0452    3.1936
    ## 42       Prevalence     EpiphyteDetected  -0.4089    0.4213 1397.0000
    ## 43       Prevalence         BladeAreaLog   1.7424    0.2894 1397.0000
    ## 44       Prevalence               Lacuna   0.7969    0.3358 1397.0000
    ## 45       Prevalence         CanopyHeight   0.4854     0.712 1397.0000
    ## 46       Prevalence           DensityLog   1.4305    0.4228 1397.0000
    ## 47       Prevalence    TempAnomWarm_June  -0.0335     0.027 1397.0000
    ## 48       Prevalence MonthlyMeanTemp_June   0.9348    0.5268 1397.0000
    ## 49       Prevalence    TidalHeightBinary   0.6154    0.1406 1397.0000
    ## 50       Prevalence           YearFactor        -         -    2.0000
    ## 51       Prevalence       YearFactor = 1  -0.3658    0.7168       Inf
    ## 52       Prevalence       YearFactor = 2  -0.2837    0.7627       Inf
    ## 53       Prevalence       YearFactor = 3   0.1399    0.7264       Inf
    ## 54     ~~DensityLog       ~~CanopyHeight   0.1691         - 1397.0000
    ##    Crit.Value P.Value Std.Estimate    
    ## 1    287.6230  0.0000            - ***
    ## 2    790.0767  0.0000            - ***
    ## 3    247.6103  0.0000            - ***
    ## 4      0.0001  0.9919            -    
    ## 5     64.6152  0.0000            - ***
    ## 6     -2.1422  0.0988            -    
    ## 7     -1.6117  0.1824            -    
    ## 8     -1.6107  0.1826            -    
    ## 9    337.5495  0.0000            - ***
    ## 10    73.5429  0.0000            - ***
    ## 11   298.8005  0.0000            - ***
    ## 12     6.0710  0.0037            -  **
    ## 13     7.0513  0.0021            -  **
    ## 14     7.3958  0.0018            -  **
    ## 15   271.8225  0.0000            - ***
    ## 16    25.0994  0.0000            - ***
    ## 17   429.3625  0.0000            - ***
    ## 18     9.0237  0.0008            - ***
    ## 19    10.2139  0.0005            - ***
    ## 20    10.6662  0.0004            - ***
    ## 21     0.8829  0.3773            -    
    ## 22    -2.6131  0.0090            -  **
    ## 23     3.2527  0.0011            -  **
    ## 24     2.0507  0.0403            -   *
    ## 25    -1.0287  0.3036            -    
    ## 26    -1.9815  0.0475            -   *
    ## 27    -1.5022  0.1330            -    
    ## 28     1.5669  0.4568            -    
    ## 29     2.8588  0.0043            -  **
    ## 30     3.2841  0.0010            -  **
    ## 31     3.1157  0.0018            -  **
    ## 32     0.0202  0.8876            -    
    ## 33   120.1280  0.0000            - ***
    ## 34     0.0034  0.9534            -    
    ## 35     0.7031  0.4036            -    
    ## 36     4.1848  0.0535            -    
    ## 37   354.6272  0.0000            - ***
    ## 38     3.3241  0.0366            -   *
    ## 39    30.5152  0.0001            - ***
    ## 40    26.8739  0.0000            - ***
    ## 41    31.2393  0.0000            - ***
    ## 42    -0.9707  0.3317            -    
    ## 43     6.0203  0.0000            - ***
    ## 44     2.3732  0.0176            -   *
    ## 45     0.6817  0.4954            -    
    ## 46     3.3835  0.0007            - ***
    ## 47    -1.2423  0.2141            -    
    ## 48     1.7744  0.0760            -    
    ## 49     4.3774  0.0000            - ***
    ## 50     6.7387  0.0344            -   *
    ## 51    -0.5103  0.6098            -    
    ## 52    -0.3720  0.7099            -    
    ## 53     0.1925  0.8473            -    
    ## 54     6.4056  0.0000       0.1691 ***

## prev + amp

``` r
dis4 <- dis2[-which(is.na(dis2$Ampithoid)),]

sem_prev_amp <- psem(
  lmer(Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearFactor +
         (1|Meadow) + (1|Region),
       data=dis4),
  lmer(CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         YearFactor + 
         (1|Meadow) + (1|Region),
       data=dis4),
  lmer(DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June +
         YearFactor + 
         (1|Meadow) + (1|Region),
       data=dis4),
  glmer(EpiphyteDetected ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  Ampithoid +
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          TidalHeightBinary + YearFactor + 
          (1|Region) + (1|Meadow),
        data=dis4,
        family = "binomial"),
  lmer(BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         TidalHeightBinary + YearFactor +
         (1|Region) + (1|Meadow),
       data=dis4),
  glmer(Prevalence ~ EpiphyteDetected + BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + 
          TempAnomWarm_June + MonthlyMeanTemp_June +
          TidalHeightBinary + YearFactor + 
          (1|Region) + (1|Meadow),
        data=dis4,
        family = "binomial"),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.114957 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00617484 (tol = 0.002, component 1)

``` r
summary(sem_prev_amp)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning in B * (sd.x/sd.y): longer object length is not a multiple of shorter
    ## object length

    ## Warning: Categorical variables detected. Please refer to documentation for
    ## interpretation of Estimates!

    ## 
    ## Structural Equation Model of sem_prev_amp 
    ## 
    ## Call:
    ##   Ampithoid ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearFactor
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearFactor
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearFactor
    ##   EpiphyteDetected ~ CanopyHeight + DensityLog + BladeAreaLog + Ampithoid + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   BladeAreaLog ~ Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   Prevalence ~ EpiphyteDetected + BladeAreaLog + Ampithoid + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + TidalHeightBinary + YearFactor
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  126.477   457.624
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
    ##   CanopyHeight ~ TidalHeightBinary + ...      coef 1386.022     0.0010  0.9744 
    ##     DensityLog ~ TidalHeightBinary + ...      coef 1386.079     0.0255  0.8733 
    ##      Ampithoid ~ TidalHeightBinary + ...      coef 1384.021     0.0087  0.9259 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.477 with P-value = 0.998 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##           Response            Predictor Estimate Std.Error        DF Crit.Value
    ##          Ampithoid    TempAnomWarm_June    0.028    0.0017 1383.7765   272.3522
    ##          Ampithoid MonthlyMeanTemp_June  -0.6274    0.0352 1370.6257   313.2513
    ##          Ampithoid         CanopyHeight  -0.7991     0.059 1342.8841   181.7453
    ##          Ampithoid           DensityLog  -0.0849     0.031 1406.4872     7.4783
    ##          Ampithoid           YearFactor        -         -    2.0000   312.6606
    ##          Ampithoid       YearFactor = 3  -0.8183    0.9192    4.9758    -0.8903
    ##          Ampithoid       YearFactor = 2  -0.5308    0.9194    4.9795    -0.5773
    ##          Ampithoid       YearFactor = 1  -0.4144    0.9194    4.9810    -0.4507
    ##       CanopyHeight    TempAnomWarm_June  -0.0092     7e-04 1132.3872   154.6253
    ##       CanopyHeight MonthlyMeanTemp_June   0.0909    0.0157 1044.0865    32.4600
    ##       CanopyHeight           YearFactor        -         -    2.0000   191.3898
    ##       CanopyHeight       YearFactor = 2   0.6305     0.227    4.9374     2.7777
    ##       CanopyHeight       YearFactor = 3   0.6913    0.2269    4.9288     3.0471
    ##       CanopyHeight       YearFactor = 1   0.7656    0.2271    4.9416     3.3714
    ##         DensityLog    TempAnomWarm_June  -0.0032    0.0013  351.7335     5.2735
    ##         DensityLog MonthlyMeanTemp_June  -0.0572    0.0286  261.6749     3.6573
    ##         DensityLog           YearFactor        -         -    2.0000   198.2958
    ##         DensityLog       YearFactor = 3   2.0897    0.2291    4.7892     9.1219
    ##         DensityLog       YearFactor = 2   2.1106    0.2295    4.8162     9.1957
    ##         DensityLog       YearFactor = 1   2.3251    0.2298    4.8240    10.1187
    ##   EpiphyteDetected         CanopyHeight   5.1593    1.9621 1417.0000     2.6295
    ##   EpiphyteDetected           DensityLog   0.6064    1.1217 1417.0000     0.5406
    ##   EpiphyteDetected         BladeAreaLog   2.4285    0.7328 1417.0000     3.3143
    ##   EpiphyteDetected            Ampithoid   3.6402    1.2068 1417.0000     3.0165
    ##   EpiphyteDetected    TempAnomWarm_June   0.1278    0.0447 1417.0000     2.8585
    ##   EpiphyteDetected MonthlyMeanTemp_June  -1.2205    0.7483 1417.0000    -1.6310
    ##   EpiphyteDetected    TidalHeightBinary   -0.457    0.3817 1417.0000    -1.1972
    ##   EpiphyteDetected           YearFactor        -         -    2.0000    46.6168
    ##   EpiphyteDetected       YearFactor = 1   4.6231    2.5648       Inf     1.8025
    ##   EpiphyteDetected       YearFactor = 3   5.0992    2.5591       Inf     1.9926
    ##   EpiphyteDetected       YearFactor = 2  10.1836     2.696       Inf     3.7774
    ##       BladeAreaLog            Ampithoid   0.0503    0.0376  365.8333     1.6978
    ##       BladeAreaLog         CanopyHeight   0.8793    0.0732   81.5025   127.2183
    ##       BladeAreaLog           DensityLog  -0.0075    0.0432  170.9452     0.0265
    ##       BladeAreaLog    TempAnomWarm_June    1e-04    0.0014   61.0062     0.0103
    ##       BladeAreaLog MonthlyMeanTemp_June  -0.0443    0.0184    7.3391     5.2932
    ##       BladeAreaLog    TidalHeightBinary  -0.2476    0.0128 1384.7449   375.8766
    ##       BladeAreaLog           YearFactor        -         -    2.0000     3.2713
    ##       BladeAreaLog       YearFactor = 1   1.3164    0.0559    3.9490    23.5579
    ##       BladeAreaLog       YearFactor = 2   1.3599    0.0588    5.0631    23.1218
    ##       BladeAreaLog       YearFactor = 3   1.3764    0.0557    4.2858    24.7189
    ##         Prevalence     EpiphyteDetected   0.8068    0.3442 1417.0000     2.3440
    ##         Prevalence         BladeAreaLog   1.9471    0.3003 1417.0000     6.4843
    ##         Prevalence            Ampithoid   -1.404    0.3467 1417.0000    -4.0500
    ##         Prevalence         CanopyHeight  -0.9987    0.7011 1417.0000    -1.4245
    ##         Prevalence           DensityLog   1.7871    0.4421 1417.0000     4.0419
    ##         Prevalence    TempAnomWarm_June    0.001    0.0139 1417.0000     0.0716
    ##         Prevalence MonthlyMeanTemp_June   0.3401    0.1482 1417.0000     2.2957
    ##         Prevalence    TidalHeightBinary   0.5342    0.1411 1417.0000     3.7866
    ##         Prevalence           YearFactor        -         -    2.0000     1.2250
    ##         Prevalence       YearFactor = 2  -0.9101    0.5096       Inf    -1.7860
    ##         Prevalence       YearFactor = 3  -0.6049    0.4364       Inf    -1.3861
    ##         Prevalence       YearFactor = 1  -0.5867    0.4313       Inf    -1.3602
    ##       ~~DensityLog       ~~CanopyHeight   0.2376         - 1417.0000     9.1992
    ##   P.Value Std.Estimate    
    ##    0.0000        0.617 ***
    ##    0.0000      -3.1529 ***
    ##    0.0000      -0.6676 ***
    ##    0.0063      -0.1089  **
    ##    0.0000            - ***
    ##    0.4143            -    
    ##    0.5888            -    
    ##    0.6711            -    
    ##    0.0000      -0.2415 ***
    ##    0.0000       0.5467 ***
    ##    0.0000            - ***
    ##    0.0395            -   *
    ##    0.0291            -   *
    ##    0.0202            -   *
    ##    0.0222      -0.0547   *
    ##    0.0569       -0.224    
    ##    0.0000            - ***
    ##    0.0003            - ***
    ##    0.0003            - ***
    ##    0.0002            - ***
    ##    0.0086            -  **
    ##    0.5888            -    
    ##    0.0009            - ***
    ##    0.0026            -  **
    ##    0.0043            -  **
    ##    0.1029            -    
    ##    0.2312            -    
    ##    0.0000            - ***
    ##    0.0715            -    
    ##    0.0463            -   *
    ##    0.0002            - ***
    ##    0.1934        0.046    
    ##    0.0000       0.6718 ***
    ##    0.8708      -0.0087    
    ##    0.9193       0.0029    
    ##    0.0533      -0.2034    
    ##    0.0000      -0.2714 ***
    ##    0.0386            -   *
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.0191            -   *
    ##    0.0000            - ***
    ##    0.0001            - ***
    ##    0.1543            -    
    ##    0.0001            - ***
    ##    0.9429            -    
    ##    0.0217            -   *
    ##    0.0002            - ***
    ##    0.5420            -    
    ##    0.0741            -    
    ##    0.1657            -    
    ##    0.1738            -    
    ##    0.0000       0.2376 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##           Response method Marginal Conditional
    ##          Ampithoid   none     0.25        1.00
    ##       CanopyHeight   none     0.10        0.99
    ##         DensityLog   none     0.08        0.97
    ##   EpiphyteDetected  delta     0.24        0.67
    ##       BladeAreaLog   none     0.61        0.72
    ##         Prevalence  delta     0.18        0.38

ampithoid is negative for prevalence

``` r
coefs(sem_prev_amp)
```

    ## Warning: Categorical or non-linear variables detected. Please refer to
    ## documentation for interpretation of Estimates!

    ##            Response            Predictor Estimate Std.Error        DF
    ## 1         Ampithoid    TempAnomWarm_June    0.028    0.0017 1383.7765
    ## 2         Ampithoid MonthlyMeanTemp_June  -0.6274    0.0352 1370.6257
    ## 3         Ampithoid         CanopyHeight  -0.7991     0.059 1342.8841
    ## 4         Ampithoid           DensityLog  -0.0849     0.031 1406.4872
    ## 5         Ampithoid           YearFactor        -         -    2.0000
    ## 6         Ampithoid       YearFactor = 3  -0.8183    0.9192    4.9758
    ## 7         Ampithoid       YearFactor = 2  -0.5308    0.9194    4.9795
    ## 8         Ampithoid       YearFactor = 1  -0.4144    0.9194    4.9810
    ## 9      CanopyHeight    TempAnomWarm_June  -0.0092     7e-04 1132.3872
    ## 10     CanopyHeight MonthlyMeanTemp_June   0.0909    0.0157 1044.0865
    ## 11     CanopyHeight           YearFactor        -         -    2.0000
    ## 12     CanopyHeight       YearFactor = 2   0.6305     0.227    4.9374
    ## 13     CanopyHeight       YearFactor = 3   0.6913    0.2269    4.9288
    ## 14     CanopyHeight       YearFactor = 1   0.7656    0.2271    4.9416
    ## 15       DensityLog    TempAnomWarm_June  -0.0032    0.0013  351.7335
    ## 16       DensityLog MonthlyMeanTemp_June  -0.0572    0.0286  261.6749
    ## 17       DensityLog           YearFactor        -         -    2.0000
    ## 18       DensityLog       YearFactor = 3   2.0897    0.2291    4.7892
    ## 19       DensityLog       YearFactor = 2   2.1106    0.2295    4.8162
    ## 20       DensityLog       YearFactor = 1   2.3251    0.2298    4.8240
    ## 21 EpiphyteDetected         CanopyHeight   5.1593    1.9621 1417.0000
    ## 22 EpiphyteDetected           DensityLog   0.6064    1.1217 1417.0000
    ## 23 EpiphyteDetected         BladeAreaLog   2.4285    0.7328 1417.0000
    ## 24 EpiphyteDetected            Ampithoid   3.6402    1.2068 1417.0000
    ## 25 EpiphyteDetected    TempAnomWarm_June   0.1278    0.0447 1417.0000
    ## 26 EpiphyteDetected MonthlyMeanTemp_June  -1.2205    0.7483 1417.0000
    ## 27 EpiphyteDetected    TidalHeightBinary   -0.457    0.3817 1417.0000
    ## 28 EpiphyteDetected           YearFactor        -         -    2.0000
    ## 29 EpiphyteDetected       YearFactor = 1   4.6231    2.5648       Inf
    ## 30 EpiphyteDetected       YearFactor = 3   5.0992    2.5591       Inf
    ## 31 EpiphyteDetected       YearFactor = 2  10.1836     2.696       Inf
    ## 32     BladeAreaLog            Ampithoid   0.0503    0.0376  365.8333
    ## 33     BladeAreaLog         CanopyHeight   0.8793    0.0732   81.5025
    ## 34     BladeAreaLog           DensityLog  -0.0075    0.0432  170.9452
    ## 35     BladeAreaLog    TempAnomWarm_June    1e-04    0.0014   61.0062
    ## 36     BladeAreaLog MonthlyMeanTemp_June  -0.0443    0.0184    7.3391
    ## 37     BladeAreaLog    TidalHeightBinary  -0.2476    0.0128 1384.7449
    ## 38     BladeAreaLog           YearFactor        -         -    2.0000
    ## 39     BladeAreaLog       YearFactor = 1   1.3164    0.0559    3.9490
    ## 40     BladeAreaLog       YearFactor = 2   1.3599    0.0588    5.0631
    ## 41     BladeAreaLog       YearFactor = 3   1.3764    0.0557    4.2858
    ## 42       Prevalence     EpiphyteDetected   0.8068    0.3442 1417.0000
    ## 43       Prevalence         BladeAreaLog   1.9471    0.3003 1417.0000
    ## 44       Prevalence            Ampithoid   -1.404    0.3467 1417.0000
    ## 45       Prevalence         CanopyHeight  -0.9987    0.7011 1417.0000
    ## 46       Prevalence           DensityLog   1.7871    0.4421 1417.0000
    ## 47       Prevalence    TempAnomWarm_June    0.001    0.0139 1417.0000
    ## 48       Prevalence MonthlyMeanTemp_June   0.3401    0.1482 1417.0000
    ## 49       Prevalence    TidalHeightBinary   0.5342    0.1411 1417.0000
    ## 50       Prevalence           YearFactor        -         -    2.0000
    ## 51       Prevalence       YearFactor = 2  -0.9101    0.5096       Inf
    ## 52       Prevalence       YearFactor = 3  -0.6049    0.4364       Inf
    ## 53       Prevalence       YearFactor = 1  -0.5867    0.4313       Inf
    ## 54     ~~DensityLog       ~~CanopyHeight   0.2376         - 1417.0000
    ##    Crit.Value P.Value Std.Estimate    
    ## 1    272.3522  0.0000            - ***
    ## 2    313.2513  0.0000            - ***
    ## 3    181.7453  0.0000            - ***
    ## 4      7.4783  0.0063            -  **
    ## 5    312.6606  0.0000            - ***
    ## 6     -0.8903  0.4143            -    
    ## 7     -0.5773  0.5888            -    
    ## 8     -0.4507  0.6711            -    
    ## 9    154.6253  0.0000            - ***
    ## 10    32.4600  0.0000            - ***
    ## 11   191.3898  0.0000            - ***
    ## 12     2.7777  0.0395            -   *
    ## 13     3.0471  0.0291            -   *
    ## 14     3.3714  0.0202            -   *
    ## 15     5.2735  0.0222            -   *
    ## 16     3.6573  0.0569            -    
    ## 17   198.2958  0.0000            - ***
    ## 18     9.1219  0.0003            - ***
    ## 19     9.1957  0.0003            - ***
    ## 20    10.1187  0.0002            - ***
    ## 21     2.6295  0.0086            -  **
    ## 22     0.5406  0.5888            -    
    ## 23     3.3143  0.0009            - ***
    ## 24     3.0165  0.0026            -  **
    ## 25     2.8585  0.0043            -  **
    ## 26    -1.6310  0.1029            -    
    ## 27    -1.1972  0.2312            -    
    ## 28    46.6168  0.0000            - ***
    ## 29     1.8025  0.0715            -    
    ## 30     1.9926  0.0463            -   *
    ## 31     3.7774  0.0002            - ***
    ## 32     1.6978  0.1934            -    
    ## 33   127.2183  0.0000            - ***
    ## 34     0.0265  0.8708            -    
    ## 35     0.0103  0.9193            -    
    ## 36     5.2932  0.0533            -    
    ## 37   375.8766  0.0000            - ***
    ## 38     3.2713  0.0386            -   *
    ## 39    23.5579  0.0000            - ***
    ## 40    23.1218  0.0000            - ***
    ## 41    24.7189  0.0000            - ***
    ## 42     2.3440  0.0191            -   *
    ## 43     6.4843  0.0000            - ***
    ## 44    -4.0500  0.0001            - ***
    ## 45    -1.4245  0.1543            -    
    ## 46     4.0419  0.0001            - ***
    ## 47     0.0716  0.9429            -    
    ## 48     2.2957  0.0217            -   *
    ## 49     3.7866  0.0002            - ***
    ## 50     1.2250  0.5420            -    
    ## 51    -1.7860  0.0741            -    
    ## 52    -1.3861  0.1657            -    
    ## 53    -1.3602  0.1738            -    
    ## 54     9.1992  0.0000       0.2376 ***
