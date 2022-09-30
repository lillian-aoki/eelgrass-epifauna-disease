Blade-level SEMs with meadow-scale predictors
================

SEMs to assess temperature, grazing, disease relationships. All data
(2019 and 2021) from sites where available.

SEMs are set up with meadow-scale predictors (e.g. epifauna abundance,
canopy height) and modeled at the blade scale for lesion area, grazing
scars, and blade area.

Some relationships have been eliminated through the LMM/GLMM modeling
(e.g. epifauna does not predict grazing)

Note, remove tidal height because it gets messy (missing “path” from
tidal height to canopy, density even though they are measured at site
level)

Different SEMs compared to answer specific questions:  
- does temperature afffect lesion area directly or indirectly?  
- does epifauna abundance or grazing influence lesion area directly? -
does grazing drive disease or does disease drive grazing (i.e. increased
grazing increases disease versus diseaed plants accumulating more
grazing scars)

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
dis2 <- subset(dis, LesionArea>0)
dis2$LesionAreaLog <- log10(dis2$LesionArea)
```

## Lesion Area models

#### full model

``` r
les1 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
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
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
lmer(LesionAreaLog ~ GrazingScars + BladeAreaLog + Epifauna + DensityLog + CanopyHeight + 
               TempAnomWarm_June + MonthlyMeanTemp_June +
                YearBinary + (1|Meadow) + (1|Region),
             data=dis2),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(les1)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of les1 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~ GrazingScars + BladeAreaLog + Epifauna + DensityLog + CanopyHeight + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  108.898   401.415
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                  Independ.Claim Test.Type   DF Crit.Value P.Value 
    ##   GrazingScars ~ Epifauna + ...      coef 1664    -0.4699  0.6384 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.898 with P-value = 0.638 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ##        Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ##        Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ##        Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ##        Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ##    CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ##    CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ##    CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ##      DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ##      DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ##      DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ##    GrazingScars         CanopyHeight   0.6802    0.6756 1664.0000     1.0068
    ##    GrazingScars           DensityLog  -0.5606    0.5337 1664.0000    -1.0505
    ##    GrazingScars         BladeAreaLog   1.1143    0.2687 1664.0000     4.1470
    ##    GrazingScars    TempAnomWarm_June  -0.0558    0.0286 1664.0000    -1.9499
    ##    GrazingScars MonthlyMeanTemp_June   1.9906    0.3214 1664.0000     6.1927
    ##    GrazingScars           YearBinary  -1.5330    0.1741 1664.0000    -8.8041
    ##    BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ##    BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ##    BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ##    BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ##    BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ##   LesionAreaLog         GrazingScars   0.1006    0.0358 1634.6387     7.8599
    ##   LesionAreaLog         BladeAreaLog   0.4434    0.0675 1654.9916    42.9538
    ##   LesionAreaLog             Epifauna   0.4556    0.1163  107.4135    14.1766
    ##   LesionAreaLog           DensityLog   0.3956    0.1414   65.8157     6.6571
    ##   LesionAreaLog         CanopyHeight   0.9021    0.2108  100.3294    17.0585
    ##   LesionAreaLog    TempAnomWarm_June   0.0063    0.0052   46.8209     1.3629
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0755    0.0427   10.4685     2.6688
    ##   LesionAreaLog           YearBinary   0.1322    0.0501  774.4539     6.8063
    ##    ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.3298 ***
    ##    0.0000       -0.261 ***
    ##    0.0000      -0.6213 ***
    ##    0.0000        -0.39 ***
    ##    0.0000       -0.146 ***
    ##    0.0000      -0.6201 ***
    ##    0.0000       0.8334 ***
    ##    0.0000      -0.2207 ***
    ##    0.0000      -0.4317 ***
    ##    0.0000        1.183 ***
    ##    0.0000       -0.059 ***
    ##    0.3140            -    
    ##    0.2935            -    
    ##    0.0000            - ***
    ##    0.0512            -    
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.5822       0.0304    
    ##    0.0000       0.5662 ***
    ##    0.9227       0.0071    
    ##    0.0055         0.14  **
    ##    0.1682      -0.1359    
    ##    0.1681       0.0312    
    ##    0.0051       0.0698  **
    ##    0.0000       0.2482 ***
    ##    0.0003       0.3136 ***
    ##    0.0121        0.285   *
    ##    0.0001       0.3605 ***
    ##    0.2490       0.0852    
    ##    0.1320       0.2285    
    ##    0.0093       0.0917  **
    ##    0.0000       0.3194 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.38        0.95
    ##    CanopyHeight   none     0.18        0.99
    ##      DensityLog   none     0.32        1.00
    ##    GrazingScars  delta     0.43        0.91
    ##    BladeAreaLog   none     0.50        0.65
    ##   LesionAreaLog   none     0.11        0.37

``` r
coefs(les1)
```

    ##         Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1       Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ## 2       Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ## 3       Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ## 4       Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ## 5       Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ## 6   CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ## 7   CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ## 8   CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ## 9     DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ## 10    DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ## 11    DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ## 12  GrazingScars         CanopyHeight   0.6802    0.6756 1664.0000     1.0068
    ## 13  GrazingScars           DensityLog  -0.5606    0.5337 1664.0000    -1.0505
    ## 14  GrazingScars         BladeAreaLog   1.1143    0.2687 1664.0000     4.1470
    ## 15  GrazingScars    TempAnomWarm_June  -0.0558    0.0286 1664.0000    -1.9499
    ## 16  GrazingScars MonthlyMeanTemp_June   1.9906    0.3214 1664.0000     6.1927
    ## 17  GrazingScars           YearBinary  -1.5330    0.1741 1664.0000    -8.8041
    ## 18  BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ## 19  BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ## 20  BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ## 21  BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ## 22  BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ## 23  BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ## 24 LesionAreaLog         GrazingScars   0.1006    0.0358 1634.6387     7.8599
    ## 25 LesionAreaLog         BladeAreaLog   0.4434    0.0675 1654.9916    42.9538
    ## 26 LesionAreaLog             Epifauna   0.4556    0.1163  107.4135    14.1766
    ## 27 LesionAreaLog           DensityLog   0.3956    0.1414   65.8157     6.6571
    ## 28 LesionAreaLog         CanopyHeight   0.9021    0.2108  100.3294    17.0585
    ## 29 LesionAreaLog    TempAnomWarm_June   0.0063    0.0052   46.8209     1.3629
    ## 30 LesionAreaLog MonthlyMeanTemp_June   0.0755    0.0427   10.4685     2.6688
    ## 31 LesionAreaLog           YearBinary   0.1322    0.0501  774.4539     6.8063
    ## 32  ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.3298 ***
    ## 2   0.0000      -0.2610 ***
    ## 3   0.0000      -0.6213 ***
    ## 4   0.0000      -0.3900 ***
    ## 5   0.0000      -0.1460 ***
    ## 6   0.0000      -0.6201 ***
    ## 7   0.0000       0.8334 ***
    ## 8   0.0000      -0.2207 ***
    ## 9   0.0000      -0.4317 ***
    ## 10  0.0000       1.1830 ***
    ## 11  0.0000      -0.0590 ***
    ## 12  0.3140       0.0829    
    ## 13  0.2935      -0.1232    
    ## 14  0.0000       0.1903 ***
    ## 15  0.0512      -0.2299    
    ## 16  0.0000       1.8384 ***
    ## 17  0.0000      -0.3246 ***
    ## 18  0.5822       0.0304    
    ## 19  0.0000       0.5662 ***
    ## 20  0.9227       0.0071    
    ## 21  0.0055       0.1400  **
    ## 22  0.1682      -0.1359    
    ## 23  0.1681       0.0312    
    ## 24  0.0051       0.0698  **
    ## 25  0.0000       0.2482 ***
    ## 26  0.0003       0.3136 ***
    ## 27  0.0121       0.2850   *
    ## 28  0.0001       0.3605 ***
    ## 29  0.2490       0.0852    
    ## 30  0.1320       0.2285    
    ## 31  0.0093       0.0917  **
    ## 32  0.0000       0.3194 ***

Passes global fit (p=0.68), AIC = 108.898 Lesion area has grazing and
epifauna links, no temp effect significant

#### alternative model - no temp to lesion

``` r
les2 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
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
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
          YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
lmer(LesionAreaLog ~ GrazingScars + BladeAreaLog + Epifauna + DensityLog + CanopyHeight + 
                YearBinary + (1|Meadow) + (1|Region),
             data=dis2),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(les2)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of les2 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~ GrazingScars + BladeAreaLog + Epifauna + DensityLog + CanopyHeight + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  119.006   400.689
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                               Independ.Claim Test.Type        DF Crit.Value
    ##      LesionAreaLog ~ TempAnomWarm_June + ...      coef  211.7501     4.0383
    ##   LesionAreaLog ~ MonthlyMeanTemp_June + ...      coef   37.8371     6.0186
    ##                GrazingScars ~ Epifauna + ...      coef 1664.0000    -0.4699
    ##   P.Value  
    ##    0.0457 *
    ##    0.0189 *
    ##    0.6384  
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 15.006 with P-value = 0.02 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ##        Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ##        Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ##        Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ##        Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ##    CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ##    CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ##    CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ##      DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ##      DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ##      DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ##    GrazingScars         CanopyHeight   0.6802    0.6756 1664.0000     1.0068
    ##    GrazingScars           DensityLog  -0.5606    0.5337 1664.0000    -1.0505
    ##    GrazingScars         BladeAreaLog   1.1143    0.2687 1664.0000     4.1470
    ##    GrazingScars    TempAnomWarm_June  -0.0558    0.0286 1664.0000    -1.9499
    ##    GrazingScars MonthlyMeanTemp_June   1.9906    0.3214 1664.0000     6.1927
    ##    GrazingScars           YearBinary  -1.5330    0.1741 1664.0000    -8.8041
    ##    BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ##    BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ##    BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ##    BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ##    BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ##   LesionAreaLog         GrazingScars   0.1181    0.0352 1656.6097    11.2060
    ##   LesionAreaLog         BladeAreaLog   0.4423    0.0673 1645.9382    42.8073
    ##   LesionAreaLog             Epifauna   0.3329       0.1  155.7337    10.1063
    ##   LesionAreaLog           DensityLog   0.3330    0.1196   62.8137     6.4332
    ##   LesionAreaLog         CanopyHeight   0.6041    0.1684  194.3187    12.1621
    ##   LesionAreaLog           YearBinary   0.1021    0.0484 1197.7087     4.3472
    ##    ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.3298 ***
    ##    0.0000       -0.261 ***
    ##    0.0000      -0.6213 ***
    ##    0.0000        -0.39 ***
    ##    0.0000       -0.146 ***
    ##    0.0000      -0.6201 ***
    ##    0.0000       0.8334 ***
    ##    0.0000      -0.2207 ***
    ##    0.0000      -0.4317 ***
    ##    0.0000        1.183 ***
    ##    0.0000       -0.059 ***
    ##    0.3140            -    
    ##    0.2935            -    
    ##    0.0000            - ***
    ##    0.0512            -    
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.5822       0.0304    
    ##    0.0000       0.5662 ***
    ##    0.9227       0.0071    
    ##    0.0055         0.14  **
    ##    0.1682      -0.1359    
    ##    0.1681       0.0312    
    ##    0.0008        0.082 ***
    ##    0.0000       0.2476 ***
    ##    0.0018       0.2291  **
    ##    0.0137       0.2399   *
    ##    0.0006       0.2414 ***
    ##    0.0373       0.0709   *
    ##    0.0000       0.3194 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.38        0.95
    ##    CanopyHeight   none     0.18        0.99
    ##      DensityLog   none     0.32        1.00
    ##    GrazingScars  delta     0.43        0.91
    ##    BladeAreaLog   none     0.50        0.65
    ##   LesionAreaLog   none     0.10        0.31

``` r
# coefs(les2)
```

Does not pass global fit test (p = 0.02). AIC = 119.006. D-sep shows
temp effects on lesion area are missing and significant.

#### alternative model - no temp to grazing

``` r
les3 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
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
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
           YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
          YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
lmer(LesionAreaLog ~ GrazingScars + BladeAreaLog + Epifauna + DensityLog + CanopyHeight + 
       TempAnomWarm_June + MonthlyMeanTemp_June +
                YearBinary + (1|Meadow) + (1|Region),
             data=dis2),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(les3)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of les3 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~ GrazingScars + BladeAreaLog + Epifauna + DensityLog + CanopyHeight + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  192.544   474.227
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                              Independ.Claim Test.Type   DF Crit.Value P.Value
    ##      GrazingScars ~ TempAnomWarm_June + ...      coef 1664     4.3191  0.0000
    ##   GrazingScars ~ MonthlyMeanTemp_June + ...      coef 1664     7.8055  0.0000
    ##               GrazingScars ~ Epifauna + ...      coef 1664    -0.4699  0.6384
    ##      
    ##   ***
    ##   ***
    ##      
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 88.544 with P-value = 0 and on 6 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ##        Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ##        Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ##        Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ##        Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ##    CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ##    CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ##    CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ##      DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ##      DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ##      DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ##    GrazingScars         CanopyHeight  -0.7680    0.6017 1664.0000    -1.2765
    ##    GrazingScars           DensityLog   0.2753     0.509 1664.0000     0.5409
    ##    GrazingScars         BladeAreaLog   1.0510    0.2611 1664.0000     4.0258
    ##    GrazingScars           YearBinary  -1.5824     0.168 1664.0000    -9.4217
    ##    BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ##    BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ##    BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ##    BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ##    BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ##   LesionAreaLog         GrazingScars   0.1006    0.0358 1634.6387     7.8599
    ##   LesionAreaLog         BladeAreaLog   0.4434    0.0675 1654.9916    42.9538
    ##   LesionAreaLog             Epifauna   0.4556    0.1163  107.4135    14.1766
    ##   LesionAreaLog           DensityLog   0.3956    0.1414   65.8157     6.6571
    ##   LesionAreaLog         CanopyHeight   0.9021    0.2108  100.3294    17.0585
    ##   LesionAreaLog    TempAnomWarm_June   0.0063    0.0052   46.8209     1.3629
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0755    0.0427   10.4685     2.6688
    ##   LesionAreaLog           YearBinary   0.1322    0.0501  774.4539     6.8063
    ##    ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.3298 ***
    ##    0.0000       -0.261 ***
    ##    0.0000      -0.6213 ***
    ##    0.0000        -0.39 ***
    ##    0.0000       -0.146 ***
    ##    0.0000      -0.6201 ***
    ##    0.0000       0.8334 ***
    ##    0.0000      -0.2207 ***
    ##    0.0000      -0.4317 ***
    ##    0.0000        1.183 ***
    ##    0.0000       -0.059 ***
    ##    0.2018            -    
    ##    0.5886            -    
    ##    0.0001            - ***
    ##    0.0000            - ***
    ##    0.5822       0.0304    
    ##    0.0000       0.5662 ***
    ##    0.9227       0.0071    
    ##    0.0055         0.14  **
    ##    0.1682      -0.1359    
    ##    0.1681       0.0312    
    ##    0.0051       0.0698  **
    ##    0.0000       0.2482 ***
    ##    0.0003       0.3136 ***
    ##    0.0121        0.285   *
    ##    0.0001       0.3605 ***
    ##    0.2490       0.0852    
    ##    0.1320       0.2285    
    ##    0.0093       0.0917  **
    ##    0.0000       0.3194 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.38        0.95
    ##    CanopyHeight   none     0.18        0.99
    ##      DensityLog   none     0.32        1.00
    ##    GrazingScars  delta     0.11        0.32
    ##    BladeAreaLog   none     0.50        0.65
    ##   LesionAreaLog   none     0.11        0.37

``` r
# coefs(les3)
```

Doesn’t pass global test (p=0), AIC = 192.544. Temp effects on grazing
scars are missing

#### alternative model - no grazing to lesion

``` r
les4 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
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
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
          YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
lmer(LesionAreaLog ~ BladeAreaLog + Epifauna + DensityLog + CanopyHeight + 
               TempAnomWarm_June + MonthlyMeanTemp_June +
                YearBinary + (1|Meadow) + (1|Region),
             data=dis2),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(les4)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of les4 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + Epifauna + DensityLog + CanopyHeight + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  117.449   404.549
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                       Independ.Claim Test.Type       DF Crit.Value P.Value   
    ##        GrazingScars ~ Epifauna + ...      coef 1664.000    -0.4699  0.6384   
    ##   LesionAreaLog ~ GrazingScars + ...      coef 1634.639     7.8599  0.0051 **
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 11.449 with P-value = 0.022 and on 4 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ##        Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ##        Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ##        Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ##        Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ##    CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ##    CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ##    CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ##      DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ##      DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ##      DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ##    GrazingScars         CanopyHeight   0.6802    0.6756 1664.0000     1.0068
    ##    GrazingScars           DensityLog  -0.5606    0.5337 1664.0000    -1.0505
    ##    GrazingScars         BladeAreaLog   1.1143    0.2687 1664.0000     4.1470
    ##    GrazingScars    TempAnomWarm_June  -0.0558    0.0286 1664.0000    -1.9499
    ##    GrazingScars MonthlyMeanTemp_June   1.9906    0.3214 1664.0000     6.1927
    ##    GrazingScars           YearBinary  -1.5330    0.1741 1664.0000    -8.8041
    ##    BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ##    BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ##    BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ##    BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ##    BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ##   LesionAreaLog         BladeAreaLog   0.4637    0.0673 1655.8792    47.2935
    ##   LesionAreaLog             Epifauna   0.4626    0.1176  113.4100    14.3427
    ##   LesionAreaLog           DensityLog   0.3916     0.145   72.3073     6.2688
    ##   LesionAreaLog         CanopyHeight   0.9237    0.2137  100.7996    17.4452
    ##   LesionAreaLog    TempAnomWarm_June   0.0062    0.0053   45.0128     1.2327
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1028    0.0449   11.3674     4.4126
    ##   LesionAreaLog           YearBinary   0.1042    0.0494  751.1567     4.3558
    ##    ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.3298 ***
    ##    0.0000       -0.261 ***
    ##    0.0000      -0.6213 ***
    ##    0.0000        -0.39 ***
    ##    0.0000       -0.146 ***
    ##    0.0000      -0.6201 ***
    ##    0.0000       0.8334 ***
    ##    0.0000      -0.2207 ***
    ##    0.0000      -0.4317 ***
    ##    0.0000        1.183 ***
    ##    0.0000       -0.059 ***
    ##    0.3140            -    
    ##    0.2935            -    
    ##    0.0000            - ***
    ##    0.0512            -    
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.5822       0.0304    
    ##    0.0000       0.5662 ***
    ##    0.9227       0.0071    
    ##    0.0055         0.14  **
    ##    0.1682      -0.1359    
    ##    0.1681       0.0312    
    ##    0.0000       0.2595 ***
    ##    0.0002       0.3184 ***
    ##    0.0145       0.2822   *
    ##    0.0001       0.3692 ***
    ##    0.2728        0.084    
    ##    0.0587       0.3112    
    ##    0.0372       0.0723   *
    ##    0.0000       0.3194 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.38        0.95
    ##    CanopyHeight   none     0.18        0.99
    ##      DensityLog   none     0.32        1.00
    ##    GrazingScars  delta     0.43        0.91
    ##    BladeAreaLog   none     0.50        0.65
    ##   LesionAreaLog   none     0.11        0.40

Doesn’t pass (p=0.02), AIC= 117.449, d-sep shows need for grazing to
lesion

#### alternative model - no epifauna to lesion

``` r
les5 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
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
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TempAnomWarm_June + MonthlyMeanTemp_June + 
           YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
lmer(LesionAreaLog ~ GrazingScars + BladeAreaLog + DensityLog + CanopyHeight + 
               TempAnomWarm_June + MonthlyMeanTemp_June +
               YearBinary + (1|Meadow) + (1|Region),
             data=dis2),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(les5)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of les5 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~ GrazingScars + BladeAreaLog + DensityLog + CanopyHeight + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  123.318   410.418
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                   Independ.Claim Test.Type        DF Crit.Value P.Value    
    ##    GrazingScars ~ Epifauna + ...      coef 1664.0000    -0.4699  0.6384    
    ##   LesionAreaLog ~ Epifauna + ...      coef  107.4135    14.1766  0.0003 ***
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 17.318 with P-value = 0.002 and on 4 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ##        Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ##        Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ##        Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ##        Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ##    CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ##    CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ##    CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ##      DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ##      DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ##      DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ##    GrazingScars         CanopyHeight   0.6802    0.6756 1664.0000     1.0068
    ##    GrazingScars           DensityLog  -0.5606    0.5337 1664.0000    -1.0505
    ##    GrazingScars         BladeAreaLog   1.1143    0.2687 1664.0000     4.1470
    ##    GrazingScars    TempAnomWarm_June  -0.0558    0.0286 1664.0000    -1.9499
    ##    GrazingScars MonthlyMeanTemp_June   1.9906    0.3214 1664.0000     6.1927
    ##    GrazingScars           YearBinary  -1.5330    0.1741 1664.0000    -8.8041
    ##    BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ##    BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ##    BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ##    BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ##    BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ##   LesionAreaLog         GrazingScars   0.1019    0.0359 1633.8629     8.0029
    ##   LesionAreaLog         BladeAreaLog   0.4486    0.0678 1655.2495    43.6682
    ##   LesionAreaLog           DensityLog   0.1375    0.1362   72.4809     0.8889
    ##   LesionAreaLog         CanopyHeight   0.4445    0.1814   98.4393     5.4961
    ##   LesionAreaLog    TempAnomWarm_June  -0.0041     0.005   37.0721     0.5847
    ##   LesionAreaLog MonthlyMeanTemp_June   0.0926    0.0493   12.7280     2.9325
    ##   LesionAreaLog           YearBinary   0.0634    0.0474  938.4697     1.7458
    ##    ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.3298 ***
    ##    0.0000       -0.261 ***
    ##    0.0000      -0.6213 ***
    ##    0.0000        -0.39 ***
    ##    0.0000       -0.146 ***
    ##    0.0000      -0.6201 ***
    ##    0.0000       0.8334 ***
    ##    0.0000      -0.2207 ***
    ##    0.0000      -0.4317 ***
    ##    0.0000        1.183 ***
    ##    0.0000       -0.059 ***
    ##    0.3140            -    
    ##    0.2935            -    
    ##    0.0000            - ***
    ##    0.0512            -    
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.5822       0.0304    
    ##    0.0000       0.5662 ***
    ##    0.9227       0.0071    
    ##    0.0055         0.14  **
    ##    0.1682      -0.1359    
    ##    0.1681       0.0312    
    ##    0.0047       0.0708  **
    ##    0.0000       0.2511 ***
    ##    0.3489       0.0991    
    ##    0.0211       0.1776   *
    ##    0.4493      -0.0548    
    ##    0.1111       0.2803    
    ##    0.1867        0.044    
    ##    0.0000       0.3194 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.38        0.95
    ##    CanopyHeight   none     0.18        0.99
    ##      DensityLog   none     0.32        1.00
    ##    GrazingScars  delta     0.43        0.91
    ##    BladeAreaLog   none     0.50        0.65
    ##   LesionAreaLog   none     0.09        0.43

Fails global fit test (p=0.002), AIC = 123.318 D-sep indicates need for
epifauna to lesion area

#### alternative model - reverse direction from lesion to grazing

``` r
les6 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
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
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  LesionAreaLog +
          TempAnomWarm_June + MonthlyMeanTemp_June + 
           YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
         YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
lmer(LesionAreaLog ~  BladeAreaLog + Epifauna + DensityLog + CanopyHeight + 
               TempAnomWarm_June + MonthlyMeanTemp_June +
                YearBinary + (1|Meadow) + (1|Region),
             data=dis2),
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(les6)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of les6 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + LesionAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + Epifauna + DensityLog + CanopyHeight + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  109.475   401.992
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                  Independ.Claim Test.Type   DF Crit.Value P.Value 
    ##   GrazingScars ~ Epifauna + ...      coef 1664     -0.709  0.4783 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 1.475 with P-value = 0.478 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response            Predictor Estimate Std.Error        DF Crit.Value
    ##        Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ##        Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ##        Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ##        Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ##        Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ##    CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ##    CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ##    CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ##      DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ##      DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ##      DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ##    GrazingScars         CanopyHeight   0.6200     0.675 1664.0000     0.9185
    ##    GrazingScars           DensityLog  -0.5105    0.5331 1664.0000    -0.9576
    ##    GrazingScars         BladeAreaLog   1.0262    0.2713 1664.0000     3.7832
    ##    GrazingScars        LesionAreaLog   0.2362    0.0928 1664.0000     2.5443
    ##    GrazingScars    TempAnomWarm_June  -0.0484    0.0291 1664.0000    -1.6649
    ##    GrazingScars MonthlyMeanTemp_June   1.8900    0.3284 1664.0000     5.7547
    ##    GrazingScars           YearBinary  -1.5317    0.1743 1664.0000    -8.7884
    ##    BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ##    BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ##    BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ##    BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ##    BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ##    BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ##   LesionAreaLog         BladeAreaLog   0.4637    0.0673 1655.8792    47.2935
    ##   LesionAreaLog             Epifauna   0.4626    0.1176  113.4100    14.3427
    ##   LesionAreaLog           DensityLog   0.3916     0.145   72.3073     6.2688
    ##   LesionAreaLog         CanopyHeight   0.9237    0.2137  100.7996    17.4452
    ##   LesionAreaLog    TempAnomWarm_June   0.0062    0.0053   45.0128     1.2327
    ##   LesionAreaLog MonthlyMeanTemp_June   0.1028    0.0449   11.3674     4.4126
    ##   LesionAreaLog           YearBinary   0.1042    0.0494  751.1567     4.3558
    ##    ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.3298 ***
    ##    0.0000       -0.261 ***
    ##    0.0000      -0.6213 ***
    ##    0.0000        -0.39 ***
    ##    0.0000       -0.146 ***
    ##    0.0000      -0.6201 ***
    ##    0.0000       0.8334 ***
    ##    0.0000      -0.2207 ***
    ##    0.0000      -0.4317 ***
    ##    0.0000        1.183 ***
    ##    0.0000       -0.059 ***
    ##    0.3583            -    
    ##    0.3383            -    
    ##    0.0002            - ***
    ##    0.0109            -   *
    ##    0.0959            -    
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.5822       0.0304    
    ##    0.0000       0.5662 ***
    ##    0.9227       0.0071    
    ##    0.0055         0.14  **
    ##    0.1682      -0.1359    
    ##    0.1681       0.0312    
    ##    0.0000       0.2595 ***
    ##    0.0002       0.3184 ***
    ##    0.0145       0.2822   *
    ##    0.0001       0.3692 ***
    ##    0.2728        0.084    
    ##    0.0587       0.3112    
    ##    0.0372       0.0723   *
    ##    0.0000       0.3194 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.38        0.95
    ##    CanopyHeight   none     0.18        0.99
    ##      DensityLog   none     0.32        1.00
    ##    GrazingScars  delta     0.44        0.89
    ##    BladeAreaLog   none     0.50        0.65
    ##   LesionAreaLog   none     0.11        0.40

Passes global fit (p=0.478). AIC = 109.475 (slightly higher than for the
original model)

#### alternative model - lesion and grazing are correlated errors

``` r
les7 <- psem(
  lmer(Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + 
         CanopyHeight + DensityLog 
       + YearBinary +
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
  glmer(GrazingScars ~ CanopyHeight + DensityLog + 
          BladeAreaLog +  
          TempAnomWarm_June + MonthlyMeanTemp_June + 
          YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis2,
        family = "binomial"),
  lmer(BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + 
         TempAnomWarm_June + MonthlyMeanTemp_June +
          YearBinary +
         (1|Region) + (1|Meadow),
       data=dis2),
lmer(LesionAreaLog ~ BladeAreaLog + Epifauna + DensityLog + CanopyHeight + 
               TempAnomWarm_June + MonthlyMeanTemp_June +
                YearBinary + (1|Meadow) + (1|Region),
             data=dis2),
  LesionAreaLog%~~%GrazingScars,
  DensityLog%~~%CanopyHeight
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(les7)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of les7 
    ## 
    ## Call:
    ##   Epifauna ~ TempAnomWarm_June + MonthlyMeanTemp_June + CanopyHeight + DensityLog + YearBinary
    ##   CanopyHeight ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   DensityLog ~ TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   GrazingScars ~ CanopyHeight + DensityLog + BladeAreaLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   BladeAreaLog ~ Epifauna + CanopyHeight + DensityLog + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + Epifauna + DensityLog + CanopyHeight + TempAnomWarm_June + MonthlyMeanTemp_June + YearBinary
    ##   LesionAreaLog ~~ GrazingScars
    ##   DensityLog ~~ CanopyHeight
    ## 
    ##     AIC      BIC
    ##  106.898   393.998
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                  Independ.Claim Test.Type   DF Crit.Value P.Value 
    ##   GrazingScars ~ Epifauna + ...      coef 1664    -0.4699  0.6384 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.898 with P-value = 0.638 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##          Response            Predictor Estimate Std.Error        DF Crit.Value
    ##          Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ##          Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ##          Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ##          Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ##          Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ##      CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ##      CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ##      CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ##        DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ##        DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ##        DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ##      GrazingScars         CanopyHeight   0.6802    0.6756 1664.0000     1.0068
    ##      GrazingScars           DensityLog  -0.5606    0.5337 1664.0000    -1.0505
    ##      GrazingScars         BladeAreaLog   1.1143    0.2687 1664.0000     4.1470
    ##      GrazingScars    TempAnomWarm_June  -0.0558    0.0286 1664.0000    -1.9499
    ##      GrazingScars MonthlyMeanTemp_June   1.9906    0.3214 1664.0000     6.1927
    ##      GrazingScars           YearBinary  -1.5330    0.1741 1664.0000    -8.8041
    ##      BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ##      BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ##      BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ##      BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ##      BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ##      BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ##     LesionAreaLog         BladeAreaLog   0.4637    0.0673 1655.8792    47.2935
    ##     LesionAreaLog             Epifauna   0.4626    0.1176  113.4100    14.3427
    ##     LesionAreaLog           DensityLog   0.3916     0.145   72.3073     6.2688
    ##     LesionAreaLog         CanopyHeight   0.9237    0.2137  100.7996    17.4452
    ##     LesionAreaLog    TempAnomWarm_June   0.0062    0.0053   45.0128     1.2327
    ##     LesionAreaLog MonthlyMeanTemp_June   0.1028    0.0449   11.3674     4.4126
    ##     LesionAreaLog           YearBinary   0.1042    0.0494  751.1567     4.3558
    ##   ~~LesionAreaLog       ~~GrazingScars   0.0641         - 1664.0000     2.6162
    ##      ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##   P.Value Std.Estimate    
    ##    0.0000      -0.3298 ***
    ##    0.0000       -0.261 ***
    ##    0.0000      -0.6213 ***
    ##    0.0000        -0.39 ***
    ##    0.0000       -0.146 ***
    ##    0.0000      -0.6201 ***
    ##    0.0000       0.8334 ***
    ##    0.0000      -0.2207 ***
    ##    0.0000      -0.4317 ***
    ##    0.0000        1.183 ***
    ##    0.0000       -0.059 ***
    ##    0.3140            -    
    ##    0.2935            -    
    ##    0.0000            - ***
    ##    0.0512            -    
    ##    0.0000            - ***
    ##    0.0000            - ***
    ##    0.5822       0.0304    
    ##    0.0000       0.5662 ***
    ##    0.9227       0.0071    
    ##    0.0055         0.14  **
    ##    0.1682      -0.1359    
    ##    0.1681       0.0312    
    ##    0.0000       0.2595 ***
    ##    0.0002       0.3184 ***
    ##    0.0145       0.2822   *
    ##    0.0001       0.3692 ***
    ##    0.2728        0.084    
    ##    0.0587       0.3112    
    ##    0.0372       0.0723   *
    ##    0.0045       0.0641  **
    ##    0.0000       0.3194 ***
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##        Epifauna   none     0.38        0.95
    ##    CanopyHeight   none     0.18        0.99
    ##      DensityLog   none     0.32        1.00
    ##    GrazingScars  delta     0.43        0.91
    ##    BladeAreaLog   none     0.50        0.65
    ##   LesionAreaLog   none     0.11        0.40

``` r
coefs(les7)
```

    ##           Response            Predictor Estimate Std.Error        DF Crit.Value
    ## 1         Epifauna    TempAnomWarm_June  -0.0168    0.0012  502.2433   181.9292
    ## 2         Epifauna MonthlyMeanTemp_June  -0.0593    0.0139  310.2016    17.1176
    ## 3         Epifauna         CanopyHeight  -1.0700    0.0461  890.9103   514.5266
    ## 4         Epifauna           DensityLog  -0.3725     0.035  520.6339   105.7151
    ## 5         Epifauna           YearBinary  -0.1448    0.0085 1614.2718   290.1036
    ## 6     CanopyHeight    TempAnomWarm_June  -0.0183     5e-04 1641.8168  1567.1384
    ## 7     CanopyHeight MonthlyMeanTemp_June   0.1100    0.0063 1644.4669   303.3077
    ## 8     CanopyHeight           YearBinary  -0.1271    0.0034 1635.9877  1395.0871
    ## 9       DensityLog    TempAnomWarm_June  -0.0230     6e-04 1641.7880  1464.7801
    ## 10      DensityLog MonthlyMeanTemp_June   0.2815    0.0082 1646.8366  1178.9853
    ## 11      DensityLog           YearBinary  -0.0613    0.0044 1635.9812   192.5625
    ## 12    GrazingScars         CanopyHeight   0.6802    0.6756 1664.0000     1.0068
    ## 13    GrazingScars           DensityLog  -0.5606    0.5337 1664.0000    -1.0505
    ## 14    GrazingScars         BladeAreaLog   1.1143    0.2687 1664.0000     4.1470
    ## 15    GrazingScars    TempAnomWarm_June  -0.0558    0.0286 1664.0000    -1.9499
    ## 16    GrazingScars MonthlyMeanTemp_June   1.9906    0.3214 1664.0000     6.1927
    ## 17    GrazingScars           YearBinary  -1.5330    0.1741 1664.0000    -8.8041
    ## 18    BladeAreaLog             Epifauna   0.0247    0.0431  117.1401     0.3043
    ## 19    BladeAreaLog         CanopyHeight   0.7930    0.0757   91.0275   101.4793
    ## 20    BladeAreaLog           DensityLog   0.0055    0.0525   67.2687     0.0095
    ## 21    BladeAreaLog    TempAnomWarm_June   0.0058    0.0019   45.9981     8.4781
    ## 22    BladeAreaLog MonthlyMeanTemp_June  -0.0251    0.0157   10.7879     2.1819
    ## 23    BladeAreaLog           YearBinary   0.0252     0.018  759.5661     1.9031
    ## 24   LesionAreaLog         BladeAreaLog   0.4637    0.0673 1655.8792    47.2935
    ## 25   LesionAreaLog             Epifauna   0.4626    0.1176  113.4100    14.3427
    ## 26   LesionAreaLog           DensityLog   0.3916     0.145   72.3073     6.2688
    ## 27   LesionAreaLog         CanopyHeight   0.9237    0.2137  100.7996    17.4452
    ## 28   LesionAreaLog    TempAnomWarm_June   0.0062    0.0053   45.0128     1.2327
    ## 29   LesionAreaLog MonthlyMeanTemp_June   0.1028    0.0449   11.3674     4.4126
    ## 30   LesionAreaLog           YearBinary   0.1042    0.0494  751.1567     4.3558
    ## 31 ~~LesionAreaLog       ~~GrazingScars   0.0641         - 1664.0000     2.6162
    ## 32    ~~DensityLog       ~~CanopyHeight   0.3194         - 1664.0000    13.7385
    ##    P.Value Std.Estimate    
    ## 1   0.0000      -0.3298 ***
    ## 2   0.0000      -0.2610 ***
    ## 3   0.0000      -0.6213 ***
    ## 4   0.0000      -0.3900 ***
    ## 5   0.0000      -0.1460 ***
    ## 6   0.0000      -0.6201 ***
    ## 7   0.0000       0.8334 ***
    ## 8   0.0000      -0.2207 ***
    ## 9   0.0000      -0.4317 ***
    ## 10  0.0000       1.1830 ***
    ## 11  0.0000      -0.0590 ***
    ## 12  0.3140       0.0829    
    ## 13  0.2935      -0.1232    
    ## 14  0.0000       0.1903 ***
    ## 15  0.0512      -0.2299    
    ## 16  0.0000       1.8384 ***
    ## 17  0.0000      -0.3246 ***
    ## 18  0.5822       0.0304    
    ## 19  0.0000       0.5662 ***
    ## 20  0.9227       0.0071    
    ## 21  0.0055       0.1400  **
    ## 22  0.1682      -0.1359    
    ## 23  0.1681       0.0312    
    ## 24  0.0000       0.2595 ***
    ## 25  0.0002       0.3184 ***
    ## 26  0.0145       0.2822   *
    ## 27  0.0001       0.3692 ***
    ## 28  0.2728       0.0840    
    ## 29  0.0587       0.3112    
    ## 30  0.0372       0.0723   *
    ## 31  0.0045       0.0641  **
    ## 32  0.0000       0.3194 ***

Passes global test p=0.638, AIC = 106.898 Exactly 2 less than global
model.

``` r
df.AIC <- data.frame(model=c("les1", "les2", "les3", "les4", "les5", "les6", "les7"),
                     AIC=c(108.898, 119.006, 192.544, 117.449, 123.318, 109.475, 106.898))
df.AIC$deltaAIC <- df.AIC$AIC-min(df.AIC$AIC)
df.AIC$likelihood <- exp(-df.AIC$deltaAIC/2)
df.AIC$weight <- df.AIC$likelihood/sum(df.AIC$likelihood)
tibble(df.AIC)
```

    ## # A tibble: 7 × 5
    ##   model   AIC deltaAIC likelihood   weight
    ##   <chr> <dbl>    <dbl>      <dbl>    <dbl>
    ## 1 les1   109.     2      3.68e- 1 2.23e- 1
    ## 2 les2   119.    12.1    2.35e- 3 1.42e- 3
    ## 3 les3   193.    85.6    2.52e-19 1.53e-19
    ## 4 les4   117.    10.6    5.12e- 3 3.10e- 3
    ## 5 les5   123.    16.4    2.72e- 4 1.65e- 4
    ## 6 les6   109.     2.58   2.76e- 1 1.67e- 1
    ## 7 les7   107.     0      1   e+ 0 6.06e- 1
