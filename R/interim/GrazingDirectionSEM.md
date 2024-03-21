Grazing Scar direction SEM results
================

# Simple SEMs to asses direction of grazing to disease relationship

Strictly assessing three variables - disease (prevalence or lesion
area), leaf area (log-transformed), and grazing scar presence/absence.
Also using tidal height as it affects leaf area (and gives more
flexiblity to the SEM). Random effects are site and region.

Testing two possiblities - direction from grazing to disease or from
disease to grazing.

``` r
# data ###
dis <- read_csv("data/epiphyte_SEM_data_all_large.csv")
```

    ## Rows: 1350 Columns: 49
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (8): Meter, SampleId, Region, SiteCode, TidalHeight, BrokenTip, Notes,...
    ## dbl  (40): Transect, Blade, LongestBladeLength, LongestBladeWidth, SheathLen...
    ## date  (1): SampleDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# same data as for complex SEM because no grazing scars in 2020 except WA
dis$BladeAreaLog <- log10(dis$BladeArea)
dis_gz <- select(dis, c(Year, Meadow, Region, Blade, BladeAreaLog, TidalHeightBinary, YearBinary, GrazingScars, Prevalence, LesionArea))
dis_gz1 <- na.omit(dis_gz)
dis_gz1$Meadow_Year <- paste(dis_gz1$Meadow, dis_gz1$Year, sep="_")
print("meadow-year combos in prevalence SEM")
```

    ## [1] "meadow-year combos in prevalence SEM"

``` r
length(unique(dis_gz1$Meadow_Year))
```

    ## [1] 44

``` r
unique(dis_gz1$Meadow_Year)
```

    ##  [1] "AK_A_2019" "AK_B_2019" "AK_C_2019" "AK_D_2019" "AK_E_2019" "AK_F_2019"
    ##  [7] "BC_A_2019" "BC_B_2019" "BC_C_2019" "BC_D_2019" "BC_E_2019" "WA_A_2019"
    ## [13] "WA_B_2019" "WA_C_2019" "WA_D_2019" "WA_E_2019" "OR_B_2019" "OR_C_2019"
    ## [19] "OR_D_2019" "OR_E_2019" "BB_A_2019" "BB_C_2019" "BB_D_2019" "BB_E_2019"
    ## [25] "AK_A_2021" "AK_B_2021" "AK_C_2021" "AK_D_2021" "AK_E_2021" "AK_F_2021"
    ## [31] "BC_A_2021" "BC_C_2021" "BC_E_2021" "BC_B_2021" "BC_D_2021" "WA_A_2021"
    ## [37] "WA_B_2021" "WA_D_2021" "WA_E_2021" "OR_C_2021" "OR_D_2021" "OR_E_2021"
    ## [43] "SD_A_2021" "SD_D_2021"

``` r
print("nrows of disease data (sample size)")
```

    ## [1] "nrows of disease data (sample size)"

``` r
nrow(dis_gz1)
```

    ## [1] 1315

## Prevalence - grazing to disease

``` r
sem_prev_gz <- psem(
  lmer(BladeAreaLog ~ TidalHeightBinary + YearBinary + 
         (1|Region) + (1|Meadow),
       data=dis_gz1),
  glmer(GrazingScars ~ BladeAreaLog + 
           YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_gz1,
        family = "binomial"),
  glmer(Prevalence ~ BladeAreaLog + GrazingScars +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_gz1,
        family = "binomial")
)
summary(sem_prev_gz, conserve = T)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_gz 
    ## 
    ## Call:
    ##   BladeAreaLog ~ TidalHeightBinary + YearBinary
    ##   GrazingScars ~ BladeAreaLog + YearBinary
    ##   Prevalence ~ BladeAreaLog + GrazingScars + TidalHeightBinary + YearBinary
    ## 
    ##     AIC      BIC
    ##  37.059   130.328
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type   DF Crit.Value P.Value 
    ##   GrazingScars ~ TidalHeightBinary + ...      coef 1315    -0.5406  0.5888 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 1.059 with P-value = 0.589 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response         Predictor Estimate Std.Error       DF Crit.Value P.Value
    ##   BladeAreaLog TidalHeightBinary  -0.2433    0.0140 1287.014   303.6239  0.0000
    ##   BladeAreaLog        YearBinary  -0.0489    0.0154 1293.438    10.0646  0.0015
    ##   GrazingScars      BladeAreaLog   0.9999    0.2343 1315.000     4.2672  0.0000
    ##   GrazingScars        YearBinary  -0.4774    0.1440 1315.000    -3.3160  0.0009
    ##     Prevalence      BladeAreaLog   1.5477    0.2743 1315.000     5.6425  0.0000
    ##     Prevalence      GrazingScars   1.1393    0.1521 1315.000     7.4901  0.0000
    ##     Prevalence TidalHeightBinary   0.6456    0.1436 1315.000     4.4966  0.0000
    ##     Prevalence        YearBinary   0.2081    0.1414 1315.000     1.4721  0.1410
    ##   Std.Estimate    
    ##        -0.2876 ***
    ##        -0.0575  **
    ##              - ***
    ##              - ***
    ##              - ***
    ##              - ***
    ##              - ***
    ##              -    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##   BladeAreaLog   none     0.07        0.72
    ##   GrazingScars  delta     0.05        0.19
    ##     Prevalence  delta     0.11        0.40

``` r
coefs(sem_prev_gz)
```

    ##       Response         Predictor Estimate Std.Error       DF Crit.Value P.Value
    ## 1 BladeAreaLog TidalHeightBinary  -0.2433    0.0140 1287.014   303.6239  0.0000
    ## 2 BladeAreaLog        YearBinary  -0.0489    0.0154 1293.438    10.0646  0.0015
    ## 3 GrazingScars      BladeAreaLog   0.9999    0.2343 1315.000     4.2672  0.0000
    ## 4 GrazingScars        YearBinary  -0.4774    0.1440 1315.000    -3.3160  0.0009
    ## 5   Prevalence      BladeAreaLog   1.5477    0.2743 1315.000     5.6425  0.0000
    ## 6   Prevalence      GrazingScars   1.1393    0.1521 1315.000     7.4901  0.0000
    ## 7   Prevalence TidalHeightBinary   0.6456    0.1436 1315.000     4.4966  0.0000
    ## 8   Prevalence        YearBinary   0.2081    0.1414 1315.000     1.4721  0.1410
    ##   Std.Estimate    
    ## 1      -0.2876 ***
    ## 2      -0.0575  **
    ## 3       0.2107 ***
    ## 4      -0.1184 ***
    ## 5       0.2814 ***
    ## 6       0.2227 ***
    ## 7       0.1388 ***
    ## 8       0.0445

## Prevalence - disease to grazing

``` r
sem_prev_dis <- psem(
  lmer(BladeAreaLog ~ TidalHeightBinary + YearBinary + 
         (1|Region) + (1|Meadow),
       data=dis_gz1),
  glmer(GrazingScars ~ BladeAreaLog + Prevalence + 
           YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_gz1,
        family = "binomial"),
  glmer(Prevalence ~ BladeAreaLog +
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=dis_gz1,
        family = "binomial")
)
summary(sem_prev_dis)
```

    ##   |                                                                              |                                                                      |   0%

    ## boundary (singular) fit: see help('isSingular')

    ##   |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_prev_dis 
    ## 
    ## Call:
    ##   BladeAreaLog ~ TidalHeightBinary + YearBinary
    ##   GrazingScars ~ BladeAreaLog + Prevalence + YearBinary
    ##   Prevalence ~ BladeAreaLog + TidalHeightBinary + YearBinary
    ## 
    ##     AIC      BIC
    ##  40.777   134.046
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type   DF Crit.Value P.Value 
    ##   GrazingScars ~ TidalHeightBinary + ...      coef 1315    -1.6862  0.0918 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 4.777 with P-value = 0.092 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response         Predictor Estimate Std.Error       DF Crit.Value P.Value
    ##   BladeAreaLog TidalHeightBinary  -0.2433    0.0140 1287.014   303.6239  0.0000
    ##   BladeAreaLog        YearBinary  -0.0489    0.0154 1293.438    10.0646  0.0015
    ##   GrazingScars      BladeAreaLog   0.7595    0.2346 1315.000     3.2372  0.0012
    ##   GrazingScars        Prevalence   1.1144    0.1503 1315.000     7.4152  0.0000
    ##   GrazingScars        YearBinary  -0.5052    0.1478 1315.000    -3.4175  0.0006
    ##     Prevalence      BladeAreaLog   1.6551    0.2686 1315.000     6.1623  0.0000
    ##     Prevalence TidalHeightBinary   0.5982    0.1396 1315.000     4.2835  0.0000
    ##     Prevalence        YearBinary   0.0871    0.1373 1315.000     0.6346  0.5257
    ##   Std.Estimate    
    ##        -0.2876 ***
    ##        -0.0575  **
    ##              -  **
    ##              - ***
    ##              - ***
    ##              - ***
    ##              - ***
    ##              -    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##       Response method Marginal Conditional
    ##   BladeAreaLog   none     0.07        0.72
    ##   GrazingScars  delta     0.09        0.20
    ##     Prevalence  delta     0.07        0.38

``` r
coefs(sem_prev_dis)
```

    ##       Response         Predictor Estimate Std.Error       DF Crit.Value P.Value
    ## 1 BladeAreaLog TidalHeightBinary  -0.2433    0.0140 1287.014   303.6239  0.0000
    ## 2 BladeAreaLog        YearBinary  -0.0489    0.0154 1293.438    10.0646  0.0015
    ## 3 GrazingScars      BladeAreaLog   0.7595    0.2346 1315.000     3.2372  0.0012
    ## 4 GrazingScars        Prevalence   1.1144    0.1503 1315.000     7.4152  0.0000
    ## 5 GrazingScars        YearBinary  -0.5052    0.1478 1315.000    -3.4175  0.0006
    ## 6   Prevalence      BladeAreaLog   1.6551    0.2686 1315.000     6.1623  0.0000
    ## 7   Prevalence TidalHeightBinary   0.5982    0.1396 1315.000     4.2835  0.0000
    ## 8   Prevalence        YearBinary   0.0871    0.1373 1315.000     0.6346  0.5257
    ##   Std.Estimate    
    ## 1      -0.2876 ***
    ## 2      -0.0575  **
    ## 3       0.1553  **
    ## 4       0.2672 ***
    ## 5      -0.1216 ***
    ## 6       0.3125 ***
    ## 7       0.1335 ***
    ## 8       0.0194

AIC is greater for dis –\> gz (40.78) vs gz –\> dis (37.06) - suggests
that the gz –\> disease model better fits the data. Standardized
coefficients are similar (0.2227 vs 0.2672); p-values are equivalent
(0.0000). Positive relationship - either disease infection increases
likelihood of grazing, or presence of grazing increases likelihood of
disease. Based on the AIC improvement, there is some evidence for more
support for the pathway from grazing to disease.

## Lesion area - grazing to disease

``` r
les_gz <- subset(dis_gz1, LesionArea>0)
les_gz$LesionAreaLog <- log10(les_gz$LesionArea)
les_gz <- na.omit(les_gz)

print("sample size for lesion models")
```

    ## [1] "sample size for lesion models"

``` r
nrow(les_gz)
```

    ## [1] 574

``` r
sem_les_dis <- psem(
  lmer(BladeAreaLog ~ TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_gz),
  glmer(GrazingScars ~ BladeAreaLog + 
          YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_gz,
        family = "binomial"),
  lmer(LesionAreaLog ~ BladeAreaLog + GrazingScars + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_gz)
)
summary(sem_les_dis)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_dis 
    ## 
    ## Call:
    ##   BladeAreaLog ~ TidalHeightBinary + YearBinary
    ##   GrazingScars ~ BladeAreaLog + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + GrazingScars + TidalHeightBinary + YearBinary
    ## 
    ##     AIC      BIC
    ##  39.435   122.135
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type  DF Crit.Value P.Value 
    ##   GrazingScars ~ TidalHeightBinary + ...      coef 574    -0.6936   0.488 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 1.435 with P-value = 0.488 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response         Predictor Estimate Std.Error       DF Crit.Value P.Value
    ##    BladeAreaLog TidalHeightBinary  -0.2145    0.0182 548.6350   138.4136  0.0000
    ##    BladeAreaLog        YearBinary  -0.0218    0.0209 553.5709     1.0928  0.2963
    ##    GrazingScars      BladeAreaLog   0.7688    0.3615 574.0000     2.1269  0.0334
    ##    GrazingScars        YearBinary  -1.1618    0.2233 574.0000    -5.2038  0.0000
    ##   LesionAreaLog      BladeAreaLog   0.4990    0.1104 306.1643    19.0459  0.0000
    ##   LesionAreaLog      GrazingScars   0.1696    0.0580 566.8985     8.4598  0.0038
    ##   LesionAreaLog TidalHeightBinary  -0.0043    0.0577 562.2870     0.0055  0.9407
    ##   LesionAreaLog        YearBinary   0.0038    0.0615 561.3019     0.0038  0.9511
    ##   Std.Estimate    
    ##        -0.2802 ***
    ##        -0.0282    
    ##              -   *
    ##              - ***
    ##         0.2697 ***
    ##         0.1188  **
    ##        -0.0031    
    ##         0.0027    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##    BladeAreaLog   none     0.05        0.79
    ##    GrazingScars  delta     0.09        0.25
    ##   LesionAreaLog   none     0.09        0.31

``` r
coefs(sem_les_dis)
```

    ##        Response         Predictor Estimate Std.Error       DF Crit.Value
    ## 1  BladeAreaLog TidalHeightBinary  -0.2145    0.0182 548.6350   138.4136
    ## 2  BladeAreaLog        YearBinary  -0.0218    0.0209 553.5709     1.0928
    ## 3  GrazingScars      BladeAreaLog   0.7688    0.3615 574.0000     2.1269
    ## 4  GrazingScars        YearBinary  -1.1618    0.2233 574.0000    -5.2038
    ## 5 LesionAreaLog      BladeAreaLog   0.4990    0.1104 306.1643    19.0459
    ## 6 LesionAreaLog      GrazingScars   0.1696    0.0580 566.8985     8.4598
    ## 7 LesionAreaLog TidalHeightBinary  -0.0043    0.0577 562.2870     0.0055
    ## 8 LesionAreaLog        YearBinary   0.0038    0.0615 561.3019     0.0038
    ##   P.Value Std.Estimate    
    ## 1  0.0000      -0.2802 ***
    ## 2  0.2963      -0.0282    
    ## 3  0.0334       0.1429   *
    ## 4  0.0000      -0.2794 ***
    ## 5  0.0000       0.2697 ***
    ## 6  0.0038       0.1188  **
    ## 7  0.9407      -0.0031    
    ## 8  0.9511       0.0027

## Lesion area - disease to grazing

``` r
sem_les_gz <- psem(
  lmer(BladeAreaLog ~ TidalHeightBinary + YearBinary +
         (1|Region) + (1|Meadow),
       data=les_gz),
  glmer(GrazingScars ~ BladeAreaLog + LesionAreaLog + 
          YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_gz,
        family = "binomial"),
  lmer(LesionAreaLog ~ BladeAreaLog + 
          TidalHeightBinary + YearBinary + 
          (1|Region) + (1|Meadow),
        data=les_gz)
)
summary(sem_les_gz)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

    ## 
    ## Structural Equation Model of sem_les_gz 
    ## 
    ## Call:
    ##   BladeAreaLog ~ TidalHeightBinary + YearBinary
    ##   GrazingScars ~ BladeAreaLog + LesionAreaLog + YearBinary
    ##   LesionAreaLog ~ BladeAreaLog + TidalHeightBinary + YearBinary
    ## 
    ##     AIC      BIC
    ##  39.294   121.994
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                           Independ.Claim Test.Type  DF Crit.Value P.Value 
    ##   GrazingScars ~ TidalHeightBinary + ...      coef 574    -0.6377  0.5237 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 1.294 with P-value = 0.524 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##        Response         Predictor Estimate Std.Error       DF Crit.Value P.Value
    ##    BladeAreaLog TidalHeightBinary  -0.2145    0.0182 548.6350   138.4136  0.0000
    ##    BladeAreaLog        YearBinary  -0.0218    0.0209 553.5709     1.0928  0.2963
    ##    GrazingScars      BladeAreaLog   0.5757    0.3664 574.0000     1.5711  0.1162
    ##    GrazingScars     LesionAreaLog   0.4537    0.1508 574.0000     3.0090  0.0026
    ##    GrazingScars        YearBinary  -1.1632    0.2247 574.0000    -5.1757  0.0000
    ##   LesionAreaLog      BladeAreaLog   0.5191    0.1110 307.1347    20.3961  0.0000
    ##   LesionAreaLog TidalHeightBinary  -0.0099    0.0580 561.9366     0.0288  0.8653
    ##   LesionAreaLog        YearBinary  -0.0371    0.0601 564.1033     0.3753  0.5404
    ##   Std.Estimate    
    ##        -0.2802 ***
    ##        -0.0282    
    ##              -    
    ##              -  **
    ##              - ***
    ##         0.2806 ***
    ##         -0.007    
    ##         -0.026    
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## ---
    ## Individual R-squared:
    ## 
    ##        Response method Marginal Conditional
    ##    BladeAreaLog   none     0.05        0.79
    ##    GrazingScars  delta     0.11        0.27
    ##   LesionAreaLog   none     0.07        0.30

``` r
coefs(sem_les_gz)
```

    ##        Response         Predictor Estimate Std.Error       DF Crit.Value
    ## 1  BladeAreaLog TidalHeightBinary  -0.2145    0.0182 548.6350   138.4136
    ## 2  BladeAreaLog        YearBinary  -0.0218    0.0209 553.5709     1.0928
    ## 3  GrazingScars      BladeAreaLog   0.5757    0.3664 574.0000     1.5711
    ## 4  GrazingScars     LesionAreaLog   0.4537    0.1508 574.0000     3.0090
    ## 5  GrazingScars        YearBinary  -1.1632    0.2247 574.0000    -5.1757
    ## 6 LesionAreaLog      BladeAreaLog   0.5191    0.1110 307.1347    20.3961
    ## 7 LesionAreaLog TidalHeightBinary  -0.0099    0.0580 561.9366     0.0288
    ## 8 LesionAreaLog        YearBinary  -0.0371    0.0601 564.1033     0.3753
    ##   P.Value Std.Estimate    
    ## 1  0.0000      -0.2802 ***
    ## 2  0.2963      -0.0282    
    ## 3  0.1162       0.1057    
    ## 4  0.0026       0.1541  **
    ## 5  0.0000      -0.2764 ***
    ## 6  0.0000       0.2806 ***
    ## 7  0.8653      -0.0070    
    ## 8  0.5404      -0.0260

AIC is equivalent between the two models for lesion area. Standardized
estimate is slightly larger for dis –\> gz (0.15) compared to gz –\> dis
(0.12). P-values are small and equivalent for both (0.0038 vs 0.0026).
No strong evidence in support of either direction.

Note however that the relationship is positive (i.e. larger lesions make
grazing more likely/grazing makes lesions larger). Given this, it seems
perhaps more biologically likely that lesions make grazing more likely?
