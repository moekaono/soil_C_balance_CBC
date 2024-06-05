CBC Rh:SR lmer
================
MOEKA
2024-06-03

``` r
ratio_raw <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1mAvjZZHU6QzANVULKPTnQAf7BJ7tgJtJDmgdV6Y0xLk/edit?pli=1#gid=751977988",
             sheet = "Copy of CBC")
```

    ## ! Using an auto-discovered, cached token.

    ##   To suppress this message, modify your code or options to clearly consent to
    ##   the use of a cached token.

    ##   See gargle's "Non-interactive auth" vignette for more details:

    ##   <https://gargle.r-lib.org/articles/non-interactive-auth.html>

    ## ℹ The googlesheets4 package is using a cached token for 'mono@tamu.edu'.

    ## ✔ Reading from "compartment_flux".

    ## ✔ Range ''Copy of CBC''.

    ## New names:
    ## • `` -> `...33`
    ## • `` -> `...52`
    ## • `` -> `...58`
    ## • `` -> `...63`
    ## • `` -> `...75`
    ## • `` -> `...78`
    ## • `` -> `...83`
    ## • `` -> `...88`

``` r
ratio <- ratio_raw[1:12,c(1,36,37)]

ratio$stands <- substring(ratio$Site,1,2)

ratio_long <- 
  ratio %>%
  pivot_longer(!c(Site, stands), names_to = "time", values_to = "value") %>%
  mutate(time = ifelse(grepl("dorm", time), "dorm", "grow"))

ratio_long <- 
  ratio_long %>%
  mutate(across(Site:time, as.factor))
```

``` r
m_ratio <- lmerTest::lmer(value ~ stands*time +(1|Site), ratio_long)
m_ratio2 <- lmerTest::lmer(value ~ stands + time +(1|Site), ratio_long)

summary(m_ratio)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: value ~ stands * time + (1 | Site)
    ##    Data: ratio_long
    ## 
    ## REML criterion at convergence: -20.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1721 -0.5413  0.1254  0.4834  1.5511 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Site     (Intercept) 0.005638 0.07509 
    ##  Residual             0.007349 0.08573 
    ## Number of obs: 24, groups:  Site, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)        0.66800    0.05698 15.14570  11.723 5.34e-09 ***
    ## standsIB          -0.06275    0.08058 15.14570  -0.779   0.4481    
    ## standsNB          -0.08500    0.08058 15.14570  -1.055   0.3080    
    ## timegrow          -0.12425    0.06062  9.00000  -2.050   0.0706 .  
    ## standsIB:timegrow  0.01900    0.08573  9.00000   0.222   0.8295    
    ## standsNB:timegrow  0.03425    0.08573  9.00000   0.400   0.6988    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) stndIB stndNB timgrw stnIB:
    ## standsIB    -0.707                            
    ## standsNB    -0.707  0.500                     
    ## timegrow    -0.532  0.376  0.376              
    ## stndsIB:tmg  0.376 -0.532 -0.266 -0.707       
    ## stndsNB:tmg  0.376 -0.266 -0.532 -0.707  0.500

``` r
anova(m_ratio)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##               Sum Sq  Mean Sq NumDF DenDF F value  Pr(>F)  
    ## stands      0.008056 0.004028     2     9  0.5481 0.59618  
    ## time        0.068054 0.068054     1     9  9.2600 0.01395 *
    ## stands:time 0.001178 0.000589     2     9  0.0801 0.92365  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_ratio)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.565
    ##      Marginal R2: 0.231

``` r
summary(m_ratio2)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: value ~ stands + time + (1 | Site)
    ##    Data: ratio_long
    ## 
    ## REML criterion at convergence: -27.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.22275 -0.55882  0.03685  0.44217  1.75734 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Site     (Intercept) 0.006252 0.07907 
    ##  Residual             0.006120 0.07823 
    ## Number of obs: 24, groups:  Site, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  0.65912    0.05082 10.97236  12.969 5.36e-08 ***
    ## standsIB    -0.05325    0.06824  9.00035  -0.780  0.45520    
    ## standsNB    -0.06787    0.06824  9.00035  -0.995  0.34586    
    ## timegrow    -0.10650    0.03194 10.99979  -3.335  0.00666 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB stndNB
    ## standsIB -0.671              
    ## standsNB -0.671  0.500       
    ## timegrow -0.314  0.000  0.000

``` r
anova(m_ratio2)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##          Sum Sq  Mean Sq NumDF   DenDF F value   Pr(>F)   
    ## stands 0.006709 0.003355     2  9.0004  0.5481 0.596167   
    ## time   0.068054 0.068054     1 10.9998 11.1195 0.006657 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_ratio2)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.623
    ##      Marginal R2: 0.237

``` r
# use simpler version as conditional r2 is higher
```

``` r
ratio_aov <- 
  ratio_long %>%
  mutate(id = as.factor(row_number())) %>%
  rstatix::anova_test(value ~ time + stands)

get_anova_table(ratio_aov)
```

    ## ANOVA Table (type II tests)
    ## 
    ##   Effect DFn DFd     F     p p<.05   ges
    ## 1   time   1  20 5.793 0.026     * 0.225
    ## 2 stands   2  20 0.869 0.435       0.080

``` r
lsmeans(m_ratio2, pairwise ~ stands | time, adjust = "tukey") 
```

    ## $lsmeans
    ## time = dorm:
    ##  stands lsmean     SE df lower.CL upper.CL
    ##  AB      0.659 0.0508 11    0.547    0.771
    ##  IB      0.606 0.0508 11    0.494    0.718
    ##  NB      0.591 0.0508 11    0.479    0.703
    ## 
    ## time = grow:
    ##  stands lsmean     SE df lower.CL upper.CL
    ##  AB      0.553 0.0508 11    0.441    0.665
    ##  IB      0.499 0.0508 11    0.387    0.611
    ##  NB      0.485 0.0508 11    0.373    0.597
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## time = dorm:
    ##  contrast estimate     SE df t.ratio p.value
    ##  AB - IB    0.0532 0.0682  9   0.780  0.7237
    ##  AB - NB    0.0679 0.0682  9   0.995  0.5982
    ##  IB - NB    0.0146 0.0682  9   0.214  0.9751
    ## 
    ## time = grow:
    ##  contrast estimate     SE df t.ratio p.value
    ##  AB - IB    0.0532 0.0682  9   0.780  0.7237
    ##  AB - NB    0.0679 0.0682  9   0.995  0.5982
    ##  IB - NB    0.0146 0.0682  9   0.214  0.9751
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
lsmeans(m_ratio2, pairwise ~ stands, adjust = "tukey")
```

    ## $lsmeans
    ##  stands lsmean     SE df lower.CL upper.CL
    ##  AB      0.606 0.0482  9    0.497    0.715
    ##  IB      0.553 0.0482  9    0.443    0.662
    ##  NB      0.538 0.0482  9    0.429    0.647
    ## 
    ## Results are averaged over the levels of: time 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast estimate     SE df t.ratio p.value
    ##  AB - IB    0.0532 0.0682  9   0.780  0.7237
    ##  AB - NB    0.0679 0.0682  9   0.995  0.5982
    ##  IB - NB    0.0146 0.0682  9   0.214  0.9751
    ## 
    ## Results are averaged over the levels of: time 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
# contrast
# ref: https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/
emm3 = emmeans(m_ratio2, specs = pairwise ~ stands)


burn = c(1,1,0)
unburn = c(0,0,1)
IB_NB = c(0,1,1)
AB = c(1, 0, 0)

burn_overall = burn/2
IB_NB_overall = IB_NB/2

contrast(emm3, method = list("burn - unburn" = burn_overall - unburn), adjust = "tukey")
```

    ##  contrast      estimate     SE df t.ratio p.value
    ##  burn - unburn   0.0413 0.0591  9   0.698  0.5028
    ## 
    ## Results are averaged over the levels of: time 
    ## Degrees-of-freedom method: kenward-roger

``` r
contrast(emm3, method = list("AB - IB+NB" = AB - IB_NB_overall), adjust = "tukey")
```

    ##  contrast   estimate     SE df t.ratio p.value
    ##  AB - IB+NB   0.0606 0.0591  9   1.025  0.3322
    ## 
    ## Results are averaged over the levels of: time 
    ## Degrees-of-freedom method: kenward-roger
