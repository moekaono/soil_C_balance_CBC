LMM on CBC monthly variables
================
Moeka
2024-04-05

``` r
setwd("G:/My Drive/Research/Projects/CBC") 
cbc_mo_raw <- read.csv("cbc_mo_var.csv")

# change dtype
cbc_mo <- 
  cbc_mo_raw %>%
  filter(MY %in% c("20-Oct", "20-Nov", "20-Dec", 
                 "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", "21-Jun", 
                 "21-Jul", "21-Aug", "21-Sep")) %>%
  mutate(across(D_FR:LF_gC_mean, as.numeric)) %>%
  mutate(
    new_pl = paste0(stands, "P", plot_num),
    NPP_fungi = D_FR - P_FR,
    FR_mass_gC = LF_gC_mean + DF_gC_mean) %>% 
  rename("SR_flux" = "SR",
         "SR" = "SR_mo_gC", 
         "Rh" = "Rh_season_mo_gC",
         "Ra" = "Ra_mo_gC") # all gC/m2/mo
```

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(D_FR:LF_gC_mean, as.numeric)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
cbc_mo[cbc_mo == '#DIV/0!'] <- NA

cbc_mo$stands <- as.factor(cbc_mo$stands)
cbc_mo$new_pl <- as.factor(cbc_mo$new_pl)

MY_order_v1 <- c("20-Jul", "20-Aug", "20-Sep", "20-Oct", "20-Nov", "20-Dec", 
                 "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", "21-Jun", 
                 "21-Jul", "21-Aug", "21-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan")
              

cbc_mo$MY <- factor(cbc_mo$MY, level = MY_order_v1)

# interested variavles
vars <- c("D_FR","P_FR", "SR","Rh","Ra", "D_L", "Ts","vwc", "FR_mass_gC", "NPP_fungi")
```

REFERENCE: <https://www.datanovia.com/en/lessons/mixed-anova-in-r/>
<https://rpkgs.datanovia.com/rstatix/reference/anova_test.html>

Here, I will perform mixed ANOVA which is used to compare the means of
groups cross-classified by two different types of factor variables,
including between-subject factors (independent categories) and
within-subjects factories (related categories, also known as repeated
measures).

Assumptions to be considered are; (1) no significant outliers, (2)
normality of the data, (3) homogeneity of variances, (4) assumptions of
sphericity, and (5) homogeneity of covariances.

``` r
# log transformation
cbc_mo_log <- 
  cbc_mo %>%
  mutate(across(D_FR:LF_gC_mean, log))
```

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(D_FR:LF_gC_mean, log)`.
    ## Caused by warning:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
# sqrt
cbc_mo_sqrt <- 
  cbc_mo %>%
  mutate(across(D_FR:LF_gC_mean, sqrt))
```

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(D_FR:LF_gC_mean, sqrt)`.
    ## Caused by warning:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
# box-cox transformation
bc_trans <- function(df){
  output <- cbc_mo[,c(1:3,30)]
  summary <- data.frame(var = NULL, skewness = NULL, lambda = NULL)
  
  
  for(i in vars){
    bc_trans <- BoxCoxTrans(df[[i]], na.rm = T)
    summary <- rbind(summary, c(i, bc_trans$skewness, bc_trans$lambda))
    output <- cbind(output, predict(bc_trans, df[[i]]))
  }
    colnames(summary) <- c("var", "skewness", "lambda")
    return(list(output, summary))
}

# 2 layers
cbc_mo_bc <- bc_trans(cbc_mo) 
colnames(cbc_mo_bc[[1]])[5:14] <- vars

cbc_mo_bc[[2]]
```

    ##           var           skewness lambda
    ## 1        D_FR   1.39811459598347    0.3
    ## 2        P_FR  0.455140218648749   <NA>
    ## 3          SR  0.638065314470547    0.4
    ## 4          Rh  0.413641802561403    0.5
    ## 5          Ra   1.13496572856835    0.2
    ## 6         D_L  0.996947287758875    0.3
    ## 7          Ts -0.223246100370724    1.1
    ## 8         vwc  0.695463861708537    0.5
    ## 9  FR_mass_gC  0.337635067873747    0.3
    ## 10  NPP_fungi  0.565069426287239   <NA>

``` r
# inverse
# cbc_mo_inv <- 
#   cbc_mo %>%
#   mutate(across(D_FR:vwc, ~1/.))
```

``` r
density_plot <- function(df, title){
  p_list <- list()
  for (i in vars) {
    p_list[[i]] <-
      ggdensity(subset(df, complete.cases(i)), i, fill = "stands", alpha = .3) +
      stat_overlay_normal_density(color = "red", linetype = "dashed")
  }
  
  annotate_figure(do.call(grid.arrange,c(p_list, ncol = 4)), 
                  top = text_grob(title, size = 20))
}


# for raw data
density_plot(cbc_mo, "Raw data")
```

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

![](CBC_mo_LMM_files/figure-gfm/density%20plot-1.png)<!-- -->

``` r
# for log data
density_plot(cbc_mo_log, "Log-transformed data")
```

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).
    ## Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

![](CBC_mo_LMM_files/figure-gfm/density%20plot-2.png)<!-- -->

``` r
# for sqrt data
density_plot(cbc_mo_sqrt, "Square-Root Transformation")
```

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).
    ## Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 28 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 28 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

![](CBC_mo_LMM_files/figure-gfm/density%20plot-3.png)<!-- -->

``` r
# box-cox transformation
density_plot(cbc_mo_bc[[1]], "Box-Cox Transformation")
```

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).
    ## Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 12 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

![](CBC_mo_LMM_files/figure-gfm/density%20plot-4.png)<!-- -->

``` r
# inverse
#density_plot(cbc_mo_inv, "Inversed")
```

The normality assumption can be checked by computing Shapiro-Wilk test
for each time point. If the data is normally distributed, the p-value
should be greater than 0.05.

``` r
# Normality test 


UnivShapiro <-
  function(df, groupvar, columns){
    var.list <- rlang::syms(names(df[columns]))
    result <- 
      df %>%
      group_by(!!sym(groupvar)) %>%
      shapiro_test(vars = var.list) %>%
      arrange(variable) %>%
      mutate(normal_dis = ifelse(p > 0.05, T, F))
      
    result
  }



shapiro_raw <- 
  UnivShapiro(cbc_mo, groupvar = "stands", columns = vars)

shapiro_log <- 
  UnivShapiro(cbc_mo_log, groupvar = "stands", columns = vars)
# not good for var with 0 - vwc & P_FR

shapiro_sqrt <- 
  UnivShapiro(cbc_mo_sqrt, groupvar = "stands", columns = vars)
# good on SR component
# okay for vwc

shapiro_bc <- 
  UnivShapiro(cbc_mo_bc[[1]], groupvar = "stands", columns = vars)
# good on SR component, D_L, D_FR
# P_FR & vwc dont work

shapiro_inv <- 
  UnivShapiro(cbc_mo_inv, groupvar = "stands", columns = vars)
```

    ## Error in eval(expr, envir, enclos): object 'cbc_mo_inv' not found

``` r
# worst - never mind
```

Based on the density plots and shapiro test,

D_FR, SR, Rh, Ra, D_L -\> Box-Cox transformation Ts, P_FR \> Raw data
vwc \_\> sqrt

to get the normality as close as possible.

``` r
MY_order_v1 <- c("20-Aug","20-Sep", "20-Oct", "20-Nov", "20-Dec", 
                 "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", 
                 "21-Jun", "21-Jul", "21-Aug", "21-Sep", "21-Oct", "21-Nov", "21-Dec", 
                 "22-Jan")
```

Better use a linear mixed model (LMM) as this method is stronger in
missing data and requires different assumptions

``` r
# REF: http://www.dwoll.de/rexrepos/posts/anovaMixed.html#mixed-effects-analysis-1
# REF: https://www.introspective-mode.org/repeated-measures-anova-versus-linear-mixed-models/


# for SR components 
# Aug20 - Jul21
# box cox work the best

cbc_mo_bc_SR <- 
  cbc_mo_bc[[1]] %>% filter(!MY %in% c("21-Aug", "21-Sep"))

m_SR <- lmerTest::lmer(SR ~ stands + MY +(1|new_pl), cbc_mo_bc_SR)
#m_SR <- lmerTest::lmer(SR ~ stands*MY+(1|new_pl), cbc_mo_bc_SR)
summary(m_SR)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: SR ~ stands + MY + (1 | new_pl)
    ##    Data: cbc_mo_bc_SR
    ## 
    ## REML criterion at convergence: 420.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.02821 -0.61388  0.01615  0.70728  2.09735 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.3235   0.5688  
    ##  Residual             1.9821   1.4079  
    ## Number of obs: 120, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  14.6720     0.5283 36.8555  27.773  < 2e-16 ***
    ## standsIB     -0.4891     0.5107  9.0000  -0.958  0.36324    
    ## standsNB      1.0542     0.5107  9.0000   2.064  0.06902 .  
    ## MY20-Nov     -2.5723     0.5748 99.0000  -4.475 2.04e-05 ***
    ## MY20-Dec     -4.7292     0.5748 99.0000  -8.228 7.77e-13 ***
    ## MY21-Jan     -6.8091     0.5748 99.0000 -11.847  < 2e-16 ***
    ## MY21-Feb     -6.3825     0.5748 99.0000 -11.105  < 2e-16 ***
    ## MY21-Mar     -3.9568     0.5748 99.0000  -6.884 5.37e-10 ***
    ## MY21-Apr     -2.4858     0.5748 99.0000  -4.325 3.64e-05 ***
    ## MY21-May     -1.5196     0.5748 99.0000  -2.644  0.00953 ** 
    ## MY21-Jun      0.2266     0.5748 99.0000   0.394  0.69426    
    ## MY21-Jul      0.7678     0.5748 99.0000   1.336  0.18465    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB stndNB MY20-N MY20-D MY21-Jan MY21-F MY21-Mr MY21-A
    ## standsIB -0.483                                                           
    ## standsNB -0.483  0.500                                                    
    ## MY20-Nov -0.544  0.000  0.000                                             
    ## MY20-Dec -0.544  0.000  0.000  0.500                                      
    ## MY21-Jan -0.544  0.000  0.000  0.500  0.500                               
    ## MY21-Feb -0.544  0.000  0.000  0.500  0.500  0.500                        
    ## MY21-Mar -0.544  0.000  0.000  0.500  0.500  0.500    0.500               
    ## MY21-Apr -0.544  0.000  0.000  0.500  0.500  0.500    0.500  0.500        
    ## MY21-May -0.544  0.000  0.000  0.500  0.500  0.500    0.500  0.500   0.500
    ## MY21-Jun -0.544  0.000  0.000  0.500  0.500  0.500    0.500  0.500   0.500
    ## MY21-Jul -0.544  0.000  0.000  0.500  0.500  0.500    0.500  0.500   0.500
    ##          MY21-My MY21-Jun
    ## standsIB                 
    ## standsNB                 
    ## MY20-Nov                 
    ## MY20-Dec                 
    ## MY21-Jan                 
    ## MY21-Feb                 
    ## MY21-Mar                 
    ## MY21-Apr                 
    ## MY21-May                 
    ## MY21-Jun  0.500          
    ## MY21-Jul  0.500   0.500

``` r
anova(m_SR)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##        Sum Sq Mean Sq NumDF DenDF F value Pr(>F)    
    ## stands  18.91   9.454     2     9  4.7695 0.0387 *  
    ## MY     785.50  87.277     9    99 44.0330 <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_SR)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.787
    ##      Marginal R2: 0.753

``` r
# Rh
m_Rh <- lmerTest::lmer(Rh ~ stands +(1|new_pl), cbc_mo_bc_SR)
#m_Rh <- lmerTest::lmer(Rh ~ stands*MY +(1|new_pl), cbc_mo_bc_SR)
summary(m_Rh)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Rh ~ stands + (1 | new_pl)
    ##    Data: cbc_mo_bc_SR
    ## 
    ## REML criterion at convergence: 617.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.04234 -0.66318 -0.03636  0.79305  2.02326 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 1.066    1.033   
    ##  Residual             9.851    3.139   
    ## Number of obs: 120, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  11.9545     0.7161  9.0000  16.694 4.44e-08 ***
    ## standsIB     -1.1363     1.0127  9.0000  -1.122    0.291    
    ## standsNB      0.3621     1.0127  9.0000   0.358    0.729    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB
    ## standsIB -0.707       
    ## standsNB -0.707  0.500

``` r
anova(m_Rh)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##        Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## stands 23.488  11.744     2     9  1.1921 0.3473

``` r
r2_nakagawa(m_Rh)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.130
    ##      Marginal R2: 0.036

``` r
# Ra
m_Ra <- lmerTest::lmer(Ra ~ stands +(1|new_pl), cbc_mo_bc_SR)
#m_Ra <- lmerTest::lmer(Ra ~ stands*MY +(1|new_pl), cbc_mo_bc_SR)
summary(m_Ra)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Ra ~ stands + (1 | new_pl)
    ##    Data: cbc_mo_bc_SR
    ## 
    ## REML criterion at convergence: 410.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.83351 -0.70713  0.05712  0.80281  1.97306 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.1512   0.3888  
    ##  Residual             1.6948   1.3018  
    ## Number of obs: 120, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   4.7672     0.2831 9.0000  16.837 4.12e-08 ***
    ## standsIB      0.1622     0.4004 9.0000   0.405   0.6948    
    ## standsNB      0.7651     0.4004 9.0000   1.911   0.0884 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB
    ## standsIB -0.707       
    ## standsNB -0.707  0.500

``` r
anova(m_Ra)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##        Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## stands 6.8716  3.4358     2     9  2.0273 0.1876

``` r
r2_nakagawa(m_Ra)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.133
    ##      Marginal R2: 0.056

``` r
# D_L
# Dec 20 - Nov 21
cbc_mo_bc_DL <- 
  cbc_mo_bc[[1]] %>% 
  filter(!MY %in% c("20-Aug", "20-Sep", "20-Oct", "20-Nov", "21-Dec", "22-Jan"))
                     
m_DL <- lmerTest::lmer(D_L ~ stands +(1|new_pl),cbc_mo_bc_DL)
#m_DL <- lmerTest::lmer(D_L ~ stands*MY +(1|new_pl),cbc_mo_bc_DL)
summary(m_DL)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: D_L ~ stands + (1 | new_pl)
    ##    Data: cbc_mo_bc_DL
    ## 
    ## REML criterion at convergence: 421.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.78341 -0.54239 -0.07567  0.41527  2.45426 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.08441  0.2905  
    ##  Residual             1.89805  1.3777  
    ## Number of obs: 120, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  4.96591    0.26183  9.00000  18.966 1.45e-08 ***
    ## standsIB    -0.09869    0.37028  9.00000  -0.267    0.796    
    ## standsNB     0.28772    0.37028  9.00000   0.777    0.457    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB
    ## standsIB -0.707       
    ## standsNB -0.707  0.500

``` r
anova(m_DL)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##        Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## stands 2.2319  1.1159     2     9  0.5879 0.5755

``` r
r2_nakagawa(m_DL)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.055
    ##      Marginal R2: 0.013

``` r
# D_FR
# Oct 20 - Sep 21
cbc_mo_bc_DFR <- 
  cbc_mo_bc[[1]] %>% filter(!MY %in% c("20-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan"))

m_DFR <- lmerTest::lmer(D_FR ~ stands +(1|new_pl),cbc_mo_bc_DFR)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
#m_DFR <- lmerTest::lmer(D_FR ~ stands*MY +(1|new_pl),cbc_mo_bc_DFR)
summary(m_DFR)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: D_FR ~ stands + (1 | new_pl)
    ##    Data: cbc_mo_bc_DFR
    ## 
    ## REML criterion at convergence: 580.7
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.60959 -0.53971 -0.02588  0.64366  2.37845 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.000    0.000   
    ##  Residual             4.835    2.199   
    ## Number of obs: 132, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)   4.0579     0.3174 129.0000  12.785   <2e-16 ***
    ## standsIB     -0.2925     0.4589 129.0000  -0.637    0.525    
    ## standsNB      0.2707     0.4708 129.0000   0.575    0.566    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB
    ## standsIB -0.692       
    ## standsNB -0.674  0.466
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
anova(m_DFR)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##        Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## stands 6.6658  3.3329     2   129  0.6893 0.5038

``` r
r2_nakagawa(m_DFR)
```

    ## Warning: Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Solution: Respecify random structure! You may also decrease the
    ##   `tolerance` level to enforce the calculation of random effect variances.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: NA
    ##      Marginal R2: 0.010

``` r
# P_FR
# Oct 20 - Sep 21

cbc_mo_PFR <- cbc_mo %>% filter(!MY %in% c("20-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan"))

m_PFR <- lmerTest::lmer(P_FR ~ stands +(1|new_pl), cbc_mo_PFR)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
#m_PFR <- lmerTest::lmer(P_FR ~ stands*MY +(1|new_pl), cbc_mo_PFR)
summary(m_PFR)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: P_FR ~ stands + (1 | new_pl)
    ##    Data: cbc_mo_PFR
    ## 
    ## REML criterion at convergence: 1255
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8585 -0.6517 -0.2379  0.5974  3.1650 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept)   0.0     0.00   
    ##  Residual             900.7    30.01   
    ## Number of obs: 132, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)   17.822      4.332 129.000   4.114 6.88e-05 ***
    ## standsIB       1.738      6.264 129.000   0.277    0.782    
    ## standsNB       3.376      6.425 129.000   0.525    0.600    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB
    ## standsIB -0.692       
    ## standsNB -0.674  0.466
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
anova(m_PFR)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##        Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## stands 249.91  124.95     2   129  0.1387 0.8706

``` r
r2_nakagawa(m_PFR)
```

    ## Warning: Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Solution: Respecify random structure! You may also decrease the
    ##   `tolerance` level to enforce the calculation of random effect variances.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: NA
    ##      Marginal R2: 0.002

``` r
# FR_mass
# Oct 20 - Sep 21

cbc_mo_FR <- cbc_mo %>% filter(!MY %in% c("20-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan"))
m_FR <- lmerTest::lmer(FR_mass_gC ~ stands +(1|new_pl), cbc_mo_FR)
#m_FR <- lmerTest::lmer(FR_mass_gC ~ stands*MY +(1|new_pl), cbc_mo_FR)
summary(m_FR)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: FR_mass_gC ~ stands + (1 | new_pl)
    ##    Data: cbc_mo_FR
    ## 
    ## REML criterion at convergence: 1259.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9910 -0.6679 -0.1875  0.6958  3.3355 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 360.7    18.99   
    ##  Residual             826.0    28.74   
    ## Number of obs: 132, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  132.898     10.363   8.840  12.824 5.17e-07 ***
    ## standsIB     -19.405     14.716   8.983  -1.319    0.220    
    ## standsNB      22.082     14.775   9.131   1.495    0.169    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) stndIB
    ## standsIB -0.704       
    ## standsNB -0.701  0.494

``` r
anova(m_FR)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##        Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## stands 6467.1  3233.6     2 9.1276  3.9146 0.05921 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_FR)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.435
    ##      Marginal R2: 0.188

``` r
# vwc
# Sep 20 - Aug 21?
cbc_mo_sqrt_vwc <- 
  cbc_mo_raw %>%
  select(c(stands, plot_num, MY, vwc)) %>%
  mutate(vwc_sqrt = sqrt(vwc),
         new_pl = factor(paste0(stands, "P", plot_num)),
         MY = factor(MY, level = MY_order_v1)) %>%
  filter(MY %in% c("20-Sep", "20-Oct", "20-Nov", "20-Dec", 
                 "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", 
                 "21-Jun", "21-Jul", "21-Aug")) %>%
  filter(!MY %in% c("21-Aug", "21-May")) # missing values

m_vwc <- lmerTest::lmer(vwc ~ stands*MY +(1|new_pl), cbc_mo_sqrt_vwc)
summary(m_vwc)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: vwc ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_sqrt_vwc
    ## 
    ## REML criterion at convergence: -318.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.30487 -0.43757 -0.04883  0.45281  2.91746 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  new_pl   (Intercept) 0.0002091 0.01446 
    ##  Residual             0.0007556 0.02749 
    ## Number of obs: 115, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)        0.021031   0.017582 70.888144   1.196  0.23562    
    ## standsIB          -0.012812   0.023458 66.211521  -0.546  0.58679    
    ## standsNB           0.009819   0.023458 66.211521   0.419  0.67687    
    ## MY20-Oct           0.006019   0.021112 76.364156   0.285  0.77633    
    ## MY20-Nov           0.015969   0.021112 76.364156   0.756  0.45174    
    ## MY20-Dec           0.089669   0.021112 76.364156   4.247 6.04e-05 ***
    ## MY21-Jan           0.072259   0.021112 76.364156   3.423  0.00100 ***
    ## MY21-Feb           0.052219   0.021112 76.364156   2.473  0.01560 *  
    ## MY21-Mar           0.059719   0.021112 76.364156   2.829  0.00597 ** 
    ## MY21-Apr           0.009869   0.021112 76.364156   0.467  0.64150    
    ## MY21-Jun           0.091354   0.021112 76.364156   4.327 4.52e-05 ***
    ## MY21-Jul           0.094469   0.021112 76.364156   4.475 2.63e-05 ***
    ## standsIB:MY20-Oct  0.011312   0.028697 76.050725   0.394  0.69454    
    ## standsNB:MY20-Oct -0.036432   0.029865 76.424735  -1.220  0.22625    
    ## standsIB:MY20-Nov -0.005788   0.028697 76.050725  -0.202  0.84069    
    ## standsNB:MY20-Nov -0.035419   0.028697 76.050725  -1.234  0.22091    
    ## standsIB:MY20-Dec -0.040838   0.028697 76.050725  -1.423  0.15880    
    ## standsNB:MY20-Dec -0.085769   0.028697 76.050725  -2.989  0.00377 ** 
    ## standsIB:MY21-Jan  0.017122   0.028697 76.050725   0.597  0.55251    
    ## standsNB:MY21-Jan -0.025059   0.028697 76.050725  -0.873  0.38528    
    ## standsIB:MY21-Feb -0.017238   0.028697 76.050725  -0.601  0.54983    
    ## standsNB:MY21-Feb -0.023469   0.028697 76.050725  -0.818  0.41601    
    ## standsIB:MY21-Mar -0.005438   0.028697 76.050725  -0.189  0.85021    
    ## standsNB:MY21-Mar -0.019769   0.028697 76.050725  -0.689  0.49298    
    ## standsIB:MY21-Apr  0.014762   0.028697 76.050725   0.514  0.60846    
    ## standsNB:MY21-Apr  0.032424   0.037888 77.788505   0.856  0.39475    
    ## standsIB:MY21-Jun  0.019177   0.028697 76.050725   0.668  0.50598    
    ## standsNB:MY21-Jun -0.032804   0.028697 76.050725  -1.143  0.25657    
    ## standsIB:MY21-Jul -0.008938   0.028697 76.050725  -0.311  0.75630    
    ## standsNB:MY21-Jul -0.063369   0.028697 76.050725  -2.208  0.03024 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 30 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
anova(m_vwc)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##             Sum Sq   Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## stands    0.002038 0.0010188     2  8.972  1.3484    0.3076    
    ## MY        0.104668 0.0116297     9 76.015 15.3924 6.033e-14 ***
    ## stands:MY 0.018306 0.0010170    18 75.931  1.3460    0.1849    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_vwc)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.651
    ##      Marginal R2: 0.554

``` r
# Ts
# Sep 20 - Aug 21?
cbc_mo_Ts <- 
  cbc_mo_raw %>% 
  mutate(new_pl = factor(paste0(stands, "P", plot_num)),
         MY = factor(MY, level = MY_order_v1)) %>%
  filter(MY %in% c("20-Sep", "20-Oct", "20-Nov", "20-Dec", 
                 "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", 
                 "21-Jun", "21-Jul", "21-Aug"))

m_Ts <- lmerTest::lmer(Ts ~ stands*MY +(1|new_pl), cbc_mo_Ts)
#m_Ts <- lmerTest::lmer(Ts ~ stands*MY +(1|new_pl), cbc_mo_Ts)
summary(m_Ts)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Ts ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_Ts
    ## 
    ## REML criterion at convergence: 400.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9045 -0.3525  0.0000  0.4601  3.5914 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.04048  0.2012  
    ##  Residual             2.66302  1.6319  
    ## Number of obs: 129, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)        26.78495    0.94906  92.95521  28.223  < 2e-16 ***
    ## standsIB            1.67005    1.25562  92.90965   1.330  0.18676    
    ## standsNB           -1.32995    1.25562  92.90965  -1.059  0.29226    
    ## MY20-Oct           -5.53495    1.24754  86.66927  -4.437 2.67e-05 ***
    ## MY20-Nov          -10.80495    1.24754  86.66927  -8.661 2.28e-13 ***
    ## MY20-Dec          -13.37495    1.24754  86.66927 -10.721  < 2e-16 ***
    ## MY21-Jan          -14.03495    1.24754  86.66927 -11.250  < 2e-16 ***
    ## MY21-Feb          -14.90495    1.24754  86.66927 -11.947  < 2e-16 ***
    ## MY21-Mar          -11.27495    1.24754  86.66927  -9.038 3.87e-14 ***
    ## MY21-Apr           -6.96682    1.24754  86.66927  -5.584 2.65e-07 ***
    ## MY21-May           -2.41995    1.24754  86.66927  -1.940  0.05566 .  
    ## MY21-Jun            0.73678    1.24754  86.66927   0.591  0.55633    
    ## MY21-Jul            0.07061    1.24754  86.66927   0.057  0.95500    
    ## MY21-Aug            0.67733    1.49163  87.26611   0.454  0.65089    
    ## standsIB:MY20-Oct   0.57495    1.69937  85.87797   0.338  0.73594    
    ## standsNB:MY20-Oct   1.61004    1.76430  86.69441   0.913  0.36400    
    ## standsIB:MY20-Nov   0.28495    1.69937  85.87797   0.168  0.86723    
    ## standsNB:MY20-Nov   4.20995    1.69937  85.87797   2.477  0.01520 *  
    ## standsIB:MY20-Dec  -2.41005    1.69937  85.87797  -1.418  0.15975    
    ## standsNB:MY20-Dec  -0.70505    1.69937  85.87797  -0.415  0.67926    
    ## standsIB:MY21-Jan  -4.52005    1.69937  85.87797  -2.660  0.00933 ** 
    ## standsNB:MY21-Jan  -1.11505    1.69937  85.87797  -0.656  0.51348    
    ## standsIB:MY21-Feb   0.91995    1.69937  85.87797   0.541  0.58967    
    ## standsNB:MY21-Feb  -0.70005    1.69937  85.87797  -0.412  0.68140    
    ## standsIB:MY21-Mar   2.25995    1.69937  85.87797   1.330  0.18708    
    ## standsNB:MY21-Mar   3.33495    1.69937  85.87797   1.962  0.05295 .  
    ## standsIB:MY21-Apr  -3.87318    1.69937  85.87797  -2.279  0.02514 *  
    ## standsNB:MY21-Apr  -0.33752    2.21620  89.88024  -0.152  0.87930    
    ## standsIB:MY21-May  -3.60099    1.76429  86.66927  -2.041  0.04429 *  
    ## standsNB:MY21-May  -1.41439    2.21620  89.88024  -0.638  0.52496    
    ## standsIB:MY21-Jun  -1.94678    1.69937  85.87797  -1.146  0.25515    
    ## standsNB:MY21-Jun  -0.80178    1.69937  85.87797  -0.472  0.63826    
    ## standsIB:MY21-Jul  -0.58061    1.69937  85.87797  -0.342  0.73344    
    ## standsNB:MY21-Jul   0.45439    1.69937  85.87797   0.267  0.78981    
    ## standsIB:MY21-Aug   0.18548    2.36222  89.71405   0.079  0.93759    
    ## standsNB:MY21-Aug  -2.06362    1.94457  87.04352  -1.061  0.29153    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 36 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
anova(m_Ts)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##           Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
    ## stands      45.7   22.83     2 11.497   8.5718 0.0052637 ** 
    ## MY        4502.8  409.34    11 86.344 153.7133 < 2.2e-16 ***
    ## stands:MY  158.4    7.20    22 86.163   2.7032 0.0005626 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_Ts)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.933
    ##      Marginal R2: 0.932

``` r
# REF: https://ademos.people.uic.edu/Chapter18.html


check_LMM_assumption <- 
  function(model, df, var){
    # 1.Linearity
  p1 <- 
    ggplot() + 
    geom_point(aes(x = resid(model), y = df[[var]] %>% na.omit())) + 
    theme_bw() + xlab("Residual") + ylab(var)
 
   # 2. Homogeneity of variance  
  p2 <- plot(model)
  
  # the normality of residuals
  p3 <- qqmath(model, id = 0.05)
 
  grid.arrange(grobs = list(p1,p2, p3), ncol = 3)
  }

# to check the homogeneity of variances by anova - similar to Levene's test
# p > 0.05 indicate no significant differences between subject variances
check_LMM_homogeneity <- 
  function(model, df, var){
    res <- data.frame(residual_2 = (residuals(model))^2, 
                      id = df[!is.na(df[[var]]),] %>% select(new_pl)) 
    
    anova(lm(residual_2 ~ new_pl, res))
   
  }
```

``` r
# SR
check_LMM_assumption(m_SR, cbc_mo_bc_SR, "SR")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-1.png)<!-- -->

``` r
check_LMM_homogeneity(m_SR, cbc_mo_bc_SR, "SR")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  46.44  4.2220   0.652 0.7802
    ## Residuals 108 699.31  6.4751

``` r
# looks good


# Rh
check_LMM_assumption(m_Rh, cbc_mo_bc_SR, "Rh")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-2.png)<!-- -->

``` r
check_LMM_homogeneity(m_Rh, cbc_mo_bc_SR, "Rh")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df  Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  1063.6  96.688  0.9776 0.4712
    ## Residuals 108 10681.3  98.901

``` r
# homogeneity of variances don't meet = differences in study plots...

# Ra
check_LMM_assumption(m_Ra, cbc_mo_bc_SR, "Ra")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-3.png)<!-- -->

``` r
check_LMM_homogeneity(m_Ra, cbc_mo_bc_SR, "Ra")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df  Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  42.932  3.9029   1.536  0.129
    ## Residuals 108 274.417  2.5409

``` r
# homogeneity of variances don't meet = differences in study plots...


# D_L
# Dec 20 - Nov 21
check_LMM_assumption(m_DL, cbc_mo_bc_DL, "D_L")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-4.png)<!-- -->

``` r
check_LMM_homogeneity(m_DL, cbc_mo_bc_DL, "D_L")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  38.78  3.5259  0.4782 0.9132
    ## Residuals 108 796.38  7.3739

``` r
# looks good

# D_FR
# Oct 20 - Sep 21
check_LMM_assumption(m_DFR, cbc_mo_bc_DFR, "D_FR")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-5.png)<!-- -->

``` r
check_LMM_homogeneity(m_DFR, cbc_mo_bc_DFR, "D_FR")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df Sum Sq Mean Sq F value  Pr(>F)  
    ## new_pl     11  787.8  71.617   1.844 0.05382 .
    ## Residuals 120 4660.6  38.838                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# homogeneity of variances don't meet = differences in study plots...

# P_FR
# Oct 20 - Sep 21
check_LMM_assumption(m_PFR, cbc_mo_PFR, "P_FR")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-6.png)<!-- -->

``` r
check_LMM_homogeneity(m_PFR, cbc_mo_PFR, "P_FR")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df    Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  34763345 3160304  1.5571 0.1203
    ## Residuals 120 243560437 2029670

``` r
# looks good

# vwc
# Sep 20 - Aug 21?
check_LMM_assumption(m_vwc, cbc_mo_sqrt_vwc, "vwc")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-7.png)<!-- -->

``` r
check_LMM_homogeneity(m_vwc, cbc_mo_sqrt_vwc, "vwc")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df     Sum Sq    Mean Sq F value  Pr(>F)  
    ## new_pl     11 2.3609e-05 2.1462e-06  2.2397 0.01752 *
    ## Residuals 103 9.8705e-05 9.5830e-07                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# homogeneity of variances don't meet = differences in study plots...

# Ts
# Sep 20 - Aug 21?
check_LMM_assumption(m_Ts, cbc_mo_Ts, "Ts")
```

![](CBC_mo_LMM_files/figure-gfm/Check%20LMM%20assumptions-8.png)<!-- -->

``` r
check_LMM_homogeneity(m_Ts, cbc_mo_Ts, "Ts")
```

    ## Analysis of Variance Table
    ## 
    ## Response: residual_2
    ##            Df  Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  219.39  19.945  1.1777   0.31
    ## Residuals 117 1981.48  16.936

``` r
# looks good
```

What I want to know is if there are any significant differences among
stand in each month rather than differences in month..

``` r
# emmeans(model, pairwise ~ stands)
# emmeans(model, pairwise ~ MY)

# this gives you the stand differences in each month
lsmeans(m_Ts, pairwise ~ stands | MY, adjust = "tukey") 
```

    ## $lsmeans
    ## MY = 20-Sep:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      26.78 0.954 93.0    24.89     28.7
    ##  IB      28.45 0.822 92.8    26.82     30.1
    ##  NB      25.45 0.822 92.8    23.82     27.1
    ## 
    ## MY = 20-Oct:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      21.25 0.822 92.8    19.62     22.9
    ##  IB      23.50 0.822 92.8    21.86     25.1
    ##  NB      21.53 0.953 92.9    19.64     23.4
    ## 
    ## MY = 20-Nov:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      15.98 0.822 92.8    14.35     17.6
    ##  IB      17.93 0.822 92.8    16.30     19.6
    ##  NB      18.86 0.822 92.8    17.23     20.5
    ## 
    ## MY = 20-Dec:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      13.41 0.822 92.8    11.78     15.0
    ##  IB      12.67 0.822 92.8    11.04     14.3
    ##  NB      11.38 0.822 92.8     9.74     13.0
    ## 
    ## MY = 21-Jan:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      12.75 0.822 92.8    11.12     14.4
    ##  IB       9.90 0.822 92.8     8.27     11.5
    ##  NB      10.30 0.822 92.8     8.67     11.9
    ## 
    ## MY = 21-Feb:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      11.88 0.822 92.8    10.25     13.5
    ##  IB      14.47 0.822 92.8    12.84     16.1
    ##  NB       9.85 0.822 92.8     8.22     11.5
    ## 
    ## MY = 21-Mar:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      15.51 0.822 92.8    13.88     17.1
    ##  IB      19.44 0.822 92.8    17.81     21.1
    ##  NB      17.52 0.822 92.8    15.88     19.1
    ## 
    ## MY = 21-Apr:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      19.82 0.822 92.8    18.19     21.5
    ##  IB      17.61 0.822 92.8    15.98     19.2
    ##  NB      18.15 1.667 93.0    14.84     21.5
    ## 
    ## MY = 21-May:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      24.36 0.822 92.8    22.73     26.0
    ##  IB      22.43 0.954 93.0    20.54     24.3
    ##  NB      21.62 1.667 93.0    18.31     24.9
    ## 
    ## MY = 21-Jun:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      27.52 0.822 92.8    25.89     29.2
    ##  IB      27.25 0.822 92.8    25.61     28.9
    ##  NB      25.39 0.822 92.8    23.76     27.0
    ## 
    ## MY = 21-Jul:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      26.86 0.822 92.8    25.22     28.5
    ##  IB      27.95 0.822 92.8    26.31     29.6
    ##  NB      25.98 0.822 92.8    24.35     27.6
    ## 
    ## MY = 21-Aug:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      27.46 1.174 93.0    25.13     29.8
    ##  IB      29.32 1.667 93.0    26.01     32.6
    ##  NB      24.07 0.953 92.9    22.18     26.0
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## MY = 20-Sep:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB    -1.670 1.26 92.9  -1.326  0.3843
    ##  AB - NB     1.330 1.26 92.9   1.056  0.5436
    ##  IB - NB     3.000 1.16 92.8   2.580  0.0305
    ## 
    ## MY = 20-Oct:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB    -2.245 1.16 92.8  -1.931  0.1357
    ##  AB - NB    -0.280 1.26 92.9  -0.222  0.9731
    ##  IB - NB     1.965 1.26 92.9   1.561  0.2678
    ## 
    ## MY = 20-Nov:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB    -1.955 1.16 92.8  -1.682  0.2177
    ##  AB - NB    -2.880 1.16 92.8  -2.477  0.0396
    ##  IB - NB    -0.925 1.16 92.8  -0.796  0.7067
    ## 
    ## MY = 20-Dec:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB     0.740 1.16 92.8   0.636  0.8004
    ##  AB - NB     2.035 1.16 92.8   1.750  0.1922
    ##  IB - NB     1.295 1.16 92.8   1.114  0.5080
    ## 
    ## MY = 21-Jan:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB     2.850 1.16 92.8   2.451  0.0422
    ##  AB - NB     2.445 1.16 92.8   2.103  0.0948
    ##  IB - NB    -0.405 1.16 92.8  -0.348  0.9354
    ## 
    ## MY = 21-Feb:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB    -2.590 1.16 92.8  -2.228  0.0718
    ##  AB - NB     2.030 1.16 92.8   1.746  0.1938
    ##  IB - NB     4.620 1.16 92.8   3.974  0.0004
    ## 
    ## MY = 21-Mar:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB    -3.930 1.16 92.8  -3.380  0.0030
    ##  AB - NB    -2.005 1.16 92.8  -1.725  0.2015
    ##  IB - NB     1.925 1.16 92.8   1.656  0.2278
    ## 
    ## MY = 21-Apr:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB     2.203 1.16 92.8   1.895  0.1459
    ##  AB - NB     1.667 1.86 93.0   0.897  0.6435
    ##  IB - NB    -0.536 1.86 93.0  -0.288  0.9553
    ## 
    ## MY = 21-May:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB     1.931 1.26 92.9   1.534  0.2801
    ##  AB - NB     2.744 1.86 93.0   1.477  0.3068
    ##  IB - NB     0.813 1.92 93.0   0.424  0.9060
    ## 
    ## MY = 21-Jun:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB     0.277 1.16 92.8   0.238  0.9693
    ##  AB - NB     2.132 1.16 92.8   1.834  0.1644
    ##  IB - NB     1.855 1.16 92.8   1.595  0.2527
    ## 
    ## MY = 21-Jul:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB    -1.089 1.16 92.8  -0.937  0.6184
    ##  AB - NB     0.876 1.16 92.8   0.753  0.7325
    ##  IB - NB     1.965 1.16 92.8   1.690  0.2144
    ## 
    ## MY = 21-Aug:
    ##  contrast estimate   SE   df t.ratio p.value
    ##  AB - IB    -1.856 2.04 93.0  -0.910  0.6354
    ##  AB - NB     3.394 1.51 93.0   2.244  0.0692
    ##  IB - NB     5.249 1.92 93.0   2.733  0.0203
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
lsmeans(m_Ts, pairwise ~ stands, adjust = "tukey")
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $lsmeans
    ##  stands lsmean    SE    df lower.CL upper.CL
    ##  AB       20.3 0.269  8.23     19.7     20.9
    ##  IB       20.9 0.285 10.54     20.3     21.5
    ##  NB       19.2 0.316 13.16     18.5     19.9
    ## 
    ## Results are averaged over the levels of: MY 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast estimate    SE    df t.ratio p.value
    ##  AB - IB    -0.611 0.392  9.35  -1.559  0.3091
    ##  AB - NB     1.124 0.415 10.65   2.707  0.0506
    ##  IB - NB     1.735 0.426 11.86   4.074  0.0042
    ## 
    ## Results are averaged over the levels of: MY 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
lsmeans(m_vwc, pairwise ~ stands | MY, adjust = "tukey")
```

    ## $lsmeans
    ## MY = 20-Sep:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.021031 0.0176 71.3 -0.014078   0.0561
    ##  IB     0.008219 0.0155 60.8 -0.022836   0.0393
    ##  NB     0.030850 0.0155 60.8 -0.000205   0.0619
    ## 
    ## MY = 20-Oct:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.027050 0.0155 60.8 -0.004005   0.0581
    ##  IB     0.025550 0.0155 60.8 -0.005505   0.0566
    ##  NB     0.000438 0.0176 71.2 -0.034705   0.0356
    ## 
    ## MY = 20-Nov:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.037000 0.0155 60.8  0.005945   0.0681
    ##  IB     0.018400 0.0155 60.8 -0.012655   0.0495
    ##  NB     0.011400 0.0155 60.8 -0.019655   0.0425
    ## 
    ## MY = 20-Dec:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.110700 0.0155 60.8  0.079645   0.1418
    ##  IB     0.057050 0.0155 60.8  0.025995   0.0881
    ##  NB     0.034750 0.0155 60.8  0.003695   0.0658
    ## 
    ## MY = 21-Jan:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.093290 0.0155 60.8  0.062235   0.1243
    ##  IB     0.097600 0.0155 60.8  0.066545   0.1287
    ##  NB     0.078050 0.0155 60.8  0.046995   0.1091
    ## 
    ## MY = 21-Feb:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.073250 0.0155 60.8  0.042195   0.1043
    ##  IB     0.043200 0.0155 60.8  0.012145   0.0743
    ##  NB     0.059600 0.0155 60.8  0.028545   0.0907
    ## 
    ## MY = 21-Mar:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.080750 0.0155 60.8  0.049695   0.1118
    ##  IB     0.062500 0.0155 60.8  0.031445   0.0936
    ##  NB     0.070800 0.0155 60.8  0.039745   0.1019
    ## 
    ## MY = 21-Apr:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.030900 0.0155 60.8 -0.000155   0.0620
    ##  IB     0.032850 0.0155 60.8  0.001795   0.0639
    ##  NB     0.073143 0.0294 85.0  0.014766   0.1315
    ## 
    ## MY = 21-Jun:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.112385 0.0155 60.8  0.081330   0.1434
    ##  IB     0.118750 0.0155 60.8  0.087695   0.1498
    ##  NB     0.089400 0.0155 60.8  0.058345   0.1205
    ## 
    ## MY = 21-Jul:
    ##  stands   lsmean     SE   df  lower.CL upper.CL
    ##  AB     0.115500 0.0155 60.8  0.084445   0.1466
    ##  IB     0.093750 0.0155 60.8  0.062695   0.1248
    ##  NB     0.061950 0.0155 60.8  0.030895   0.0930
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## MY = 20-Sep:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.01281 0.0235 66.8   0.546  0.8490
    ##  AB - NB  -0.00982 0.0235 66.8  -0.418  0.9082
    ##  IB - NB  -0.02263 0.0220 60.8  -1.030  0.5607
    ## 
    ## MY = 20-Oct:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.00150 0.0220 60.8   0.068  0.9974
    ##  AB - NB   0.02661 0.0235 66.7   1.133  0.4974
    ##  IB - NB   0.02511 0.0235 66.7   1.069  0.5365
    ## 
    ## MY = 20-Nov:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.01860 0.0220 60.8   0.847  0.6755
    ##  AB - NB   0.02560 0.0220 60.8   1.166  0.4781
    ##  IB - NB   0.00700 0.0220 60.8   0.319  0.9456
    ## 
    ## MY = 20-Dec:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.05365 0.0220 60.8   2.443  0.0454
    ##  AB - NB   0.07595 0.0220 60.8   3.458  0.0028
    ##  IB - NB   0.02230 0.0220 60.8   1.015  0.5701
    ## 
    ## MY = 21-Jan:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB  -0.00431 0.0220 60.8  -0.196  0.9790
    ##  AB - NB   0.01524 0.0220 60.8   0.694  0.7679
    ##  IB - NB   0.01955 0.0220 60.8   0.890  0.6485
    ## 
    ## MY = 21-Feb:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.03005 0.0220 60.8   1.368  0.3638
    ##  AB - NB   0.01365 0.0220 60.8   0.622  0.8089
    ##  IB - NB  -0.01640 0.0220 60.8  -0.747  0.7367
    ## 
    ## MY = 21-Mar:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.01825 0.0220 60.8   0.831  0.6854
    ##  AB - NB   0.00995 0.0220 60.8   0.453  0.8932
    ##  IB - NB  -0.00830 0.0220 60.8  -0.378  0.9244
    ## 
    ## MY = 21-Apr:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB  -0.00195 0.0220 60.8  -0.089  0.9957
    ##  AB - NB  -0.04224 0.0332 83.4  -1.272  0.4150
    ##  IB - NB  -0.04029 0.0332 83.4  -1.213  0.4488
    ## 
    ## MY = 21-Jun:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB  -0.00637 0.0220 60.8  -0.290  0.9548
    ##  AB - NB   0.02298 0.0220 60.8   1.047  0.5507
    ##  IB - NB   0.02935 0.0220 60.8   1.336  0.3808
    ## 
    ## MY = 21-Jul:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.02175 0.0220 60.8   0.990  0.5857
    ##  AB - NB   0.05355 0.0220 60.8   2.438  0.0459
    ##  IB - NB   0.03180 0.0220 60.8   1.448  0.3231
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
lsmeans(m_vwc, pairwise ~ stands, adjust = "tukey")
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $lsmeans
    ##  stands lsmean      SE    df lower.CL upper.CL
    ##  AB     0.0702 0.00848  8.87   0.0510   0.0894
    ##  IB     0.0558 0.00844  8.71   0.0366   0.0750
    ##  NB     0.0510 0.00884 10.40   0.0314   0.0706
    ## 
    ## Results are averaged over the levels of: MY 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.01440 0.0120 8.79   1.204  0.4808
    ##  AB - NB   0.01915 0.0122 9.63   1.563  0.3064
    ##  IB - NB   0.00475 0.0122 9.54   0.389  0.9208
    ## 
    ## Results are averaged over the levels of: MY 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
lsmeans(m_SR, pairwise ~ stands | MY, adjust = "tukey")
```

    ## $lsmeans
    ## MY = 20-Oct:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      14.67 0.528 36.9    13.60    15.74
    ##  IB      14.18 0.528 36.9    13.11    15.25
    ##  NB      15.73 0.528 36.9    14.66    16.80
    ## 
    ## MY = 20-Nov:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      12.10 0.528 36.9    11.03    13.17
    ##  IB      11.61 0.528 36.9    10.54    12.68
    ##  NB      13.15 0.528 36.9    12.08    14.22
    ## 
    ## MY = 20-Dec:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB       9.94 0.528 36.9     8.87    11.01
    ##  IB       9.45 0.528 36.9     8.38    10.52
    ##  NB      11.00 0.528 36.9     9.93    12.07
    ## 
    ## MY = 21-Jan:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB       7.86 0.528 36.9     6.79     8.93
    ##  IB       7.37 0.528 36.9     6.30     8.44
    ##  NB       8.92 0.528 36.9     7.85     9.99
    ## 
    ## MY = 21-Feb:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB       8.29 0.528 36.9     7.22     9.36
    ##  IB       7.80 0.528 36.9     6.73     8.87
    ##  NB       9.34 0.528 36.9     8.27    10.41
    ## 
    ## MY = 21-Mar:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      10.72 0.528 36.9     9.64    11.79
    ##  IB      10.23 0.528 36.9     9.16    11.30
    ##  NB      11.77 0.528 36.9    10.70    12.84
    ## 
    ## MY = 21-Apr:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      12.19 0.528 36.9    11.12    13.26
    ##  IB      11.70 0.528 36.9    10.63    12.77
    ##  NB      13.24 0.528 36.9    12.17    14.31
    ## 
    ## MY = 21-May:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      13.15 0.528 36.9    12.08    14.22
    ##  IB      12.66 0.528 36.9    11.59    13.73
    ##  NB      14.21 0.528 36.9    13.14    15.28
    ## 
    ## MY = 21-Jun:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      14.90 0.528 36.9    13.83    15.97
    ##  IB      14.41 0.528 36.9    13.34    15.48
    ##  NB      15.95 0.528 36.9    14.88    17.02
    ## 
    ## MY = 21-Jul:
    ##  stands lsmean    SE   df lower.CL upper.CL
    ##  AB      15.44 0.528 36.9    14.37    16.51
    ##  IB      14.95 0.528 36.9    13.88    16.02
    ##  NB      16.49 0.528 36.9    15.42    17.56
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## MY = 20-Oct:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 20-Nov:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 20-Dec:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 21-Jan:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 21-Feb:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 21-Mar:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 21-Apr:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 21-May:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 21-Jun:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## MY = 21-Jul:
    ##  contrast estimate    SE df t.ratio p.value
    ##  AB - IB     0.489 0.511  9   0.958  0.6199
    ##  AB - NB    -1.054 0.511  9  -2.064  0.1527
    ##  IB - NB    -1.543 0.511  9  -3.022  0.0349
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
lsmeans(m_Rh, pairwise ~ stands | MY, adjust = "tukey")
```

    ## Error in emmfcn(...): No variable named MY in the reference grid

``` r
lsmeans(m_Ra, pairwise ~ stands | MY, adjust = "tukey")
```

    ## Error in emmfcn(...): No variable named MY in the reference grid

``` r
# contrast
# ref: https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/
emm1 = emmeans(m_Ts, specs = pairwise ~ stands)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
emm2 = emmeans(m_vwc, specs = pairwise ~ stands)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
burn = c(1,1,0)
unburn = c(0,0,1)

burn_overall = burn/2

contrast(emm1, method = list("burn - unburn" = burn_overall - unburn), adjust = "tukey")
```

    ##  contrast      estimate    SE   df t.ratio p.value
    ##  burn - unburn     1.43 0.372 11.9   3.841  0.0024
    ## 
    ## Results are averaged over the levels of: MY 
    ## Degrees-of-freedom method: kenward-roger

``` r
contrast(emm2, method = list("burn - unburn" = burn_overall - unburn), adjust = "tukey")
```

    ##  contrast      estimate     SE   df t.ratio p.value
    ##  burn - unburn   0.0119 0.0107 9.85   1.119  0.2896
    ## 
    ## Results are averaged over the levels of: MY 
    ## Degrees-of-freedom method: kenward-roger
