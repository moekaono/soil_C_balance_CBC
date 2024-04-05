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
  mutate(across(D_FR:vwc, as.numeric)) %>%
  mutate(new_pl = paste0(stands, "P", plot_num)) %>% 
  rename("SR_flux" = "SR",
         "SR" = "SR_mo_gC", 
         "Rh" = "Rh_season_mo_gC",
         "Ra" = "Ra_mo_gC") # all gC/m2/mo
```

    ## Warning: There were 3 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(D_FR:vwc, as.numeric)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.

``` r
cbc_mo[cbc_mo == '#DIV/0!'] <- NA

cbc_mo$stands <- as.factor(cbc_mo$stands)
cbc_mo$new_pl <- as.factor(cbc_mo$new_pl)

MY_order_v1 <- c("20-Jul", "20-Aug", "20-Sep", "20-Oct", "20-Nov", "20-Dec", 
                 "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", "21-Jun", 
                 "21-Jul", "21-Aug", "21-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan")
              

cbc_mo$MY <- factor(cbc_mo$MY, level = MY_order_v1)

# interested variavles
vars <- c("D_FR","P_FR", "SR","Rh","Ra", "D_L", "Ts","vwc")
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
  mutate(across(D_FR:vwc, log))
```

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(D_FR:vwc, log)`.
    ## Caused by warning:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
# sqrt
cbc_mo_sqrt <- 
  cbc_mo %>%
  mutate(across(D_FR:vwc, sqrt))
```

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(D_FR:vwc, sqrt)`.
    ## Caused by warning:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
# box-cox transformation
bc_trans <- function(df){
  output <- cbc_mo[,c(1:3, 28)]
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
colnames(cbc_mo_bc[[1]])[5:12] <- vars

cbc_mo_bc[[2]]
```

    ##    var           skewness lambda
    ## 1 D_FR   1.40887926456789    0.3
    ## 2 P_FR  0.467229693280081   <NA>
    ## 3   SR  0.788254287268379    0.4
    ## 4   Rh   0.83534567202595    0.3
    ## 5   Ra   1.08627410710574    0.2
    ## 6  D_L    1.1527898873942    0.2
    ## 7   Ts -0.255836140348404    1.2
    ## 8  vwc  0.784857334243315   <NA>

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

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 93 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 93 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

![](CBC_mo_LMM_files/figure-gfm/density%20plot-1.png)<!-- -->

``` r
# for log data
density_plot(cbc_mo_log, "Log-transformed data")
```

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 144 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 144 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 94 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 94 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

![](CBC_mo_LMM_files/figure-gfm/density%20plot-2.png)<!-- -->

``` r
# for sqrt data
density_plot(cbc_mo_sqrt, "Square-Root Transformation")
```

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 111 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 111 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 93 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 93 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

![](CBC_mo_LMM_files/figure-gfm/density%20plot-3.png)<!-- -->

``` r
# box-cox transformation
density_plot(cbc_mo_bc[[1]], "Box-Cox Transformation")
```

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 95 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 60 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 35 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 82 rows containing non-finite outside the scale range
    ## (`stat_overlay_normal_density()`).

    ## Warning: Removed 93 rows containing non-finite outside the scale range
    ## (`stat_density()`).

    ## Warning: Removed 93 rows containing non-finite outside the scale range
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
                 "21-Jan", "21-Feb", "21-Mar", "21-Apr", "21-May", "21-Jun", "21-Jul", "21-Aug", "21-Sep", "21-Oct", "21-Nov", "21-Dec", 
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

m_SR <- lmerTest::lmer(SR ~ stands*MY+(1|new_pl), cbc_mo_bc_SR)
summary(m_SR)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: SR ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_bc_SR
    ## 
    ## REML criterion at convergence: 423.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.48990 -0.59319  0.00547  0.55514  2.47546 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.1915   0.4376  
    ##  Residual             1.7322   1.3161  
    ## Number of obs: 144, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)        19.8577     0.6935  97.3878  28.635  < 2e-16 ***
    ## standsIB           -3.8841     0.9807  97.3878  -3.960 0.000142 ***
    ## standsNB           -5.0193     0.9807  97.3878  -5.118 1.55e-06 ***
    ## MY20-Sep           -3.3961     0.9306  99.0000  -3.649 0.000422 ***
    ## MY20-Oct           -4.8149     0.9306  99.0000  -5.174 1.20e-06 ***
    ## MY20-Nov           -7.6064     0.9306  99.0000  -8.173 1.02e-12 ***
    ## MY20-Dec           -9.2567     0.9306  99.0000  -9.947  < 2e-16 ***
    ## MY21-Jan          -10.8133     0.9306  99.0000 -11.619  < 2e-16 ***
    ## MY21-Feb          -11.6674     0.9306  99.0000 -12.537  < 2e-16 ***
    ## MY21-Mar          -10.5096     0.9306  99.0000 -11.293  < 2e-16 ***
    ## MY21-Apr           -7.2098     0.9306  99.0000  -7.747 8.32e-12 ***
    ## MY21-May           -6.5703     0.9306  99.0000  -7.060 2.32e-10 ***
    ## MY21-Jun           -5.1212     0.9306  99.0000  -5.503 2.94e-07 ***
    ## MY21-Jul           -5.7480     0.9306  99.0000  -6.176 1.46e-08 ***
    ## standsIB:MY20-Sep   4.3114     1.3161  99.0000   3.276 0.001453 ** 
    ## standsNB:MY20-Sep   8.8080     1.3161  99.0000   6.692 1.33e-09 ***
    ## standsIB:MY20-Oct   3.0390     1.3161  99.0000   2.309 0.023021 *  
    ## standsNB:MY20-Oct   5.3170     1.3161  99.0000   4.040 0.000106 ***
    ## standsIB:MY20-Nov   3.7533     1.3161  99.0000   2.852 0.005293 ** 
    ## standsNB:MY20-Nov   5.2606     1.3161  99.0000   3.997 0.000124 ***
    ## standsIB:MY20-Dec   2.7120     1.3161  99.0000   2.061 0.041968 *  
    ## standsNB:MY20-Dec   4.7819     1.3161  99.0000   3.633 0.000446 ***
    ## standsIB:MY21-Jan   1.7070     1.3161  99.0000   1.297 0.197644    
    ## standsNB:MY21-Jan   4.2170     1.3161  99.0000   3.204 0.001824 ** 
    ## standsIB:MY21-Feb   4.4014     1.3161  99.0000   3.344 0.001166 ** 
    ## standsNB:MY21-Feb   5.3646     1.3161  99.0000   4.076 9.26e-05 ***
    ## standsIB:MY21-Mar   5.6030     1.3161  99.0000   4.257 4.71e-05 ***
    ## standsNB:MY21-Mar   7.9668     1.3161  99.0000   6.053 2.55e-08 ***
    ## standsIB:MY21-Apr   1.6434     1.3161  99.0000   1.249 0.214736    
    ## standsNB:MY21-Apr   6.4399     1.3161  99.0000   4.893 3.85e-06 ***
    ## standsIB:MY21-May   2.0360     1.3161  99.0000   1.547 0.125057    
    ## standsNB:MY21-May   7.0274     1.3161  99.0000   5.339 5.94e-07 ***
    ## standsIB:MY21-Jun   3.7131     1.3161  99.0000   2.821 0.005781 ** 
    ## standsNB:MY21-Jun   6.2415     1.3161  99.0000   4.742 7.10e-06 ***
    ## standsIB:MY21-Jul   5.3411     1.3161  99.0000   4.058 9.89e-05 ***
    ## standsNB:MY21-Jul   8.1178     1.3161  99.0000   6.168 1.51e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 36 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
anova(m_SR)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##            Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## stands      22.36  11.182     2     9  6.4553   0.01824 *  
    ## MY        1345.45 122.314    11    99 70.6113 < 2.2e-16 ***
    ## stands:MY  152.66   6.939    22    99  4.0059 9.682e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_SR)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.864
    ##      Marginal R2: 0.849

``` r
# Rh
m_Rh <- lmerTest::lmer(Rh ~ stands*MY +(1|new_pl), cbc_mo_bc_SR)
summary(m_Rh)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Rh ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_bc_SR
    ## 
    ## REML criterion at convergence: 329.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.01817 -0.66566  0.02911  0.65156  1.85089 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.3567   0.5973  
    ##  Residual             0.6564   0.8102  
    ## Number of obs: 144, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)        10.9669     0.5033 45.6889  21.791  < 2e-16 ***
    ## standsIB           -2.2258     0.7117 45.6889  -3.127 0.003067 ** 
    ## standsNB           -2.8482     0.7117 45.6889  -4.002 0.000228 ***
    ## MY20-Sep           -1.6558     0.5729 99.0000  -2.890 0.004731 ** 
    ## MY20-Oct           -2.3685     0.5729 99.0000  -4.134 7.46e-05 ***
    ## MY20-Nov           -3.1546     0.5729 99.0000  -5.507 2.89e-07 ***
    ## MY20-Dec           -4.1027     0.5729 99.0000  -7.162 1.43e-10 ***
    ## MY21-Jan           -5.0804     0.5729 99.0000  -8.868 3.21e-14 ***
    ## MY21-Feb           -5.5385     0.5729 99.0000  -9.668 5.80e-16 ***
    ## MY21-Mar           -4.8580     0.5729 99.0000  -8.480 2.22e-13 ***
    ## MY21-Apr           -2.9293     0.5729 99.0000  -5.113 1.55e-06 ***
    ## MY21-May           -3.2900     0.5729 99.0000  -5.743 1.02e-07 ***
    ## MY21-Jun           -2.5562     0.5729 99.0000  -4.462 2.15e-05 ***
    ## MY21-Jul           -2.8775     0.5729 99.0000  -5.023 2.26e-06 ***
    ## standsIB:MY20-Sep   2.1214     0.8102 99.0000   2.618 0.010220 *  
    ## standsNB:MY20-Sep   4.2555     0.8102 99.0000   5.253 8.61e-07 ***
    ## standsIB:MY20-Oct   1.5089     0.8102 99.0000   1.862 0.065506 .  
    ## standsNB:MY20-Oct   2.6417     0.8102 99.0000   3.261 0.001525 ** 
    ## standsIB:MY20-Nov   1.8501     0.8102 99.0000   2.284 0.024531 *  
    ## standsNB:MY20-Nov   2.4674     0.8102 99.0000   3.046 0.002977 ** 
    ## standsIB:MY20-Dec   1.2759     0.8102 99.0000   1.575 0.118474    
    ## standsNB:MY20-Dec   2.2796     0.8102 99.0000   2.814 0.005907 ** 
    ## standsIB:MY21-Jan   0.7122     0.8102 99.0000   0.879 0.381481    
    ## standsNB:MY21-Jan   2.0729     0.8102 99.0000   2.559 0.012025 *  
    ## standsIB:MY21-Feb   2.2832     0.8102 99.0000   2.818 0.005831 ** 
    ## standsNB:MY21-Feb   2.6437     0.8102 99.0000   3.263 0.001513 ** 
    ## standsIB:MY21-Mar   2.9634     0.8102 99.0000   3.658 0.000410 ***
    ## standsNB:MY21-Mar   4.1067     0.8102 99.0000   5.069 1.86e-06 ***
    ## standsIB:MY21-Apr   0.6659     0.8102 99.0000   0.822 0.413077    
    ## standsNB:MY21-Apr   3.1563     0.8102 99.0000   3.896 0.000178 ***
    ## standsIB:MY21-May   1.0185     0.8102 99.0000   1.257 0.211647    
    ## standsNB:MY21-May   3.5415     0.8102 99.0000   4.371 3.05e-05 ***
    ## standsIB:MY21-Jun   1.8950     0.8102 99.0000   2.339 0.021347 *  
    ## standsNB:MY21-Jun   3.1236     0.8102 99.0000   3.856 0.000205 ***
    ## standsIB:MY21-Jul   2.7106     0.8102 99.0000   3.346 0.001161 ** 
    ## standsNB:MY21-Jul   4.0168     0.8102 99.0000   4.958 2.95e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 36 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
anova(m_Rh)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##            Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## stands      1.783  0.8915     2     9  1.3583 0.3051394    
    ## MY        274.479 24.9527    11    99 38.0159 < 2.2e-16 ***
    ## stands:MY  40.379  1.8354    22    99  2.7963 0.0002756 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_Rh)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.802
    ##      Marginal R2: 0.694

``` r
# Ra
m_Ra <- lmerTest::lmer(Ra ~ stands*MY +(1|new_pl), cbc_mo_bc_SR)
summary(m_Ra)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Ra ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_bc_SR
    ## 
    ## REML criterion at convergence: 249.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.75732 -0.56800 -0.03469  0.52878  2.42746 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.2731   0.5226  
    ##  Residual             0.3028   0.5503  
    ## Number of obs: 144, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)         7.7122     0.3794 31.0894  20.325  < 2e-16 ***
    ## standsIB           -0.9155     0.5366 31.0894  -1.706 0.097975 .  
    ## standsNB           -1.2546     0.5366 31.0894  -2.338 0.025997 *  
    ## MY20-Sep           -1.0000     0.3891 99.0000  -2.570 0.011656 *  
    ## MY20-Oct           -1.4515     0.3891 99.0000  -3.731 0.000319 ***
    ## MY20-Nov           -3.1050     0.3891 99.0000  -7.980 2.65e-12 ***
    ## MY20-Dec           -3.6910     0.3891 99.0000  -9.486 1.45e-15 ***
    ## MY21-Jan           -4.2245     0.3891 99.0000 -10.857  < 2e-16 ***
    ## MY21-Feb           -4.5388     0.3891 99.0000 -11.665  < 2e-16 ***
    ## MY21-Mar           -4.0876     0.3891 99.0000 -10.506  < 2e-16 ***
    ## MY21-Apr           -2.9801     0.3891 99.0000  -7.659 1.28e-11 ***
    ## MY21-May           -2.0325     0.3891 99.0000  -5.224 9.73e-07 ***
    ## MY21-Jun           -1.5424     0.3891 99.0000  -3.964 0.000139 ***
    ## MY21-Jul           -1.7968     0.3891 99.0000  -4.618 1.17e-05 ***
    ## standsIB:MY20-Sep   1.2782     0.5503 99.0000   2.323 0.022226 *  
    ## standsNB:MY20-Sep   2.6764     0.5503 99.0000   4.864 4.34e-06 ***
    ## standsIB:MY20-Oct   0.8598     0.5503 99.0000   1.562 0.121363    
    ## standsNB:MY20-Oct   1.6164     0.5503 99.0000   2.938 0.004115 ** 
    ## standsIB:MY20-Nov   1.3141     0.5503 99.0000   2.388 0.018832 *  
    ## standsNB:MY20-Nov   1.8852     0.5503 99.0000   3.426 0.000893 ***
    ## standsIB:MY20-Dec   0.9340     0.5503 99.0000   1.697 0.092764 .  
    ## standsNB:MY20-Dec   1.7003     0.5503 99.0000   3.090 0.002598 ** 
    ## standsIB:MY21-Jan   0.4315     0.5503 99.0000   0.784 0.434814    
    ## standsNB:MY21-Jan   1.4114     0.5503 99.0000   2.565 0.011818 *  
    ## standsIB:MY21-Feb   1.5170     0.5503 99.0000   2.757 0.006950 ** 
    ## standsNB:MY21-Feb   1.8644     0.5503 99.0000   3.388 0.001011 ** 
    ## standsIB:MY21-Mar   1.9471     0.5503 99.0000   3.539 0.000614 ***
    ## standsNB:MY21-Mar   2.7941     0.5503 99.0000   5.078 1.80e-06 ***
    ## standsIB:MY21-Apr   0.5998     0.5503 99.0000   1.090 0.278362    
    ## standsNB:MY21-Apr   2.2698     0.5503 99.0000   4.125 7.73e-05 ***
    ## standsIB:MY21-May   0.4530     0.5503 99.0000   0.823 0.412301    
    ## standsNB:MY21-May   2.1823     0.5503 99.0000   3.966 0.000138 ***
    ## standsIB:MY21-Jun   1.0668     0.5503 99.0000   1.939 0.055382 .  
    ## standsNB:MY21-Jun   1.9061     0.5503 99.0000   3.464 0.000788 ***
    ## standsIB:MY21-Jul   1.6540     0.5503 99.0000   3.006 0.003357 ** 
    ## standsNB:MY21-Jul   2.5666     0.5503 99.0000   4.664 9.70e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 36 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
anova(m_Ra)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##            Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## stands      1.013  0.5066     2     9  1.6730  0.241112    
    ## MY        239.121 21.7383    11    99 71.7951 < 2.2e-16 ***
    ## stands:MY  16.554  0.7525    22    99  2.4852  0.001193 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_Ra)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.876
    ##      Marginal R2: 0.765

``` r
# D_L
# Dec 20 - Nov 21
cbc_mo_bc_DL <- 
  cbc_mo_bc[[1]] %>% 
  filter(!MY %in% c("20-Aug", "20-Sep", "20-Oct", "20-Nov", "21-Dec", "22-Jan"))
                     
m_DL <- lmerTest::lmer(D_L ~ stands*MY +(1|new_pl),cbc_mo_bc_DL)
summary(m_DL)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: D_L ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_bc_DL
    ## 
    ## REML criterion at convergence: 249.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.53374 -0.56266  0.01737  0.46864  2.64810 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.1240   0.3521  
    ##  Residual             0.3229   0.5682  
    ## Number of obs: 144, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)        5.58164    0.33424 58.47140  16.700  < 2e-16 ***
    ## standsIB          -0.45159    0.47268 58.47140  -0.955 0.343325    
    ## standsNB           0.43994    0.47268 58.47140   0.931 0.355820    
    ## MY21-Jan          -0.91022    0.40178 99.00001  -2.265 0.025662 *  
    ## MY21-Feb          -3.31586    0.40178 99.00001  -8.253 6.87e-13 ***
    ## MY21-Mar          -1.73029    0.40178 99.00001  -4.307 3.91e-05 ***
    ## MY21-Apr          -1.59923    0.40178 99.00001  -3.980 0.000131 ***
    ## MY21-May          -1.55541    0.40178 99.00001  -3.871 0.000194 ***
    ## MY21-Jun          -1.61381    0.40178 99.00001  -4.017 0.000115 ***
    ## MY21-Jul          -1.66513    0.40178 99.00001  -4.144 7.19e-05 ***
    ## MY21-Aug          -1.61506    0.40178 99.00001  -4.020 0.000114 ***
    ## MY21-Sep          -0.26100    0.40178 99.00001  -0.650 0.517448    
    ## MY21-Oct           0.21586    0.40178 99.00001   0.537 0.592293    
    ## MY21-Nov           0.92858    0.40178 99.00001   2.311 0.022899 *  
    ## standsIB:MY21-Jan -0.37330    0.56821 99.00001  -0.657 0.512716    
    ## standsNB:MY21-Jan -0.93228    0.56821 99.00001  -1.641 0.104025    
    ## standsIB:MY21-Feb  0.14916    0.56821 99.00001   0.263 0.793468    
    ## standsNB:MY21-Feb  0.23912    0.56821 99.00001   0.421 0.674789    
    ## standsIB:MY21-Mar  0.01620    0.56821 99.00001   0.029 0.977310    
    ## standsNB:MY21-Mar -0.73427    0.56821 99.00001  -1.292 0.199273    
    ## standsIB:MY21-Apr  0.30589    0.56821 99.00001   0.538 0.591552    
    ## standsNB:MY21-Apr -0.36926    0.56821 99.00001  -0.650 0.517280    
    ## standsIB:MY21-May  0.93732    0.56821 99.00001   1.650 0.102192    
    ## standsNB:MY21-May -0.00854    0.56821 99.00001  -0.015 0.988039    
    ## standsIB:MY21-Jun  1.08350    0.56821 99.00001   1.907 0.059435 .  
    ## standsNB:MY21-Jun -0.20523    0.56821 99.00001  -0.361 0.718728    
    ## standsIB:MY21-Jul  1.08808    0.56821 99.00001   1.915 0.058388 .  
    ## standsNB:MY21-Jul -0.24006    0.56821 99.00001  -0.422 0.673587    
    ## standsIB:MY21-Aug  0.54220    0.56821 99.00001   0.954 0.342295    
    ## standsNB:MY21-Aug  0.27952    0.56821 99.00001   0.492 0.623855    
    ## standsIB:MY21-Sep  0.05255    0.56821 99.00001   0.092 0.926501    
    ## standsNB:MY21-Sep -0.30119    0.56821 99.00001  -0.530 0.597249    
    ## standsIB:MY21-Oct  0.58400    0.56821 99.00001   1.028 0.306550    
    ## standsNB:MY21-Oct  0.16358    0.56821 99.00001   0.288 0.774033    
    ## standsIB:MY21-Nov  0.54586    0.56821 99.00001   0.961 0.339061    
    ## standsNB:MY21-Nov -0.28973    0.56821 99.00001  -0.510 0.611257    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 36 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
anova(m_DL)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##            Sum Sq Mean Sq NumDF DenDF F value Pr(>F)    
    ## stands      0.394   0.197     2     9  0.6100 0.5643    
    ## MY        171.330  15.575    11    99 48.2424 <2e-16 ***
    ## stands:MY   8.205   0.373    22    99  1.1552 0.3057    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_DL)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.812
    ##      Marginal R2: 0.740

``` r
# D_FR
# Oct 20 - Sep 21
cbc_mo_bc_DFR <- 
  cbc_mo_bc[[1]] %>% filter(!MY %in% c("20-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan"))

m_DFR <- lmerTest::lmer(D_FR ~ stands*MY +(1|new_pl),cbc_mo_bc_DFR)
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(m_DFR)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: D_FR ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_bc_DFR
    ## 
    ## REML criterion at convergence: 468.9
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.95186 -0.50658 -0.02227  0.59691  2.55921 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept) 0.000    0.000   
    ##  Residual             4.596    2.144   
    ## Number of obs: 132, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)         3.9602     1.0719 97.0000   3.694 0.000365 ***
    ## standsIB           -0.1091     1.5160 97.0000  -0.072 0.942776    
    ## standsNB            1.3400     1.5160 97.0000   0.884 0.378932    
    ## MY20-Nov           -1.8685     1.5160 97.0000  -1.233 0.220715    
    ## MY20-Dec            0.1752     1.5160 97.0000   0.116 0.908230    
    ## MY21-Jan            0.3364     1.5160 97.0000   0.222 0.824845    
    ## MY21-Feb            0.1914     1.5160 97.0000   0.126 0.899787    
    ## MY21-Mar            0.3622     1.5160 97.0000   0.239 0.811683    
    ## MY21-Apr           -0.6272     1.5160 97.0000  -0.414 0.679977    
    ## MY21-May            0.9544     1.5160 97.0000   0.630 0.530469    
    ## MY21-Jun            0.6573     1.5160 97.0000   0.434 0.665548    
    ## MY21-Jul           -0.6869     1.5160 97.0000  -0.453 0.651484    
    ## MY21-Aug            1.0556     1.5160 97.0000   0.696 0.487892    
    ## MY21-Sep            0.6226     1.5160 97.0000   0.411 0.682202    
    ## standsIB:MY20-Nov   2.0412     2.1439 97.0000   0.952 0.343424    
    ## standsNB:MY20-Nov   2.3974     2.1439 97.0000   1.118 0.266225    
    ## standsIB:MY20-Dec   0.6189     2.1439 97.0000   0.289 0.773442    
    ## standsNB:MY20-Dec  -0.6225     2.1439 97.0000  -0.290 0.772167    
    ## standsIB:MY21-Jan  -1.8900     2.1439 97.0000  -0.882 0.380189    
    ## standsNB:MY21-Jan  -2.8617     2.1439 97.0000  -1.335 0.185056    
    ## standsIB:MY21-Feb  -2.6026     2.1439 97.0000  -1.214 0.227702    
    ## standsNB:MY21-Feb  -1.8803     2.1439 97.0000  -0.877 0.382615    
    ## standsIB:MY21-Mar  -0.5906     2.1439 97.0000  -0.275 0.783529    
    ## standsNB:MY21-Mar  -4.2343     2.1439 97.0000  -1.975 0.051106 .  
    ## standsIB:MY21-Apr   1.6222     2.1439 97.0000   0.757 0.451077    
    ## standsNB:MY21-Apr   0.4760     2.1439 97.0000   0.222 0.824754    
    ## standsIB:MY21-May  -1.7948     2.2314 97.0000  -0.804 0.423163    
    ## standsIB:MY21-Jun  -0.2078     2.2314 97.0000  -0.093 0.925995    
    ## standsNB:MY21-Jun  -1.4234     2.1439 97.0000  -0.664 0.508315    
    ## standsIB:MY21-Jul   3.7396     2.2314 97.0000   1.676 0.096983 .  
    ## standsNB:MY21-Jul   2.2792     2.8361 97.0000   0.804 0.423568    
    ## standsIB:MY21-Aug  -0.2869     2.2314 97.0000  -0.129 0.897968    
    ## standsNB:MY21-Aug  -2.0768     2.2314 97.0000  -0.931 0.354314    
    ## standsIB:MY21-Sep  -1.9069     2.1439 97.0000  -0.889 0.375961    
    ## standsNB:MY21-Sep  -1.0479     2.1439 97.0000  -0.489 0.626104    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 35 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
anova(m_DFR)
```

    ## Missing cells for: standsNB:MY21-May.  
    ## Interpret type III hypotheses with care.

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##            Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## stands      8.277  4.1384     2    97  0.9004 0.4098
    ## MY         58.988  5.3625    11    97  1.1667 0.3202
    ## stands:MY 123.537  5.8827    21    97  1.2799 0.2080

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
    ##      Marginal R2: 0.235

``` r
# P_FR
# Oct 20 - Sep 21

cbc_mo_PFR <- cbc_mo %>% filter(!MY %in% c("20-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan"))

m_PFR <- lmerTest::lmer(P_FR ~ stands*MY +(1|new_pl), cbc_mo_PFR)
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
    ## boundary (singular) fit: see help('isSingular')

``` r
summary(m_PFR)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: P_FR ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_PFR
    ## 
    ## REML criterion at convergence: 958.7
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.35658 -0.59407  0.04242  0.52670  2.64429 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  new_pl   (Intercept)   0.0     0.00   
    ##  Residual             716.3    26.76   
    ## Number of obs: 132, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error      df t value Pr(>|t|)   
    ## (Intercept)         35.055     13.382  97.000   2.620  0.01022 * 
    ## standsIB             2.580     18.925  97.000   0.136  0.89184   
    ## standsNB            17.450     18.925  97.000   0.922  0.35877   
    ## MY20-Nov           -26.682     18.925  97.000  -1.410  0.16176   
    ## MY20-Dec             3.043     18.925  97.000   0.161  0.87261   
    ## MY21-Jan           -42.805     18.925  97.000  -2.262  0.02594 * 
    ## MY21-Feb           -13.925     18.925  97.000  -0.736  0.46362   
    ## MY21-Mar           -11.017     18.925  97.000  -0.582  0.56180   
    ## MY21-Apr           -27.852     18.925  97.000  -1.472  0.14432   
    ## MY21-May             6.985     18.925  97.000   0.369  0.71286   
    ## MY21-Jun           -19.912     18.925  97.000  -1.052  0.29532   
    ## MY21-Jul           -20.755     18.925  97.000  -1.097  0.27548   
    ## MY21-Aug           -54.932     18.925  97.000  -2.903  0.00458 **
    ## MY21-Sep             1.063     18.925  97.000   0.056  0.95534   
    ## standsIB:MY20-Nov   10.285     26.763  97.000   0.384  0.70160   
    ## standsNB:MY20-Nov    3.117     26.763  97.000   0.116  0.90751   
    ## standsIB:MY20-Dec  -30.780     26.763  97.000  -1.150  0.25294   
    ## standsNB:MY20-Dec  -59.815     26.763  97.000  -2.235  0.02771 * 
    ## standsIB:MY21-Jan    8.310     26.763  97.000   0.310  0.75685   
    ## standsNB:MY21-Jan   25.032     26.763  97.000   0.935  0.35194   
    ## standsIB:MY21-Feb  -17.185     26.763  97.000  -0.642  0.52232   
    ## standsNB:MY21-Feb  -30.365     26.763  97.000  -1.135  0.25935   
    ## standsIB:MY21-Mar   -5.508     26.763  97.000  -0.206  0.83739   
    ## standsNB:MY21-Mar  -16.033     26.763  97.000  -0.599  0.55054   
    ## standsIB:MY21-Apr   16.115     26.763  97.000   0.602  0.54849   
    ## standsNB:MY21-Apr   -2.215     26.763  97.000  -0.083  0.93421   
    ## standsIB:MY21-May  -46.193     27.856  97.000  -1.658  0.10049   
    ## standsIB:MY21-Jun   40.597     27.856  97.000   1.457  0.14824   
    ## standsNB:MY21-Jun   25.277     26.763  97.000   0.944  0.34727   
    ## standsIB:MY21-Jul    4.180     27.856  97.000   0.150  0.88103   
    ## standsNB:MY21-Jul  -57.320     35.405  97.000  -1.619  0.10869   
    ## standsIB:MY21-Aug   34.017     27.856  97.000   1.221  0.22498   
    ## standsNB:MY21-Aug   11.624     27.856  97.000   0.417  0.67739   
    ## standsIB:MY21-Sep  -19.875     26.763  97.000  -0.743  0.45951   
    ## standsNB:MY21-Sep  -67.975     26.763  97.000  -2.540  0.01268 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 35 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
anova(m_PFR)
```

    ## Missing cells for: standsNB:MY21-May.  
    ## Interpret type III hypotheses with care.

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##           Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)   
    ## stands       120   60.02     2    97  0.0838 0.919683   
    ## MY         19488 1771.60    11    97  2.4733 0.008935 **
    ## stands:MY  27930 1329.99    21    97  1.8568 0.022802 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
    ##      Marginal R2: 0.334

``` r
# vwc
# Sep 20 - Aug 21?
cbc_mo_sqrt_vwc <- 
  cbc_mo_sqrt %>% 
  filter(!MY %in% c("21-Sep", "21-Oct", "21-Nov", "21-Dec", "22-Jan"))

m_vwc <- lmerTest::lmer(vwc ~ stands*MY +(1|new_pl), cbc_mo_sqrt_vwc)
```

    ## fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
summary(m_vwc)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: vwc ~ stands * MY + (1 | new_pl)
    ##    Data: cbc_mo_sqrt_vwc
    ## 
    ## REML criterion at convergence: -217.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3535 -0.4379  0.0000  0.5078  2.8129 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  new_pl   (Intercept) 0.0005493 0.02344 
    ##  Residual             0.0029598 0.05440 
    ## Number of obs: 123, groups:  new_pl, 12
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)        0.13676    0.03376 81.36620   4.051 0.000116 ***
    ## standsIB          -0.06395    0.04491 77.66179  -1.424 0.158499    
    ## standsNB           0.03841    0.04491 77.66179   0.855 0.395078    
    ## MY20-Oct           0.02072    0.04174 81.10318   0.496 0.620941    
    ## MY20-Nov           0.04960    0.04174 81.10318   1.188 0.238217    
    ## MY20-Dec           0.19257    0.04174 81.10318   4.613 1.46e-05 ***
    ## MY21-Jan           0.16733    0.04174 81.10318   4.008 0.000135 ***
    ## MY21-Feb           0.13251    0.04174 81.10318   3.174 0.002123 ** 
    ## MY21-Mar           0.14401    0.04174 81.10318   3.450 0.000893 ***
    ## MY21-Apr           0.03571    0.04174 81.10318   0.855 0.394883    
    ## MY21-May           0.16991    0.04174 81.10318   4.070 0.000108 ***
    ## MY21-Jun           0.19821    0.04174 81.10318   4.748 8.72e-06 ***
    ## MY21-Jul           0.19901    0.04174 81.10318   4.767 8.09e-06 ***
    ## MY21-Aug          -0.01005    0.06377 82.74171  -0.158 0.875190    
    ## standsIB:MY20-Oct  0.05855    0.05677 80.71535   1.031 0.305447    
    ## standsNB:MY20-Oct -0.13956    0.05906 81.23679  -2.363 0.020511 *  
    ## standsIB:MY20-Nov  0.01070    0.05677 80.71535   0.189 0.850954    
    ## standsNB:MY20-Nov -0.12378    0.05677 80.71535  -2.180 0.032136 *  
    ## standsIB:MY20-Dec -0.02685    0.05677 80.71535  -0.473 0.637449    
    ## standsNB:MY20-Dec -0.18304    0.05677 80.71535  -3.224 0.001823 ** 
    ## standsIB:MY21-Jan  0.06966    0.05677 80.71535   1.227 0.223359    
    ## standsNB:MY21-Jan -0.07383    0.05677 80.71535  -1.301 0.197116    
    ## standsIB:MY21-Feb -0.00621    0.05677 80.71535  -0.109 0.913165    
    ## standsNB:MY21-Feb -0.06523    0.05677 80.71535  -1.149 0.253914    
    ## standsIB:MY21-Mar  0.03118    0.05677 80.71535   0.549 0.584318    
    ## standsNB:MY21-Mar -0.06210    0.05677 80.71535  -1.094 0.277243    
    ## standsIB:MY21-Apr  0.06921    0.05677 80.71535   1.219 0.226339    
    ## standsNB:MY21-Apr  0.06714    0.07482 83.16524   0.897 0.372115    
    ## standsIB:MY21-May  0.04324    0.05904 81.10318   0.732 0.466019    
    ## standsIB:MY21-Jun  0.07255    0.05677 80.71535   1.278 0.204880    
    ## standsNB:MY21-Jun -0.08982    0.05677 80.71535  -1.582 0.117493    
    ## standsIB:MY21-Jul  0.02284    0.05677 80.71535   0.402 0.688487    
    ## standsNB:MY21-Jul -0.15562    0.05677 80.71535  -2.741 0.007532 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 33 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
anova(m_vwc)
```

    ## Missing cells for: standsNB:MY21-May, standsIB:MY21-Aug, standsNB:MY21-Aug.  
    ## Interpret type III hypotheses with care.

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##            Sum Sq  Mean Sq NumDF  DenDF F value  Pr(>F)    
    ## stands    0.01396 0.006979     2  8.717  2.3579 0.15188    
    ## MY        0.57382 0.052166    11 81.021 17.6245 < 2e-16 ***
    ## stands:MY 0.10850 0.005711    19 80.623  1.9293 0.02255 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
r2_nakagawa(m_vwc)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.694
    ##      Marginal R2: 0.637

``` r
# Ts
# Sep 20 - Aug 21?
cbc_mo_Ts <- 
  cbc_mo %>% 
  filter(!MY %in% c("20-Jul","20-Aug","21-Sep", "21-Oct", 
                   "21-Nov", "21-Dec", "22-Jan"))

m_Ts <- lmerTest::lmer(Ts ~ stands*MY +(1|new_pl), cbc_mo_Ts)
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
    ##            Df Sum Sq Mean Sq F value  Pr(>F)  
    ## new_pl     11  55.76  5.0688  1.6696 0.08705 .
    ## Residuals 132 400.73  3.0358                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
    ##            Df  Sum Sq Mean Sq F value   Pr(>F)   
    ## new_pl     11  7.7355 0.70323  2.9741 0.001475 **
    ## Residuals 132 31.2113 0.23645                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
    ##            Df Sum Sq  Mean Sq F value  Pr(>F)  
    ## new_pl     11 1.7266 0.156961  2.3202 0.01228 *
    ## Residuals 132 8.9297 0.067649                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
    ##            Df  Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  1.5796 0.14360  1.0792 0.3828
    ## Residuals 132 17.5643 0.13306

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
    ## new_pl     11  459.9  41.809  1.9869 0.03534 *
    ## Residuals 120 2525.0  21.042                  
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
    ##            Df   Sum Sq Mean Sq F value Pr(>F)
    ## new_pl     11  9064244  824022   1.068 0.3928
    ## Residuals 120 92590059  771584

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
    ##            Df     Sum Sq   Mean Sq F value  Pr(>F)  
    ## new_pl     11 0.00023815 2.165e-05  1.9991 0.03489 *
    ## Residuals 111 0.00120212 1.083e-05                  
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
lsmeans(m_vwc, pairwise ~ stands | MY, adjust = "tukey")  
```

    ## $lsmeans
    ## MY = 20-Sep:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.1368 0.0338 82.0   0.0695    0.204
    ##  IB     0.0728 0.0296 73.6   0.0138    0.132
    ##  NB     0.1752 0.0296 73.6   0.1161    0.234
    ## 
    ## MY = 20-Oct:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.1575 0.0296 73.6   0.0985    0.217
    ##  IB     0.1521 0.0296 73.6   0.0931    0.211
    ##  NB     0.0563 0.0339 81.7  -0.0111    0.124
    ## 
    ## MY = 20-Nov:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.1864 0.0296 73.6   0.1273    0.245
    ##  IB     0.1331 0.0296 73.6   0.0741    0.192
    ##  NB     0.1010 0.0296 73.6   0.0420    0.160
    ## 
    ## MY = 20-Dec:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.3293 0.0296 73.6   0.2703    0.388
    ##  IB     0.2385 0.0296 73.6   0.1795    0.298
    ##  NB     0.1847 0.0296 73.6   0.1257    0.244
    ## 
    ## MY = 21-Jan:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.3041 0.0296 73.6   0.2451    0.363
    ##  IB     0.3098 0.0296 73.6   0.2508    0.369
    ##  NB     0.2687 0.0296 73.6   0.2096    0.328
    ## 
    ## MY = 21-Feb:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.2693 0.0296 73.6   0.2102    0.328
    ##  IB     0.1991 0.0296 73.6   0.1401    0.258
    ##  NB     0.2424 0.0296 73.6   0.1834    0.301
    ## 
    ## MY = 21-Mar:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.2808 0.0296 73.6   0.2217    0.340
    ##  IB     0.2480 0.0296 73.6   0.1890    0.307
    ##  NB     0.2571 0.0296 73.6   0.1981    0.316
    ## 
    ## MY = 21-Apr:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.1725 0.0296 73.6   0.1134    0.231
    ##  IB     0.1777 0.0296 73.6   0.1187    0.237
    ##  NB     0.2780 0.0574 90.0   0.1640    0.392
    ## 
    ## MY = 21-May:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.3067 0.0296 73.6   0.2477    0.366
    ##  IB     0.2860 0.0338 82.0   0.2187    0.353
    ##  NB     nonEst     NA   NA       NA       NA
    ## 
    ## MY = 21-Jun:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.3350 0.0296 73.6   0.2760    0.394
    ##  IB     0.3436 0.0296 73.6   0.2846    0.403
    ##  NB     0.2836 0.0296 73.6   0.2245    0.343
    ## 
    ## MY = 21-Jul:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.3358 0.0296 73.6   0.2768    0.395
    ##  IB     0.2947 0.0296 73.6   0.2356    0.354
    ##  NB     0.2186 0.0296 73.6   0.1595    0.278
    ## 
    ## MY = 21-Aug:
    ##  stands lsmean     SE   df lower.CL upper.CL
    ##  AB     0.1267 0.0572 89.8   0.0131    0.240
    ##  IB     nonEst     NA   NA       NA       NA
    ##  NB     nonEst     NA   NA       NA       NA
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## MY = 20-Sep:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.06395 0.0450 78.6   1.422  0.3344
    ##  AB - NB  -0.03841 0.0450 78.6  -0.854  0.6705
    ##  IB - NB  -0.10236 0.0419 73.6  -2.444  0.0442
    ## 
    ## MY = 20-Oct:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.00540 0.0419 73.6   0.129  0.9909
    ##  AB - NB   0.10115 0.0450 78.4   2.248  0.0695
    ##  IB - NB   0.09575 0.0450 78.4   2.128  0.0908
    ## 
    ## MY = 20-Nov:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.05325 0.0419 73.6   1.271  0.4158
    ##  AB - NB   0.08537 0.0419 73.6   2.038  0.1103
    ##  IB - NB   0.03212 0.0419 73.6   0.767  0.7244
    ## 
    ## MY = 20-Dec:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.09080 0.0419 73.6   2.168  0.0836
    ##  AB - NB   0.14463 0.0419 73.6   3.453  0.0026
    ##  IB - NB   0.05383 0.0419 73.6   1.285  0.4081
    ## 
    ## MY = 21-Jan:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB  -0.00571 0.0419 73.6  -0.136  0.9898
    ##  AB - NB   0.03542 0.0419 73.6   0.846  0.6760
    ##  IB - NB   0.04113 0.0419 73.6   0.982  0.5906
    ## 
    ## MY = 21-Feb:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.07016 0.0419 73.6   1.675  0.2216
    ##  AB - NB   0.02682 0.0419 73.6   0.640  0.7984
    ##  IB - NB  -0.04334 0.0419 73.6  -1.035  0.5575
    ## 
    ## MY = 21-Mar:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.03277 0.0419 73.6   0.782  0.7150
    ##  AB - NB   0.02369 0.0419 73.6   0.566  0.8388
    ##  IB - NB  -0.00908 0.0419 73.6  -0.217  0.9744
    ## 
    ## MY = 21-Apr:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB  -0.00526 0.0419 73.6  -0.126  0.9914
    ##  AB - NB  -0.10555 0.0646 89.4  -1.634  0.2368
    ##  IB - NB  -0.10029 0.0646 89.4  -1.553  0.2716
    ## 
    ## MY = 21-May:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.02071 0.0450 78.6   0.461  0.6463
    ##  AB - NB    nonEst     NA   NA      NA      NA
    ##  IB - NB    nonEst     NA   NA      NA      NA
    ## 
    ## MY = 21-Jun:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB  -0.00860 0.0419 73.6  -0.205  0.9770
    ##  AB - NB   0.05141 0.0419 73.6   1.227  0.4410
    ##  IB - NB   0.06002 0.0419 73.6   1.433  0.3295
    ## 
    ## MY = 21-Jul:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB   0.04111 0.0419 73.6   0.981  0.5909
    ##  AB - NB   0.11721 0.0419 73.6   2.798  0.0178
    ##  IB - NB   0.07610 0.0419 73.6   1.817  0.1712
    ## 
    ## MY = 21-Aug:
    ##  contrast estimate     SE   df t.ratio p.value
    ##  AB - IB    nonEst     NA   NA      NA      NA
    ##  AB - NB    nonEst     NA   NA      NA      NA
    ##  IB - NB    nonEst     NA   NA      NA      NA
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for varying family sizes
