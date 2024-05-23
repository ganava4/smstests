
# smstests

The goal of smstests is to produce simple multivariate statistical tests
for means and variances / covariances for one single factor with two or
more levels, including multiple t- and Levene tests, Hotelling’s T^2
test, extended Levene tests, one-way MANOVA, van Valen’s test and Box’s
M test.

## Installation

You can install the development version of smstests from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") # In case devtools is not installed
devtools::install_github("ganava4/smstests")
```

    ## Downloading GitHub repo ganava4/smstests@HEAD

    ## 
    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##          checking for file 'C:\Users\ganav\AppData\Local\Temp\RtmpcVInba\remotes425476186df\ganava4-smstests-ebc4b86/DESCRIPTION' ...  ✔  checking for file 'C:\Users\ganav\AppData\Local\Temp\RtmpcVInba\remotes425476186df\ganava4-smstests-ebc4b86/DESCRIPTION'
    ##       ─  preparing 'smstests':
    ##    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
    ##       ─  checking for LF line-endings in source and make files and shell scripts
    ## ─  checking for empty or unneeded directories
    ##       ─  building 'smstests_0.0.0.9000.tar.gz'
    ##      
    ## 

    ## Installing package into 'C:/Users/ganav/AppData/Local/Temp/RtmpOaY0LY/temp_libpath2fa033af2f38'
    ## (as 'lib' is unspecified)

## Example

``` r
library(smstests)
## basic example code
data(skulls)
# Box's M test using an F-approximation
BoxM.skulls <- BoxM.F(skulls, Period)
summary.BoxM.F(BoxM.skulls, long = TRUE)
```

     Box's M-test for Homogeneity of Covariance Matrices (F approximation)

     Data: skulls 
     Variables: Maximum_breadth Basibregmatic_height Basialveolar_length Nasal_height 
     Factor: Period 
     Levels: Early predynastic Late predynastic 12th and 13th Dynasty Ptolemaic period Roman period 

    Covariance matrix for each group
    Early predynastic 
                         Maximum_breadth Basibregmatic_height Basialveolar_length
    Maximum_breadth            26.309195            4.1517241           0.4540230
    Basibregmatic_height        4.151724           19.9724138          -0.7931034
    Basialveolar_length         0.454023           -0.7931034          34.6264368
    Nasal_height                7.245977            0.3931034          -1.9195402
                         Nasal_height
    Maximum_breadth         7.2459770
    Basibregmatic_height    0.3931034
    Basialveolar_length    -1.9195402
    Nasal_height            7.6367816

    Late predynastic 
                         Maximum_breadth Basibregmatic_height Basialveolar_length
    Maximum_breadth            23.136782             1.010345           4.7678161
    Basibregmatic_height        1.010345            21.596552           3.3655172
    Basialveolar_length         4.767816             3.365517          18.8919540
    Nasal_height                1.842529             5.624138           0.1908046
                         Nasal_height
    Maximum_breadth         1.8425287
    Basibregmatic_height    5.6241379
    Basialveolar_length     0.1908046
    Nasal_height            8.7367816

    12th and 13th Dynasty 
                         Maximum_breadth Basibregmatic_height Basialveolar_length
    Maximum_breadth           12.1195402           0.78620690          -0.7747126
    Basibregmatic_height       0.7862069          24.78620690           3.5931034
    Basialveolar_length       -0.7747126           3.59310345          20.7229885
    Nasal_height               0.8988506          -0.08965517           1.6701149
                         Nasal_height
    Maximum_breadth        0.89885057
    Basibregmatic_height  -0.08965517
    Basialveolar_length    1.67011494
    Nasal_height          12.59885057

    Ptolemaic period 
                         Maximum_breadth Basibregmatic_height Basialveolar_length
    Maximum_breadth            15.362069            -5.534483           -2.172414
    Basibregmatic_height       -5.534483            26.355172            8.110345
    Basialveolar_length        -2.172414             8.110345           21.085057
    Nasal_height                2.051724             6.148276            5.328736
                         Nasal_height
    Maximum_breadth          2.051724
    Basibregmatic_height     6.148276
    Basialveolar_length      5.328736
    Nasal_height             7.964368

    Roman period 
                         Maximum_breadth Basibregmatic_height Basialveolar_length
    Maximum_breadth           28.6264368           -0.2298851          -1.8793103
    Basibregmatic_height      -0.2298851           24.7126437          11.7241379
    Basialveolar_length       -1.8793103           11.7241379          25.5689655
    Nasal_height              -1.9942529            2.1494253           0.3965517
                         Nasal_height
    Maximum_breadth        -1.9942529
    Basibregmatic_height    2.1494253
    Basialveolar_length     0.3965517
    Nasal_height           13.8264368

    Pooled Covariance Matrix
                         Maximum_breadth Basibregmatic_height Basialveolar_length
    Maximum_breadth          21.11080460           0.03678161          0.07908046
    Basibregmatic_height      0.03678161          23.48459770          5.20000000
    Basialveolar_length       0.07908046           5.20000000         24.17908046
    Nasal_height              2.00896552           2.84505747          1.13333333
                         Nasal_height
    Maximum_breadth          2.008966
    Basibregmatic_height     2.845057
    Basialveolar_length      1.133333
    Nasal_height            10.152644

     Box's M = 2.8725e-11 
     F statistic = 1.1406 , Num df = 40.0 , Den df = 46378.7 , p-value = 0.2498
