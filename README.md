<!-- README.md is generated from README.Rmd. Please edit that file -->

# `laggedcor`: R packages to calculate the lagged correlation between two time-series data. The time-series data can be wearable or omics data.  <img src="man/figures/logo.png" align="right" alt="" width="100" />

[![](https://www.r-pkg.org/badges/version/laggedcor?color=green)](https://cran.r-project.org/package=laggedcor)
[![](https://img.shields.io/github/languages/code-size/laggedcor/laggedcor.svg)](https://github.com/laggedcor/laggedcor)
[![Dependencies](https://tinyverse.netlify.com/badge/laggedcor)](https://cran.r-project.org/package=laggedcor)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<br>

<h2 style="color:red;"><i class="fas fa-info-circle"> About</h2>

---

The `laggedcor` is a collection of R packages designed for MS-based untargeted metabolomics data processing. All packages share an underlying design philosophy, grammar, and data structures.

`laggedcor` is a comprehensive computational framework for MS-based untargeted metabolomics data processing and analysis, including raw data processing (peak detecting), data cleaning (missing value processing, data normalization, and integration), statistical analysis, metabolite annotation, and biological function mining (pathway enrichment, feature-based metabolic module analysis).

<img src="man/figures/laggedcor_all_packages.png" align="middle" alt="" width = "100%"/>

<h2 style="color:red;"><i class="fas fa-cloud-download-alt"> Installation</h2>

---

You can install `laggedcor` from
[GitHub](https://github.com/jaspershen/laggedcor).

``` r
if(!require(devtools)){
install.packages("devtools")
}
devtools::install_github("jaspershen/laggedcor")
```

Then you can use `laggedcor_install()` to install all the packages in
`laggedcor`.

``` r
library(laggedcor)
```

<h2 style="color:red;"><i class="fas fa-code-branch"> Packages</h2>

---

Now, `laggedcor` contains 8 packages, which are listed below:

<h3 style="color:#e474ac;"><i class="fas fa-code"> massdataset</h3> <a href="https://laggedcor.github.io/massdataset/" target="_blank"><img src="man/figures/massdataset_logo.png" align="left" alt="" width="120" /></a>

<br>

`massdataset` is used organize metabolomics experiment data into a `mass_dataset` class object, that can be processed by all the `laggedcor` packages.

<br>

<h3 style="color:#f4ac64;"><i class="fas fa-code"> massprocesser</h3> <a href="https://laggedcor.github.io/massprocesser/" target="_blank"><img src="man/figures/massprocesser_logo.png" align="left" alt="" width="120" /></a>

<br>

`massprocesser` is a R package which is used for mass spectrometry based untargeted metabolomics raw data processing.

<br>

<h3 style="color:#2098b8;"><i class="fas fa-code"> masscleaner</h3> <a href="https://laggedcor.github.io/masscleaner/" target="_blank"><img src="man/figures/masscleaner_logo.png" align="left" alt="" width="120" /></a>

<br>

`masscleaner` is a R package which is used for metabolomics data cleaning.

<br>
<br>

<h3 style="color:#abcc2c;"><i class="fas fa-code"> massqc</h3> <a href="https://laggedcor.github.io/massqc/" target="_blank"><img src="man/figures/massqc_logo.png" align="left" alt="" width="120" /></a>

<br>

`massqc` is used for data quality assessment and control.

<br>
<br>

<h3 style="color:#2ca4e4;"><i class="fas fa-code"> metid</h3> <a href="https://laggedcor.github.io/metid/" target="_blank"><img src="man/figures/metid_logo2.png" align="left" alt="" width="120" /></a>

<br>

`metid` is used for metabolite database construction and metabolite annotation.

<br>
<br>

<h3 style="color:#dce45c;"><i class="fas fa-code"> massstat</h3> <a href="https://laggedcor.github.io/massstat/" target="_blank"><img src="man/figures/massstat_logo.png" align="left" alt="" width="120" /></a>

<br>

`massstat` is used for statistical analysis.

<br>
<br>

<h3 style="color:#ccac9c;"><i class="fas fa-code"> metpath</h3> <a href="https://laggedcor.github.io/metpath/" target="_blank"><img src="man/figures/metpath_logo.png" align="left" alt="" width="120" /></a>


<br>

`metpath` is used for pathway enrichment analysis.

<br>
<br>

<h3 style="color:#ec1c04;"><i class="fas fa-code"> tinytools</h3> <a href="https://laggedcor.github.io/tinytools/" target="_blank"><img src="man/figures/tinytools_logo2.png" align="left" alt="" width="120" /></a>

<br>

`tinytools` is a collection of useful tiny tools for mass spectrometry data processing and analysis.

<br>

<h2 style="color:red;"><i class="fas fa-question-circle"> Need help?</h2>

---

If you have any questions about `laggedcor`, please don’t hesitate to
email me (<shenxt@stanford.edu>) or reach out me via the social medias below.

<i class="fa fa-weixin"></i>  [shenxt1990](https://www.shenxt.info/files/wechat_QR.jpg)

<i class="fa fa-envelope"></i>  <shenxt@stanford.edu>

<i class="fa fa-twitter"></i>  [Twitter](https://twitter.com/xiaotaoshen1990)

<i class="fa fa-map-marker-alt"></i>  [M339, Alway Buidling, Cooper Lane,
Palo Alto, CA
94304](https://www.google.com/maps/place/Alway+Building/@37.4322345,-122.1770883,17z/data=!3m1!4b1!4m5!3m4!1s0x808fa4d335c3be37:0x9057931f3b312c29!8m2!3d37.4322345!4d-122.1748996)

<h2 style="color:red;"><i class="fas fa-location-arrow"> Citation</h2>

---

If you use laggedcor in you publications, please cite this publication:

X. Shen, R. Wang, X. Xiong, Y. Yin, Y. Cai, Z. Ma, N. Liu, and Z.-J.
Zhu\* (Corresponding Author), Metabolic Reaction Network-based Recursive
Metabolite Annotation for Untargeted Metabolomics, Nature
Communications, 2019, 10: 1516.  
[Web Link](https://www.nature.com/articles/s41467-019-09550-x).

Thank you very much!
