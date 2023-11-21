# Quick setup R

### 1. Make sure you have the latest R downloaded
- Type `R.version$version.string` in R it should be **4.3.2** (aka 'Eye Holes' with `R.version$nickname`)
- If not download the latest version for [Windows](https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe) or [Mac](https://cran.r-project.org/bin/macosx/)

### 2. Make sure you have the latest Rstudio downloaded
- You can check this by clicking on the menu `About Rstudio` in the Rstudio or Help interface dropdown.
- If not download the latest version of [Rstudio](https://posit.co/download/rstudio-desktop/)

### 3. Make sure you have the packages installed

- You can do this by running the following code 

```if (!require(pacman)) {  install.packages("pacman")}
pacman::p_load(patchwork, tidyverse, lavaan, ggpp, plyr,               ggrain) #for rainclouds```




