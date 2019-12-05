[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

# Supporting Material for Gilligan & Vandenbergh 2019.

Calculations to support J.M. Gilligan and M.P. Vandenbergh, 
"A Framework for Assessing the Impact of Private Governance"
(a working paper).

The calculations are documented in `renewable_generation.pdf` and 
`residential_consumption.pdf`.

The data (downloaded from the U.S. Environmental Protection Agency, the U.S.
Energy Information Administration, and the US Census Bureau), can be found in the 
`data` directory. The code for replicating the calculations is embedded in the 
files `residential_consumption.Rmd` and `renewable_generation.Rmd`. Loading
these files in [R](https://r-project.org) or [RStudio](https://rstudio.com)
and rendering them using the [`rmarkdown`](https://rmarkdown.rstudio.com/) 
package will reproduce the `.pdf` output.

When you rebuild the files, you should have the 
[`pacman`](https://github.com/trinker/pacman) R package installed.
This package will take care of automatically installing any other missing 
R pacakges when the documents are compiled.
