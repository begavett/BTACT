# BTACT Scoring Shiny app

This is an application for Shiny that can be used to generate factor scores and regression based norms for the bi-factor Brief Test of Adult Cognition By Telephone (BTACT).
Users can choose to apply any of the following demographic corrections to derive regression-based norms:
	Age
	Age + Education
	Age + Gender
	Age + Occupation
	Age + Education + Gender
	Age + Education + Occupation
	Age + Gender + Occupation
	Age + Education + Gender + Occupation
To run this app, the user must download and install the free R software package from http://www.r-project.org
Once R is installed, the shiny package must be installed. In R, simply type the code below to install shiny.

```R
install.packages("shiny", dependencies = TRUE)
```

The easiest way to run the BTACT Shiny app is by typing the code below into R.

```R
library(shiny)

runGitHub("BTACT", "begavett")
```
