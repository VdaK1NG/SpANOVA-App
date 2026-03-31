<h1> Suplemmentary Shiny App for Sp-ANOVA </h1>

This application is designed for the resolution of spatial models using the Bayesian paradigm and the INLA methodology. Therefore, since it has been designed for all kinds of users, we will explain in some detail its functionality and its main sections. <!--Additionally, in this *README* file, the theoretical foundations underlying the application will be briefly presented. Which means, a summary of Bayesian inference and the foundaments of the INLA methodology.-->

<p>To run this app on your own computer or laptop use the following code:</p>
    <pre><code>
shiny::runGitHub(repo="SpANOVA-App", username="VdaK1NG")
    </code></pre>

<h1> App dependencies </h1>

As mentioned above, this application was built to solve spatial models, for which it needs several packages. Although these dependencies would be installed automatically when running the application itself, if the automatic process fails you can try to install these packages manually. The code for the manual installation is the following:

```
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyWidgets")
install.packages("shinydashboardPlus")
install.packages("shinyBS")
install.packages("shinyjs")
install.packages("periscope")
install.packages("splines")
install.packages("INLA",
  repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), 
  dep=TRUE)
install.packages("inlabru")
install.packages("fmesher")
install.packages("ggplot2")
install.packages("ggtext")
install.packages("lattice")
install.packages("rintrojs")
install.packages("patchwork")
install.packages("viridis")
install.packages("dplyr")
```

The INLA package installation could give some problems or errors[^1], in such case it is desirable to visit the [INLA home page](https://www.r-inla.org/), where the installation is explained in some detail and many FAQ are answered.

<h1> Main application sections </h1>

The Shiny App contains four different sections:

1. App Description: a brief description of the app.
2. Simulation Study: in this section 
3. Sensitivity Analysis: in this section 
4. Case of Study: in this section 

<h2> 1. App Description </h2>

AAAAAA

<h2> 2. Simulation Study </h2>

AAAAAA

<h2> 3. Sensitivity Analysis </h2>

AAAAAA

<h2> 4. Case of Study </h2>

AAAAAA
