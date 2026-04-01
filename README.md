<h1> Suplemmentary Shiny App for Sp-ANOVA </h1>

This application is part of the supplementary material from the article <i>"Spatial-ANOVA: Multidimensional Analysis Of Suicide-RelatedEmergency Calls"</i> by P. Escobar-Hernandez, A. López-Quílez, F. Palmí-Perales and M. Marco. Specifically, it contains the full results from the Simulation Study, including the Sensitivity Analysis, as well as the full results from the Case of Study.

<p>To run this app simply use the following code:</p>

```
shiny::runGitHub(repo="SpANOVA-App", username="VdaK1NG")
```

<h1> App dependencies </h1>

The App will automatically check if the required packages are installed, and install them if not. If you prefer to install the packages manually you can use the following code:

```
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyWidgets")
install.packages("shinydashboardPlus")
install.packages("shinyBS")
install.packages("shinyjs")
install.packages("periscope")
install.packages("ggplot2")
install.packages("sp")
install.packages("sf")
install.packages("stringr")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("rintrojs")
install.packages("ggiraph")
install.packages("fresh")
install.packages("DT")
```

<h1> Main application sections </h1>

The Shiny App contains four different sections:

<strong>1. App Description: </strong> a brief description of the app. 
<strong> 2. Simulation Study: </strong> this section contains the full results of the simulation study. User is able to select all the different scenarios considered and check the performance of the different specifications. Moreover, if the user wants, he can select one of the specifications from the scenario and represent the spatial effects and RME using the button Paint. However, if used once, the app will automatically paint after every change on either scenario considered or specification change. The original scenarios from which the simulation comes are highlighted with bold on the paint section.
<strong> 3. Sensitivity Analysis: </strong> this section contains the full results from the Sensitivity Analysis, representing for each scenario considered the performance of all of the specifications for all the different possibilities considered. In particular, we examined whether the inferential outcomes under the proposed model formulation varied according to three implementation choices: prior specification for the precision parameters of the spatial effects (penalized-complexity prior versus uniform prior), scaling of the spatial effects (scale.model = TRUE versus FALSE), and the use of fixed copies for the shared spatial effects (sp.copy.fixed = TRUE versus FALSE).
<strong> 4. Case of Study: </strong> contains the results from the analysis of suicide-related emergency calls in the spanish community of Valencia, using the modelization proposed. User can walk through the results on all of the speficiations for the three combinations of covariates included (Caller vs COVID), Caller vs Gender and Gender vs COVID).

