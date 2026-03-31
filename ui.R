# Cargamos las librerías
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("shinydashboardPlus")
library("shinyBS")
library("shinyjs")
library("periscope")
library("ggplot2")
library("sp")
library("sf")
library("stringr")
library("dplyr")
library("gridExtra")
library("ggiraph")
library("rintrojs")
library("fresh")
library("DT")


# Load accessory functions
source("source.R")

                      ###################################################
#######################                       UI                        #######################
                      ###################################################

header <- dashboardHeader(
  tags$li(
    class = "dropdown",
    tags$style(".main-header {max-height: 200px}"),
    tags$style(".main-header .logo {height: 115px; padding-top: 15px}"),
    setBackgroundColor("black")
  ),
  title = div(img(src = "logoUV-cropped.svg", height = 60), span(HTML("SpANOVA"), style = {
    "padding-left: 0px"
  })),
  titleWidth = 2200
)

## Sidebar content
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 115px}"),
  minified = FALSE, width = 200,
  sidebarMenu(
    id = "sidebarID",
    menuItem("App Description", tabName = "introduction", icon = icon("gitlab")),
    menuItem("Simulation Study", tabName = "sim_res", icon = icon("calculator")),
    menuItem("Sensitivity Analysis", tabName = "sim_sens", icon = icon("delicious")),
    menuItem("Case of Study", tabName = "case_res", icon = icon("chart-line"))
  )
)

## Body content
body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tags$head(tags$style('#tab_sim .box-header{ display: none}')), # Remover título cajas específicas
  
  # Modificar colores cajas
  tags$style(HTML(" 

                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#FF2F1B
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#ffffff;
                    border-left-color:#ffffff;
                    border-right-color:#ffffff;
                    border-top-color:#ffffff;
                    background: #ffffff
                    }

                    ")),
  # Esconder mensajes de error
  tags$style(type="text/css", 
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  introjsUI(),
  use_theme("mytheme.css"),
  shinybusy::add_busy_spinner(spin = "radar", margins = c(5, 5), position = "bottom-right"), 
  # useShinyjs(),
  tabItems(
    # UI Descriptivo ########################################
    tabItem(tabName = "introduction",
            fluidRow(width=12, 
                     box(width=12, solidHeader = TRUE, title = "App Description", status = "black",
                     # h1("Introduction", style = "color: DarkRed; text-align: center; text-decoration: underline; font-weight: bold"),
                     withMathJax(),
                     p("This Shiny app is part of the supplementary material of the paper \"Spatial-ANOVA: Multidimensional Analysis Of Suicide-Related Emergency Calls\" by Escobar-Hernández, López-Quílez A. and Palmí-Perales F. 
                       Sections included are:"),
                     HTML("
                    <ul>
                    <li> <strong>Simulation Study: </strong> contains all the results from the simulation study. User can select a specific simulated scenario and go through all the results obtained for each particular specification. </li>
                    <li> <strong>Sensitivity Analysis: </strong> contains information corresponding to the sensitivity analysis of the simulation study, where we addressed different prior specifications for the spatial effects (uniform vs pc prior) , 
                    fixed coppy of the shared spatial effects (TRUE or FALSE), and scale of the shared spatial effects (TRUE or FALSE). </li>
                    <li> <strong>Case Study:</strong> contains the results from the analysis of suicide-related emergency calls in the spanish community of Valencia, using the modelization proposed. User can walk through the results on all of the 
                    speficiations for the three combinations of covariates included (Caller vs COVID), Caller vs Gender and Gender vs COVID). </li>
                    </ul>"),
                     p("For further details on the modelization, data generation for the simulation study and explanation of the resuls from the case study we refer the user to the full article.")
             
            )
            )
    ), 
    tabItem(tabName = "sim_res",
      fluidRow(
        column(width=2, 
               box(width=12, solidHeader = TRUE, title = "Choose Simulation Data", status = "black",
               radioButtons("base_type", "Baseline Risk", choices = c("DIF", "SIM"), selected = "DIF"), 
               radioButtons("mod_type", "Type of Data Scenario", choices = c("M0", "M1", "M2", "M3", "M4", "M5", "M6"), selected = "M0"), 
               selectInput("risk_version", "Select a Risk Version", choices = c("-"), selected = "-")
               ), 
               box(width=12, solidHeader = TRUE, title = "Choose Model to Paint", status = "black",
                   radioButtons("sp_ef_mod", "Model", 
                                
                                choiceNames = list(
                                  tags$span(style = "font-weight: bold;", "M0"), 
                                  
                                  tags$span(style = "font-weight: bold;", "M1"), 
                                  
                                  tags$span(style = "font-weight: bold;", "M2-ind(F1L1-F2L1)"), 
                                  tags$span(style = "font-weight: bold;", "M2-ind(F1L1-F2L1)"), 
                                  tags$span(style = "font-weight: bold;", "M2-ind(F1L1-F2L1)"), 
                                  tags$span(style = "font-weight: bold;", "M2-ind(F1L1-F2L1)"), 
                                  
                                  tags$span(style = "font-weight: bold;", "M3-F1.(F1L1)"), 
                                  tags$span(style = "font-weight: normal;", "M3-F1.(F1L2)"), 
                                  
                                  tags$span(style = "font-weight: normal;", "M4-F2.(F2L1)"), 
                                  tags$span(style = "font-weight: bold;", "M4-F2.(F2L2)"), 
                                  
                                  tags$span(style = "font-weight: normal;", "M5-F1.(F1L1)+F2.(F2L1)"), 
                                  tags$span(style = "font-weight: bold;", "M5-F1.(F1L1)+F2.(F2L2)"), 
                                  tags$span(style = "font-weight: normal;", "M5-F1.(F1L2)+F2.(F2L1)"), 
                                  tags$span(style = "font-weight: normal;", "M5-F1.(F1L2)+F2.(F2L2)"), 
                                  
                                  tags$span(style = "font-weight: normal;", "M6-F1.(F1L1)+F2.(F2L1)"),
                                  tags$span(style = "font-weight: normal;", "M6-F1.(F1L2)+F2.(F2L1)"),
                                  tags$span(style = "font-weight: bold;", "M6-F1.(F1L1)+F2.(F2L2)"),
                                  tags$span(style = "font-weight: normal;", "M6-F1.(F1L2)+F2.(F2L2)"),
                                  tags$span(style = "font-weight: normal;", "M6-F2.(F2L1)+F1.(F1L1)"),
                                  tags$span(style = "font-weight: normal;", "M6-F2.(F2L2)+F1.(F1L1)"),
                                  tags$span(style = "font-weight: normal;", "M6-F2.(F2L1)+F1.(F1L2)"),
                                  tags$span(style = "font-weight: normal;", "M6-F2.(F2L2)+F1.(F1L2)")
                                ),
                                choiceValues = mods_names,
                                selected = "M0"), 
                   actionButton("action", label = "Paint")
               )
        ), 
        
        column(width=10,
               box(width = 12, solidHeader = FALSE, id = 'tab_sim', title = NULL, headerBorder = FALSE, dataTableOutput("table_simres")), 
               box(width = 12, solidHeader = TRUE, title = "Random Effects", status = "primary",
                   plotOutput("plot_sp_sim"), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                   br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                   br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()), 
               box(width = 12, solidHeader = TRUE, title = "RME", status = "primary",
                   plotOutput("plot_rme_sim"), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                   br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                   br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br())
        )
      )
    ), 
    tabItem(tabName = "sim_sens",
            fluidRow(
              column(width=2, 
                     box(width=12, solidHeader = TRUE, title = "Choose Simulation Data", status = "black",
                         radioButtons("base_type_sens", "Baseline Risk", choices = c("DIF", "SIM"), selected = "DIF"), 
                         radioButtons("mod_type_sens", "Type of Data Scenario", choices = c("M0", "M1", "M2", "M3", "M4", "M5", "M6"), selected = "M0"), 
                         selectInput("risk_version_sens", "Select a Risk Version", choices = c("-"), selected = "-")
                     ), 
              ), 
              
              column(width=10,
                     box(width=12, solidHeader = TRUE, title = "Version Description", status = "black", dataTableOutput("table_version")),
                     box(width = 12, solidHeader = TRUE, id = 'tab_sense', title = "DIC (sp.null) Comparison", status = "primary", dataTableOutput("table_sense")), 
                     box(width = 12, solidHeader = TRUE, title = "Model 1 Specification", status = "primary",
                         plotOutput("plot_m1_sens"), br(), br(), br(), br(), br()), 
                     box(width = 12, solidHeader = TRUE, title = "Model 2 Specification", status = "primary",
                         plotOutput("plot_m2_sens"), br(), br(), br(), br(), br()), 
                     box(width = 12, solidHeader = TRUE, title = "Model 3 Specification", status = "primary",
                         plotOutput("plot_m3_sens"), br(), br(), br(), br(), br()), 
                     box(width = 12, solidHeader = TRUE, title = "Model 4 Specification", status = "primary",
                         plotOutput("plot_m4_sens"), hr(), br(), br(), br(), br()), 
                     box(width = 12, solidHeader = TRUE, title = "Model 5 Specification", status = "primary",
                         plotOutput("plot_m5_sens"), br(), br(), br(), 
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br()), 
                     box(width = 12, solidHeader = TRUE, title = "Model 6 Specification", status = "primary",
                         plotOutput("plot_m6_sens"), 
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br())
              )
            )
    ), 
    tabItem(tabName = "case_res",
            fluidRow(
              column(width=2, 
                     box(width=12, solidHeader = TRUE, title = "Choose Variable and Model", status = "black",
                         selectInput("vars", "Covariates", choices = c(mods), selected = NA), 
                          selectInput("n_mod", "Model Number", choices = c(1:22), selected = 1)
                     )
              ), 
              
              column(width=10,
                     box(width = 12, solidHeader = FALSE, id = 'tab_sim', title = NULL, headerBorder = FALSE, dataTableOutput("tabla_summary")), 
                     box(width = 12, solidHeader = TRUE, title = "Intercept", status = "primary",
                         plotOutput("plot_int"), br(), br(), br(), br(), br()), 
                     box(width = 12, solidHeader = TRUE, title = "Spatial Effects", status = "primary",
                         plotOutput("plot_sp"), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()), 
                     box(width = 12, solidHeader = TRUE, title = "RME", status = "primary",
                         plotOutput("plot_rme"), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br())
              )
            )
    )
  )
)

dashboardPage(skin = "red", header, sidebar, body)
