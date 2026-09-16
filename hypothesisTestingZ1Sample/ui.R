#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


fluidPage(
    withMathJax(),
    titlePanel("Hypothesis Testing (Z-Test) Demonstrator"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("mu", "Population Mean (mu):", value = 140),
            numericInput("xbar", "Sample Mean (x bar):", value = 145),
            numericInput("s", "Sample Standard Deviation (s):", value = 15),
            numericInput("n", "Sample Size (n):", value = 40, min = 31),
            numericInput("alpha", "Significance Level (alpha):", value = 0.05, min = 0.001, max = 0.20, step = 0.01),
            radioButtons("hypothesis", "Alternative Hypothesis:",
                         choices = c("Two-sided (Not equal)" = "equal",
                                     "Less than" = "less",
                                     "Greater than" = "greater"))
        ),
        
        mainPanel(
            uiOutput("data_summary"),
            hr(),
            uiOutput("hypotheses"),
            hr(),
            plotOutput("main_plot"),
            hr(),
            uiOutput("assumptions"),
            hr(),
            uiOutput("results")
        )
    )
)