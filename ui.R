library(shiny)
library(shinydashboard)
library(shinycssloaders)

dashboardPage(
    dashboardHeader(title = "Clustering Analysis", disable = TRUE),
    dashboardSidebar(
      tags$style(HTML(".main-sidebar li a { font-size: 20px; }")),
        menuItem("Clustering Analysis", tabName = "Clustering Analysis", icon = icon("dashboard")),
        selectizeInput('factors', 'Cluster Inputs', choices = list(
            Factors = c('Customer Age' = 'AGE', 'Customer Gender' = 'GENDER', 'Days Since Last Purchase'='LAST_PURCHASE', 'Total Number of Purchases'='NUM_TRANSACTIONS', 'Total Spendings'='TOTAL_SPENDINGS')
        ), multiple = TRUE),
        sliderInput("k", "Number of Clusters", 2, 10, 3),
        checkboxInput("normalize", "Normalize Data", FALSE),
        actionButton("exportButton", "Generate Report", width="88%"),
        actionButton("crButton", "Generate Recommendations", width="88%")
    ),
    dashboardBody(tabsetPanel(id = "tab",
        tabPanel("K-Means Clustering",
                 fixedRow(
                     column(7, div(style = "height:800px; width:800px;", withSpinner(plotOutput("graph_km"))))
                 ),
                 fixedRow(
                     column(7, div(style = "width:800px;", DT::dataTableOutput("km_table")))
                 )),
        tabPanel("Hierarchical Clustering (Complete)", 
                 fixedRow(
                     column(7, div(style = "height:800px; width:800px;", withSpinner(plotOutput("graph_hc"))))
                 ),
                 fixedRow(
                     column(7, div(style = "width:800px;", DT::dataTableOutput("hc_table")))
                 )),
        tabPanel("Hierarchical Clustering (Average)", 
                 fixedRow(
                     column(7, div(style = "height:800px; width:800px;", withSpinner(plotOutput("graph_ha"))))
                 ),
                 fixedRow(
                     column(7, div(style = "width:800px;", DT::dataTableOutput("ha_table")))
                 )),
        tabPanel("Silhouette Plot", withSpinner(plotOutput("hc_si"))),
        tabPanel("Scree Plot", withSpinner(plotOutput("hc_scree"))),
        tabPanel("Cluster Recommendations",
                 fixedRow(
                     column(7, div(style = "width:800px;", DT::dataTableOutput("cr_table")))
                 ))
        
    ))
)
