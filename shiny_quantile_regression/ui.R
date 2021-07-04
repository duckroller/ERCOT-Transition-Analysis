vars <- names(ercot_ts[,5:89])

pageWithSidebar(
    headerPanel('ERCOT Pricing Quantile Analysis Dashboard'),
    sidebarPanel(
        helpText("Figure it out bozo"),
        actionButton("go", "Quantilize!"),
        hr(),
        #selectInput('algo', 'Clustering Method', c("K-Means Clustering", "Hierarchical Clustering")),
        #numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
        #hr(),
        #selectInput('xcol', 'X Variable', vars, selected = vars[[12]]),
        # selectInput('ycol', 'Y Variable', vars),
        pickerInput(
            inputId = "ycol",
            label = "Select explained variable",
            choices = vars,
            selected = vars[8],
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count = 1"
            ),
            multiple = FALSE
        ),
        pickerInput(
            inputId = "xcols",
            label = "Select predictor variables",
            choices = vars,
            selected = vars[c(19:20,22:24)],
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
        ),
        
        # checkboxGroupInput(inputId = "quant_choice", 
        #                     label = "Select qunatiles to evaluate", 
        #                     choices = c(0.1,0.25,0.5,0.75,0.9,0.95), 
        #                     selected = c(0.25, 0.5, 0.75)),
        
        sliderInput("obs", "Quantiles",
                    min = 0, max = 1, value = c(0.4, 0.8)
        ),
        
        # dateRangeInput('dateRange',
        #                label = paste('Date range input',
        #                              'yyy/mm/dd week starts on day 1 (Monday),',
        #                              'separator is "-", start view is year'),
        #                start = ercot_ts$Date[1], end = ercot_ts$Date[-1],
        #                min = ercot_ts$Date[1], max = ercot_ts$Date[-1],
        #                separator = " - ", format = "yyyy-mm-dd",
        #                startview = 'year', weekstart = 1),
        


        #verbatimTextOutput('selected_states'),
        #selectInput('state_choice', 'States', state.name, multiple=TRUE, selectize=FALSE),
        #tags$head(tags$style(HTML('#state_choice{overflow-y: scroll;height: 16em;}'))),
        #tags$h3("States to Cluster"),
        # pickerInput(
        #         inputId = "state_choice",
        #         label = "Select States to Cluster",
        #         choices = state.name,
        #         selected = state.name,
        #         options = list(
        #             `actions-box` = TRUE,
        #             size = 10,
        #             `selected-text-format` = "count > 3"
        #         ),
        #         multiple = TRUE
        # ),
            #checkboxGroupInput("state_choice", 
             #                  label = NULL, 
              #                 choices = state.name,
               #                selected = state.name,
                #               ),
        
        #downloadButton("downloadData", "Download Cluster Table"),
        
    width = 3),
    mainPanel(
        plotOutput('quantile_plot')
        #plotOutput('plot1'),
        #plotOutput('plot2'),
        #plotOutput('ggpairsplot'),
        # fluidRow(
        #     splitLayout(cellArgs = list(style = "overflow-x: hidden;"),
        #                 plotOutput('plot3'),
        #                 plotOutput('plot4'))
        # ),
        # DT::dataTableOutput("table"),
      #  renderText("Data Sources"),
#         helpText("Energy-Related CO2 Emission Data Tables, Table 4: 2017 State energy-related carbon dioxide emissions by sector: https://www.eia.gov/environment/emissions/state/.
# State Population Totals - US Census Bureau: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html.
# GDP by State - Bureau of Economic Analysis: https://www.bea.gov/data/gdp/gdp-state.
#FiveThirtyEight's Partisan Lean: https://github.com/fivethirtyeight/data/tree/master/partisan-lean."),
    )
)
