vars <- names(ercot_ts[,5:91])
qs <- c(.1, .25, .5, .75, .9 ,.95, .99, 1)

pageWithSidebar(
    headerPanel('ERCOT Pricing Quantile Analysis Dashboard'),
    sidebarPanel(
        helpText("Figure it out bozo"),
        actionButton("go", "Quantilize!"),
        hr(),
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
        pickerInput(
          inputId = "quantiles",
          label = "Quantiles",
          choices = qs,
          selected = c(.25, .5, .75, .9),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 5"
          ),
          multiple = TRUE
        ),
        pickerInput(
          inputId = "hour_of_day",
          label = "Choose which hour(s) to regress upon (24 hour format)",
          choices = c(1:24),
          selected = 16,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 5"
          ),
          multiple = TRUE
        ),
        pickerInput(
          inputId = "month_choice",
          label = "Choose which months to regress upon",
          choices = c(1:12),
          selected = c(1:12),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        ),
         sliderInput("date_range", "Pick your year range",
                     min = 2011, max = 2020, value = c(2013, 2015), sep="", ticks=FALSE
         ),
        radioButtons("transform", "Transform data?", c("No", "Natural Log", "Scale"),
                     selected = "No"), 
        
        
        
        
    width = 3),
    mainPanel(
        plotOutput('quantile_plot'),
        renderText("Explained: "),
        textOutput("selected_var1"),
        renderText("Explanatory: "),
        textOutput("selected_var2"),
        renderText("Year Range: "),
        textOutput("selected_var3"),
        renderText("Months: "),
        textOutput("selected_var4"),
        renderText("Hour(s): "),
        textOutput("selected_var5"),
        renderText("Quantiles: "),
        textOutput("selected_var6"),
        #plotOutput('plot1'),
        #plotOutput('plot2'),
        plotOutput('ggpairsplot'),
        plotOutput('quantile_plot2')
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
