library(shiny)

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
        sliderInput(paste0(prefix, "_", "n_obs"), "Number of observations (in Years):", min = 0, max = 40, value = 20),
        sliderInput(paste0(prefix, "_", "start_capital"), "Initial capital invested :", min = 100000, max = 10000000, value = 2000000, step = 100000, pre = "$", sep = ","),
        sliderInput(paste0(prefix, "_", "annual_mean_return"), "Annual investment return (in %):", min = 0.0, max = 30.0, value = 5.0, step = 0.5),
        sliderInput(paste0(prefix, "_", "annual_ret_std_dev"), "Annual investment volatility (in %):", min = 0.0, max = 25.0, value = 7.0, step = 0.1)
      ),
      column(6,
        sliderInput(paste0(prefix, "_", "annual_inflation"), "Annual inflation (in %):", min = 0, max = 20, value = 2.5, step = 0.1),
        sliderInput(paste0(prefix, "_", "annual_inf_std_dev"), "Annual inflation volatility. (in %):", min = 0.0, max = 5.0, value = 1.5, step = 0.05),
        sliderInput(paste0(prefix, "_", "monthly_withdrawals"), "Monthly capital withdrawals:", min = 1000, max = 100000, value = 10000, step = 1000, pre = "$", sep = ","),
        sliderInput(paste0(prefix, "_", "n_sim"), "Number of simulations:", min = 0, max = 2000, value = 200)
      )
    ),
    p(actionButton(paste0(prefix, "_", "recalc"),
      "Re-run simulation", icon("random")
    ))
  )
}

shinyUI(fluidPage(
    tags$head(
        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.6/js/materialize.min.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.6/css/materialize.min.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "output.css"),
  tags$style(type="text/css",
    "label {font-size: 12px;}",
    ".recalculating {opacity: 1.0;}"
  )
        #includeCSS("output.css")
    ),
    tagAppendChildren(tag("main",""), list(
        div(class="jumbo", style="background-image:url('cover.png')"),
        tags$div(class="container icons",
            tags$div(class="big-icon", style="background-image:url('display.png')"),
            tags$div(class="rate",
                tags$a(class="star-btn add-btn btn-floating btn-large waves-effect waves-light blue darken-1",
                    tags$i(class="material-icons")
                ),
                tags$a(class="like-btn add-btn btn-floating btn-large waves-effect waves-light blue darken-1",
                    tags$i(class="material-icons")
                )),
            tags$div(class="add",
                tags$a(class="add-btn btn-floating btn-large waves-effect waves-light red",
                    tags$i(class="material-icons")
                ))
        ),
        div(class="details",
            tags$h3("Leonardo DiCaprio"),
            tags$p("Prudential Insurance")
        ),
        div(class="row", 
            h3("Savings Plan"),
            fluidRow(
                column(6, 
                       h5("Bank Total: $500,000"),
                        plotOutput("savingsWithBank", height="600px")),
                column(3, h5("Breakdown of Finances"), tableOutput('table')),
                column(3, h5("Amount in Banks"), tableOutput('table2'))
            )),
        div(class="row", 
            h3("Retirement Plan"),
            p("One of the biggest risks to a comfortable retirement is running out of money too soon. See the plot below for your customised retirement shortfall")
            ),
  #fluidRow(
    #column(6, tags$h3("Scenario A")),
    #column(6, tags$h3("Scenario B"))
  #),
  fluidRow(
    column(12, renderInputs("a"))
    #column(6, renderInputs("b"))
  ),
  fluidRow(
    #column(6,
      plotOutput("a_distPlot", height = "600px")
    #)
    #column(6,
      #plotOutput("b_distPlot", height = "600px")
    #)
  ),

        div(class="row", 
            h3("Death"),
            p("In the event of your passing, your loved ones may need extra help to tide through the touch times")
            )
        )
    )
))
