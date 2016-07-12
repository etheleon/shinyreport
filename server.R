library(shiny)

doughnut <- function (x, labels = names(x), edges = 201, outer.radius = 0.8, 
          inner.radius=1.6, clockwise = FALSE,
          init.angle = if (clockwise) 90 else 0, density = NULL, 
          angle = 45, col = NULL, border = FALSE, lty = NULL, 
          main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x < 0))
        stop("'x' values must be positive.")
    if (is.null(labels))
        labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
        xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
        col <- if (is.null(density))
          palette()
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
        -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), 
             y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                  outer.radius)
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
                angle = angle[i], border = border[i], 
                col = col[i], lty = lty[i])
        Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
            text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], 
                 xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
                 ...)
        }
        ## Add white disc          
        Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
        polygon(Pin$x, Pin$y, density = density[i], 
                angle = angle[i], border = border[i], 
                col = "white", lty = lty[i])
    }
 
    title(main = main, ...)
    invisible(NULL)
}

library(shiny)

paramNames <- c("start_capital", "annual_mean_return", "annual_ret_std_dev",
	"annual_inflation", "annual_inf_std_dev", "monthly_withdrawals", "n_obs",
	"n_sim")

# Define server logic required to generate and plot a random distribution
#
# Idea and original code by Pierre Chretien
# Small updates by Michael Kapler
#
shinyServer(function(input, output, session) {

	getParams <- function(prefix) {
		input[[paste0(prefix, "_recalc")]]

		params <- lapply(paramNames, function(p) {
			input[[paste0(prefix, "_", p)]]
		})
		names(params) <- paramNames
		params
	}

  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  navA <- reactive(do.call(simulate_nav, getParams("a")))
  navB <- reactive(do.call(simulate_nav, getParams("b")))

  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$a_distPlot <- renderPlot({
  	plot_nav(navA())
  })
  output$b_distPlot <- renderPlot({
  	plot_nav(navB())
  })

  output$table = renderTable(data.frame(income="100,000", expenses="20,000", savings="80,000"))
  output$table2 = renderTable(data.frame(DBS="100k", OCBC="150,000", SC="200,000", UOB="300,000"))
  output$savingsWithBank <- renderPlot({
        vv = c(3,5,9,12)
        names(vv) = c("DBS", "OCBC", "STANDARD CHARTERED", "UOB")
        doughnut( vv,
        inner.radius=0.5,
        col=c(
            rgb(0.2,0.2,0.4,0.5),
            rgb(0.8,0.2,0.4,0.5),
            rgb(0.2,0.9,0.4,0.4),
            rgb(0.0,0.9,0.8,0.4))
        )
  })
})

simulate_nav <- function(start_capital = 2000000, annual_mean_return = 5.0,
                         annual_ret_std_dev = 7.0, annual_inflation = 2.5,
                         annual_inf_std_dev = 1.5, monthly_withdrawals = 1000,
                         n_obs = 20, n_sim = 200) {
  #-------------------------------------
  # Inputs
  #-------------------------------------

  # Initial capital
  start.capital = start_capital

  # Investment
  annual.mean.return = annual_mean_return / 100
  annual.ret.std.dev = annual_ret_std_dev / 100

  # Inflation
  annual.inflation = annual_inflation / 100
  annual.inf.std.dev = annual_inf_std_dev / 100

  # Withdrawals
  monthly.withdrawals = monthly_withdrawals

  # Number of observations (in Years)
  n.obs = n_obs

  # Number of simulations
  n.sim = n_sim


  #-------------------------------------
  # Simulation
  #-------------------------------------

  # number of months to simulate
  n.obs = 12 * n.obs

  # monthly Investment and Inflation assumptions
  monthly.mean.return = annual.mean.return / 12
  monthly.ret.std.dev = annual.ret.std.dev / sqrt(12)

  monthly.inflation = annual.inflation / 12
  monthly.inf.std.dev = annual.inf.std.dev / sqrt(12)

  # simulate Returns
  monthly.invest.returns = matrix(0, n.obs, n.sim)
  monthly.inflation.returns = matrix(0, n.obs, n.sim)

  monthly.invest.returns[] = rnorm(n.obs * n.sim, mean = monthly.mean.return, sd = monthly.ret.std.dev)
  monthly.inflation.returns[] = rnorm(n.obs * n.sim, mean = monthly.inflation, sd = monthly.inf.std.dev)

  # simulate Withdrawals
  nav = matrix(start.capital, n.obs + 1, n.sim)
  for (j in 1:n.obs) {
    nav[j + 1, ] = nav[j, ] * (1 + monthly.invest.returns[j, ] - monthly.inflation.returns[j, ]) - monthly.withdrawals
  }

  # once nav is below 0 => run out of money
  nav[ nav < 0 ] = NA

  # convert to millions
  nav = nav / 1000000

  return(nav)
}

plot_nav <- function(nav) {

  layout(matrix(c(1,2,1,3),2,2))

  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))

  # plot all scenarios
  matplot(nav,
    type = 'l', lwd = 0.5, lty = 1, col = 1:5,
    xlab = 'Months', ylab = 'Millions',
    main = 'Projected Value of Initial Capital')

  # plot % of scenarios that are still paying
  p.alive = 1 - rowSums(is.na(nav)) / ncol(nav)

  plot(100 * p.alive, las = 1, xlab = 'Months', ylab = 'Percentage Paying',
    main = 'Percentage of Paying Scenarios', ylim=c(0,100))
  grid()


  last.period = nrow(nav)

  # plot distribution of final wealth
  final.nav = nav[last.period, ]
  final.nav = final.nav[!is.na(final.nav)]

  if(length(final.nav) ==  0) return()

  plot(density(final.nav, from=0, to=max(final.nav)), las = 1, xlab = 'Final Capital',
    main = paste0('Distribution of Final Capital\n', 100 * p.alive[last.period], '% are still paying'))
  grid()
}
