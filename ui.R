# addResourcePath(prefix="img", directoryPath="img/")
library(shiny)
library(rmarkdown)
library(rCharts)
pageTitle <- "Probability distribution live demo app by @ksmzn #Shiny "
statDist <- "Probability Distributions by @ksmzn" # Title
titleLink <- span(statDist)
googleAnalytics <- tags$head(includeScript("js/google-analytics.js"))
distPanel <- function(name){
  doc <- div(class="title-panel",
             span(class="dist", name),
             a(href = paste('http://en.wikipedia.org/wiki/', name, sep=''),
               img(src='img/external.png'),
               'Wikipedia'
               )
             )
  return(doc)
}

shinyUI(
  navbarPage(
    titleLink,
    windowTitle = pageTitle,
    id = "top-nav",
    inverse=TRUE,
    tabPanel("About",
      fluidRow(
        column(12,
#               offset = 1,
          includeMarkdown("about.md")
        )
      )
    ),
    navbarMenu("Continuous distributions",
      tabPanel("Erlang distribution",
        distPanel("Erlang distribution"),
        fluidRow(
          column(12,
            helpText("$$
              f(x; n, \\lambda)=
              {\\lambda^{n} x^{n-1} e^{-\\lambda x} \\over (n-1)!}\\quad\\mbox{for }x>0
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("erlang", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("erlang", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("erlang", "shape", sep="."), "\\(n\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("erlang", "scale", sep="."), "\\(\\lambda\\)",
                        min = 0, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("erlangPlot")
          )
        )
      ),
#       tabPanel("Generalized Hyperbolic Distribution"),
#       tabPanel("Wishart distribution"),
      tabPanel("F-distribution",
        distPanel("F-distribution"),
        fluidRow(
          column(12,
            helpText("$$ f(x) = \\frac{1}{\\mathrm{B}(d_1/2, d_2/2)} \\; \\left(\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_1/2} \\; \\left(1-\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_2/2} \\; x^{-1} $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("f", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("f", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("f", "df1", sep="."), "DF \\(d_1\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("f", "df2", sep="."), "DF \\(d_2\\)",
                        min = 1, max = 20, value = 1, step= 1)
          ),
          mainPanel(
            lineChartOutput("fPlot")
          )
        )
      ),
      tabPanel("Noncentral F-distribution",
        titlePanel("Noncentral F-distribution"),
        fluidRow(
          column(12,
            helpText("$$ f(x)
              =\\sum\\limits_{k=0}^\\infty
              \\frac{e^{-\\lambda/2}(\\lambda/2)^k}
              { B\\left(\\frac{\\nu_2}{2},\\frac{\\nu_1}{2}+k\\right) k!}
              \\left(\\frac{\\nu_1}{\\nu_2}\\right)^{\\frac{\\nu_1}{2}+k}
              \\left(\\frac{\\nu_2}{\\nu_2+\\nu_1x}\\right)
              ^{\\frac{\\nu_1+\\nu_2}{2}+k}x^{\\nu_1/2-1+k}
              \\ \\ \\ \\ \\mathrm{for\\ } x > 0
              $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("ncf", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("ncf", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("ncf", "df1", sep="."), "DF \\(\\nu_1\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("ncf", "df2", sep="."), "DF \\(\\nu_2\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("ncf", "ncp", sep="."), "非中心度 \\(\\lambda\\)",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("ncfPlot")
          )
        )
      ),
      tabPanel("Chi-squared distribution",
        distPanel("Chi-squared distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x;k)=\\frac{(1/2)^{k/2}}{\\Gamma(k/2)} x^{k/2 - 1} e^{-x/2}
              \\ \\ \\ \\ \\mathrm{for\\ } x > 0$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("chisq", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("chisq", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("chisq", "df", sep="."), "DF \\(k\\)",
                        min = 1, max = 20, value = 1, step= 1)
          ),
          mainPanel(
            lineChartOutput("chisqPlot")
          )
        )
               ),
      tabPanel("Noncentral chi-squared distribution",
        distPanel("Noncentral chi-squared distribution"),
        fluidRow(
          column(12,
            helpText("$$f_X(x; k,\\lambda) =
              \\sum_{i=0}^\\infty \\frac{e^{-\\lambda/2} (\\lambda/2)^i}{i!} f_{Y_{k+2i}}(x)
              \\ \\ \\ \\ \\mathrm{for\\ } x > 0\\\\
              \\\\ Y_q \\mathrm{\\ follows\\ chi-squared\\ distribution\\ with\\ DF \\ } q $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("ncChisq", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("ncChisq", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("ncChisq", "df", sep="."), "DF \\(k\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("ncChisq", "ncp", sep="."), "Noncentrality \\(\\lambda\\)",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("ncChisqPlot")
          )
        )
               ),
#       tabPanel("Gumbel distribution"),
      tabPanel("Gamma distribution",
        distPanel("Gamma distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x) = x^{k-1} \\frac{e^{-x/\\theta}}{\\Gamma(k)\\,\\theta^k}
                     \\ \\ \\ \\ \\mathrm{for\\ } x > 0$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("gamma", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("gamma", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("gamma", "shape", sep="."), "Shape \\(k\\)",
                        min = 0, max = 20, value = 1, step= 0.1),
            sliderInput(paste("gamma", "scale", sep="."), "尺度 \\(\\theta\\)",
                        min = 0, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("gammaPlot")
          )
        )
               ),
#       tabPanel("Inverse Gaussian distribution"),
      tabPanel("Cauchy distribution",
        distPanel("Cauchy distribution"),
        fluidRow(
          column(12,
            helpText("$$\\begin{align}f(x; x_0,\\gamma) &= { 1 \\over \\pi } \\left[ { \\gamma \\over (x - x_0)^2 + \\gamma^2  } \\right]\\end{align}$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("cauchy", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("cauchy", "range", sep="."), "Range",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("cauchy", "location", sep="."), "位置 \\(x_0\\)",
                        min = -20, max = 20, value = 0, step= 0.1),
            sliderInput(paste("cauchy", "scale", sep="."), "尺度 \\(\\gamma\\)",
                        min = 0.1, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("cauchyPlot")
          )
        )
      ),
      tabPanel("Exponential distribution",
        distPanel("Exponential distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x; \\lambda) = \\left\\{ \\begin{array}{ll} \\lambda e^{-\\lambda x} & (x \\geq 0) \\\\ 0 & (x < 0)\\end{array}\\right.$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("exp", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("exp","range",sep="."), "Range",
                        min = -10, max = 100, value = c(0, 5), step= 0.5),
            sliderInput(paste("exp","rate",sep="."), "\\(\\lambda\\)",
                        min = 0, max = 50, value = 1, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("expPlot")
          )
        )
      ),
      tabPanel("Normal distribution",
        distPanel("Normal distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x)=\\frac{1}{\\sqrt{2\\pi\\sigma^{2}}} \\exp\\!\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2} \\right)$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("norm", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("norm", "range", sep="."), "Range",
                        min = -100, max = 100, value = c(-10, 10), step= 1),
            sliderInput(paste("norm", "mean", sep="."), "Mean \\(\\mu\\)",
                        min = -50, max = 50, value = 0, step= 1),
            sliderInput(paste("norm", "sd", sep="."), "Standard deviation \\(\\sigma\\)",
                        min = 0, max = 10, value = 1, step= 0.5)
          ),
          mainPanel(
#             plotOutput("normalPlot")
              lineChartOutput("normalPlot")
          )
        )
      ),
#       tabPanel("Hyperbolic secant distribution"),
      tabPanel("Log-normal distribution",
        distPanel("Log-normal distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x) = \\frac{1}{\\sqrt{2\\pi} \\sigma x} e^{-\\frac{ (\\ln{x}-\\mu)^2}{2\\sigma^2} }, \\quad 0<x< \\infty$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("lnorm", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("lnorm", "range", sep="."), "Range",
                        min = 0, max = 200, value = c(0, 20), step= 0.5),
            sliderInput(paste("lnorm", "meanlog", sep="."), "Meanlog",
                        min = -50, max = 50, value = 0, step= 0.05),
            sliderInput(paste("lnorm", "sdlog", sep="."), "Standard deviationlog",
                        min = 0, max = 10, value = 1, step= 0.05)
          ),
          mainPanel(
            lineChartOutput("lnormalPlot")
          )
        )
      ),
      tabPanel("Student's t-distribution",
        distPanel("Student's t-distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x) = \\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi\\,}\\,\\Gamma(\\nu/2)} (1+x^2/\\nu)^{-(\\nu+1)/2}$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("t", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("t", "range", sep="."), "Range",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("t", "df", sep="."), "DF \\(\\nu\\)",
                        min = 1, max = 20, value = 1, step= 1)
          ),
          mainPanel(
            lineChartOutput("tPlot")
          )
        )
      ),
      tabPanel("Noncentral t-distribution",
        distPanel("Noncentral t-distribution"),
        fluidRow(
          column(12,
            helpText("$$ f(x) =\\frac{\\nu^{\\frac{\\nu}{2}} \\exp\\left (-\\frac{\\nu\\mu^2}{2(x^2+\\nu)} \\right )}{\\sqrt{\\pi}\\Gamma(\\frac{\\nu}{2})2^{\\frac{\\nu-1}{2}}(x^2+\\nu)^{\\frac{\\nu+1}{2}}} \\int_0^\\infty y^\\nu\\exp\\left (-\\frac{1}{2}\\left(y-\\frac{\\mu x}{\\sqrt{x^2+\\nu}}\\right)^2\\right ) dy$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("nct", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("nct", "range", sep="."), "Range",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("nct", "df", sep="."), "DF \\(\\nu\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("nct", "ncp", sep="."), "非中心度 \\(\\mu\\)",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("nctPlot")
          )
        )
      ),
#       tabPanel("Dirichlet distribution"),
#       tabPanel("Pareto distribution"),
      tabPanel("Beta distribution",
        distPanel("Beta distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x)=\\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)}$$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("beta", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("beta", "range", sep="."), "Range",
                        min = 0, max = 1, value = c(0, 1), step= 0.01),
            sliderInput(paste("beta", "shape1", sep="."), "Shape \\(\\alpha\\)",
                        min = 0, max = 20, value = 2, step= 0.1),
            sliderInput(paste("beta", "shape2", sep="."), "Shape \\(\\beta\\)",
                        min = 0, max = 20, value = 2, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("betaPlot")
          )
        )
      ),
      tabPanel("Noncentral beta distribution",
        titlePanel("Noncentral beta distribution"),
        fluidRow(
          column(12,
            helpText("$$
              f(x) = \\sum_{j=0}^\\infty \\frac{1}{j!}
              \\left(\\frac{\\lambda}{2}\\right)^je^{-\\lambda/2}
              \\frac{x^{\\alpha+j-1}(1-x)^{\\beta-1}}{B(\\alpha+j,\\beta)}
            $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("ncbeta", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("ncbeta", "range", sep="."), "Range",
                        min = 0, max = 1, value = c(0, 1), step= 0.01),
            sliderInput(paste("ncbeta", "shape1", sep="."), "Shape \\(\\alpha\\)",
                        min = 0, max = 20, value = 2, step= 0.1),
            sliderInput(paste("ncbeta", "shape2", sep="."), "Shape \\(\\beta\\)",
                        min = 0, max = 20, value = 2, step= 0.1),
            sliderInput(paste("ncbeta", "ncp", sep="."), "Noncentrality \\(\\lambda\\)",
                        min = 0, max = 20, value = 0, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("ncbetaPlot")
          )
        )
      ),
#       tabPanel("Laplace distribution"),
#       tabPanel("Rayleigh distribution"),
#       tabPanel("Lévy distribution"),
      tabPanel("Uniform distribution",
        distPanel("Uniform distribution"),
        fluidRow(
          column(12,
            helpText("$$
  f(x)=\\begin{cases}
  \\frac{1}{b - a} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
  0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
  \\end{cases} 
            $$")
#             helpText("$$
#               f(x)=\\left\\{\\begin{matrix}
#               \\frac{1}{b - a} & \\ \\ \\ \\mathrm{for}\\ a \\le x \\le b, \\\\  \\\\
#               0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b, \\end{matrix}\\right
#             $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("unif", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("unif", "range", sep="."), "Range",
                        min = -100, max = 100, value = c(0, 1), step= 0.5)
          ),
          mainPanel(
            lineChartOutput("unifPlot")
          )
        )
      ),
      tabPanel("Logistic distribution",
        distPanel("Logistic distribution"),
        fluidRow(
          column(12,
            helpText("$$
              f(x;\\mu,s) = \\frac{\\exp(-\\frac{x-\\mu}{s})}{s(1+\\exp(-\\frac{x-\\mu}{s}))^2}
            $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("logis", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("logis", "range", sep="."), "Range",
                        min = -100, max = 100, value = c(-10, 10), step= 0.5),
            sliderInput(paste("logis", "location", sep="."), "Location \\(\\mu\\)",
                        min = -20, max = 20, value = 2, step= 0.1),
            sliderInput(paste("logis", "scale", sep="."), "Scale \\(s\\)",
                        min = 0, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("logisPlot")
          )
        )
               ),
      tabPanel("Weibull distribution",
        distPanel("Weibull distribution"),
        fluidRow(
          column(12,
            helpText("$$
                     f(t)=\\frac{m}{\\eta}\\left(\\frac{t}{\\eta}\\right)^{m-1}
                     \\exp \\left\\{-\\left(\\frac{t}{\\eta}\\right)^m\\right\\}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("weibull", "p_or_c", sep="."), "",
                      c("Probability density function (PDF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("weibull", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 0.5),
            sliderInput(paste("weibull", "shape", sep="."), "Shape \\(m\\)",
                        min = 0.1, max = 20, value = 1, step= 0.1),
            sliderInput(paste("weibull", "scale", sep="."), "Scale \\(\\eta\\)",
                        min = 0.1, max = 20, value = 1, step= 0.1)
          ),
          mainPanel(
            lineChartOutput("weibullPlot")
          )
        )
      )
    ),
    navbarMenu("Discrete distributions",
      tabPanel("Geometric distribution",
        distPanel("Geometric distribution"),
        fluidRow(
          column(12,
            helpText("$$\\Pr(X = k) = p(1-p)^{k}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("geom", "p_or_c", sep="."), "",
                      c("Probability mass function (PMF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("geom", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 1),
            sliderInput(paste("geom", "prob", sep="."), "Probability of successful trial \\(p\\)",
                        min = 0, max = 1, value = 0.5, step= 0.01)
          ),
          mainPanel(
            scatterChartOutput("geomPlot")
          )
        )
      ),
      tabPanel("Hypergeometric distribution",
        distPanel("Hypergeometric distribution"),
        fluidRow(
          column(12,
            helpText("$$
              \\operatorname{P}(X=x)
              = \\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("hyper", "p_or_c", sep="."), "",
                      c("Probability mass function (PMF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("hyper", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 1),
            sliderInput(paste("hyper", "m", sep="."), "Number of white balls in the urn \\(m\\)",
                        min = 0, max = 100, value = 50, step= 1),
            sliderInput(paste("hyper", "n", sep="."), "Number of black balls in the urn \\(n\\)",
                        min = 0, max = 100, value = 50, step= 1),
            sliderInput(paste("hyper", "k", sep="."), "Number of balls drawn from the urn \\(k\\)",
                        min = 0, max = 100, value = 10, step= 1)
          ),
          mainPanel(
            scatterChartOutput("hyperPlot")
          )
        )
      ),
#       tabPanel("Zipf distribution",),
#       tabPanel("Multinomial distribution",),
      tabPanel("Binomial distribution",
        distPanel("Binomial distribution"),
        fluidRow(
          column(12,
            helpText("$$P[X=k]={n\\choose k}p^k(1-p)^{n-k}\\quad\\mbox{for}\\ k=0,1,2,\\dots,n 
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("binom", "p_or_c", sep="."), "",
                      c("Probability mass function (PMF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("binom", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 1),
            sliderInput(paste("binom", "size", sep="."), "Number of trials \\(n\\)",
                        min = 0, max = 40, value = 10, step= 1),
            sliderInput(paste("binom", "prob", sep="."), "Probability of successful trial \\(p\\)",
                        min = 0, max = 1, value = 0.5, step= 0.01)
          ),
          mainPanel(
            scatterChartOutput("binomPlot")
          )
        )
      ),
      tabPanel("Negative binomial distribution",
        distPanel("Negative binomial distribution"),
        fluidRow(
          column(12,
            helpText("$$f(x)=P(X=x) = {x-1 \\choose r-1} p^r (1-p)^{x-r}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("nbinom", "p_or_c", sep="."), "",
                      c("Probability mass function (PMF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("nbinom", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 1),
            sliderInput(paste("nbinom", "size", sep="."), "target for number of successful trials \\(r\\)",
                        min = 1, max = 20, value = 1, step= 1),
            sliderInput(paste("nbinom", "prob", sep="."), "Probability of successful trial \\(p\\)",
                        min = 0, max = 1, value = 0.5, step= 0.01)
          ),
          mainPanel(
            scatterChartOutput("nbinomPlot")
          )
        )
      ),
#       tabPanel("Poisson binomial distribution",),
#       tabPanel("Bernoulli distribution",),
      tabPanel("Poisson distribution",
        distPanel("Poisson distribution"),
        fluidRow(
          column(12,
            helpText("$$P(X=k)=\\frac{\\lambda^k e^{-\\lambda}}{k!}
                     $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("pois", "p_or_c", sep="."), "",
                      c("Probability mass function (PMF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("pois", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 1),
            sliderInput(paste("pois", "lambda", sep="."), "\\(\\lambda\\)",
                        min = 1, max = 20, value = 1, step= 0.5)
          ),
          mainPanel(
            scatterChartOutput("poisPlot")
          )
        )
      ),
      tabPanel("Discrete uniform distribution",
        distPanel("Discrete uniform distribution"),
        fluidRow(
          column(12,
            helpText("$$
              f(x)=\\begin{cases}
              \\frac{1}{n} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
              0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
              \\end{cases} 
            $$")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons(paste("dunif", "p_or_c", sep="."), "",
                      c("Probability mass function (PMF)"="p", "Cumulative distribution function (CDF)"="c")
            ),
            sliderInput(paste("dunif", "range", sep="."), "Range",
                        min = 0, max = 100, value = c(0, 20), step= 1)
            ),
          mainPanel(
            scatterChartOutput("dunifPlot")
          )
        )
      )
    ),
    tabPanel(title="@ksmzn", value="https://twitter.com/ksmzn", icon=icon("twitter")),
    tabPanel(title="GitHub(J)", value="http://github.com/ksmzn/ShinyDistributionsApp", icon=icon("github")),
    tabPanel(title="GitHub(E)", value="https://github.com/kaz-yos/ShinyDistributionsApp", icon=icon("github")),
    tabPanel(title="Blog(J)", value="http://ksmzn.hatenablog.com/", icon=icon("pencil")),
    googleAnalytics,
#     tags$head(includeScript("addthis.js")),
#     tags$head(includeScript("jquery.socialbutton-1.9.1.min.js")),
#     header=tagList(
#       includeScript("")
#     ),
    footer=tagList(
#       tags$div(id="social-button"),
      includeCSS("css/style.css"),
#       includeScript("js/share.min.js"),
#       includeScript("js/execute-share.js"),
      includeScript("js/top-nav-links.js"),
      withMathJax()
    )
  )
)
