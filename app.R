#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# logifySlider javascript function
JS.logify <-
    "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num)); }
    })
  }
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
    "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    logifySlider('A_l', sci = false)
    logifySlider('A_max', sci = false)
    logifySlider('log_slider2', sci = true)
  }, 5)})
"

    
# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinythemes::shinytheme("cerulean"),
    tags$head(tags$script(HTML(JS.logify))),
    tags$head(tags$script(HTML(JS.onload))),
    
    # Application title
    titlePanel("MIS Fundamental Sampling Error Simulator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          wellPanel(
            h4(
              "Mass of Sample"
            ),
            
                numericInput("MinM_s", "Minimum Sample Mass", 
                         min = 0.1, 
                         max = 10000, value = 10),
            
            
            numericInput("MaxM_s", "Maximum Sample Mass", 
                         min = 0.1, 
                         max = 10000, value = 1000),
            
            sliderInput("M_s",
                        "Range of Sample Mass (g):",
                        min = 0.1,
                        max = 10000,
                        value = c(10,1000)),
            HTML("<I>Enter mass (grams) of sample being considered. Either the text box or the slider may be used. 
                 We suggest setting the minimum sample mass to be equal to the analytic subsample.</i><br><br>")
            
            ),

          wellPanel(
            h4(
              "Diameter"
            ),
                 sliderInput("d",
                        "Diameter (cm)",
                        min = 0.01,
                        max = 1,
                        value = 0.5),
            HTML("<I>Enter diameter (centimeters) of largest particle</i><br><br>")
            ),

          wellPanel(
            h4("Concentration of contaminant"),
            numericInput("A_l",
                        "Concentration of Contaminant (unitless):",
                        min = 0,
                        max = 1,
                        value = 4e-4),
            HTML("<i>Contaminant concentration in mass/mass ratio (unitless.) <br>
                 1% = 0.01, <br>
                 1,000 ppm = 0.001 <br>
                 100 ppm = 0.0001 <br>
                 1 ppm = 0.000001 <br><br></i>")
            ),
    wellPanel(
      h4("Density of contaminant particles and surrounding particles (gangue)"),
      
            sliderInput("gamma_m",
                        "Density of Analyte of Interest (g/cc):",
                        min = 0.01,
                        max = 10,
                        value = 2.5),
            
            sliderInput("gamma_s",
                        "Density of gangue (g/cc)",
                        min = 0.01,
                        max = 10,
                        value = 2.5,
                        ),
            HTML("<i>grams/cubic centimeter (g/cc) <br><br></i>")
            ),
            
    wellPanel(
      h4("Maximum concentration of contaminant in a particulate"),
            numericInput("A_max",
                        "Max Concentration of Particle (unitless):",
                        min = 0,
                        max = 1,
                        value = 1
                        ),
      HTML("<i>The maximum concentration in a particulate is used to calculate the liberation factor. Valid when A_max much greater than A_l</i>")
            ),
            
    wellPanel(
      h4("Particulate Shape Factor and Sorting"),
            selectInput("f", 
                        "Shape Factor", 
                        choices = list("Cube = 1" = 1, 
                                       "Sphere = 0.5" = 0.5,
                                       "Flakes (mica) = 0.2" = 0.2,
                                       "Needles (asbestos) = 10" = 10), 
                        selected = 0.5)
           ,
            selectInput("g",
                        "Granulometric Factor",
                        choices = list("Non-calibrated material = 0.25" =  0.25,
                                       "Calibrated material = 0.55" = 0.55,
                                       "Naturally calibrated material = 0.75" = 0.75,
                                       "Perfectly calubrated material = 1" = 1),
                        selected = 0.25),
      HTML("<i>Shape factor converts sieve size to volume. </i>"),
      HTML("<i>Granulometric Factor accounts for particulates not being the same size</i>")
      
            
                        
                        
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            withMathJax("$$\\text{Fundamental Sampling Error: }S^2_{FSE} = \\frac{clfgd^3}{M_s}$$"),
        
            withMathJax("$$\\text{Relative Standard Deviation (RSD): }\\frac{\\sqrt{S^2_{FSE}}}{\\text{Average Concentation}}$$"),
            
            
            fluidRow(
              splitLayout(cellWidths = c("50%", "50%"), plotOutput("FactorPlot"), plotOutput("FSEPlot"))
            ),
            
            withMathJax("$$\\text{C: Mineralogical Factor,}  \\frac{g}{cc}$$"),
            withMathJax("$$\\text{l: Liberation Factor,} (unitless)$$"),
            withMathJax("$$\\text{f: Shape Factor,} (unitless)$$"),
            withMathJax("$$\\text{g: Granulometric factor} (unitless)$$"),
            withMathJax("$$\\text{d: Diameter,} (cm)$$"),
            
            
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    observeEvent(input$MinM_s, {
        updateSliderInput(session, "M_s", value = c(input$MinM_s, input$MaxM_s))
    })
    
    observeEvent(input$MaxM_s, {
        updateSliderInput(session, "M_s", value = c(input$MinM_s, input$MaxM_s))
    })
    
    observeEvent(input$M_s, {
        updateNumericInput(session, "MinM_s",  value = input$M_s[[1]])
        updateNumericInput(session, "MaxM_s", value = input$M_s[[2]])
    })
    
    output$FactorPlot <- renderPlot({
        
        # generate c
        Al <- input$A_l
        Amax <- input$A_max
        c <- input$gamma_m * ((1-Al)^2/(Al)) + input$gamma_s * (1-Al)
        
        
        l <- (Amax - Al)/ (1-Al)
        
        f <- as.numeric(input$f)
        g <- as.numeric(input$g)
        d3 <- input$d^3
        
        clfgd3 <- c(c,l, f, g, d3)
        
        names(clfgd3) <- c("c", 
                           "l", 
                           "f",
                           "g",
                           expression(d^3))

        # draw the histogram with the specified number of bins
        # print(clfgd3)
        # 
         clfgd3 <- log10(clfgd3)
        # M_s_range <- seq(from = input$M_s[[1]], to = input$M_s[[2]], length = 100)
        # total_FSE = sqrt(10^sum(clfgd3)/input$M_s)
        # 
        # print(total_FSE)
         
         # calculate mass needed to achieve RSD of 0.32
         
        barplot(clfgd3, col = 'lightblue', ylab="Log of Factor")
       
    })
    
    output$FSEPlot <-  renderPlot({
        
        # generate c
        Al <- input$A_l
        Amax <- input$A_max
        c <- input$gamma_m * ((1-Al)^2/(Al)) + input$gamma_s * (1-Al)
        
        
        l <- (Amax - Al)/ (1-Al)
        
        f <- as.numeric(input$f)
        g <- as.numeric(input$g)
        d3 <- input$d^3
        
        clfgd3 <- c(c,l, f, g, d3)
        
        names(clfgd3) <- c("c", 
                           "l", 
                           "f",
                           "g",
                           expression(d^3))
        
        # minimum mass needed to get 0.32 RSD
        minmass = prod(clfgd3)/(0.32^2)
        
        
        
        # draw the histogram with the specified number of bins
        #print(clfgd3)

        clfgd3 <- log10(clfgd3)
        M_s_range <- seq(from = input$M_s[[1]], to = input$M_s[[2]], length = 100)
        total_FSE = sqrt(10^sum(clfgd3)/M_s_range)
        
        #minmass = 10^(sum(clfgd3) - 0.32^2)
        #print(minmass)
        
        # RSD at minimummass
        print(M_s_range[[1]])
        rsdmax = prod(10^(clfgd3))/M_s_range[[1]]
        
        # print(total_FSE)
        plot(M_s_range, total_FSE, type = "l",
             xlab = "Sample Mass (grams)",
             ylab = "Fundamental Error (RSD)",
             ylim =c(0,5), lty = 1, lwd = 2)
        abline(h=0.32, col = "red", lty = 2)
        text(y = 0.4, x = M_s_range[50], "0.32 RSD", col = "red")
        text(y = 4, x = M_s_range[50], paste("Min. mass to achieve \n0.32 RSD:", 
                                               signif(minmass,3), "grams"), col = "darkblue")
        text(y = 2, x = M_s_range[50], paste("RSD at minimum mass:\n",signif(sqrt(rsdmax),3) ))
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
