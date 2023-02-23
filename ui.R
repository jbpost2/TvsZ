###########################################################################
##R Shiny App to visualize t vs Z scores 
###########################################################################
  
library(shiny)
library(shinydashboard)

# Define UI for application that displays an about page and the app itself

dashboardPage(skin="red",
  #add title
  dashboardHeader(title="Comparing Standard Normal Random Variables & the (central) t Distribution",titleWidth=750),
              
  #define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("archive")),
    menuItem("Application", tabName = "app", icon = icon("laptop"))
  )),
              
  #define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
        fluidRow(
          #add in latex functionality if needed
          withMathJax(),
                
          #two columns for each of the two items
          column(6,
            #Description of App
            h1("What does this app do?"),
            #box to contain description
            box(background="red",width=12,
              h4("This application visualizes the standard normal distribution and t distributions with user specified degrees of freedom."),
              h4("Probabilities can be calculated from each distribution and compared.")
            )
          ),
                
          column(6,
            #How to use the app
            h1("How to use the app?"),
            #box to contain description
            box(background="red",width=12,
              h4("The controls for the app are located to the left and the visualization and information are available on the right."),
              h4("The box on the top left has a slider that allows the user to change the degrees of freedom for the t-distribution.  There is a play button on the right which will produce an animation that varies the degrees of freedom from the current degrees of freedom to 50."),
              h4("Below the slider there is a check box.  If checked, probabilities can be specified, the graphs will be shaded appropriately, and output will be created below the graph on the right displaying the numeric values for the probabilities rounded to six decimal places."),
              h4("The radio buttons that appear can be used to select a less than, between, or greater than probability and boxes show up allowing you to specify the values of interest.")
              )
          )
        )
      ),
      
      #actual app layout      
      tabItem(tabName = "app",        
        fluidRow(
          column(4,
            box(width = 12, background = "red",
              sliderInput("df",h3('Select the Degrees of Freedom for your t-distribution'), min = 1, max = 50, step = 1, value = 1, animate = TRUE)
            ),
            box(width=12,background="red",title=h3("Comparison of Probabilities?"),
                checkboxInput("prob", "Find a probability"),
                conditionalPanel('input.prob', 
                                 radioButtons("probType", label = "Type of Calculation", list("Less than (or equal)" = "Less", "Between" = "Between", "Greater than (or equal)" = "Greater")),
                                 conditionalPanel('input.probType == "Less"',
                                                 numericInput("probL", "Value", value = 0, step = 0.1)),
                                 conditionalPanel('input.probType == "Between"',
                                                 numericInput("probLB", "Lower endpoint", value = -1, step = 0.1),
                                                 numericInput("probUB", "Upper endpoint", value = 1, step = 0.1)),
                                 conditionalPanel('input.probType == "Greater"',
                                                 numericInput("probU", "Value", value = 0, step = 0.1))
                )
            )
          ),
      
          #Show a plot of the distribution  
          column(8,
            fluidRow(
              box(width = 8, plotOutput("distPlot"), height = '100%')
            ), 
            fluidRow(
              conditionalPanel("input.prob", h2("Probability"),
                               box(width = 8, background = "red", uiOutput("text"))
              )
            )
          )
        )
      )
    )
  )
)


