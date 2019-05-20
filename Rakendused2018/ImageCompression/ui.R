library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tabsetPanel(type = "tabs",
              tabPanel("About",
                       h2("Image compression application"),
                       h3("Created by Herman Aadamsoo"),
                       p("The purpose of the application is to give an overview of using the
                         k-means clustering algorithm for image compression. A user must load
                         an image and is then able to choose k which represents the number of
                         different colors used for the transformed image."),
                       p("The chosen image must be in .png format and
                         must have three color layers (red, green, blue) and one alpha layer.
                         The initial imported image contains a number of pixels and each pixel contains
                         one value for each color layer ranging from 0 to 1."),
                       p("(0,0,0) is black"),
                       p("(1,1,1) is white"),
                       p("(1,0,0) is red"),
                       p("(0,1,0) is green"),
                       p("(0,0,1) is blue"),
                       p("Given a natural number k, the k-means clustering algorithm divides all
                         colors into k clusters and chooses an optimal representation color (mean
                         color) for each cluster. Each pixel in the initial image is clustered and
                         its value is set to be the clusters representative value."),
                       h3("Image tab"),
                       p("The user can upload an image, choose the number of different colors (k) and
                         maximal number of iterations for the k-means clustering algorithm. The user
                         can also see both the initial image and the transformed image in this tab."),
                       h3("Overview tab"),
                       p("This tab gives an overview of the color distribution, mean color values and
                         color variances for both initial and transformed image"),
                       h3("Difference"),
                       p("In this tab the difference between the initial and transformed image is
                         shown."),
                       p("On top of the page lays the difference as an image. For each pixel the
                         value is calculated  as P=1-|P_O - P_T| where P is the pixels value, P_O
                         the pixels value on the original image and P_T is the pixels value on the
                         transformed image."),
                       p("On the left hand side the mean and variance is shown for each color
                         channel. The difference of each pixel is calculated as P=P_O - P_T where
                         P_O is the pixels original value and P_T is the transformed value."),
                       p("On the right hand side the mean and variance of absolute difference
                         are displayed. Each pixel represents the absolute value betweeen the original
                         and transformed pixel")
              ),
              tabPanel("Images",
                       fluidRow(
                         column(width = 5,
                                align = "center",
                                h2("Initial image"),
                                plotOutput("image")
                          ),
                          column(width = 5,
                                align = "center",
                                h2("Transformed image"),
                                plotOutput("transform")
                          )
                        ),
                       fluidRow(
                         column(3,
                                fileInput("img_file", "Choose image file"
                                )
                         ),
                         column(3,
                                numericInput("k_value", "Choose k value",
                                             value = 10,
                                             min = 5,
                                             max = 50,
                                             step = 1)
                         ),
                         column(3,
                                sliderInput("max_iters", "Max. number of iterations",
                                            value = 10,
                                            min = 10,
                                            max = 100,
                                            step = 1)
                         )
                         
                       )
              ),
              tabPanel("Overview",
                       fluidRow(
                         column(width = 5,
                                align = "center",
                                h2("Original color distribution"),
                                plotOutput("ogDist"),
                                h3("Mean color values"),
                                tableOutput("og_mean"),
                                h3("Color variance"),
                                tableOutput("og_var")
                                ),
                         column(width = 5,
                                align = "center",
                                h2("New color distribution"),
                                plotOutput("newDist"),
                                h3("Mean color values"),
                                tableOutput("new_mean"),
                                h3("Color variance"),
                                tableOutput("new_var")
                                )
                       )
              ),
              tabPanel("Difference",
                       fluidRow(
                         column(width = 8,
                                align = "center",
                                h2("Image substraction"),
                                plotOutput("diff")
                          )
                        ),
                       fluidRow(
                         column(
                           width = 4,
                           align = "center",
                           h3("Difference means"),
                           tableOutput("diff_mean"),
                           h3("Difference variances"),
                           tableOutput("diff_var")
                         ),
                         column(
                           width = 4,
                           align = "center",
                           h3("Abs. difference means"),
                           tableOutput("diff_abs_mean"),
                           h3("Abs. difference variances"),
                           tableOutput("diff_abs_var")
                         )
                       )
  )),
  mainPanel(
  )
))
