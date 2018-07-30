library(shiny)
library(shinydashboard)
library(rhandsontable)
library(plyr)
library(dplyr)
library(ggplot2)


dashboardPage(
  
  dashboardHeader(title = "ZW graphs"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("File upload", tabName = "fileUpload", icon = icon("file")),
      menuItem("Sample grouping", tabName = "sampleGroups", icon = icon("object-group")),
      menuItem("Distribution graphs", tabName = "distribution", icon = icon("signal")),
      menuItem("Cumulative graphs", tabName = "cumulative", icon = icon("signal"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      ### File upload / download
      tabItem( tabName = "fileUpload",
               h1("1) Upload files"), br(),
               helpText(p("Select and upload the raw data files containing size porfile and zeta potential (ZP) measurements.",
                          "All uploaded files and the corresponding characteristics will appear in the table below.",
                          "The table allows for a quick overview of the files that have been uploaded and enables to verify ",
                          "the matching of size and ZP profile files belonging to the same sample."),
                        p("After the files have been uploaded, the merged data sets can be downloaded using the download tool."),
                        p("Note that size profile files can be analyzed independently, however, ZP files require matching size ",
                          "profile files to be uploaded as well, since they do not contain total particle number or dilution ",
                          "factor data")),
               
               fluidRow(
                 column(5, 
                        fileInput("files", h3("File input"), multiple = TRUE)
                        
                 ),
                 column(5,
                        uiOutput("downloadPanelHeader"),
                        uiOutput("downloadPanelDropdown"),
                        uiOutput("downloadPanelButton")
                   )
               ),
               h3("Uploaded files"),
               helpText(p("The table below displays the characteristics and statuses of the uploaded files."),
                        p("Columns ", strong("sampleName"), " and ", strong("dilutionFactor"), "can be amended.",
                          "So, if the automatically generated sample names are not acceptable or do not allow ",
                          "size and ZP profiles to be matched automatically, they can be changed. It is advised to ",
                          "keep the sample names minimalistic and unique. Also, make sure that size profile and ",
                          "ZP data files match if both are present for the given sample.",
                          "Dilution factors can be changed as well."),
                        p("All changes made will aslo take affect when the merged data sets are downloaded.")), br(),
               
               rHandsontableOutput("inputFilesTable"), br(),
               h4("Table options"),
               actionButton("clear", "Clear"), actionButton("undo", "Undo manual changes"),
               br()
      ),
      
      ### Sample grouping
      tabItem( tabName = "sampleGroups",
               h1("2) Sample grouping"), br(),
               helpText(p("The following two table enable sample assignment to experimental groups."),
                        p("The ", strong("first set of experimental groups"), " is mandatory for data visualization ",
                          "in the following panels. By default, all samples are assigned to one experimental ",
                          "group labeled 'Group1'."),
                        p("The ", strong("second set of experimental groups"), " is optional. By default, no samples are assgined ",
                          "to any groups. The second set can be used to further improve data visualization."),
                        p("Experimental groups can be added and removed by inserting rows into the tables (right click on the table)"),
                        p("Group names can be changed by clicking on the corresponding cell"),
                        p("Samples are added and removed to groups by ticking the corresponding boxes in group rows in the table. ",
                          "One sample can be assigned to multiple groups")
               ),
               
               uiOutput("sampleGroupsBox1"),
               br(), br(),
               
               uiOutput("sampleGroupsBox2")
               
      ),
      
      ### Distribution plots
      tabItem( tabName = "distribution",
               h1("3) Distribution graphs"),
               br(),
               helpText("Various size and ZP distribution graphs can be generated using the options listed below the grap"),
               br(),
               
               plotOutput("plot1", height = 600), br(),
               
               box(
                 title = strong("Main options"),
                 fluidRow(
                   column(2,
                          actionButton("plot1_refresh", "Refresh"),
                          helpText("If changes are made to uploaded files or sample groupings, refresh the graph for changes to take effect.")
                   ),
                   column(2,
                          radioButtons("plot1_dataSetType", 
                                       h4("Data set"), 
                                       choices = list("Size profile data" = 1, 
                                                      "ZP profile data" = 2),
                                       selected = 1),
                          helpText("If ZP files are not uploaded, the ZP data is unavailable.")
                   ),
                   column(2,
                          radioButtons("plot1_dataSetFormat", 
                                       h4("Data format"), 
                                       choices = list("Absolute numbers" = 1, 
                                                      "Relative numbers" = 2),
                                       selected = 1),
                          helpText("If 'Relative numbers' is selected, frequency of particles is displayed instead of total numbers.")
                   ),
                   column(6,
                          conditionalPanel(
                            condition = "input.plot1_dataSetType == 1",
                            sliderInput("plot1_range_size", "Range of nm displayed",
                                        min = 15, max = 5985, step = 30, value = c(15, 495))
                          ),
                          conditionalPanel(
                            condition = "input.plot1_dataSetType == 2",
                            sliderInput("plot1_range_zp", "Range of mV displayed",
                                        min = -135, max = 130, step = 5, value = c(-135, 130))
                          )
                   )
                 ),
                 width = NULL
               ),
               br(),
               
               box(title = strong("Customisation options"),
                   fluidRow(
                     column(2,
                            radioButtons("plot1_groupSettings", 
                                         h4("Experimental groups"), 
                                         choices = list("Primary" = 1, 
                                                        "Secondary" = 2,
                                                        "Both" = 3,
                                                        "None" = 4),
                                         selected = 1),
                            helpText("Sets the grouping data used for graphs.",
                                     "Selection of secondary experimental groups is ignored if it has not been assigned.")
                     ),
                     conditionalPanel(
                       condition = "input.plot1_groupSettings != 4",
                       column(2,
                              radioButtons("plot1_sampleSettings", h4("Sample grouping settings"),
                                           choices = list("Grouped samples" = 1,
                                                          "Individual samples" = 2),
                                           selected = 2),
                              helpText("Determines whether each sample is displayed separately or merged into a group.")
                       ),
                       column(2,
                              radioButtons("plot1_facetSettings", h4("Plot faceting settings"),
                                           choices = list("Facet primary experimental groups" = 1,
                                                          "Facet secondary experimental groups" = 2,
                                                          "Facet experimental groups" = 3,
                                                          "No faceting" = 4),
                                           selected = 4),
                              helpText("Faceting enables groups to be separated unto sub-graphs.",
                                       "Faceting of secondary experimental groups is ignored if it has not been assigned.")
                       ),
                       column(2,
                              radioButtons("plot1_colorSettings", h4("Plot coloring settings"),
                                           choices = list("Color primary experimental groups" = 1,
                                                          "Color secondary experimental groups" = 2,
                                                          "Color samples separately" = 3),
                                           selected = 3),
                              helpText("Coloring enables groups to be separated by color",
                                       "Coloring of secondary experimental groups is ignored if it has not been assigned.",
                                       "Samples can only be colored separately if samples are displayed individually ",
                                       "(Sample grouping settings).")
                       ),
                       column(2,
                              radioButtons("plot1_styleSettings", h4("Plot style settings"),
                                           choices = list("Lineplot" = 1,
                                                          "Barplot" = 2,
                                                          "Scatterplot" = 3),
                                           selected = 1)
                       ),
                       column(2, 
                              conditionalPanel(
                                condition = "input.plot_1sampleSettings == 1",
                                radioButtons("plot1_errorBarSettings", h4("Error bar settings"),
                                             choices = list("Standard deviation (SD)" = 1, 
                                                            "Standard error of the mean (SEM)" = 2,
                                                            "None" = 3),
                                             selected = 1),
                                helpText("If samples are aggregated into groups, error bars can be added to the means.")
                                
                              )
                       )
                     )
                   ),
                   width = NULL
               )
      ),
      
      ### Cumulative plots
      tabItem( tabName = "cumulative",
               h1("3) Cumulative graphs"),
               br(),
               helpText("Various size and ZP cumulative graphs can be generated using the options listed below the grap"),
               br(),
               
               plotOutput("plot2", height = 600), br(),
               
               box(
                 title = strong("Main options"),
                 fluidRow(
                   column(2,
                          actionButton("plot2_refresh", "Refresh"),
                          helpText("If changes are made to uploaded files or sample groupings, refresh the graph for changes to take effect.")
                          
                   ),
                   column(2,
                          radioButtons("plot2_dataSetType", 
                                       h4("Data set"), 
                                       choices = list("Size profile data" = 1, 
                                                      "ZP profile data" = 2),
                                       selected = 1),
                          helpText("If ZP files are not uploaded, the ZP data is unavailable.")
                          
                   ),
                   column(6,
                          conditionalPanel(
                            condition = "input.plot2_dataSetType == 1",
                            sliderInput("plot2_range_size", "Range of nm displayed",
                                        min = 15, max = 5985, step = 30, value = c(15, 495))
                          ),
                          conditionalPanel(
                            condition = "input.plot2_dataSetType == 2",
                            sliderInput("plot2_range_zp", "Range of mV displayed",
                                        min = -135, max = 130, step = 5, value = c(-135, 130))
                          )
                   )
                 ),
                 width = NULL
               ),
               br(),
               
               box(title = strong("Customisation options"),
                   fluidRow(
                     column(2,
                            radioButtons("plot2_groupSettings", 
                                         h4("Experimental groups"), 
                                         choices = list("Primary" = 1, 
                                                        "Secondary" = 2,
                                                        "Both" = 3,
                                                        "None" = 4),
                                         selected = 1),
                            helpText("Sets the grouping data used for graphs.",
                                     "Selection of secondary experimental groups is ignored if it has not been assigned.")
                     ),
                     conditionalPanel(
                       condition = "input.plot2_groupSettings != 4",
                       column(2,
                              radioButtons("plot2_sampleSettings", h4("Sample grouping settings"),
                                           choices = list("Grouped samples" = 1,
                                                          "Individual samples" = 2),
                                           selected = 2),
                              helpText("Determines whether each sample is displayed separately or merged into a group.")
                              
                       ),
                       column(2,
                              radioButtons("plot2_facetSettings", h4("Plot faceting settings"),
                                           choices = list("Facet primary experimental groups" = 1,
                                                          "Facet secondary experimental groups" = 2,
                                                          "Facet experimental groups" = 3,
                                                          "No faceting" = 4),
                                           selected = 4),
                              helpText("Faceting enables groups to be separated unto sub-graphs.",
                                       "Faceting of secondary experimental groups is ignored if it has not been assigned.")
                       ),
                       column(2,
                              radioButtons("plot2_colorSettings", h4("Plot coloring settings"),
                                           choices = list("Color primary experimental groups" = 1,
                                                          "Color secondary experimental groups" = 2,
                                                          "Color samples separately" = 3),
                                           selected = 3),
                              helpText("Coloring enables groups to be separated by color",
                                       "Coloring of secondary experimental groups is ignored if it has not been assigned.",
                                       "Samples can only be colored separately if samples are displayed individually ",
                                       "(Sample grouping settings).")
                       ),
                       column(2, 
                              conditionalPanel(
                                condition = "input.plot2_sampleSettings == 1",
                                radioButtons("plot2_errorBarSettings", h4("'Error hue' settings"),
                                             choices = list("Standard deviation (SD)" = 1, 
                                                            "Standard error of the mean (SEM)" = 2,
                                                            "None" = 3),
                                             selected = 1),
                                helpText("If samples are aggregated into groups, 'error' hues can be added to the means.")
                                
                              )
                       )
                     )
                   ),
                   width = NULL
               )
      )
    )
  )
)
