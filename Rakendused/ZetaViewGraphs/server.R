library(shiny)
library(shinydashboard)
library(rhandsontable)
library(plyr)
library(dplyr)
library(ggplot2)

source("zwGraphs_objects_functions.R")


### Server

function(input, output) {
  
  inputData_reactive <- reactiveValues(data = inputData.display.empty)
  
  observeEvent(input$files, {
    add_input_data(input$files)
    inputData_reactive$data <- inputData
  })
  
  observeEvent(input$clear, { 
    inputData <<- inputData.empty
    inputData_reactive$data = inputData.display.empty
    
    dilutionFactorList <<- list()
    names(dilutionFactorList) <<- c()
    particleNoList <<- list()
    names(particleNoList) <<- c()
    
    inputData.noManual <<- inputData.empty
  })
  
  observeEvent(input$undo, {
    
    if (inputData_reactive$data[1,1] == "") {
      inputData_reactive$data <- inputData.display.empty
    } else {
      inputData_reactive$data <- inputData.noManual
    }
    
  })
  
  output$inputFilesTable <- renderRHandsontable({
    
    inputData_displayTable <- inputData_reactive$data %>%
      select(sampleName, fileType, status, warnings, dilutionFactor, measuredParticles, fileName)
    
    rhandsontable(inputData_displayTable) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_col("fileType", readOnly = TRUE) %>%
      hot_col("status", readOnly = TRUE) %>%
      hot_col("warnings", readOnly = TRUE) %>%
      hot_col("measuredParticles", readOnly = TRUE) %>%
      hot_col("fileName", readOnly = TRUE)
    
  })
  
  observeEvent(input$inputFilesTable, {
    if (nrow(inputData) > 0) {
      modifiedTable <- hot_to_r(input$inputFilesTable)
      inputData$sampleName <<- modifiedTable[,1]
      inputData$dilutionFactor <<- modifiedTable[,5]
      
      refresh_input_data(inputData)
      inputData_reactive$data <- inputData
    }
  })
  
  downloadDataSet <- reactive({
    switch(input$dataset,
           "Size" = sizeDF,
           "ZP" = zpDF,
           "Size frequency" = sizeFreqDF,
           "ZP frequency" = zpFreqDF)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(downloadDataSet(), file, row.names = FALSE)
    }
  )
  
  # Data set download UI render
  observeEvent(input$files, {
    
    sizeDF <<- make_size_table(inputData)
    sizeFreqDF <<- make_size_freq_table(inputData)
    
    if (!is.null(sizeDF)) {
      zpDF <<- make_zp_table(inputData)
      zpFreqDF <<- make_zp_freq_table(inputData)
    }
    
    output$downloadPanelHeader <- renderUI({
      h3("Download merged data")
    })
    output$downloadPanelDropdown <- renderUI({
      selectInput("dataset", "Choose a dataset:",
                  choices = c("", "Size", "ZP", "Size frequency", "ZP frequency"),
                  selected = "")
    })
    output$downloadPanelButton <- renderUI({
      downloadButton("downloadData", "Download")
    })
    
  })
  
  
  ### Groups tab
  
  groups1_reactive <- reactiveValues(data = DFgroups1.empty)
  groups2_reactive <- reactiveValues(data = DFgroups2.empty)
  
  observeEvent(input$inputFilesTable, {
    
    req(input$files)
    
    if (!length(inputData) == 0) {
      
      DFgroups1 <<- add_sample_groups(DFgroups1, inputData, defaultStatus = TRUE)
      groups1_reactive$data <- DFgroups1
      
      DFgroups2 <<- add_sample_groups(DFgroups2, inputData, defaultStatus = FALSE)
      groups2_reactive$data <- DFgroups2
      
    }
    
  })
  
  output$groupsTable1 <- renderRHandsontable({
    
    temp.react <- input$group1Reset
    
    rhandsontable(groups1_reactive$data, useTypes = TRUE, readOnly = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  output$groupsTable2 <- renderRHandsontable({
    
    temp.react <- input$group2Reset
    
    rhandsontable(groups2_reactive$data, useTypes = TRUE, readOnly = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  observeEvent(input$group1Reset, {
    
    req(input$files)
    
    DFgroups1 <<- NULL
    DFgroups1 <<- add_sample_groups(DFgroups1, inputData, defaultStatus = TRUE)
    groups1_reactive$data <- DFgroups1
    
  })
  
  observeEvent(input$group2Reset, {
    
    req(input$files)
    
    DFgroups2 <<- NULL
    DFgroups2 <<- add_sample_groups(DFgroups2, inputData, defaultStatus = FALSE)
    groups2_reactive$data <- DFgroups2
    
  })
  
  observeEvent(input$groupsTable1, {
    
    temp.react <- input$files
    
    DFgroups1 <<- as.data.frame(hot_to_r(input$groupsTable1))
    
    if (nrow(DFgroups1) == 0) {
      GroupsList.level1 <<- NULL
    } else {
      GroupsList.level1 <<- convert_groups_table_to_list(DFgroups1)
      
      if (length(GroupsList.level1) == 1 & length(GroupsList.level1[[1]]) == 0)  {
        GroupsList.level1 <<- NULL
      }
    }
    fix_groupings.output <- fix_groupings(GroupsList.level1, GroupsList.level2)
    GroupsList.level1 <<- fix_groupings.output[["list1"]]
    GroupsList.level2 <<- fix_groupings.output[["list2"]]
    
  })
  
  observeEvent(input$groupsTable2, {
    
    temp.react <- input$files
    
    DFgroups2 <<- as.data.frame(hot_to_r(input$groupsTable2))
    
    if (nrow(DFgroups2) == 0) {
      GroupsList.level2 <<- NULL
    } else {
      GroupsList.level2 <<- convert_groups_table_to_list(DFgroups2)
      
      if (length(GroupsList.level2) == 1 & length(GroupsList.level2[[1]]) == 0)  {
        GroupsList.level2 <<- NULL
      }
    }
    fix_groupings.output <- fix_groupings(GroupsList.level1, GroupsList.level2)
    GroupsList.level1 <<- fix_groupings.output[["list1"]]
    GroupsList.level2 <<- fix_groupings.output[["list2"]]
    
  })
  
  # Sample groups table UI render
  observeEvent(input$files, {
    
    output$sampleGroupsBox1 <- renderUI({
      box(
        h4("Primary experimental groups"),
        br(),
        rHandsontableOutput("groupsTable1"), br(),
        actionButton("group1Reset", "Reset"),
        br(),
        width = 12
      )
    })
    
    output$sampleGroupsBox2 <- renderUI({
      box(
        h4("Secondary experimental groups (optional)"),
        br(),
        rHandsontableOutput("groupsTable2"), br(),
        actionButton("group2Reset", "Reset"),
        br(),
        width = 12
      )
    })
    
  })
  
  
  ### Distribution graphs
  
  output$plot1 <- renderPlot({
    
    temp.react <- input$plot1_refresh
    
    if (input$plot1_dataSetType == "1") {
      rangeStart <- input$plot1_range_size[1]
      rangeStop <- input$plot1_range_size[2]
    } else {
      rangeStart <- input$plot1_range_zp[1]
      rangeStop <- input$plot1_range_zp[2]
    }
    
    if (!length(inputData) == 0) {
      
      plotData <- choose_distribution_plot_data_set(input$plot1_dataSetType, input$plot1_dataSetFormat)
      
      plotFunctionParameters <- translate_distribution_plot_parameters(
        input$plot1_groupSettings, 
        input$plot1_sampleSettings, 
        input$plot1_errorBarSettings, 
        input$plot1_facetSettings,
        input$plot1_colorSettings, 
        input$plot1_styleSettings, 
        GroupsList.level1, GroupsList.level2)
      
      p1 <- plotDistribution(plotData, 
                             groups = plotFunctionParameters[["groups"]],  
                             conditions = plotFunctionParameters[["conditions"]], 
                             plotType = plotFunctionParameters[["plotType"]], 
                             plotStyle = plotFunctionParameters[["plotStyle"]],
                             errorBars = plotFunctionParameters[["errorBars"]], 
                             facetByGroup = plotFunctionParameters[["facetByGroup"]], 
                             facetByCondition = plotFunctionParameters[["facetByCondition"]], 
                             colorBy = plotFunctionParameters[["colorBy"]])
      
      p1 + coord_cartesian(xlim = c(rangeStart, rangeStop))
    }
    
  })
  
  ### Additive graphs
  
  output$plot2 <- renderPlot({

    temp.react <- input$plot2_refresh

    if (input$plot2_dataSetType == "1") {
      rangeStart <- input$plot2_range_size[1]
      rangeStop <- input$plot2_range_size[2]
    } else {
      rangeStart <- input$plot2_range_zp[1]
      rangeStop <- input$plot2_range_zp[2]
    }

    if (!length(inputData) == 0) {

      plotData <- choose_additive_plot_data_set(input$plot2_dataSetType)

      plotFunctionParameters <- translate_additive_plot_parameters(
        input$plot2_groupSettings,
        input$plot2_sampleSettings,
        input$plot2_errorBarSettings,
        input$plot2_facetSettings,
        input$plot2_colorSettings,
        GroupsList.level1, GroupsList.level2)

      p2 <- plotAdditive(plotData,
                             groups = plotFunctionParameters[["groups"]],
                             conditions = plotFunctionParameters[["conditions"]],
                             type = plotFunctionParameters[["plotType"]],
                             meansOnly = plotFunctionParameters[["meansOnly"]],
                             facetByGroup = plotFunctionParameters[["facetByGroup"]],
                             facetByCondition = plotFunctionParameters[["facetByCondition"]],
                             colorBy = plotFunctionParameters[["colorBy"]])

      p2 + coord_cartesian(xlim = c(rangeStart, rangeStop))
    }

  })
  
}