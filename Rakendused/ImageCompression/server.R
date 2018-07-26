
# Dependencies ------------------------------------------------------------
library(shiny)
library(OpenImageR)
library(grid)
library(tidyverse)


# Global variables --------------------------------------------------------
current_img = NA
transformed_img = NA


# Functions ---------------------------------------------------------------

# shows the original image
plot_image = function(img_dir){
  img = readImage(img_dir)
  # saved as a global variable for other functions
  current_img <<- img
  r = img[,,1]
  g = img[,,2]
  b = img[,,3]
  col = rgb(r, g, b)
  dim(col) = dim(r)
  grid.raster(col, interpolate=FALSE)
}

transform_image = function(img, k, iters){
  # color layers are separated
  r = img[,,1]
  g = img[,,2]
  b = img[,,3]
  # image dimensions for restoring the image
  img_dim = dim(r)
  
  # binding into a dataframe for k-means algorithm
  df = matrix(c(as.vector(r),
                as.vector(g),
                as.vector(b)), ncol = 3, byrow = F) %>% 
    as_data_frame()
  # implementing k-means
  transformation = kmeans(df, k, iter.max = iters)
  # color values are replaced with approximations
  converted = df %>% 
    mutate(cluster = transformation$cluster,
           r = transformation$centers[cluster,1],
           g = transformation$centers[cluster,2],
           b = transformation$centers[cluster,3])
  
  # new color layers are created
  new_r = converted$r
  dim(new_r) = img_dim
  new_g = converted$g
  dim(new_g) = img_dim
  new_b = converted$b
  dim(new_b) = img_dim
  
  # image is created from the new layers
  col = rgb(new_r, new_g, new_b)
  dim(col) = dim(r)
  
  # saved as a global variable for other functions
  transformed_img <<- col
  
  # image is shown
  grid.raster(col, interpolate=FALSE)
}


# returns a dataframe of color channels of the original image
make_main_df = function(){
  r = current_img[,,1]
  g = current_img[,,2]
  b = current_img[,,3]
  # binding into a df
  df = matrix(c(as.vector(r),
                as.vector(g),
                as.vector(b)), ncol = 3, byrow = F) %>% 
    as_data_frame() %>% 
    rename(r = V1,
           g = V2,
           b = V3)
  return(df)
}

# returns a data frame of color channels of transformed image
make_transf_df = function(){
  df = col2rgb(transformed_img, alpha = F) %>%
    matrix(ncol = 3, byrow = T) %>% 
    as_data_frame() %>% 
    mutate(r = V1 / 255,
           g = V2 / 255,
           b = V3 / 255) %>%
    select(r, g, b)
  return(df)
}


# plots the density of color distributions
plot_density = function(name){
  if (name == "main"){
    df = make_main_df()
  }
  else{
    df = make_transf_df()
  }
  df %>% 
    rename(red = r,
           green = g,
           blue = b) %>% 
    gather(channel, "value", c(red, green, blue)) %>% 
    ggplot(aes(value, fill = channel, color = channel)) +
    geom_density(alpha = 0.2) +
    scale_fill_manual(values = c('blue', 'green', 'red')) +
    scale_color_manual(values = c('blue', 'green', 'red')) +
    scale_x_continuous(limits = c(0,1)) +
    facet_wrap(~channel, nrow = 1) +
    theme(legend.position = "none")
}

# shows the difference image
# original image is substracted from the transformed image
plot_difference = function(){
  r_og = current_img[,,1]
  g_og = current_img[,,2]
  b_og = current_img[,,3]
  img_dim = dim(r_og)
  
  # differences are found for all color channels
  df = make_transf_df() %>% 
    mutate(red = r_og,
           green = g_og,
           vlue = b_og,
           r = 1-abs(r-r_og),
           g = 1-abs(g-g_og),
           b = 1-abs(b-b_og)) %>% 
    select(r,g,b)
  
  # new color layers are created
  new_r = df$r
  dim(new_r) = img_dim
  new_g = df$g
  dim(new_g) = img_dim
  new_b = df$b
  dim(new_b) = img_dim
  
  # image is created from the new layers
  col = rgb(new_r, new_g, new_b)
  dim(col) = img_dim
  
  # final image
  grid.raster(col, interpolate=FALSE)
}

# returns the means of all color channels
mean_table = function(name){
  if (name == "main"){
    df = make_main_df()
  }
  else{
    df = make_transf_df()
  }
  df %>% 
    summarise(red = mean(r),
              green = mean(g),
              blue = mean(b))
}

# returns the variances of all color channels
var_table = function(name){
  if (name == "main"){
    df = make_main_df()
  }
  else{
    df = make_transf_df()
  }
  df %>% 
    summarise(red = var(r),
              green = var(g),
              blue = var(b))
}

# returns the dataframe consisting of pixel differences
# between the original and transformed image
diff_df = function(){
  df_main = make_main_df()
  df_new = make_transf_df()
  df_main %>%
    mutate(r_new = df_new$r,
           g_new = df_new$g,
           b_new = df_new$b,
           r_diff = r - r_new,
           g_diff = g - g_new,
           b_diff = b - b_new,
           r_abs = abs(r_diff),
           g_abs = abs(g_diff),
           b_abs = abs(b_diff)) %>% 
    select(r_diff, g_diff, b_diff, r_abs, g_abs, b_abs)
}


# returns the mean difference for all color channels
diff_mean = function(){
  df = diff_df()
  df %>% 
    summarise(red = mean(r_diff),
              green = mean(g_diff),
              blue = mean(b_diff))
}

# returns the mean absolute difference for all color channels
diff_abs_mean = function(){
  df = diff_df()
  df %>% 
    summarise(red = mean(r_abs),
              green = mean(g_abs),
              blue = mean(b_abs))
}

# returns the variance of difference for all color channels
diff_var = function(){
  df = diff_df()
  df %>% 
    summarise(red = var(r_diff),
              green = var(g_diff),
              blue = var(b_diff))
}

# returns the variance of absolute difference for all color channels
diff_abs_var = function(){
  df = diff_df()
  df %>% 
    summarise(red = var(r_abs),
              green = var(g_abs),
              blue = var(b_abs))
}


# Server ------------------------------------------------------------------

shinyServer(function(input, output) {
   
  output$image = renderPlot({
    in_file = input$img_file
    if (is.null(in_file)){
      return(NULL)
    }
    plot_image(in_file$datapath)
  })
  
  output$transform = renderPlot({
    k = input$k_value
    iters = input$max_iters
    if (typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    transform_image(current_img, k, iters)
  })
  
  output$ogDist = renderPlot({
    in_file = input$img_file
    if (typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    plot_density("main")
  })
  
  output$newDist = renderPlot({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA)) {
      return(NULL)
    }
    plot_density("alt")
  })
  
  output$diff = renderPlot({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA) || typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    plot_difference()
  })
  
  output$og_mean = renderTable({
    in_file = input$img_file
    if (typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    mean_table("main")
  }, digits = 4)
  
  output$og_var = renderTable({
    in_file = input$img_file
    if (typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    var_table("main")
  }, digits = 4)
  
  output$new_mean = renderTable({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA)) {
      return(NULL)
    }
    mean_table("alt")
  }, digits = 4)
  
  output$new_var = renderTable({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA)) {
      return(NULL)
    }
    var_table("alt")
  }, digits = 4)
  
  output$diff_mean = renderTable({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA) || typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    diff_mean()
  }, digits = 4)
  
  output$diff_var = renderTable({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA) || typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    diff_var()
  }, digits = 4)
  
  output$diff_abs_mean = renderTable({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA) || typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    diff_abs_mean()
  }, digits = 4)
  
  output$diff_abs_var = renderTable({
    k = input$k_value
    iters = input$max_iters
    if (typeof(transformed_img) == typeof(NA) || typeof(current_img) == typeof(NA)) {
      return(NULL)
    }
    diff_abs_var()
  }, digits = 4)
  
})
