
setwd("C:/Users/rsoren/Dropbox/projects/inla_smoking_forecast_v2")


library(shiny)
library(dplyr)
require(scales)

shinyServer(function(input, output) {
  
  df_AC_list <- lapply(list.files(path = "data/"), function(x) read.csv(paste0("data/", x)))
  df_AC <- do.call("rbind", df_AC_list) %>% 
    mutate(
      age = age + 10,
      mean_exp = exp(mean),
      lower_exp = exp(lower),
      upper_exp = exp(upper))
  
  output$plot1 <- renderPlot({
    
    iso3var <- input$iso3_input
    agevar <- input$age_input
    sexvar <- ifelse(input$sex_input == "male", 1, 2)
    
#     iso3var <- "CHN"  # for debugging
#     agevar <- 25      # for debugging
#     sexvar <- 2       # for debugging
    
    df2 <- subset(df_AC, iso3 == iso3var & age == agevar & sex == sexvar)
    plot(
      main = paste0(
        "Smoking prevalence forecast (", 
        iso3var, ", age ", agevar, "-", as.numeric(agevar)+4, ", ", ifelse(sexvar==1, "males", "females"), ")"),
      x = NULL, y = NULL, 
      xlim = c(1980, 2040), ylim = c(0, 0.5),
      xlab = "Year", ylab = "Prevalence")
    lines(df2$year, df2$mean_exp)
    lines(2014:2040, df2$lower_exp[df2$year %in% 2014:2040], lty = 2)
    lines(2014:2040, df2$upper_exp[df2$year %in% 2014:2040], lty = 2)
    abline(v = 2013, col = "lightgray")
  })
  
  output$plot2 <- renderPlot({
    
    iso3var <- input$iso3_input2
    
          coh <- 2003 # for debugging
          iso3var <- "CHN" # for debugging
    
    df3 <- subset(df_AC, iso3 == iso3var & sex == sexvar)
    df3$cohort <- df3$year - df3$age
    
    par(xpd = TRUE)
    
    plot(
      NULL, NULL, xlim = c(0, 80), ylim = c(0, 0.5),
      main = paste0("Age-specific smoking prevalence by cohort (", iso3var, ", ", ifelse(sexvar==1, "males", "females"), ")"),
      xlab = "Age", ylab = "Prevalence"
    )
    
    
    color1 <- "#B26D00"
    color2 <- "#0964B2"
    transparencyLevel <- 0.8
    lineWidth <- 2
    colorFunction <- colorRampPalette(c(color1, color2))
    colors <- colorFunction(length(1900:2013))
    colorFunction2 <- colorRampPalette(c(color2, color1)) #reversed, for legend
    colors2 <- colorFunction2(length(1900:2013)) #reversed, for legend
    names(colors) <- as.character(1900:2013)
    
    #     legend_image <- as.raster(matrix(colors2, nrow=1))
    # #     text(x=5.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
    #     rasterImage(legend_image, xleft = 0, xright = 80, ybottom = -1, ytop = 0)
    
    
    #     for (coh in 1900:2013) {
    for (coh in (input$cohortRange[1]):(input$cohortRange[2])) {
      
      df3_2 <- subset(df3, cohort == coh)
      lineColor <- colors[as.character(coh)
        ]
      nDat <- ifelse(all(is.na(df3_2$mx)), 0, table(is.na(df3_2$mx))[["FALSE"]])
      
      if (input$showActualData) { 
        if (!(nDat == 0)) { 
          lines(
            df3_2[1:nDat, "age"], df3_2[1:nDat, "mean_exp"],
            col = alpha(lineColor, transparencyLevel), lwd = lineWidth
          ) 
        }
      }      
      
      if (input$showForecasts) {
        if (nDat == 0) { 
          lines(
            df3_2[1:nrow(df3_2), "age"], df3_2[nDat:nrow(df3_2), "mean_exp"], 
            col = alpha(lineColor, transparencyLevel), lwd = lineWidth
          ) 
        } else { 
          lines(
            df3_2[nDat:nrow(df3_2), "age"], df3_2[nDat:nrow(df3_2), "mean_exp"], 
            col = alpha(lineColor, transparencyLevel), lwd = lineWidth
          ) 
        }
      }
    }
    
  })
  
})