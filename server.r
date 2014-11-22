suppressPackageStartupMessages(library(googleVis))
require(shiny)

library(datasets)

uscrime = USArrests

uscrime = cbind(rep(0, length(rownames(uscrime))), uscrime)
colnames(uscrime)[1] = "subcrime"

uscrime = cbind(rownames(USArrests), uscrime)
colnames(uscrime)[1] = "State"

shinyServer(function(input, output) {
  
  myPerc <- reactive({
    list(
    input$Perc
    )
  })

  myOptions <- reactive({
    list(
      page='disable',
      pageSize=50,
      width=600,
      sortColumn=1,
      sortAscending=FALSE
    )
  })
  
  output$myTable <- renderGvis({
    
    if (input$murder) {
      type = "Murder"
      uscrime$subcrime = uscrime$Murder
    }
    else {
      type = ""
      uscrime$subcrime=rep(0, length(rownames(uscrime)))
    }
    if (input$assault) {
      if (type == "") type = paste ("Assault")
      else type = paste (type, "+Assault", sep="")
      uscrime$subcrime = uscrime$subcrime + uscrime$Assault
    }
    if (input$rape) {
      type = paste(type, ", Rape")
      uscrime$subcrime = uscrime$subcrime + uscrime$Rape
    }
    if (type == "") {
      type = "Murder+Assault+Rape"
      uscrime$subcrime = uscrime$Murder + uscrime$Assault + uscrime$Rape
    }
    uscrime[uscrime$subcrime >= quantile(uscrime$subcrime, c(as.numeric(myPerc())/100)),]$subcrime = uscrime[uscrime$subcrime >= quantile(uscrime$subcrime, c(as.numeric(myPerc())/100)),]$subcrime
    uscrimeTable = uscrime[uscrime$subcrime >= quantile(uscrime$subcrime, c(as.numeric(myPerc())/100)),]
    names(uscrimeTable)[2] = paste("Total =", type)
    gvisTable(uscrimeTable,options=myOptions())         
  })

  output$perc <- renderText({
    paste("States in the", myPerc(), "Percentile Crime Rate")
  })
  
  output$percsub1 <- renderText({
    paste("(per 100,000)")
  })
  
  output$percsub2 <- renderText({

      if (input$murder) {
        type = paste ("Murder")
      } else {
        type = ""
      }
      if (input$assault) {
        if (type == "") {
          type = "Assault"
        } else {
          type = paste (type, "+Assault", sep="")
        }
      }
      if (input$rape) {
        if (type == "") {
          type = "Rape"
        } else {
          type = paste(type, "+Rape", sep = "")
        }
      }
      if (type == "") type = "Murder+Assault+Rape"
#    names(uscrime)[2] = type
    paste(type)
  })


  output$percsub3 <- renderText({
    paste("Use filters on the left to choose the crime types and set the percentile")
  })
  output$gvis <- renderGvis({
      
      if (input$murder) {
        type = "Murder"
        uscrime$subcrime = uscrime$Murder
      }
      else {
        type = ""
        uscrime$subcrime=rep(0, length(rownames(uscrime)))
      }
      if (input$assault) {
        type = paste (type, ", Assault")
        uscrime$subcrime = uscrime$subcrime + uscrime$Assault
      }
      if (input$rape) {
        type = paste(type, ", Rape")
        uscrime$subcrime = uscrime$subcrime + uscrime$Rape
      }
      if (type == "") {
        type = "Murder+Assault+Rape"
        uscrime$subcrime = uscrime$Murder + uscrime$Assault + uscrime$Rape
      }

    uscrime[uscrime$subcrime < quantile(uscrime$subcrime, c(as.numeric(myPerc())/100)),]$subcrime = rep(0, length(uscrime[uscrime$subcrime < quantile(uscrime$subcrime, c(as.numeric(myPerc())/100)),]$subcrime))
    gvisGeoChart(uscrime,
                 locationvar="State", colorvar="subcrime",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=500, height=400,
                              colorAxis="{colors:['white', 'yellow', 'red']}"
                 )) 
  })
})