
shinyServer(function(input, output) {
   code <- reactive({
      code = which(cities$city == input$city)
      
   })
    output$hc1 <- renderHighchart({
       code = code()
       hc = hc_list[[code]]
       hc
      
   })
   
   
})