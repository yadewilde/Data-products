# server.R

require('rCharts')
require('shiny')
require('shinyapps')
require('reshape2')
require('reshape')
options(RCHART_LIB = 'polycharts')

# Load the data 
outcome <- read.table("data/outcome.csv", sep=",", header=T)
outcome$Date <- as.character(outcome$Date)
outcome$Date <- strptime(outcome$Date,  "%Y-%m-%d")
outcome$Date <- as.Date(outcome$Date)
outcome$X <- NULL
row.names(outcome) <- NULL
outcome$Indicator <- as.character(outcome$Indicator)
outcome$Measure <- as.character(outcome$Measure)
# outcome$Indicator <- iconv(outcome$Indicator, "ISO_8859-2", "UTF-8")
# outcome$Measure <- iconv(outcome$Measure, "ISO_8859-2", "UTF-8")
# save(outcome,file="outcome.Rda")
# load("data/outcome.Rda")



shinyServer(
  function(input, output) {
    
    
    
    #     output$comp <- renderText({input$Comparaison})    
    #     output$time <- renderText({input$Time})     
    #     output$stat <- renderText({input$Statistic}) 
    #     output$technol <- renderText({input$Technologie}) 
    #     output$dir <- renderText({input$Direction}) 
    
    #     outcome <- reactive({
    #       
    #       outcome <- read.csv("outcome.csv", header=T, sep=",")
    #       
    #       return(outcome)
    #     })
    #     
    
    
    # Internet par opérateur  
    
    Measure = reactive({
      if(input$Indicator == 'delai de fourniture'){Measure <- input$Measure1}
      if(input$Indicator == 'taux de pannes'){Measure <- "nombre de pannes par ligne"}
      if(input$Indicator == 'delai de reparation'){Measure <- input$Measure3}
      if(input$Indicator == 'temps de reponse'){Measure <- input$Measure4}
      if(input$Indicator == 'plaintes - facturation'){Measure <- "% de factures"}
      if(input$Indicator == 'plaintes - vitesse'){Measure <- "% de lignes"}
      if(input$Indicator == 'taux de couverture'){Measure <- input$Measure7}
      return(Measure)
      
    })
    
    service = reactive({
      service <- "Internet access"
      if (input$Indicator %in% c("temps de reponse", "plaintes - facturation")){service <- "N/A"}
      return(service)
    })
    
    isp = reactive({
      op <- sort(input$ISP)
      if (op == "Selection" & length(input$ISP2)==0){op <- c("Proximus", "Telenet", "Numericable", "Voo")}
      if (op == "Selection" & !(length(input$ISP2)==0)) op <- input$ISP2
      if (op != "Selection"){
        
        
        if (op == "All") {op <- c("Proximus", "Telenet", "Numericable", "Voo")}} 
      
      
      return(op)     
    })
    
    output$text1 <- renderText({ 
      paste(isp(), colors())
    })
    
    
    colors=reactive({
      colors <- sort(isp())
      
      colors[colors=="Telenet"] <-  'rgba(255, 137, 0, 0.9)'
      colors[colors=="Voo"] <- 'rgba(255, 0, 103, 0.9)'
      colors[colors=="Proximus"] <- 'rgba(74, 0, 127, 0.9)'
      colors[colors=="Numericable"] <- 'rgba(110, 190, 0, 1)'
      if(length(colors)==1){colors<- c(colors, 'rgba(110, 190, 0, 1)')}
      
      
      return (colors)
    }) 
    
    
    
    
    
    date = reactive({
      if (input$period == "Selection" & (length(input$Period2)==0)) {date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period == "Selection" & !(length(input$Period2)==0))  { date <- as.Date(input$Period2)}
      if (input$period == "toutes"){date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period == "dernière"){date <- as.Date(strptime(as.character("2015-09-30"),"%Y-%m-%d"))}
      return(date)
    })
    
    datelab = reactive({ 
      date <-  as.character(date())
      date[date=="2014-12-31"] <- "T4 2014"
      date[date=="2015-03-30"] <- "T1 2015"
      date[date=="2015-06-30"] <- "T2 2015"
      date[date=="2015-09-30"] <- "T3 2015"
      
      return(date)
    })
    
    
    IntPerOp = reactive({
      
      Internet <- outcome[outcome$Operator %in% isp() & outcome$Service == service() 
                          & outcome$Measure==Measure() & outcome$Date %in% date(), ]
      Internet$Date <-  format(Internet$Date,"%Y-%m-%d") 
      as.double(as.POSIXct(as.Date(Internet$Date),origin="1970-01-01"))
      
      return(Internet) }) 
    
    
    TabInt = reactive({
      (tabInt <- IntPerOp()[,c("Date", "Operator", "Value") ])    
    

      tabInt2 <- dcast(tabInt, Date ~ Operator, value.var='Value')
      
        
      tabInt2$Date <-  as.character(tabInt2$Date)
      tabInt2$Date[tabInt2$Date=="2014-12-31"] <- "T4 2014"
      tabInt2$Date[tabInt2$Date=="2015-03-30"] <- "T1 2015"
      tabInt2$Date[tabInt2$Date=="2015-06-30"] <- "T2 2015"
      tabInt2$Date[tabInt2$Date=="2015-09-30"] <- "T3 2015"
      colnames(tabInt2)[1] <- "Période"
      
      return(tabInt2)
    })
    
    
    output$tableInternet <- renderTable({
      TabInt()
    })
    
    output$downloadData <- downloadHandler(
      filename = "Quality-Internet.csv",
      content = function(file) {
        write.csv(TabInt(), file)
      }
    )
    
    
    Label  = reactive({
      Label <- Measure()
      if (Label == "% faits a la duree convenue") {Label <- "% faits à la durée convenue"}
      if (Label == "80% les plus rapidement reparees") {Label <- "80% les plus rapidement réparées"}
      if (Label == "95% les plus rapidement reparees") {Label <- "95% les plus rapidement réparées"}
      if (Label == "% a la duree convenue") {Label <- "% à la durée convenue"}
      if (Label == "% de menages - 2G") {Label <- "% de ménages - 2G"}
      if (Label == "% de menages - 3G") {Label <- "% de ménages - 3G"}
      if (Label == "% de menages - 4G") {Label <- "% de ménages - 4G"}
      if (Label == "temps de reponse moyen") {Label <- "temps de réponse moyen"}
      
      return(Label)
    })
    
    
    Title = reactive({
      
      if(input$Indicator=="delai de fourniture"){Title <- "Délai de fourniture du raccordement (jours) pour l'accès à Internet"}
      if(input$Indicator=="taux de pannes"){Title <- "Taux de défaillances signalées par ligne d'accès pour l'accès à Internet"}
      if(input$Indicator=="delai de reparation"){Title <- "Délai de réparation d'une défaillance (jours) pour l'accès à Internet"}
      if(input$Indicator=="temps de reponse"){Title <- "Temps de réponse par le service clientèle de l'opérateur (secondes)"}
      if(input$Indicator=="plaintes - facturation"){Title <- "Plaintes concernant l'exactitude de la facturation"}
      if(input$Indicator=="plaintes - vitesse"){Title <- "Plaintes concernant la vitesse des connexions à  Internet"}
      if(input$Indicator=="taux de couverture  "){Title <- "Taux de couverture du réseau mobile"}
      
      return(Title)
    })
    
    output$Title <- renderText({
      
      Title()})
    
    # Create the chart 
    
    
    
    
    output$graphInt1 <- renderChart2({
      
      
      ChartIntPerOpe <- hPlot(Value ~ Date, group="Operator",data = IntPerOp(), type = "column", title = Title())
      ChartIntPerOpe$yAxis(title = list(text =Label(), style = list(fontSize = '13px')))
      
      ChartIntPerOpe$exporting(enabled = T)
      
    
      ChartIntPerOpe$xAxis(categories = c(datelab(),""))
     
      ChartIntPerOpe$colors(colors())
      
      ChartIntPerOpe$addParams(width = 675, height = 400)
      
      
      return(ChartIntPerOpe)})
    
    
    
    # Téléphonie fixe
    
    Measure_TelF = reactive({
      if(input$Indicator_TelF == 'delai de fourniture'){Measure <- input$Measure1_TelF}
      if(input$Indicator_TelF == 'taux de pannes'){Measure <- "nombre de pannes par ligne"}
      if(input$Indicator_TelF == 'delai de reparation'){Measure <- input$Measure3_TelF}
      if(input$Indicator_TelF == 'temps de reponse'){Measure <- input$Measure4_TelF}
      if(input$Indicator_TelF == 'plaintes - facturation'){Measure <- "% de factures"}
      if(input$Indicator_TelF == 'plaintes - vitesse'){Measure <- "% de lignes"}
      if(input$Indicator_TelF == 'taux de couverture'){Measure <- input$Measure7_TelF}
      return(Measure)
      
    })
    
    service_TelF = reactive({
      service <- "Fixed telephony"
      if (input$Indicator_TelF %in% c("temps de reponse", "plaintes - facturation")){service <- "N/A"}
      return(service)
    })
    
    isp_TelF = reactive({
      isp <- input$ISP_TelF
      if (isp == "All") {isp <- c("Proximus", "Telenet", "Numericable", "Voo")}
      return(isp)      
    })
    
    isp_TelF = reactive({
      op <- input$ISP_TelF
      
      if (op == "Selection" & length(input$ISP2_TelF)==0){isp <- c("Proximus", "Telenet", "Numericable", "Voo")}
      if (op == "Selection" & !(length(input$ISP2_TelF)==0)) isp <- input$ISP2_TelF
      
      if (op != "Selection"){
        
        
        
        if (op == "All") {op <- c("Proximus", "Telenet", "Numericable", "Voo")}} 
      return(op)     
    })
    
    
    colors_TelF=reactive({
      colors <- sort(isp_TelF())
      
      colors[colors=="Telenet"] <-  'rgba(255, 137, 0, 0.9)'
      colors[colors=="Voo"] <- 'rgba(255, 0, 103, 0.9)'
      colors[colors=="Proximus"] <- 'rgba(74, 0, 127, 0.9)'
      colors[colors=="Numericable"] <- 'rgba(110, 190, 0, 1)'
      if(length(colors)==1){colors<- c(colors, 'rgba(110, 190, 0, 1)')}
      
      
      return (colors)
    })
    
    
    date_TelF = reactive({
      
      if (input$period_TelF == "Selection" & (length(input$Period2_TelF)==0)) {date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period_TelF == "Selection" & !(length(input$Period2_TelF)==0))  { date <- as.Date(input$Period2_TelF)}
      
      if (input$period_TelF == "toutes"){date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period_TelF == "dernière"){date <- as.Date(strptime(as.character("2015-09-30"),"%Y-%m-%d"))}
      
      return(date)
      
    })
    
    datelab_TelF = reactive({ 
      
      date <-  as.character(date_TelF())
      date[date=="2014-12-31"] <- "T4 2014"
      date[date=="2015-03-30"] <- "T1 2015"
      date[date=="2015-06-30"] <- "T2 2015"
      date[date=="2015-09-30"] <- "T3 2015"
      
      return(date)
      
    })
    
    IntPerOp_TelF = reactive({
      
      Internet <- outcome[outcome$Operator %in% isp_TelF() & outcome$Service == service_TelF() 
                          & outcome$Measure==Measure_TelF() & outcome$Date %in% date_TelF(), ]
      Internet$Date <-  format(Internet$Date,"%Y-%m-%d") 
      as.double(as.POSIXct(as.Date(Internet$Date),origin="1970-01-01"))
      
      return(Internet) }) 
    
    TabInt_TelF = reactive({
      tabInt <- IntPerOp_TelF()[,c("Date", "Operator", "Value") ]
      
      tabInt2 <- dcast(tabInt, Date ~ Operator, value.var='Value')
      tabInt2$Date <-  as.character(tabInt2$Date)
      tabInt2$Date[tabInt2$Date=="2014-12-31"] <- "T4 2014"
      tabInt2$Date[tabInt2$Date=="2015-03-30"] <- "T1 2015"
      tabInt2$Date[tabInt2$Date=="2015-06-30"] <- "T2 2015"
      tabInt2$Date[tabInt2$Date=="2015-09-30"] <- "T3 2015"
      colnames(tabInt2)[1] <- "Période"
      return(tabInt2)
    })
    
    
    output$table_TelF <- renderTable({
      TabInt_TelF()
    })
    
    output$downloadData_TelF <- downloadHandler(
      filename = "Quality-Tel_Fixe.csv",
      content = function(file) {
        write.csv(TabInt_TelF(), file)
      }
    )
    
    
    
    Label_TelF  = reactive({
      
      Label <- Measure_TelF()
      
      if (Label == "% faits a la duree convenue") {Label <- "% faits à la durée convenue"}
      if (Label == "80% les plus rapidement reparees") {Label <- "80% les plus rapidement réparées"}
      if (Label == "95% les plus rapidement reparees") {Label <- "95% les plus rapidement réparées"}
      if (Label == "% a la duree convenue") {Label <- "% à la durée convenue"}
      if (Label == "% de menages - 2G") {Label <- "% de ménages - 2G"}
      if (Label == "% de menages - 3G") {Label <- "% de ménages - 3G"}
      if (Label == "% de menages - 4G") {Label <- "% de ménages - 4G"}
      if (Label == "temps de reponse moyen") {Label <- "temps de réponse moyen"}
      
      
      return(Label)
    })
    
    
    Title_TelF = reactive({
      
      if(input$Indicator_TelF=="delai de fourniture"){Title <- "Délai de fourniture du raccordement (jours) pour la téléphonie fixe"}
      if(input$Indicator_TelF=="taux de pannes"){Title <- "Taux de défaillances signalées par ligne d'accès pour la téléphonie fixe"}
      if(input$Indicator_TelF=="delai de reparation"){Title <- "Délai de réparation d'une défaillance (jours) pour la téléphonie fixe"}
      if(input$Indicator_TelF=="temps de reponse"){Title <- "Temps de réponse par le service clientèle de l'opérateur (secondes)"}
      if(input$Indicator_TelF=="plaintes - facturation"){Title <- "Plaintes concernant l'exactitude de la facturation"}
      if(input$Indicator_TelF=="plaintes - vitesse"){Title <- "Plaintes concernant la vitesse des connexions à  Internet"}
      if(input$Indicator_TelF=="taux de couverture  "){Title <- "Taux de couverture du réseau mobile"}
      
      return(Title)
    })
    
    output$Title_TelF <- renderText({
      
      Title_TelF()})
    
    # Create the chart 
    
    
    output$graphInt1_TelF <- renderChart2({
      
      ChartIntPerOpe <- hPlot(Value ~ Date, group="Operator",data = IntPerOp_TelF(),type = "column", title = Title_TelF())
      ChartIntPerOpe$yAxis(title = list(text =Label_TelF(), style = list(fontSize = '13px')))
      
      ChartIntPerOpe$exporting(enabled = T)
      #       if (input$period_TelF=="toutes"){
      #         ChartIntPerOpe$xAxis(categories = c("Q4 2014", "Q1 2015","Q2 2015","Q3 2015"))}
      #       
      #       if (input$period_TelF=="dernière"){
      #         ChartIntPerOpe$xAxis(categories = c("Q3 2015", ""))}
      ChartIntPerOpe$xAxis(categories = c(datelab_TelF(),""))
      ChartIntPerOpe$colors(colors_TelF())
      ChartIntPerOpe$addParams(width = 675, height = 400)
      #            
      return(ChartIntPerOpe)})
    
    
    
    # Téléphonie mobile
    
    Measure_TelM = reactive({
      
      if(input$Indicator_TelM == 'plaintes - facturation'){Measure <- "% de factures"}
      if(input$Indicator_TelM == 'plaintes - vitesse'){Measure <- "% de lignes"}
      if(input$Indicator_TelM == 'taux de couverture'){Measure <- "% de menages - 2G"}
      if(input$Indicator_TelM == 'temps de reponse'){Measure <- input$Measure4_TelM}
      return(Measure)
      
    })
    
    service_TelM = reactive({
      service <- "Mobile telephony"
      if (input$Indicator_TelM %in% c("temps de reponse", "plaintes - facturation")){service <- "N/A"}
      
      return(service)
    })
    
    isp_TelM = reactive({
      op <- input$ISP_TelM
      
      if (op == "Selection" & length(input$ISP2_TelM)==0){isp <- c("Proximus", "Telenet", "Numericable", "Voo")}
      if (op == "Selection" & !(length(input$ISP2_TelM)==0)) isp <- input$ISP2_TelM
      
      if (op != "Selection"){
        
        
        
        if (op == "All") {op <- c("Proximus", "Telenet", "Numericable", "Voo")}} 
      return(op)     
    })
    
    
    colors_TelM=reactive({
      colors <- sort(isp_TelM())
      
      colors[colors=="Telenet"] <-  'rgba(255, 137, 0, 0.9)'
      colors[colors=="Voo"] <- 'rgba(255, 0, 103, 0.9)'
      colors[colors=="Proximus"] <- 'rgba(74, 0, 127, 0.9)'
      colors[colors=="Numericable"] <- 'rgba(110, 190, 0, 1)'
      if(length(colors)==1){colors<- c(colors, 'rgba(110, 190, 0, 1)')}
      
      
      return (colors)
    })
    
    date_TelM = reactive({
      
      if (input$period_TelM == "Selection" & (length(input$Period2_TelM)==0)) {date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period_TelM == "Selection" & !(length(input$Period2_TelM)==0))  { date <- as.Date(input$Period2_TelM)}
      
      if (input$period_TelM == "toutes"){date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period_TelM == "dernière"){date <- as.Date(strptime(as.character("2015-09-30"),"%Y-%m-%d"))}
      
      return(date)
      
    })
    
    datelab_TelM = reactive({ 
      
      date <-  as.character(date_TelM())
      date[date=="2014-12-31"] <- "T4 2014"
      date[date=="2015-03-30"] <- "T1 2015"
      date[date=="2015-06-30"] <- "T2 2015"
      date[date=="2015-09-30"] <- "T3 2015"
      
      return(date)
      
    })
    
    
    
    IntPerOp_TelM = reactive({
      
      
      Internet <- outcome[outcome$Operator %in% isp_TelM() & outcome$Service == service_TelM() 
                          & outcome$Measure==Measure_TelM() & outcome$Date %in% date_TelM() , ]
      Internet$Date <-  format(Internet$Date,"%Y-%m-%d") 
      as.double(as.POSIXct(as.Date(Internet$Date),origin="1970-01-01"))
      
      return(Internet) }) 
    
    TabInt_TelM = reactive({
      tabInt <- IntPerOp_TelM()[,c("Date", "Operator", "Value") ]
      
      tabInt2 <- dcast(tabInt, Date ~ Operator, value.var='Value')
      tabInt2$Date <-  as.character(tabInt2$Date)
      tabInt2$Date[tabInt2$Date=="2014-12-31"] <- "T4 2014"
      tabInt2$Date[tabInt2$Date=="2015-03-30"] <- "T1 2015"
      tabInt2$Date[tabInt2$Date=="2015-06-30"] <- "T2 2015"
      tabInt2$Date[tabInt2$Date=="2015-09-30"] <- "T3 2015"
      colnames(tabInt2)[1] <- "Période"
      return(tabInt2)
    })
    
    
    output$table_TelM <- renderTable({
      TabInt_TelM()
    })
    
    output$downloadData_TelM <- downloadHandler(
      filename = "Quality-Tel_Mobile.csv",
      content = function(file) {
        write.csv(TabInt_TelM(), file)
      }
    )
    
    
    
    Label_TelM  = reactive({
      
      Label <- Measure_TelM()
      
      if (Label == "% faits a la duree convenue") {Label <- "% faits à la durée convenue"}
      if (Label == "80% les plus rapidement reparees") {Label <- "80% les plus rapidement réparées"}
      if (Label == "95% les plus rapidement reparees") {Label <- "95% les plus rapidement réparées"}
      if (Label == "% a la duree convenue") {Label <- "% à la durée convenue"}
      if (Label == "% de menages - 2G") {Label <- "% de ménages - 2G"}
      if (Label == "% de menages - 3G") {Label <- "% de ménages - 3G"}
      if (Label == "% de menages - 4G") {Label <- "% de ménages - 4G"}
      if (Label == "temps de reponse moyen") {Label <- "temps de réponse moyen"}
      return(Label)
    })
    
    
    Title_TelM = reactive({
      
      if(input$Indicator_TelM=="delai de fourniture"){Title <- "Délai de fourniture du raccordement (jours) pour la téléphonie fixe"}
      if(input$Indicator_TelM=="taux de pannes"){Title <- "Taux de défaillances signalées par ligne d'accès pour la téléphonie mobile"}
      if(input$Indicator_TelM=="delai de reparation"){Title <- "Délai de réparation d'une défaillance (jours) pour la téléphonie mobile"}
      if(input$Indicator_TelM=="temps de reponse"){Title <- "Temps de réponse par le service clientèle de l'opérateur (secondes)"}
      if(input$Indicator_TelM=="plaintes - facturation"){Title <- "Plaintes concernant l'exactitude de la facturation"}
      if(input$Indicator_TelM=="plaintes - vitesse"){Title <- "Plaintes concernant la vitesse des connexions à  Internet"}
      if(input$Indicator_TelM=="taux de couverture"){Title <- "Taux de couverture du réseau mobile"}
      
      return(Title)
    })
    
    output$Title_TelM <- renderText({
      
      Title_TelM()})
    
    # Create the chart 
    
    
    output$graphInt1_TelM <- renderChart2({
      
      ChartIntPerOpe <- hPlot(Value ~ Date, group="Operator",data = IntPerOp_TelM(),type = "column", title = Title_TelM())
      ChartIntPerOpe$yAxis(title = list(text =Label_TelM(), style = list(fontSize = '13px')))
      
      ChartIntPerOpe$exporting(enabled = T)
      #       if (input$period_TelM=="toutes"){
      #         ChartIntPerOpe$xAxis(categories = c("Q4 2014", "Q1 2015","Q2 2015","Q3 2015"))}
      #       
      #       if (input$period_TelM=="dernière"){
      #         ChartIntPerOpe$xAxis(categories = c("Q3 2015", ""))}
      ChartIntPerOpe$xAxis(categories = c(datelab_TelM(),""))      
      
      
      ChartIntPerOpe$colors(colors_TelM())
      ChartIntPerOpe$addParams(width = 675, height = 400)           
      return(ChartIntPerOpe)})
    
    ###
    
    # Internet mobile
    
    Measure_IntMob = reactive({
      
      if(input$Indicator_IntMob == 'plaintes - facturation'){Measure <- "% de factures"}
      if(input$Indicator_IntMob == 'plaintes - vitesse'){Measure <- "% de lignes"}
      if(input$Indicator_IntMob == 'taux de couverture'){Measure <- input$Measure7_IntMob}
      if(input$Indicator_IntMob == 'temps de reponse'){Measure <- input$Measure4_IntMob}
      return(Measure)
      
    })
    
    service_IntMob = reactive({
      service <- "Mobile telephony"
      if (input$Indicator_IntMob %in% c("temps de reponse", "plaintes - facturation")){service <- "N/A"}
      return(service)
    })
    
    isp_IntMob = reactive({
      op <- input$ISP_IntMob
      
      if (op == "Selection" & length(input$ISP2_IntMob)==0){isp <- c("Proximus", "Telenet", "Numericable", "Voo")}
      if (op == "Selection" & !(length(input$ISP2_IntMob)==0)) isp <- input$ISP2_IntMob
      
      if (op != "Selection"){
        
        
        
        if (op == "All") {op <- c("Proximus", "Telenet", "Numericable", "Voo")}} 
      return(op)     
    })
    
    
    colors_IntMob=reactive({
      colors <- sort(isp_IntMob())
      
      colors[colors=="Telenet"] <-  'rgba(255, 137, 0, 0.9)'
      colors[colors=="Voo"] <- 'rgba(255, 0, 103, 0.9)'
      colors[colors=="Proximus"] <- 'rgba(74, 0, 127, 0.9)'
      colors[colors=="Numericable"] <- 'rgba(110, 190, 0, 1)'
      if(length(colors)==1){colors<- c(colors, 'rgba(110, 190, 0, 1)')}
      
      
      return (colors)
    })
    
    #     date_IntMob = reactive({
    #       if (input$period_IntMob == "toutes"){date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
    #       if (input$period_IntMob == "dernière"){date <- as.Date(strptime(as.character("2015-09-30"),"%Y-%m-%d"))}
    #       return(date)
    #     })
    #     
    
    
    date_IntMob = reactive({
      
      if (input$period_IntMob == "Selection" & (length(input$Period2_IntMob)==0)) {date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period_IntMob == "Selection" & !(length(input$Period2_IntMob)==0))  { date <- as.Date(input$Period2_IntMob)}
      
      
      if (input$period_IntMob == "toutes"){date <- c(as.Date("2014-12-31"), as.Date("2015-03-30"), as.Date("2015-06-30"), as.Date("2015-09-30"))}
      if (input$period_IntMob == "dernière"){date <- as.Date(strptime(as.character("2015-09-30"),"%Y-%m-%d"))}
      
      return(date)
      
    })
    
    datelab_IntMob = reactive({ 
      
      date <-  as.character(date_TelM())
      date[date=="2014-12-31"] <- "T4 2014"
      date[date=="2015-03-30"] <- "T1 2015"
      date[date=="2015-06-30"] <- "T2 2015"
      date[date=="2015-09-30"] <- "T3 2015"
      
      return(date)
    })
    
    
    
    
    IntPerOp_IntMob = reactive({
      
      
      Internet <- outcome[outcome$Operator %in% isp_IntMob() & outcome$Service == service_IntMob() 
                          & outcome$Measure==Measure_IntMob() & outcome$Date %in% date_IntMob() , ]
      Internet$Date <-  format(Internet$Date,"%Y-%m-%d") 
      as.double(as.POSIXct(as.Date(Internet$Date),origin="1970-01-01"))
      
      return(Internet) }) 
    
    TabInt_IntMob = reactive({
      tabInt <- IntPerOp_IntMob()[,c("Date", "Operator", "Value") ]
      
      tabInt2 <- dcast(tabInt, Date ~ Operator, value.var='Value')
      tabInt2$Date <-  as.character(tabInt2$Date)
      tabInt2$Date[tabInt2$Date=="2014-12-31"] <- "T4 2014"
      tabInt2$Date[tabInt2$Date=="2015-03-30"] <- "T1 2015"
      tabInt2$Date[tabInt2$Date=="2015-06-30"] <- "T2 2015"
      tabInt2$Date[tabInt2$Date=="2015-09-30"] <- "T3 2015"
      colnames(tabInt2)[1] <- "Période"
      return(tabInt2)
    })
    
    
    output$table_IntMob <- renderTable({
      TabInt_IntMob()
    })
    
    output$downloadData_IntMob <- downloadHandler(
      filename = "Quality-Tel_Mobile.csv",
      content = function(file) {
        write.csv(TabInt_IntMob(), file)
      }
    )
    
    
    
    Label_IntMob  = reactive({
      
      Label <- Measure_IntMob()
      
      if (Label == "% faits a la duree convenue") {Label <- "% faits à la durée convenue"}
      if (Label == "80% les plus rapidement reparees") {Label <- "80% les plus rapidement réparées"}
      if (Label == "95% les plus rapidement reparees") {Label <- "95% les plus rapidement réparées"}
      if (Label == "% a la duree convenue") {Label <- "% à la durée convenue"}
      if (Label == "% de menages - 2G") {Label <- "% de ménages - 2G"}
      if (Label == "% de menages - 3G") {Label <- "% de ménages - 3G"}
      if (Label == "% de menages - 4G") {Label <- "% de ménages - 4G"}
      if (Label == "temps de reponse moyen") {Label <- "temps de réponse moyen"}
      return(Label)
    })
    
    
    Title_IntMob = reactive({
      
      if(input$Indicator_IntMob=="delai de fourniture"){Title <- "Délai de fourniture du raccordement (jours) pour la téléphonie fixe"}
      if(input$Indicator_IntMob=="taux de pannes"){Title <- "Taux de défaillances signalées par ligne d'accès pour la téléphonie mobile"}
      if(input$Indicator_IntMob=="delai de reparation"){Title <- "Délai de réparation d'une défaillance (jours) pour la téléphonie mobile"}
      if(input$Indicator_IntMob=="temps de reponse"){Title <- "Temps de réponse par le service clientèle de l'opérateur (secondes)"}
      if(input$Indicator_IntMob=="plaintes - facturation"){Title <- "Plaintes concernant l'exactitude de la facturation"}
      if(input$Indicator_IntMob=="plaintes - vitesse"){Title <- "Plaintes concernant la vitesse des connexions à  Internet"}
      if(input$Indicator_IntMob=="taux de couverture"){Title <- "Taux de couverture du réseau mobile"}
      
      return(Title)
    })
    
    output$Title_IntMob <- renderText({
      
      Title_IntMob()})
    
    # Create the chart 
    
    
    output$graphInt1_IntMob <- renderChart2({
      
      ChartIntPerOpe <- hPlot(Value ~ Date, group="Operator",data = IntPerOp_IntMob(),type = "column", title = Title_IntMob())
      ChartIntPerOpe$yAxis(title = list(text =Label_IntMob(), style = list(fontSize = '13px')))
      
      ChartIntPerOpe$exporting(enabled = T)
      #       if (input$period_IntMob=="toutes"){
      #         ChartIntPerOpe$xAxis(categories = c("Q4 2014", "Q1 2015","Q2 2015","Q3 2015"))}
      #       
      #       if (input$period_IntMob=="dernière"){
      #         ChartIntPerOpe$xAxis(categories = c("Q3 2015", ""))}
      
      ChartIntPerOpe$xAxis(categories = c(datelab_IntMob(),""))  
      ChartIntPerOpe$colors(colors_IntMob())
      ChartIntPerOpe$addParams(width = 675, height = 400)           
      return(ChartIntPerOpe)})
    
    
    
    ###
    
    
    
    
    
    
    
    
    
    
    
    
    # ENQUETE
    #########################################
    
    # Satisfaction par rapport aux services
    # Code Shiny
    
    
    output$graphSurv1 <- renderChart2({
      
      load("data/satis.RDA")
      
      # Code pour Shiny
      # compute boxplot statistics and cast it as a dataframe with no headers
      bwstats = setNames(
        as.data.frame(boxplot(SatisService ~ Year, data = satis, plot = F)$stats),
        nm = NULL
      )
      
      bwstats[1,] <- c(0,0,0)
      bwstats[2,] <- c(5,5,5)
      bwstats[3,] <- c(7,7,7)
      bwstats[4,] <- c(8,8,8)
      
      # load rCharts and initialize
      library(rCharts)
      h1 <- Highcharts$new()
      
      # pass data as a list of lists
      h1$set(series = list(list(
        name = c('Satisfaction'),
        data = bwstats
      )),title = list(text = "Degré de satisfaction par rapport au service"))
      
      
      # set xaxis/yaxis titles and labels
      
      h1$yAxis(
        title = list(text="0 = pas du tout; 10 = tout à fait", bounds = list(x = 50, y = 50, height = 300, width = 500))  
      )
      
      h1$chart(forceY = c(0, 10))
      
      h1$xAxis(
        categories = levels(as.factor(satis$Year)),
        title = list(text = 'Year')
      )
      
      h1$chart(type = 'boxplot')
      h1$addParams(width = 700, height = 400)  
      
      return(h1)})
    
    
    Satis = reactive({
      
      load("data/satis.RDA")
      
      # Code pour Shiny
      # compute boxplot statistics and cast it as a dataframe with no headers
      bwstats = setNames(
        as.data.frame(boxplot(SatisService ~ Year, data = satis, plot = F)$stats),
        nm = NULL
      )
      
      bwstats[1,] <- c(0,0,0)
      bwstats[2,] <- c(5,5,5)
      bwstats[3,] <- c(7,7,7)
      bwstats[4,] <- c(8,8,8)
      colnames(bwstats) <- c(2012,2013,2014)
      rownames(bwstats) <- c("Minimum", "Quantile 25", "Médiane", "Quantile 75", "Maximum")
      return(bwstats)
    })
    
    
    output$satisServ <- renderTable({
      Satis()
    })
    
    
    
    
    # PROBLEMES
    
    # Code Shiny
    output$graphSurv2 <- renderChart2({
      load("data/prob.RDA")
      freq <- ftable(prob)
      freqp <- as.data.frame(prop.table(freq, margin=1))
      freqp[1, "Freq"] <- 43
      freqp[2, "Freq"] <- 49
      freqp[3, "Freq"] <- 50
      freqp[4, "Freq"] <- 57
      freqp[5, "Freq"] <- 51
      freqp[6, "Freq"] <- 50
      
      
      pr <- hPlot(Freq ~ Problème, group="Year",data = freqp,type = "column", title = "Problème au cours des trois dernières années")
      pr$yAxis(title = list(text ="pourcentage(N2012=1124; N2013=1386; N2014=1179)"))
      pr$exporting(enabled = T)
      pr$addParams(width = 700, height = 400)  
      pr
      
      return(pr)})
    
    
    ProbOcc = reactive({
      load("data/prob.RDA")
      freq <- ftable(prob)
      freqp <- as.data.frame(prop.table(freq, margin=1))
      freqp[1, "Freq"] <- 43
      freqp[2, "Freq"] <- 49
      freqp[3, "Freq"] <- 50
      freqp[4, "Freq"] <- 57
      freqp[5, "Freq"] <- 51
      freqp[6, "Freq"] <- 50
      colnames(freqp) <- c("Année", "Problème?", "Pourcentage")
      tabprob <- freqp
      tabprob$Pourcentage <- round(tabprob$Pourcentage)
      return(tabprob)
    })
    
    
    output$probtab1 <- renderTable({
      ProbOcc()
    })
    
    
    
    
    output$graphSurv3 <- renderChart2({
      
      # Code pour Shiny
      
      load("data/causepro.RDA")
      #       causepro$Label <- gsub("è", "e", causepro$Label)
      #       causepro$Label <- gsub("é", "e", causepro$Label)
      #       save(causepro,file="data/causepro.RDA")
      
      # causepro <- read.csv("Causepro.csv", header=T, sep=";")
      causeprobl <- causepro
      causeprobl$Freq[is.na(causeprobl$Freq)] <- 0.0001
      pro <- hPlot(Freq ~ Label, group="Year",data = causeprobl,type = "bar", title = "Problème au cours des trois dernières années")
      pro$yAxis(title = list(text ="pourcentage (N2012=525; N2013=675; N2014=575)"))
      pro$exporting(enabled = T)
      pro$addParams(width = 700, height = 400)  
      
      return(pro)})
    
    
    
    Probcause = reactive({
      load("data/causepro.RDA")
      colnames(causepro) <- c("Année", "Pourcentage", "Cause1" , "Cause")
      causeprob <- causepro[, c(1:2, 4)]
      causeprob1 <-  reshape(causeprob, timevar = "Année", idvar = "Cause", direction = "wide")
      colnames(causeprob1) <- c("Cause","2014", "2013" ,"2012")
      causeprob1 <- causeprob1[, c(1,4,3,2)]
      rownames(causeprob1) <- NULL
      return(causeprob1)
    })
    
    
    output$probtab2 <- renderTable({
      Probcause()
    })
    
    
    
    # cond = reactive({
    # if input$l
    
    output$graphSurv4 <- renderChart2({
      
      load("data/satis.RDA")
      
      # Code pour Shiny
      # compute boxplot statistics and cast it as a dataframe with no headers
      bwstats1 = setNames(
        as.data.frame(boxplot(SatisService ~ Year, data = satis, plot = F)$stats),
        nm = NULL
      )
      
      bwstats1[1,] <- c(0,0,0)
      bwstats1[2,] <- c(5,5,5)
      bwstats1[3,] <- c(7,7,6)
      bwstats1[4,] <- c(8,8,8)
      
      # load rCharts and initialize
      library(rCharts)
      h1 <- Highcharts$new()
      
      # pass data as a list of lists
      h1$set(series = list(list(
        name = c('Satisfaction'),
        data = bwstats1
      )),title = list(text = "Correspondance entre vitesses de connexion réelle et annoncée"))
      
      
      # set xaxis/yaxis titles and labels
      
      h1$yAxis(
        title = list(text="0 = pas du tout; 10 = tout à fait", bounds = list(x = 50, y = 50, height = 300, width = 500))  
      )
      
      h1$chart(forceY = c(0, 10))
      
      h1$xAxis(
        categories = levels(as.factor(satis$Year)),
        title = list(text = 'Year')
      )
      
      h1$chart(type = 'boxplot')
      
      h1$addParams(width = 700, height = 400)  
      
      return(h1)})
    
    
    satspeed = reactive({
      load("data/satis.RDA")
      
      # Code pour Shiny
      # compute boxplot statistics and cast it as a dataframe with no headers
      bwstats1 = setNames(
        as.data.frame(boxplot(SatisService ~ Year, data = satis, plot = F)$stats),
        nm = NULL
      )
      
      bwstats1[1,] <- c(0,0,0)
      bwstats1[2,] <- c(5,5,5)
      bwstats1[3,] <- c(7,7,6)
      bwstats1[4,] <- c(8,8,8)
      colnames(bwstats1) <- c(2012,2013,2014)
      rownames(bwstats1) <- c("Minimum", "Quantile 25", "Médiane", "Quantile 75", "Maximum")
      return(bwstats1)
      
    })
    
    
    output$probtab3 <- renderTable({
      satspeed()
    })
    
    
    
    
    
    
    
  }
)