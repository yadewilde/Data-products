require('rCharts')
require('shiny')
library('shinyapps')
options(RCHART_LIB = 'polycharts')

shinyUI(fluidPage(

  p(a("Nederlands", href="http://www.ibpt.be/nl/consumenten", target="_blank")," - ",a("Deutsch", href="http://www.ibpt.be/nl/consumenten", target="_blank"),
    " - ",a("English", hdeployAppref="http://www.ibpt.be/nl/consumenten", target="_blank"), align="right", style = "color:lightred"),
    
    

a(img(src = "test.png", style="display: block; margin-left: 
        auto; margin-right: auto;height = 72, width = 72", align = "center", height=120, width=120), href="http://www.ibpt.be/fr/consommateurs", target="_blank"),
tags$head(tags$link(rel = "icon", type = "image/png", href = "barom.png")) ,
 
  #   headerPanel(
  #     list(tags$head(tags$style("body {background-color:  #F2F2F2; }")))),
  #   
  
  
h2("Baromètre de la qualité de service", style= "font-family: 
                 'Times New Roman'; font-si40pt; color:DarkCyan ", align = "center"),


column(2, sidebarLayout(position="right",
  
  sidebarPanel(strong(""),br(),
               a(img(src = "simualteur.png", height = 150, width = 150), href= "http://www.meilleurtarif.be/", target="_blank"), br(),br(),
   
                 a("Comparez et choisissez le meilleur plan tarifaire",href = "http://www.meilleurtarif.be/", target="_blank"),br(),
#                
               br(),
a(img(src = "couverture.png", height = 150, width = 150),href="http://www.ibpt.be/fr/consommateurs", target="_blank"),br(),br(),           
# p("Vérifiez la disponibilité des services"),br(),
                 a("Vérifiez la disponibilité des services",href = "http://www.ibpt.be/fr/consommateurs", target="_blank"),br(),br(),
a(img(src="speed4.png", height=400, width=400), href="http://www.speedtest.net/", target="_blank"),br(),br(),
              
               
#                p("Tester la qualité de votre connexion Internet")
                 a("Tester la qualité de votre connexion Internet",href = "http://www.speedtest.net/", target="_blank")
               
               
               ,br(), br(),
               width=10, align="center"   )
  ,
  
  
  mainPanel(
    p("")
    
    
  )  
)),


# h5("Services de communications électroniques résidentiels", style= "font-family: 
#                  'Times New Roman'; font-si40pt; color:Red ", align = "center"),

#   navbarPage("",
#              
#              tabPanel("Indicateurs de qualité",
#                       
                     
column(10,
  
  tabsetPanel(id="Service",
                                  
                                  
                                  
                                  
                                  
                                  tabPanel("Internet fixe",  
                                           
                                           
                                           
                                           column(3,     
                                                  
                                                  selectInput("Indicator", 
                                                              label = p(strong("Indicateur"),style = "color:DarkCyan"),
                                                              choices = c("Mise en service"="delai de fourniture",  # Make sure not to mix names with values
                                                                          "Pannes"="taux de pannes",
                                                                          "Réparations"="delai de reparation", 
                                                                          "Service clientèle"="temps de reponse"
                                                                          , "Facturation"="plaintes - facturation", 
                                                                          "Vitesse de connexion"="plaintes - vitesse"
                                                              ),
                                                              width='200px',
                                                              selected = "delai de fourniture"),
                                                  
                                                  
                                                  conditionalPanel(condition = "input.Indicator == 'delai de fourniture'",    
                                                                   selectInput("Measure1", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("50% des cas" = "50% les plus rapidement fournis", 
                                                                                              "95% des cas" = "95% les plus rapidement fournis",
                                                                                              "99% des cas" = "99% les plus rapidement fournis"),
                                                                               selected = "50% les plus rapidement fournis", width='200px')),
                                                  
                                                  
                                                  #                         conditionalPanel(condition = "input.Indicator == 'taux de pannes'",    
                                                  #                                          selectInput("Measure2", 
                                                  #                                                      label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                  #                                                      choices = list("Nombre de pannes" = "nombre de pannes"),
                                                  #                                                      selected = "nombre de pannes")),
                                                  
                                                  
                                                  conditionalPanel(condition = "input.Indicator == 'delai de reparation'",    
                                                                   selectInput("Measure3", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("Quantile 80" = "80% les plus rapidement reparees", 
                                                                                              "95% des cas" = "95% les plus rapidement reparees",
                                                                                              "Durée convenue" = "% a la duree convenue"),
                                                                               selected = "95% les plus rapidement reparees", width='200px')),
                                                  
                                                  conditionalPanel(condition = "input.Indicator == 'temps de reponse'",    
                                                                   selectInput("Measure4", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("Temps moyen" = "temps de reponse moyen", 
                                                                                              "Appels en 20 sec." = "% appels en 20 secondes"),
                                                                               selected = "temps de reponse moyen", width='200px')),
                                                  
                                                 
                                                  
                                                  conditionalPanel(condition = "input.Indicator == 'taux de couverture'",    
                                                                   selectInput("Measure7", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("2G" = "% de menages - 2G", 
                                                                                              "3G" = "% de menages - 3G", 
                                                                                              "4G" = "% de menages - 4G"),
                                                                               selected = "% de menages - 4G", width='200px')),
                                                  
                                                  selectInput("ISP", 
                                                              label = p(strong("Opérateur"),style = "color:DarkCyan"),
                                                              choices = c("Numericable"="Numericable","Proximus"="Proximus",  # Make sure not to mix names with values
                                                                          "Telenet"="Telenet", "Voo"="Voo",  "Tous"="All", "Sélectionner"="Selection"),
                                                              width='200px',selected = "All"),
                                                  
                                                  conditionalPanel(condition = "input.ISP == 'Selection'", 
                                                  checkboxGroupInput("ISP2", 
                                                                     label = p(strong(""),style = "color:DarkCyan"),
                                                                     choices = list("Proximus"="Proximus","Telenet"="Telenet", "Voo"="Voo", "Numericable"="Numericable"),
                                                                     selected = c("Proximus", "Telenet", "Voo", "Numericable"))), 
                                                  
                                                  
                                                  selectInput("period", 
                                                              label = p(strong("Période"),style = "color:DarkCyan"),
                                                              choices = c("Denière période"="dernière",  # Make sure not to mix names with values
                                                                          "Toutes"="toutes", "Sélectionner"="Selection"),
                                                              width='200px',selected = "toutes"), 
                                                  conditionalPanel(condition = "input.period == 'Selection'", 
                                                  checkboxGroupInput("Period2", 
                                                                     label = p(strong(""),style = "color:DarkCyan"),
                                                                     choices = list("T4 2014" = "2014-12-31",
                                                                                    "T1 2015" = "2015-03-30","T2 2015" = "2015-06-30","T3 2015" = "2015-09-30" ),
                                                                     selected = c("2014-12-31", "2015-03-30", "2015-06-30", "2015-09-30"))), 
                                                  
                                                  
                                                  
                                                  downloadButton('downloadData', 'Télécharger') 
                                           ),
                                           
                                           fluidRow(column(8, showOutput("graphInt1", lib="highcharts"), align="center")),
                                           br(),
                                           
                                           
                                           sidebarLayout(
                                             
                                             sidebarPanel(p(" ")
                                                          , 
                                                          
                                                          tags$head(
                                                            tags$style(type='text/css'
                                                                       # sidebarPanel appearance
                                                                       , ".row-fluid .span4 {width: 0px; height: px;}"
                                                                       , ".row-fluid .well {padding: 0px; border: 10px;}"
                                                                       , ".row-fluid .well {background-color: white;}"
                                                                       , ".row-fluid .span4 {background-color: blue;}"
                                                                       # button appearance
                                                                       , ".btn {padding: 7px; font-size: 100%;}"
                                                            )
                                                          )
                                                          )
,
                       
                                             
  mainPanel(
#                                                                                 

                                               
    tableOutput("tableInternet"),                                              
                                               
 p(strong("Indicateur")),

 conditionalPanel(condition = "input.Indicator == 'delai de fourniture' & input.Measure1 == '50% les plus rapidement fournis'",
  p("Délai de mise en service pour le raccordement initial au réseau: délai des 50 % des mises en services les plus rapides.")),  
                                               
conditionalPanel(condition = "input.Indicator == 'delai de fourniture' & input.Measure1 == '95% les plus rapidement fournis'",
 p("Délai de mise en service pour le raccordement initial au réseau: délai des 95 % des mises en services les plus rapides.")), 
                                               
conditionalPanel(condition = "input.Indicator == 'delai de fourniture' & input.Measure1 == '99% les plus rapidement fournis'",
p("Délai de mise en service pour le raccordement initial au réseau: délai des 99 % des mises en services les plus rapides.")), 
                                               
                                               
conditionalPanel(condition = "input.Indicator == 'taux de pannes'",
p("Taux de défaillances signalées par ligne d'accès à Internet.")),  
                                               
conditionalPanel(condition = "input.Indicator == 'delai de reparation' & input.Measure3 == '80% les plus rapidement reparees'",
p("The time by which the fastest 80 % of valid 
faults on access lines are repaired, or the percentage of faults cleared any time stated as an objective by 
the service provider standard accuracy for keeping appointments.")),

conditionalPanel(condition = "input.Indicator == 'delai de reparation'& input.Measure3 == '95% les plus rapidement reparees'",
p("The time by which the fastest 95 % of valid 
faults on access lines are repaired, or the percentage of faults cleared any time stated as an objective by 
 the service provider standard accuracy for keeping appointments.")),
           
                                              
conditionalPanel(condition = "input.Indicator == 'delai de reparation'& input.Measure3 == '% a la duree convenue'",
p("The time by which the valid 
faults on access lines are cleared at the time stated as an objective by 
the service provider.")),
                                                                                          
                                                                                   
conditionalPanel(condition = "input.Indicator == 'temps de reponse' & input.Measure4 == 'temps de reponse moyen'",
p("Mean time to answer.")),
                                              
conditionalPanel(condition = "input.Indicator == 'temps de reponse' & input.Measure4 == '% appels en 20 secondes'",
 p("Percentage of calls answered within 20 seconds.")),                                            
                                              
conditionalPanel(condition = "input.Indicator == 'plaintes - facturation'",
p("Percentage of bills resulting in a customer complaint.")),
                                               
conditionalPanel(condition = "input.Indicator == 'plaintes - vitesse'",
p("Percentage of complaints logged per customer regarding the speed of the Internet connexion.")),
                                          
p(strong("Source")),
p("Données fournies par les opérateurs conformément à la décision du Conseil de l'IBPT du xx xx 2015.")
                                             
                                             
)  
)
                                           
                  
                                             ),
                                  
                                  tabPanel("Téléphonie fixe",                                            
                                           
                                           column(3,     
                                                  
                                                  selectInput("Indicator_TelF", 
                                                              label = p(strong("Indicateur"),style = "color:DarkCyan"),
                                                              choices = c("Mise en service"="delai de fourniture",  # Make sure not to mix names with values
                                                                          "Pannes"="taux de pannes",
                                                                          "Réparations"="delai de reparation", 
                                                                          "Service clientèle"="temps de reponse"
                                                                          , "Facturation"="plaintes - facturation"
                                                              ),
                                                              width='200px',
                                                              selected = "delai de fourniture"),
                                                  
                                                  
                                                  conditionalPanel(condition = "input.Indicator_TelF == 'delai de fourniture'",    
                                                                   selectInput("Measure1_TelF", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("50% des cas" = "50% les plus rapidement fournis", 
                                                                                              "95% des cas" = "95% les plus rapidement fournis",
                                                                                              "99% des cas" = "99% les plus rapidement fournis"),
                                                                               selected = "50% les plus rapidement fournis")),
                                                  
                                                  
                                                  #                         conditionalPanel(condition = "input.Indicator == 'taux de pannes'",    
                                                  #                                          selectInput("Measure2", 
                                                  #                                                      label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                  #                                                      choices = list("Nombre de pannes" = "nombre de pannes"),
                                                  #                                                      selected = "nombre de pannes")),
                                                  
                                                  
                                                  conditionalPanel(condition = "input.Indicator_TelF == 'delai de reparation'",    
                                                                   selectInput("Measure3_TelF", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("Quantile 80" = "80% les plus rapidement reparees", 
                                                                                              "95% des cas" = "95% les plus rapidement reparees",
                                                                                              "Durée convenue" = "% a la duree convenue"),
                                                                               selected = "95% les plus rapidement reparees")),
                                                  
                                                  conditionalPanel(condition = "input.Indicator_TelF == 'temps de reponse'",    
                                                                   selectInput("Measure4_TelF", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("Temps moyen" = "temps de reponse moyen", 
                                                                                              "Appels en 20 sec." = "% appels en 20 secondes"),
                                                                               selected = "temps de reponse moyen")),
                                                  
                                                  #                         conditionalPanel(condition = "input.Indicator == 'plaintes - facturation'",    
                                                  #                                          selectInput("Measure5", 
                                                  #                                                      label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                  #                                                      choices = list("% de factures" = "% de factures"),
                                                  #                                                      selected = "% de factures")),
                                                  
                                                  
                                                  
                                                  
                                                  #                         conditionalPanel(condition = "input.Indicator == 'plaintes - vitesse'",    
                                                  #                                          selectInput("Measure6", 
                                                  #                                                      label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                  #                                                      choices = list("% de lignes" = "% de lignes"),
                                                  #                                                      selected = "% de lignes")),
                                                  
                                                  conditionalPanel(condition = "input.Indicator_TelF == 'taux de couverture'",    
                                                                   selectInput("Measure7_TelF", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("2G" = "% de menages - 2G", 
                                                                                              "3G" = "% de menages - 3G", 
                                                                                              "4G" = "% de menages - 4G"),
                                                                               selected = "% de menages - 4G")),
                                                  
                                                  selectInput("ISP_TelF", 
                                                              label = p(strong("Opérateur"),style = "color:DarkCyan"),
                                                              choices = c("Proximus"="Proximus",  # Make sure not to mix names with values
                                                                          "Telenet"="Telenet", "Voo"="Voo", "Numericable"="Numericable", "Tous"="All", "Sélectionner"="Selection"),
                                                              width='200px',selected = "All"),
                                                  
                                                 
                                                  conditionalPanel(condition = "input.ISP_TelF == 'Selection'", 
                                                                   checkboxGroupInput("ISP2_TelF", 
                                                                                      label = p(strong(""),style = "color:DarkCyan"),
                                                                                      choices = list("Proximus"="Proximus","Telenet"="Telenet", "Voo"="Voo", "Numericable"="Numericable"),
                                                                                      selected = c("Proximus", "Telenet", "Voo", "Numericable"))), 
                                                  
                                                  
                                                  selectInput("period_TelF", 
                                                              label = p(strong("Période"),style = "color:DarkCyan"),
                                                              choices = c("Denière période"="dernière",  # Make sure not to mix names with values
                                                                          "Toutes"="toutes", "Sélectionner"="Selection"),
                                                              width='200px',selected = "toutes"), 
                                                  
                                                  conditionalPanel(condition = "input.period_TelF == 'Selection'",
                                                  checkboxGroupInput("Period2_TelF", 
                                                                     label = p(strong("Périodes"),style = "color:DarkCyan"),
                                                                     choices = list("T4 2014" = "2014-12-31",
                                                                                    "T1 2015" = "2015-03-30","T2 2015" = "2015-06-30","T3 2015" = "2015-09-30" ),
                                                                     selected = c("2014-12-31", "2015-03-30", "2015-06-30", "2015-09-30"))), 
                                                  
                                                  
                                                  #                       conditionalPanel(condition = "input.period == 'toutes'",selectInput("chart", 
                                                  #                       label = p(strong("Graphique"),style = "color:DarkCyan"),
                                                  #                       choices = c("Barres"="Barres","Lignes"="Lines"),
                                                  #                       width='200px',
                                                  #                       selected = "Barres"))
                                                  
                                                  
                                                  downloadButton('downloadData_TelF', 'Télécharger') 
                                           ),
                                           
                                           fluidRow(column(8, showOutput("graphInt1_TelF", lib="highcharts"), align="center")),
                                           br(),
                                           
                                           
                                           sidebarLayout(
                                             
                                             sidebarPanel(p("")
                                             ),
                                             
                                             
                                             mainPanel(
#                                         
#                                                
                                               tableOutput("table_TelF"),
                                               
                                               
                                               
                                               p(strong("Définition")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'delai de fourniture' & input.Measure1_TelF == '50% les plus rapidement fournis'",
                                                                p("The times by which the fastest 50 % of orders are completed.")), 
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'delai de fourniture' & input.Measure1_TelF == '95% les plus rapidement fournis'",
                                                                p("The times by which the fastest 95 % of orders are completed.")), 
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'delai de fourniture' & input.Measure1_TelF == '99% les plus rapidement fournis'",
                                                                p("The times by which the fastest 99 % of orders are completed.")), 
                                               conditionalPanel(condition = "input.Indicator_TelF == 'taux de pannes'",
                                                                p("Taux de défaillances signalées par ligne d'accès pour la téléphonie fixe")),  
                                               conditionalPanel(condition = "input.Indicator_TelF == 'delai de reparation' & input.Measure3_TelF == '80% les plus rapidement reparees'",
                                                                p("The time by which the fastest 80 % of valid 
                                                                  faults on access lines are repaired, or the percentage of faults cleared any time stated as an objective by 
                                                                  the service provider standard accuracy for keeping appointments.")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'delai de reparation'& input.Measure3_TelF == '95% les plus rapidement reparees'",
                                                                p("The time by which the fastest 95 % of valid 
                                                                  faults on access lines are repaired, or the percentage of faults cleared any time stated as an objective by 
                                                                  the service provider standard accuracy for keeping appointments.")),
                                               
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'delai de reparation'& input.Measure3_TelF == '% a la duree convenue'",
                                                                p("The time by which the valid 
                                                                  faults on access lines are cleared at the time stated as an objective by 
                                                                  the service provider.")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'temps de reponse' & input.Measure4_TelF == 'temps de reponse moyen'",
                                                                p("Mean time to answer.")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'temps de reponse' & input.Measure4_TelF == '% appels en 20 secondes'",
                                                                p("Percentage of calls answered within 20 seconds.")),  
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'plaintes - facturation'",
                                                                p("Percentage of bills resulting in a customer complaint.")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelF == 'plaintes - vitesse'",
                                                                p("Percentage of complaints logged per customer regarding the speed of the Internet connexion.")),
                                               
                                               p(strong("Source")),
                                               
                                               p("Données fournies par les opérateurs conformément à la décision du Conseil de l'IBPT du xx xx 2015.")
                                               
                                               
                                               
                                             )  )  
                                           
                                           ),


tabPanel("Internet mobile",
         
         
         column(3,     
                
                selectInput("Indicator_IntMob", 
                            label = p(strong("Indicateur"),style = "color:DarkCyan"),
                            choices = c("Taux de couverture"="taux de couverture",
                                        "Facturation"="plaintes - facturation" ,"Service clientèle"="temps de reponse"
                                        ,  "Vitesse de connexion"="plaintes - vitesse"
                                        
                            ),
                            width='200px',
                            selected = "taux de couverture"),
                
                
                conditionalPanel(condition = "input.Indicator_IntMob == 'temps de reponse'",    
                                 selectInput("Measure4_IntMob", 
                                             label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                             choices = list("Temps moyen" = "temps de reponse moyen", 
                                                            "Appels en 20 sec." = "% appels en 20 secondes"),
                                             selected = "temps de reponse moyen")),
                
                conditionalPanel(condition = "input.Indicator_IntMob == 'taux de couverture'",    
                                 selectInput("Measure7_IntMob", 
                                             label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                             choices = list("2G" = "% de menages - 2G", 
                                                            "3G" = "% de menages - 3G", 
                                                            "4G" = "% de menages - 4G"),
                                             selected = "% de menages - 4G")),
                
                
                selectInput("ISP_IntMob", 
                            label = p(strong("Opérateur"),style = "color:DarkCyan"),
                            choices = c("Proximus"="Proximus",  # Make sure not to mix names with values
                                        "Telenet"="Telenet", "Voo"="Voo", "Numericable"="Numericable", "Tous"="All", "Sélectionner"="Selection"),
                            width='200px',selected = "All"),
                
                
                conditionalPanel(condition = "input.ISP_IntMob == 'Selection'", 
                                 checkboxGroupInput("ISP2_IntMob", 
                                                    label = p(strong(""),style = "color:DarkCyan"),
                                                    choices = list("Proximus"="Proximus","Telenet"="Telenet", "Voo"="Voo", "Numericable"="Numericable"),
                                                    selected = c("Proximus", "Telenet", "Voo", "Numericable"))),             
                
                selectInput("period_IntMob", 
                            label = p(strong("Période"),style = "color:DarkCyan"),
                            choices = c("Denière période"="dernière",  # Make sure not to mix names with values
                                        "Toutes"="toutes", "Sélectionner"="Selection"),
                            width='200px',selected = "toutes"), 
                
                conditionalPanel(condition = "input.period_IntMob == 'Selection'",
                                 checkboxGroupInput("Period2_IntMob", 
                                                    label = p(strong("Périodes"),style = "color:DarkCyan"),
                                                    choices = list("T4 2014" = "2014-12-31",
                                                                   "T1 2015" = "2015-03-30","T2 2015" = "2015-06-30","T3 2015" = "2015-09-30" ),
                                                    selected = c("2014-12-31", "2015-03-30", "2015-06-30", "2015-09-30"))), 
                
                
                downloadButton('downloadData_IntMob', 'Télécharger') 
         ),
         
         fluidRow(column(8, showOutput("graphInt1_IntMob", lib="highcharts"), align="center")),
         br(),
         
         
         sidebarLayout(
           
           sidebarPanel(p("")
           ),
           
           
           
           
           mainPanel(
             #                                                br(),
             #                                                
             #                                                
             #       column(12, tableOutput("tableInternet"), align="left"),
             #     p(strong("Indicateurs de qualité")),
             #     p("Suivant l'article 113 de la loi du 13 juin 2005 relative aux communications électroniques, 
             #                 les opérateurs ont ", br("l'obligation de publier sur leur site Internet des
             #                 informations actuelles concernant la qualité des services.")), 
             #     
             #     p("Les systèmes de mesures sont audités et les données présentées sur ce site sont également présentées",
             #       br("dans", a("un rapport annuel de la qualité de service.", 
             #                    href = "http://www.arcep.fr/uploads/tx_gspublication/QoS-acces-services-fixes-151014.pdf"))),
             
             
             #     p("Informez-vous sur vos droits en visitant notre rubrique",
             #       a("consommateurs",href = "http://www.ibpt.be/fr/consommateurs"),"sur notre site Internet. Parcourez également ",br("notre FAQ sur la mannière de procéder lorsque",
             #                                                                                                                          a("la qualité annoncée n'est pas présente.",href="http://www.ibpt.be/fr/consommateurs/telephone/plaintes-problemes/die-qualitat-entspricht-nicht-der-werbung"))),
             #     
             #     
             #     p("Les indicateurs sont également publiés sur le site Internet des opérateurs."),
             #     
             #     br(),
             
             
             #                                                strong(textOutput("Title_IntMob")),br(),
             #                                                
             tableOutput("table_IntMob"),
             
             
             
             p(strong("Définition")),
             
             conditionalPanel(condition = "input.Indicator_IntMob == 'taux de couverture'",
                              p("Number of households passed by a technology capable of delivering 2G, 3G or 4G.")), 
             conditionalPanel(condition = "input.Indicator_IntMob == 'taux de pannes'",
                              p("Taux de défaillances signalées par ligne d'accès à Internet mobile.")),  
             conditionalPanel(condition = "input.Indicator_IntMob == 'delai de reparation'",
                              p("The time by which the fastest 80 % and 95 % of valid 
                                faults on access lines are repaired, or the percentage of faults cleared any time stated as an objective by 
                                the service provider standard accuracy for keeping appointments.")),
             
             conditionalPanel(condition = "input.Indicator_IntMob == 'temps de reponse'",
                              p("Mean time to answer, and percentage of calls answered within 20 seconds.")),
             
             conditionalPanel(condition = "input.Indicator_IntMob == 'plaintes - facturation'",
                              p("Percentage of bills resulting in a customer complaint.")),
             
             conditionalPanel(condition = "input.Indicator_TelM == 'plaintes - vitesse'",
                              p("Percentage of complaints logged per customer regarding the speed of the Internet connexion.")),
             
             p(strong("Source")),
             
             p("Données fournies par les opérateurs conformément à la décision du Conseil de l'IBPT du xx xx 2015.")
             
             
             
                              )  )
         
         
         
         
           ),


                                  tabPanel("Téléphonie mobile",
                                           
                                           
                                           column(3,     
                                                  
                                                  selectInput("Indicator_TelM", 
                                                              label = p(strong("Indicateur"),style = "color:DarkCyan"),
                                                              choices = c("Taux de couverture"="taux de couverture",
                                                                           "Facturation"="plaintes - facturation" ,"Service clientèle"="temps de reponse"
                                                                         
                                                                          
                                                              ),
                                                              width='200px',
                                                              selected = "taux de couverture"),
                                                  
                                                  
                                                  conditionalPanel(condition = "input.Indicator_TelM == 'temps de reponse'",    
                                                                   selectInput("Measure4_TelM", 
                                                                               label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
                                                                               choices = list("Temps moyen" = "temps de reponse moyen", 
                                                                                              "Appels en 20 sec." = "% appels en 20 secondes"),
                                                                               selected = "temps de reponse moyen")),
                                                  
#                                                   conditionalPanel(condition = "input.Indicator_TelM == 'taux de couverture'",    
#                                                                    selectInput("Measure7_TelM", 
#                                                                                label = p(strong("Unité de mesure"),style = "color:DarkCyan"), 
#                                                                                choices = list("2G" = "% de menages - 2G", 
#                                                                                               "3G" = "% de menages - 3G", 
#                                                                                               "4G" = "% de menages - 4G"),
#                                                                                selected = "% de menages - 4G")),
                                                  
selectInput("ISP_TelM", 
            label = p(strong("Opérateur"),style = "color:DarkCyan"),
            choices = c("Proximus"="Proximus",  # Make sure not to mix names with values
                        "Telenet"="Telenet", "Voo"="Voo", "Numericable"="Numericable", "Tous"="All", "Sélectionner"="Selection"),
            width='200px',selected = "All"),


conditionalPanel(condition = "input.ISP_TelM == 'Selection'", 
                 checkboxGroupInput("ISP2_TelM", 
                                    label = p(strong(""),style = "color:DarkCyan"),
                                    choices = list("Proximus"="Proximus","Telenet"="Telenet", "Voo"="Voo", "Numericable"="Numericable"),
                                    selected = c("Proximus", "Telenet", "Voo", "Numericable"))),   
                                                  
                                                  selectInput("period_TelM", 
                                                              label = p(strong("Période"),style = "color:DarkCyan"),
                                                              choices = c("Denière période"="dernière",  # Make sure not to mix names with values
                                                                          "Toutes"="toutes", "Sélectionner"="Selection"),
                                                              width='200px',selected = "toutes"), 
                                                  
                                                  conditionalPanel(condition = "input.period_TelM == 'Selection'",
                                                                   checkboxGroupInput("Period2_TelM", 
                                                                                      label = p(strong("Périodes"),style = "color:DarkCyan"),
                                                                                      choices = list("T4 2014" = "2014-12-31",
                                                                                                     "T1 2015" = "2015-03-30","T2 2015" = "2015-06-30","T3 2015" = "2015-09-30" ),
                                                                                      selected = c("2014-12-31", "2015-03-30", "2015-06-30", "2015-09-30")))

                                                    , 
                                                     
                                                                                               
                                                  downloadButton('downloadData_TelM', 'Télécharger') 
                                           ),
                                           
                                           fluidRow(column(8, showOutput("graphInt1_TelM", lib="highcharts"), align="center")),
                                           br(),
                                           
                                           
                                           sidebarLayout(
                                             
                                             sidebarPanel(p("")
                                             ),
                                             
                                             
                                             
                                             
                                             mainPanel(
                                               #                                                br(),
                                               #                                                
                                               #                                                
                                               #       column(12, tableOutput("tableInternet"), align="left"),
                                               #     p(strong("Indicateurs de qualité")),
                                               #     p("Suivant l'article 113 de la loi du 13 juin 2005 relative aux communications électroniques, 
                                               #                 les opérateurs ont ", br("l'obligation de publier sur leur site Internet des
                                               #                 informations actuelles concernant la qualité des services.")), 
                                               #     
                                               #     p("Les systèmes de mesures sont audités et les données présentées sur ce site sont également présentées",
                                               #       br("dans", a("un rapport annuel de la qualité de service.", 
                                               #                    href = "http://www.arcep.fr/uploads/tx_gspublication/QoS-acces-services-fixes-151014.pdf"))),
                                               
                                               
                                               #     p("Informez-vous sur vos droits en visitant notre rubrique",
                                               #       a("consommateurs",href = "http://www.ibpt.be/fr/consommateurs"),"sur notre site Internet. Parcourez également ",br("notre FAQ sur la mannière de procéder lorsque",
                                               #                                                                                                                          a("la qualité annoncée n'est pas présente.",href="http://www.ibpt.be/fr/consommateurs/telephone/plaintes-problemes/die-qualitat-entspricht-nicht-der-werbung"))),
                                               #     
                                               #     
                                               #     p("Les indicateurs sont également publiés sur le site Internet des opérateurs."),
                                               #     
                                               #     br(),
                                               
                                               
                                               #                                                strong(textOutput("Title_TelM")),br(),
                                               #                                                
                                               tableOutput("table_TelM"),
                                               
                                               
                                               
                                               p(strong("Définition")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelM == 'taux de couverture'",
                                                                p("Number of households passed by a technology capable of delivering 2G, 3G or 4G.")), 
                                               conditionalPanel(condition = "input.Indicator_TelM == 'taux de pannes'",
                                                                p("Number of valid fault reports per access line")),  
                                               conditionalPanel(condition = "input.Indicator_TelM == 'delai de reparation'",
                                                                p("The time by which the fastest 80 % and 95 % of valid 
                                                                  faults on access lines are repaired, or the percentage of faults cleared any time stated as an objective by 
                                                                  the service provider standard accuracy for keeping appointments.")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelM == 'temps de reponse'",
                                                                p("Mean time to answer, and percentage of calls answered within 20 seconds.")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelM == 'plaintes - facturation'",
                                                                p("Percentage of bills resulting in a customer complaint.")),
                                               
                                               conditionalPanel(condition = "input.Indicator_TelM == 'plaintes - vitesse'",
                                                                p("Percentage of complaints logged per customer regarding the speed of the Internet connexion.")),
                                               
                                               p(strong("Source")),
                                               
                                               p("Données fournies par les opérateurs conformément à la décision du Conseil de l'IBPT du xx xx 2015.")
                                               
                                               
                                               
                                             )  )
                                           
                                           
                                           
                                           
                                           
                                           
                                           
                                           
                                           
                                           ), 





tabPanel("Liens utiles",
         
         
         
         sidebarLayout(
           
           sidebarPanel(p("")
           ),
           
           
           mainPanel(
             br(),
             
             
             p(strong('Liens vers les indicateurs de qualité de service sur le site des opérateurs')),
             p("Opérateurs visés par l'obligation du XX/XX/2015"),
             br(),
             a(img(src = "Numericable.png", style="display: block; margin-left: 
                   auto; margin-right: auto;height = 20, width = 20", align = "left"), 
               href="http://www.ibpt.be/fr/consommateurs"),br(),br(),
             
             
             a(img(src = "proximus.png", style="display: block; margin-left: 
                   auto; margin-right: auto;height = 20, width = 20", align = "left"), 
               href="http://www.proximus.be/fr/id_cr_quality/particuliers/notre-offre/r-orphans/mentions-legales/qualite-de-service.html"),
             br(),br(),br(),br(),
             
             
             a(img(src = "Telenet.png", style="display: block; margin-left: 
                   auto; margin-right: auto;height = 20, width = 20", align = "left"), 
               href="http://www.ibpt.be/fr/consommateurs", target="_blank"),br(),br(),br(),br(),
             
             a(img(src = "voo.png", style="display: block; margin-left: 
                   auto; margin-right: auto;height = 20, width = 20", align = "left"), 
               href="http://www.voo.be/fr/conditions-internet-fixe/"),br(),br(),br()
             
             
             
             
             
             
             
             
             
             
             
             
             
             )  )
         
         
             )








,position="above"
                                  
             )))

)
