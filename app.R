library(shinythemes)
library(shiny)
library(shinydashboard)
require(visNetwork)
library(sparkline)

options(shiny.maxRequestSize = 30*1024^2)


ui<-fluidPage(theme = shinytheme("journal"),
              navbarPage("BaobArd Grégoire Le Campion",
                         tabPanel("A propos",
                                  #code Html
                                  h3(strong("Bienvenue sur l'application BaobARD une appli qui vous permet de créer votre propre Arbre de décision !")),
                                  p(strong("L'application BaobARD"),"vous permet de créer votre propre arbre de régression ou de classification à partir d'un simple fichier csv !"),
                                  p("Si vous n'avez aucune idée de ce que peuvent être des arbres de régression ou de classification ou pourquoi les utiliser, un rappel très succint sera effectué juste ci dessous, mais la ", strong(a("présentation stockée ici sur github", href="https://lecampiong.github.io/baobard/#1")), " vous permettra d'appréhender ce que sont et permettent les arbres de décisison. Si vous souhaitez savoir aller plus loin que ce que permet cette application et réaliser vous même sur R vos arbres de décision un guide est en cours de construction sur le site web", strong(a("OUVRIR", href="http://ouvrir.passages.cnrs.fr/arbre_decision/_book/index.html")),"."),
                                  p("Outre cette première page, cette application contient 2 autres onglets."),
                                  p("Le premier vous permet de charger et visualiser les données que vous souhaitez analyser. Vous pouvez importer uniquement des fichiers au format csv."),
                                  p("Le deuxième onglet vous permettra de construire et visualiser un arbre de décision. Il vous reviendra de déterminer la variable que vous souhaitez prédire et les variables explicatives. Une fois satisfait vous pourrez télécharger votre visualisation aux formats png, pdf ou svg."),
                                  br(),
                                  h3(strong("Pourquoi un arbre de décision ?")),
                                  p("Parce que vous voulez enfin pouvoir faire plus qu'une corrélation ! Parce que la liste des pré-requis pour faire une régression ou tout autre test statistique cherchant à étudier la causalité est plus longue que le bras ! Et surtout parce que jamais vos données n'ont pu remplir une seule de ces conditions !"),
                                  p("L'arbre de décision est",strong(em("un outil tout terrain")), "parfaitement adapté aux données en SHS."),
                                  p("C'est un outil statistique d’exploration des données et de prédiction.
                           Il peut servir à expliquer aussi bien une variable qualitative (on parle alors d’arbre de classification) qu'une variable quantitative (arbre de régression).
                           Par rapport à d’autres méthodes classiques (analyse factorielle, régression, réseau de neurones...) l’arbre de décision possède de nombreux avantages :"),
                                  p("- les données d’entrée peuvent être « mixtes », c’est-à-dire qu’un même arbre peut utiliser simultanément des variables prédictives qualitatives, ordinales et continues ;"),
                                  p("- la gestion des données manquantes est particulièrement efficace ;"),
                                  p("- la construction d’un arbre est rapide et peu gourmande en ressources ;"),
                                  p("- l'arbre de décision est relativement simple à interpréter."),
                                  p("Plus un certain nombre d'autres avantages non négligeable ! Je vous invite une fois de plus à vous rendre sur la paghe dédiée sur", strong(a("OUVRIR", href="http://ouvrir.passages.cnrs.fr/arbre_decision/_book/index.html")),"."),
                                  br(),
                                  h3(strong("Citation")), 
                                  p("Dans l'éventualité où vous utiliseriez cette application pour créer un arbre de décision et que vous le publiez, vous pouvez citer cette application comme ceci :"),
                                  p(strong("Le Campion G. ", a("Baobard: un outil de visualisation et d'exploration de vos données à l'aide d'un arbre de décision.", href="https://analytics.huma-num.fr/Gregoire.LeCampion/Arbre_de_decision/")," Pôle ARD UMR 5319 UMR Passages. 2019."))
                         ),
                         
                         tabPanel("Import des données",
                                  sidebarLayout(
                                    ############################################################
                                    # 1. Le menu de gauche, présentant les options de l'import #
                                    ############################################################
                                    sidebarPanel(fileInput("file1", "Charger un fichier CSV",
                                                           multiple = FALSE,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain", "text/semicolon-separated-values,text/plain" ,
                                                                      ".csv")),
                                                 h5(helpText("Le poid des fichier est limité à 30Mo")),
                                                 h5(helpText("Charger des fichiers au format UTF-8")),
                                                 tags$hr(),
                                                 h5(helpText("Ajuster les options suivantes en fonction de votre fichier importé")),
                                                 selectizeInput("encoding", label="Encodage du fichier", choices=c("UTF-8", "ISO-8859-15", "WINDOWS-1252"), selected= "UTF-8", multiple=FALSE),
                                                 # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                                 checkboxInput("header", "1ere ligne comme en-tête", TRUE),
                                                 #déterminer le séparateur de champ
                                                 radioButtons("sep", "Séparateur de champ",
                                                              choices = c(Comma = ",",
                                                                          Semicolon = ";",
                                                                          Tab = "\t"),
                                                              selected = ","),
                                                 #déterminer le séparateur de champ
                                                 radioButtons("decimal", "Décimale pour les variables numériques",
                                                              choices = c(Virgule = ",",
                                                                          Point = "."),
                                                              selected = "."),
                                                 #déterminer séparateur de texte
                                                 radioButtons("quote", "Séparateur de texte",
                                                              choices = c("Aucun" = "",
                                                                          "Guillemet double" = '"',
                                                                          "Guillemet simple" = "'"),
                                                              selected = '"')#,
                                                 #Choix du mode de visualistaion
                                                 #radioButtons("disp", "Visualiser",
                                                #              choices = c("Uniquement les 1eres lignes" = "head",
                                                 #                         "Ensemble des données"= "all"),
                                                #              selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("Données",
                                               DT::dataTableOutput("tb1")
                                      )
                                    ))
                                  ) ),
                         tabPanel("Arbres de décision",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      selectizeInput("selectVd", label="Variable à prédire", choices=NULL, multiple=FALSE),
                                      
                                      # 1.2.1 Choix des variables prédictives :
                                      selectizeInput("selectVars", label="Variables prédictives", choices=NULL, multiple=TRUE),
                                      
                                      # 1.3. Paramètres de l'analyse :
                                      h3("Paramètres de l'analyse"),
                                      numericInput("minsplitValue", label="Nombre minimal de cas requis dans un noeud pour tenter un split", value=5, step=1, min=0, max=40),
                                      numericInput("minbucketValue", label="Nombre minimal de cas requis dans une feuille terminale", value=5, step=1, min=0, max=50),
                                      sliderInput("xvalValue", label="Nombre de divisions pour la validation croisée", value=100, min=0, max=200, step=10),
                                      
                                      # 1.4. Paramètres d'affichage graphique :
                                      h3("Paramètres d'affichage du graphique"),	
                                      radioButtons("typeGraph", label="Type de graphique", choices=list("Graphique classique", "Graphique avec couleurs et pourcentages"), selected="Graphique classique"),
                                      
                                      conditionalPanel(condition="input.typeGraph == 'Graphique classique'",
                                                       checkboxInput("uniformValue", label="Espacement vertical uniforme", value=TRUE),
                                                       checkboxInput("usenValue", label="Afficher les décomptes dans chaque noeud", value=TRUE),
                                                       checkboxInput("fancyValue", label="Distinguer les feuilles terminales", value=TRUE),
                                                       checkboxInput("allValue", label="Légender tous les noeuds", value=TRUE)
                                      )
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("1- Arbre brut",
                                                 h3("Affichage de l'arbre"),
                                                 plotOutput("displayTree", height="700px"),
                                                 radioButtons(
                                                   inputId = "filetype_tree",
                                                   label = "Quel format d'image :",
                                                   inline = TRUE,
                                                   choices = list("PDF", "PNG","SVG")),
                                                 downloadButton(outputId = "downloadTree", label = "Télécharger Arbre")
                                        ),
                                        tabPanel("2- Complexité",
                                                 plotOutput("displayCp", height="550px"),
                                                 h5("Affichage du cp optimal, c'est à dire le niveau d'élagage idéal !"),
                                                 verbatimTextOutput("cp_result")
                                        ),
                                        tabPanel("3- Arbre élagué",
                                                 h3("Affichage de l'arbre élagué"),
                                                 plotOutput("displayTree02", height="700px"),
                                                 radioButtons(
                                                   inputId = "filetype_tree2",
                                                   label = "Quel format d'image :",
                                                   inline = TRUE,
                                                   choices = list("PDF", "PNG","SVG")),
                                                 downloadButton(outputId = "downloadTree02", label = "Télécharger Arbre")
                                        ),
                                        tabPanel("4- Règles de construction",
                                                 h3("Vous trouverez ici les règles de décision et construction de l’arbre élagué"),
                                                 verbatimTextOutput("arbre_result"),
                                                 h5("Chaque ligne correspond à un noeud ou à une feuille de l’arbre. On commence par la racine (1) qui contient les n individus de notre population."),
                                                 h5("A l’étape suivante l’arbre découpe la population en fonction de la variable déterminée et créé les noeuds 2) et 3) et ainsi de suite jusqu'à arriver aux feuilles terminales.")
                                        ),
                                        tabPanel("5- Arbre interactif",
                                                 visNetworkOutput("network", height="750px"))
                                      )
                                    )
                                  )
                                  
                         )
              )
)


server <- function(input, output, session) {
  
  library(rpart)
  library(rattle)
  library(svglite)
  library(shiny)
  library(shinydashboard)
  library(shinythemes)
  library(DT)
  require(visNetwork)
  library(sparkline)
  source("tracerArbre.R")
  
  
  
  #####################################################
  # 5. Charger les données importées et les visualiser#
  #####################################################
#  data <- reactive({
#    file1 <- input$file
#    if(is.null(file1)){return()} 
#    read.csv(input$file1$datapath,
#             header = input$header,
#             fileEncoding = input$encoding,
#             sep = input$sep,
#             dec = input$decimal,
#             quote = input$quote)
#    
#  })  
#  output$table <- renderTable({
#    req(input$file1)
    
    # Si le séparateur de champs est un point-virgule,
    # Avoir une virgule comme séparateur causera une erreur dans la fonction read.csv
#    df <- reactive({ read.csv(input$file1$datapath,
#                              header = input$header,
#                              sep = input$sep,
#                              dec = input$decimal,
#                              fileEncoding = input$encoding,
#                              quote = input$quote)
#    })
    
#    if(input$disp == "head") {
#      return(head(df()))
#    }
#    else {
#      return(df())
#    }
 # })
  
#  output$tb1 <- renderUI({
#    tableOutput("table")
#  })
  
  data01 <- reactive({
    file1 <- input$file1
    req(file1)
    data01 <- read.csv(file=file1$datapath, header = input$header, sep = input$sep, quote = input$quote, dec = input$decimal, fileEncoding = input$encoding)
    data01
    })
  
  output$tb1 = DT::renderDataTable({
    data01()
  },extensions = c("FixedHeader", "Scroller"),  rownames = TRUE, filter = "top", class = "cell-border stripe",
  options = list(dom = "Blfrtip", searching = T, searchHighlight = TRUE,  scrollY = 400,  scroller = TRUE, scrollX = TRUE))
  
  
  ################################################################
  # 6. Charger les données et maj des selectizeInput pour l'arbre#
  ################################################################ 
  dat <- reactive({
    file1 <- input$file1
    req(file1)
    dataSet <- read.csv(file=file1$datapath, header = input$header, sep = input$sep, quote = input$quote, dec = input$decimal, fileEncoding = input$encoding)
    vars <- colnames(dataSet)
    #row <- nrow(dataSet)
    
    updateSelectizeInput(session, "selectVd", "Variable à prédire", choices = vars)
    updateSelectizeInput(session, "selectVars", "Variables prédictives", choices = vars)
    #updateSliderInput(session, "xvalValue", max=row)
    dataSet
  })
  
  observe({
    varVI <- colnames(dat())
    varVI <- varVI[!(varVI %in% input$selectVd)]
    updateSelectizeInput(session, "selectVars", "Variables prédictives", choices = varVI)
  })
  
  datred <- reactive({
    dat()[,c(as.character(input$selectVd), as.character(input$selectVars))]
  })
  
  arbre <- reactive({
    rpart(as.formula(paste(input$selectVd,"~", paste(input$selectVars, collapse="+"))), minsplit=input$minsplitValue, minbucket=input$minbucketValue, xval=input$xvalValue, data=datred())
  })
  
  output$displayTree <- renderPlot({
    validate(
      need(input$selectVars != "", "Sélectionnez une ou des variables prédictives"))
    tracerArbre(arbre(), type=input$typeGraph, uniform=input$uniformValue, use.n=input$usenValue, all=input$allValue, fancy=input$fancyValue)
  })
  
  output$downloadTree <- downloadHandler(
    filename = function(){
      paste("Arbre_decision", tolower(input$filetype_tree), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_tree == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_tree == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      tracerArbre(arbre(), type=input$typeGraph, uniform=input$uniformValue, use.n=input$usenValue, all=input$allValue, fancy=input$fancyValue)
      dev.off()
    })
  
  output$displayCp <- renderPlot({
    validate(
      need(input$selectVars != "", "Sélectionnez une ou des variables prédictives"))
    plotcp(arbre())
  })
  
  output$cp_result<-renderPrint({
    validate(
      need(input$selectVars != "", "Sélectionnez une ou des variables prédictives"))
    print(arbre()$cptable[which.min(arbre()$cptable[,4]),1])})
  
  arbre2 <- reactive({
    prune(arbre(), cp=arbre()$cptable[which.min(arbre()$cptable[,4]),1])
  })
  
  output$displayTree02 <- renderPlot({ 
    validate(
      need(input$selectVars != "", "Sélectionnez une ou des variables prédictives"))
    tracerArbre(arbre2(), type=input$typeGraph, uniform=input$uniformValue, use.n=input$usenValue, all=input$allValue, fancy=input$fancyValue)
  })
  
  output$downloadTree02 <- downloadHandler(
    filename = function(){
      paste("Arbre_decision_élagué", tolower(input$filetype_tree2), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_tree2 == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_tree2 == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      tracerArbre(arbre2(), type=input$typeGraph, uniform=input$uniformValue, use.n=input$usenValue, all=input$allValue, fancy=input$fancyValue)
      dev.off()
    })
  
  output$arbre_result<-renderPrint({
    validate(
      need(input$selectVars != "", "Sélectionnez une ou des variables prédictives"))
    print(arbre2())
  })
  
  output$network<- renderVisNetwork({
    validate(
      need(input$selectVars != "", "Sélectionnez une ou des variables prédictives"))
    visTree(arbre2(), nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)
  })
  
  
}


shinyApp(ui=ui,server=server)


