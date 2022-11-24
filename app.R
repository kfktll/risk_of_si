library(shiny)
library(shinyWidgets)

load("appR.RData")
options(encoding = "UTF-8")

mdasi.values <- subset(df.p, (Variable =="MDASI score"))
hads_a.values <- subset(df.p, (Variable =="HADS anxiety score"))
hads_d.values <- subset(df.p, (Variable =="HADS depression score"))
life_satisfaction.values <- subset(df.p, (Variable =="Life satisfaction"))

ui <- fixedPage(
  fixedRow(
    img(src='bjch.jpg', align = "left")
  ),
  
  setBackgroundColor(
    color = c("#FFFFFF", "#003A62"),
    gradient = "linear",
    direction = "bottom"
  ),
  wellPanel(
    fixedRow(
      h3(HTML(paste("Predicted Risk of Suicidal Ideation ", "in Patients with Advanced Malignant Tumors", sep="<br/>")), align = "center")
    ),
    
    fixedRow(
      (
        h5("To calculate the risk of SI based on the patient characteristics, first determine the point for each predictor by drawing a vertical line from that predictor to the top points scale. Then sum all of the points and draw a vertical line from the total points scale to the risk line to obtain the risk of SI. Abbreviations: HADS, Hospital Anxiety and Depression Scale; MDASI, M.D. Anderson Symptom Inventory.",
           align = "center")
      )),
    
    br(),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'Age.id',
                    label = 'Age',
                    choices = c("<51","51-65","≥66"))
        ,
        selectInput('Sex.id', 
                    'Sex',
                    c("Male","Female"))
        ,
        selectInput('Number.household.members.id', 
                    'Number of household members',
                    c("1-3","4-6","≥7"))
        ,
        selectInput('History.of.chemotherapy.id', 
                    'History of chemotherapy',
                    c("No","Yes"))
        ,
        selectInput('History.of.surgery.id', 
                    'History of surgery',
                    c("No","Yes"))
        ,
        shinyWidgets::sliderTextInput(inputId = "mdasi.score.id",
                                      label = "MDASI score",
                                      choices = c(mdasi.values$Value),
                                      grid = T)
        ,
        shinyWidgets::sliderTextInput("hads_a.score.id",
                                      "HADS anxiety score",
                                      choices = c(hads_a.values$Value),
                                      grid = T)
        ,
        shinyWidgets::sliderTextInput("hads_d.score.id",
                                      "HADS depression score",
                                      choices = c(hads_d.values$Value),
                                      grid = T)
        ,
        shinyWidgets::sliderTextInput("life_satisfaction.score.id",
                                      "Life satisfaction",
                                      choices = c(life_satisfaction.values$Value),
                                      grid = T)
      ),
      
      mainPanel(
        br(),
        h4("Predicted Risk of Suicidal Ideation", align ="center"),
        fluidRow(
          column(12,align="center",
                 h5(tableOutput("Predicted_Risk_Ideation")))),
        fixedRow(
          img(src='FigS3_Nomogram.jpg', height = 500, width = 700, style="display: block; margin-left: auto; margin-right: auto;")
        ),
        uiOutput(outputId = "text")
      )
    ),
    
    HTML('<style type="text/css">
         .span8 .well { background-color: #00FFFF; }
         </style>')
    
  )
  
)

server <- function(input, output, session) {
  
  test <- reactive({subset(df.p, (Variable =="Age" & Value == as.character(input$Age.id))|
                             (Variable =="Sex" & Value == as.character(input$Sex.id))|
                             (Variable =="Number of household members" & Value == as.character(input$Number.household.members.id))|
                             (Variable =="History of chemotherapy" & Value == as.character(input$History.of.chemotherapy.id))|
                             (Variable =="History of surgery" & Value == as.character(input$History.of.surgery.id))|
                             (Variable =="MDASI score" & Value == as.character(input$mdasi.score.id))|
                             (Variable =="HADS anxiety score" & Value == as.character(input$hads_a.score.id))|
                             (Variable =="HADS depression score" & Value == as.character(input$hads_d.score.id))|
                             (Variable =="Life satisfaction" & Value == as.character(input$life_satisfaction.score.id))
  )
  })
  
  
  final.df <- reactive({
    total.points.display <- sum(test()$Points)
    total.points = round(total.points.display,2)
    si.risk.res = -8.37e-07 * total.points ^3 + 0.00027032 * total.points ^2 + -0.017903475 * total.points + 0.3602826
    if(si.risk.res < 0.05){
      si.risk.res = "<0.05"
    } else if(si.risk.res > 0.99){
      si.risk.res = ">0.99"
    } else if(total.points > 190.02){
      si.risk.res = ">0.99"
    } else{}
    si.risk = data.frame(Term = "Suicidal Ideation Risk", Value = si.risk.res)
    
    tp <- data.frame(Term = "Total Points", Value = total.points)
    
    final.display <- rbind(tp, si.risk)
    
    
    return(final.display)
    
  })
  
  
  output$Predicted_Risk_Ideation <- renderTable(final.df(), colnames = FALSE)
  
  output$text <- renderText({
    HTML(paste0('<br/>',"<b>","Title: ","</b>", "Here is the article title",'<br/>','<br/>',
                "<b>","Authors: ","</b>","Here is the authors list",'<br/>','<br/>',
                "<b>","Affiliations: ","</b>","Here is the affiliations")
    )
  })
  
}


shinyApp(ui = ui, server = server)