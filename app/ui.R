
ui <- fluidPage(
  
  titlePanel(h1("Tfw train capacity with social distancing",align="centre")),
  
  headerPanel(uiOutput("tab",style = "font-size:15px;")),
  
  fluidRow(
    column(3,   wellPanel(  radioButtons(
      "inputSelect",
      label = ("Choose train model:"),
      choices = c(
 #       "142" = "142",
        "150" = "150"
      ),

      selected = 150),style = "font-size:17px;" ) ),
    
    
 column(3,      wellPanel(sliderInput("SocialDistance", h4("Social distancing rule (m)", align = "center"),
                                                          min = 1, max = 2, value = 2,width='100%')))

                   
  ),
  

  fluidRow(
    column(2),
    column(8,
           headerPanel(""),
           headerPanel(""),
           plotOutput("subplots", width = "100%", height = "600px"),      
           h3("Carriage diagram"),
           plotOutput("train_diagram", width = "100%")
           ) 

    
    
 
    #   column(5,
        #   plotOutput("trainemissions", width = "60%"),
          # textOutput("emissionstext")
         #  align="center"
    #)

    
  )
  
)

