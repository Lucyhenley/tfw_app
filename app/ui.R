
ui <- fluidPage(
  
  titlePanel(h1("The effect of shielding patterns on carriage capacity",align="centre")),
  
  tags$head(tags$style("#emissionstext{color: red;
                                 font-size: 25px;
                                 }")),
  
  headerPanel(uiOutput("tab",style = "font-size:15px;")),
  
  fluidRow(
    column(3,   wellPanel(  radioButtons(
      "inputSelect",
      label = ("Choose shield patterning:"),
      choices = c(
        "Sequential shield patterning" = 1,
        "Zig-Zag shield patterning" = 0,
        "Manual shield patterning" = 2
      ),

      selected = 1),style = "font-size:17px;" ) ),
    
    
    conditionalPanel(condition = "input.inputSelect == 1",
                     column(3,      wellPanel(sliderInput("NumberofShields", h4("Number of shields", align = "center"),
                                                          min = 1, max = 36, value = 36,width='100%'))),
                     column(3,       wellPanel(sliderInput("ShieldLength", h4("Length of shield", align = "center"),
                                                           min = 0, max = 1.16, value =1.16,width='100%'))),
                     
                     column(3,      wellPanel(sliderInput("SocialDistance", h4("Social distancing rule (m)", align = "center"),
                                                          min = 1, max = 2, value = 2,width='100%')))

                     
                     
    ),
    
    conditionalPanel(condition = "input.inputSelect == 0",

                     column(3,     wellPanel( sliderInput("NumberofShields1", h4("Number of shields", align = "center"),
                                                          min = 1, max = 18, value = 18,width='100%'))),
                     column(3,    wellPanel(   sliderInput("ShieldLength1", h4("Length of shield", align = "center"),
                                                           min = 0, max = 1.16, value =1.16,width='100%'))),
                     
                     column(3,    wellPanel(   sliderInput("SocialDistance1", h4("Social distancing rule (m)", align = "center"),
                                                           min = 1, max = 2, value = 2,width='100%')))
                     
    ),
    
    conditionalPanel(condition = "input.inputSelect == 2",
                     column(3,    wellPanel(   sliderInput("ShieldLength1_MANUAL", h4("Length of shield", align = "center"),
                                                           min = 0, max = 1.16, value =1.16,width='100%'))),
                     
                     column(3,    wellPanel(   sliderInput("SocialDistance_MANUAL", h4("Social distancing rule (m)", align = "center"),
                                                           min = 1, max = 2, value = 2,width='100%')))
                     
    )
    
    
  ),
  
  
  conditionalPanel(condition = "input.inputSelect == 2", column(8,   wellPanel( checkboxGroupInput("TopRowShields", "Top row shield positions:", c("1"=1,"2"=2,"3"=3,"4"=4, "5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18), selected = NULL, inline = TRUE,
                                                                                                   width = NULL),
                                                                                
                                                                                checkboxGroupInput("BottomRowShields", "Bottom row shield positions:", c("1"=1,"2"=2,"3"=3,"4"=4, "5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18), selected = NULL, inline = TRUE,
                                                                                                   width = NULL)))
                   
  ),
  
#  fluidRow(
#    column(8, h1("ediuhe")),
#    column(4,textOutput("emissionstext")       )
#  ),
  
 
  
  fluidRow(
    column(7,
           headerPanel(""),
           headerPanel(""),
           plotOutput("subplots", width = "100%", height = "600px"),      h3("Carriage diagram"),
           img(src="train_floorplan.png",width="600", height="150",align="centre")
           ), 

    
    
    column(5,
           plotOutput("trainemissions", width = "60%"),
           textOutput("emissionstext")
           , align="center"
    )

    
  )
  
  #  fluidRow(
  #    column(8, h1("ediuhe")),
  #    column(4,textOutput("emissionstext")       )
  #  ),
  
  # fluidRow(
  #  column(4),
  # column(4,      h1("Train plan", align = "center"),
  #       img(src="train_floorplan.png",width="600", height="150")
  #)
  #  )
  
)

