options(shiny.maxRequestSize=2000*1024^2)

server <- function(input, output, session) {
  
  url <- a("here", href="https://github.com/Lucyhenley/CardiffMATHBIO_NERCHackathonTwo_PublicTransport")
  output$tab <- renderUI({
    tagList("This app is Cardiff University MATHBIO's entry to COVID-19 Hackathon 2: Recovery. The goal is to reduce public transport
  emissions per person by creating a tool that will present an optimal seating arrangement under social distancing.
            To use the app, adjust the sliders to find optimal seating arrangements following social distancing in varying situations.
            For more information, click ", url, ".")
  })
  
  usable_seats <- reactive({
    if (input$inputSelect == 1 ){
      seat_locations <- remove_seats(seat_locations,input$SocialDistance)
    }
    else if (input$inputSelect == 2 ){
      
      
      seat_locations <- remove_seats(seat_locations,input$SocialDistance_MANUAL)
      
    }
    else{
      seat_locations <- remove_seats(seat_locations,input$SocialDistance1)
    }
  })
  
  usable_shields <- reactive({
    if (input$inputSelect == 1 ){
      return(shield_locations_to_use(input$ShieldLength,input$NumberofShields,shield_locations))
    }
    
    else if (input$inputSelect == 2 ){
      
      return(manual_shield_selection(input$ShieldLength1_MANUAL,shield_locations,input$TopRowShields,input$BottomRowShields))
    }
    
    
    else{
      return(use_zig_zag_shields(input$ShieldLength1,input$NumberofShields1,shield_locations))
    }
    
    
    
    
  })
  
  shielded_seats <- reactive({
    shield_loc <-   usable_shields()
    heatmaps <- 1
    
    
    if (input$inputSelect == 1 ){
      heatmaps <- shielded_heatmapper(seat_locations,shield_loc,input$SocialDistance,domain_x,domain_y)
      seats <- remove_seats_shields(seat_locations,input$SocialDistance,heatmaps)
    }
    else if (input$inputSelect == 2 ){
      
      
      if (shield_loc[4]==0){
        
        heatmaps <- heatmapper(seat_locations,input$SocialDistance_MANUAL,domain_x,domain_y)
        seats <- remove_seats_shields(seat_locations,input$SocialDistance_MANUAL,heatmaps)
        
      }
      else{
        heatmaps <- shielded_heatmapper(seat_locations,shield_loc,input$SocialDistance_MANUAL,domain_x,domain_y)
        seats <- remove_seats_shields(seat_locations,input$SocialDistance_MANUAL,heatmaps)
      }
    }
    
    
    
    else{
      heatmaps <- shielded_heatmapper(seat_locations,shield_loc,input$SocialDistance1,domain_x,domain_y)
      seats <- remove_seats_shields(seat_locations,input$SocialDistance1,heatmaps)
    }
    
    return(seats)
  })
  
  
  
  output$capacity <- renderText({
    social_distancing <- 2 #to be overwritten
    if (input$inputSelect == 1 ){
      
      social_distancing <- input$SocialDistance
      
    }
    else if (input$inputSelect == 2 ){
      
      social_distancing <- input$SocialDistance_MANUAL
      
    }
    else{
      
      social_distancing <- input$SocialDistance1
    }
    
    seat_locations <- usable_seats()
    heatmaps <- heatmapper(seat_locations,social_distancing,domain_x,domain_y)
    shield_loc <- usable_shields()
    heatmaps <- shielded_heatmapper(seat_locations,shield_loc,social_distancing,domain_x,domain_y)
    seats <- shielded_seats()
    cap <- nrow(seat_locations)
    paste("Capacity of 1 train carriage is ", cap, "% with social distancing or ", round(100*nrow(seats)/76), "% with shields")
  })
  
  output$emissionstext <- renderText({

    social_distancing <- 2 #to be overwritten
    if (input$inputSelect == 1 ){
      
      social_distancing <- input$SocialDistance
      
    }
    else if (input$inputSelect == 2 ){
      
      social_distancing <- input$SocialDistance_MANUAL
      
    }
    else{
      
      social_distancing <- input$SocialDistance1
    }
    
    seat_locations <- usable_seats()
    heatmaps <- heatmapper(seat_locations,social_distancing,domain_x,domain_y)
    shield_loc <- usable_shields()
    if (shield_loc[4] != 0) {
      heatmaps <- shielded_heatmapper(seat_locations,shield_loc,social_distancing,domain_x,domain_y)
    }
    seats <- shielded_seats()
    cap <- nrow(seat_locations)
    emission_dist <- emission_per_pass_train(cap)
    emission_shield <- emission_per_pass_train(nrow(seats))
    paste0("C02 emissions per passenger are ", floor(emission_dist),  " g/km with social distancing,
or " , floor(emission_shield), " g/km with shields.")
  })
  
  
  

  output$subplots <- renderPlot({
    
    social_distancing <- 2 #to be overwritten
    if (input$inputSelect == 1 ){
      social_distancing <- input$SocialDistance
    }
    else if (input$inputSelect == 2 ){
      
      social_distancing <- input$SocialDistance_MANUAL
      
    }
    else{
      social_distancing <- input$SocialDistance1
    }
    seat_sd <- usable_seats()
    heatmaps <- heatmapper(seat_sd,social_distancing,domain_x,domain_y)
    par(mfrow=c(3,1), mar= c(3,2,5, 1))
    captext <- paste("Capacity of 1 train carriage is ", nrow(seat_sd), " passengers with social distancing.")
    mytitle <- "Available seats with social distancing measures"

    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE,
         xlab="", ylab="")
    mtext(side=3, line=3, at=-0.07, adj=0, cex=1.6,font=2,  mytitle)
    mtext(side=3, line=1.6, at=-0.07, adj=0, cex=1.4, font=2, captext)
    points(seat_locations$x,seat_locations$y,pch=4,col=rgb(1, 0, 0,1))

    for (j in 1:nrow(seat_sd)) {
      idx1 <- 1+100*(j-1)
      idx2 <- 100*(j-1) + 100
      polygon(x=heatmaps[1,idx1:idx2],y=heatmaps[2,idx1:idx2],col=rgb(0, 0, 1,0.2))
      points(seat_sd[j,"x"],seat_sd[j,"y"],cex=2,pch=19)
    }

    lines(x_box,y_box)
    seats <- shielded_seats()
    shield_loc <- usable_shields()
    
    
    
    if (shield_loc[4]==0){
      heatmaps <- heatmapper(seat_locations,social_distancing,domain_x,domain_y)
      
    }else{
      
      heatmaps <- shielded_heatmapper(seat_locations,shield_loc,social_distancing,domain_x,domain_y)
      
    }

    captext <- paste("Capacity of 1 train carriage is ", nrow(seats), " passengers with shielding.")
    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE, xlab="", 
         ylab="")
    mytitle <- "Available seats with social distancing measures and shielding"
    mtext(side=3, line=3, at=-0.07, adj=0, cex=1.6, font=2, mytitle)
    mtext(side=3, line=1.6, at=-0.07, adj=0, cex=1.4, font=2, captext)
    points(seat_locations$x,seat_locations$y,pch=4,col=rgb(1, 0, 0,1))
    for (j in seats$n) {
      idx1 <- 1+100*(j-1)
      idx2 <- 100*(j-1) + 100
      polygon(x=heatmaps[1,idx1:idx2],y=heatmaps[2,idx1:idx2],col=rgb(0, 0, 1,0.2))
      points(seats$x[seats$n==j],seats$y[seats$n==j],pch=19,cex=2)
    }
    lines(x_box,y_box)
    
    
    if (shield_loc[4]!=0){
      for (i in 1:nrow(shield_loc)){
        lines(c(shield_loc[i,1], shield_loc[i,2]),  c(shield_loc[i,3], shield_loc[i,4]), lty = 1, lwd = 3,col ='red'   )
      }
    }
    
    
    par(mar=c(0,0,0,0))
    plot(NULL, xlim=c(0,10),ylim=c(0,10), axes=FALSE, xlab="", ylab="")

    plot_colours <- c("red","black", rgb(0,0,1,0.2))
    markertype <- c(19,19,19)
    text <- c("Unsafe seat","Available seat", "Safe radius","Shields")
    legend(x = "top",x.intersp = 0.05,inset = 0,  legend = text, lty = c(NA,NA,NA,1), pt.bg = plot_colours, pt.cex= c(2,2,4,NA),
           col=c("red","black",rgb(0,0,1,0.2),"red"), lwd=c(NA,NA,NA,3), cex=2, pch = markertype, horiz = TRUE, text.width = 1.4)

  })

  
  output$train_diagram <- renderPlot({
    
    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE, xlab="", ylab="")
    IM = readPNG("floorplan2.png")
    rasterImage(IM,0,0,135.6,21.2)
    
  })
  
  output$trainemissions <- renderPlot({
    
    social_distancing <- 2 #to be overwritten
    if (input$inputSelect == 1 ){
      
      social_distancing <- input$SocialDistance
      
    }
    else if (input$inputSelect == 2 ){
      
      social_distancing <- input$SocialDistance_MANUAL
      
    }
    else{
      
      social_distancing <- input$SocialDistance1
    }
    
    pass <- linspace(1, 76, n = 76)
    seat_locations <- usable_seats()
    heatmaps <- heatmapper(seat_locations,social_distancing,domain_x,domain_y)
    shield_loc <- usable_shields()
    
    if (shield_loc[4]==0){
      
      heatmaps <- heatmapper(seat_locations,input$SocialDistance_MANUAL,domain_x,domain_y)
      
    }else{
      
      heatmaps <- shielded_heatmapper(seat_locations,shield_loc,social_distancing,domain_x,domain_y)
      
    }
    
    seats <- shielded_seats()
    cap <- nrow(seat_locations)
    pass_dist <- nrow(seat_locations)
    pass_shield <- nrow(seats)
    emission_dist <- emission_per_pass_train(pass_dist)
    emission_shield <- emission_per_pass_train(pass_shield)
    text <- paste0("CO$_{2}$ Emissions per passenger are ", floor(emission_dist), "km$^{-1}$g with social distancing, 
                   or ", floor(emission_shield), "km$^{-1}$g with shields.")
    
    text1 <- c(bquote( CO[2] ~ "emissions per passenger are " ~ .(floor(emission_dist)) ~ " with "),
               bquote("social distancing"))
   # par(mfrow=c(2,1))
    plot(pass, emission_per_pass_train(pass),type="l", lim=c(0,76), ylim=c(20,2200), log="y",
         xlab="Number of passengers",ylab=TeX("$CO_{2}$ emissions per passenger (km$^{-1}g$)"),lwd=3)

    
    abline(h=130.4,lwd=2,col="red",lty="dashed")
    abline(h=215.3,lwd=2,col="blue",lty="dashed")
    
    lines(c(pass_dist, pass_dist), c(1, emission_per_pass_train(pass_dist)), lty = 1, lwd = 1,col="chartreuse4")   
    lines(c(-100,pass_dist ), c(emission_per_pass_train(pass_dist), emission_per_pass_train(pass_dist)), lty = 1, lwd = 1,col="chartreuse4")
    
    
    lines(c(-100,pass_shield ), c(emission_per_pass_train(pass_shield), emission_per_pass_train(pass_shield)), lty = 1, lwd = 1,col="darkorchid")
    lines(c(pass_shield, pass_shield), c(1, emission_per_pass_train(pass_shield)), lty = 1, lwd = 1,col="darkorchid")
    
    points(pass_dist,emission_per_pass_train(pass_dist),pch=4,col="chartreuse4",cex=2,lwd = 2)
    points(pass_shield,emission_per_pass_train(pass_shield),pch=4,col="darkorchid",cex=2,lwd=2)
    legend("topright",c("Train","Small car","Large car","Capacity with distancing","Capacity with shielding"),lwd=c(3,2,2,2,2), lty=c(1,5,5,0,0), pch=c(NA,NA,NA,4,4),col=c("black","red","blue","chartreuse4","darkorchid"))
   
  })
  
  output$social_distanced_capacity <- renderPlot({
    
    social_distancing <- 2 #to be overwritten
    if (input$inputSelect == 1 ){
      
      social_distancing <- input$SocialDistance
      
    }
    else if (input$inputSelect == 2 ){
      
      social_distancing <- input$SocialDistance_MANUAL
      
    }
    else{
      
      social_distancing <- input$SocialDistance1
    }
    
    seat_locations <- usable_seats()
    heatmaps <- heatmapper(seat_locations,social_distancing,domain_x,domain_y)
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE, xlab="", ylab="")
    for (j in 1:nrow(seat_locations)) {
      par(fig=c(0,1,0,1))
      idx1 <- 1+100*(j-1)
      idx2 <- 100*(j-1) + 100
      polygon(x=heatmaps[1,idx1:idx2],y=heatmaps[2,idx1:idx2],col=rgb(1, 0, 0,0.1))
      points(seat_locations[j,"x"],seat_locations[j,"y"],pch=19)
    }
    lines(x_box,y_box)
  }, height=75)
  
  output$shielded_capacity <- renderPlot({
    
    social_distancing <- 2 #to be overwritten
    if (input$inputSelect == 1 ){
      
      social_distancing <- input$SocialDistance
      
    }
    else if (input$inputSelect == 2 ){
      
      social_distancing <- input$SocialDistance_MANUAL
      
    }
    else{
      
      social_distancing <- input$SocialDistance1
    }
    
    seats <- shielded_seats()
    shield_loc <- usable_shields()
    heatmaps <- shielded_heatmapper(seat_locations,shield_loc,social_distancing,domain_x,domain_y)
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE, xlab="", ylab="")
    for (j in seats$n) {
      par(fig=c(0,1,0,1))
      idx1 <- 1+100*(j-1)
      idx2 <- 100*(j-1) + 100
      polygon(x=heatmaps[1,idx1:idx2],y=heatmaps[2,idx1:idx2],col=rgb(1, 0, 0,0.1))
      points(seats$x[seats$n==j],seats$y[seats$n==j],pch=19)
    }
    lines(x_box,y_box)
  }, height=75)
  
  
}



