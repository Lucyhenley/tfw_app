options(shiny.maxRequestSize=2000*1024^2)

server <- function(input, output, session) {

  usable_seats1 <- reactive({
    
    seat_locations <- read.csv(file=paste0("seat_locations", input$inputSelect, "_1.csv"))
    best_order <- read.csv(file=paste0("best_order", input$inputSelect, "_1.csv"))
      seat_locations <- remove_seats(seat_locations,input$SocialDistance, best_order)
      seat_locations
  })
  
  usable_seats2 <- reactive({
    
    seat_locations <- read.csv(file=paste0("seat_locations", input$inputSelect, "_2.csv"))
    best_order <- read.csv(file=paste0("best_order", input$inputSelect, "_2.csv"))
    seat_locations <- remove_seats(seat_locations,input$SocialDistance, best_order)
  })
  
  output$capacity <- renderText({
      social_distancing <- input$SocialDistance
    seat_locations <- usable_seats()
    cap <- nrow(seat_locations)
    paste("Capacity of 1 train carriage is ", cap, "% with social distancing")
  })
  
  output$emissionstext <- renderText({


    seat_locations <- usable_seats()
    heatmaps <- heatmapper(seat_locations,social_distancing,domain_x,domain_y)
    cap <- nrow(seat_locations)
    emission_dist <- emission_per_pass_train(cap)
    paste0("C02 emissions per passenger are ", floor(emission_dist),  " g/km with social distancing")
  })
  
  output$subplots <- renderPlot({
    
    social_distancing <- input$SocialDistance
    seat_all_1 <- read.csv(file=paste0("seat_locations", input$inputSelect,"_1.csv"))
    seat_all_2 <- read.csv(file=paste0("seat_locations", input$inputSelect,"_2.csv"))
    seat_locations_1  <- usable_seats1()
    seat_locations_2  <- usable_seats2()

    seat_locations_1 <- usable_seats1()
    heatmaps <- heatmapper(seat_locations_1,social_distancing,domain_x,domain_y)
    par(mfrow=c(3,1), mar= c(3,2,5, 1))
    captext <- paste("Capacity of carriage 1 is ", nrow(seat_locations_1), " passengers with social distancing.")
    mytitle <- "Available seats with social distancing measures"
    
    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE,
         xlab="", ylab="")
    mtext(side=3, line=3, at=-0.07, adj=0, cex=1.6,font=2,  mytitle)
    mtext(side=3, line=1.6, at=-0.07, adj=0, cex=1.4, font=2, captext)
    points(seat_locations_1$x,seat_locations_1$y,pch=4,col=rgb(1, 0, 0,1))
    
    for (j in 1:nrow(seat_locations_1)) {
      idx1 <- 1+100*(j-1)
      idx2 <- 100*(j-1) + 100
      polygon(x=heatmaps[1,idx1:idx2],y=heatmaps[2,idx1:idx2],col=rgb(0, 0, 1,0.2))
      points(seat_locations_1[j,"x"],seat_locations_1[j,"y"],cex=2,pch=19)
    }
    points(seat_all_1$x,seat_all_1$y,pch=4,col=rgb(1, 0, 0,1))
    
    lines(x_box,y_box)
  
    heatmaps <- heatmapper(seat_locations_2,social_distancing,domain_x,domain_y)
    
    captext <- paste("Capacity of carriage 2 is ", nrow(seat_locations_2), " passengers with social distancing.")
    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE, xlab="", 
         ylab="")
    mytitle <- "Available seats with social distancing measures and shielding"
    mtext(side=3, line=3, at=-0.07, adj=0, cex=1.6, font=2, mytitle)
    mtext(side=3, line=1.6, at=-0.07, adj=0, cex=1.4, font=2, captext)
    points(seat_locations_2$x,seat_locations_2$y,pch=4,col=rgb(1, 0, 0,1))
    for (j in 1:nrow(seat_locations_2)) {
      idx1 <- 1+100*(j-1)
      idx2 <- 100*(j-1) + 100
      polygon(x=heatmaps[1,idx1:idx2],y=heatmaps[2,idx1:idx2],col=rgb(0, 0, 1,0.2))
      points(seat_locations_2[j,"x"],seat_locations_2[j,"y"],cex=2,pch=19)
      #points(seat_locations_2$x[seat_locations_2$n==j],seat_locations_2$y[seat_locations_2$n==j],pch=19,cex=2)
    }
    points(seat_all_2$x,seat_all_2$y,pch=4,col=rgb(1, 0, 0,1))
    
    lines(x_box,y_box)
    
  
    
    par(mar=c(0,0,0,0))
    plot(NULL, xlim=c(0,10),ylim=c(0,10), axes=FALSE, xlab="", ylab="")
    
    plot_colours <- c("red","black", rgb(0,0,1,0.2))
    markertype <- c(19,19,19)
    text <- c("Unsafe seat","Available seat", "Safe radius")
    legend(x = "top",x.intersp = 0.05,inset = 0,  legend = text, lty = c(NA,NA,NA,1), pt.bg = plot_colours, pt.cex= c(2,2,4,NA),
           col=c("red","black",rgb(0,0,1,0.2),"red"), lwd=c(NA,NA,NA,3), cex=2, pch = markertype, horiz = TRUE, text.width = 1.4)
  })
  

  output$trainemissions <- renderPlot({
    
      social_distancing <- input$SocialDistance
  
    pass <- linspace(1, 76, n = 76)
    seat_locations <- usable_seats1()
    heatmaps <- heatmapper(seat_locations,social_distancing,domain_x,domain_y)
    cap <- nrow(seat_locations)
    emission_dist <- emission_per_pass_train(cap)
    text <- paste0("CO$_{2}$ Emissions per passenger are ", floor(emission_dist), "km$^{-1}$g with social distancing")
    
    text1 <- c(bquote( CO[2] ~ "emissions per passenger are " ~ .(floor(emission_dist)) ~ " with "),
               bquote("social distancing"))
    plot(pass, emission_per_pass_train(pass),type="l", lim=c(0,76), ylim=c(20,2200), log="y",
         xlab="Number of passengers",ylab=TeX("$CO_{2}$ emissions per passenger (km$^{-1}g$)"),lwd=3)

    
    abline(h=130.4,lwd=2,col="red",lty="dashed")
    abline(h=215.3,lwd=2,col="blue",lty="dashed")
    
    lines(c(cap, cap), c(1, emission_per_pass_train(cap)), lty = 1, lwd = 1,col="chartreuse4")   
    lines(c(-100,cap ), c(emission_per_pass_train(cap), emission_per_pass_train(cap)), lty = 1, lwd = 1,col="chartreuse4")
    
        points(cap,emission_per_pass_train(cap),pch=4,col="chartreuse4",cex=2,lwd = 2)
    legend("topright",c("Train","Small car","Large car","Capacity with distancing"),lwd=c(3,2,2,2), lty=c(1,5,5,0), pch=c(NA,NA,NA,4),col=c("black","red","blue","chartreuse4"))
   
  })
  
  output$social_distanced_capacity <- renderPlot({

      social_distancing <- input$SocialDistance
      
    seat_locations <- usable_seats1
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
  
  
  output$train_diagram <- renderPlot({
    
    plot(NULL, xlim=c(0,domain_x), ylim=c(0,domain_y), asp=1, axes=FALSE, xlab="", ylab="")
 #   im<-load.image(paste0("floorplan", input$inputSelect, ".png"))
#    plot(im)
    
    jpg <- stack(paste0("floorplan", input$inputSelect, ".png"))
    plotRGB(jpg)
  })
  
}



