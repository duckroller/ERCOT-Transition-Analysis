function(input, output, session) {
    
  
    selected_data <- eventReactive(input$go,  {
      
      a <- ercot_ts
      a <- subset(a, a$year >= input$date_range[1] & a$year <= input$date_range[2])
      a <- a[is.element(a$hourstart, input$hour_of_day), ]
      a <- a[is.element(a$month, input$month_choice), ]
      
      return(a)
      
      
    })
  
    quantile_regression <- eventReactive(input$go, { 
      withProgress(message = "Selecting data...", {
        
        # first, let's trim our df to the time selected
        
        # a <- ercot_ts
        # a <- subset(a, a$year >= input$date_range[1] & a$year <= input$date_range[2])
        # a <- a[is.element(a$hourstart, input$hour_of_day), ]
        # a <- a[is.element(a$month, input$month_choice), ]
        
        a <- selected_data()
        
        # now let's pull our variable columns into their own vectors
        X <- as.matrix(a[ ,input$xcols])
        Y <- a[ ,input$ycol]
        
        t <- as.numeric(input$quantiles)
        
        incProgress(amount=0.25, 
                    message="Regressing...", 
                    detail = paste("On ", as.character(input$ycol), 
                                   ", using ", as.character(input$xcols),
                                   " for the quantiles ", as.character(input$quantiles), ".",
                                   sep=""))
        
        # perform regression
        if(input$transform == "Natural Log") {
          print(log(Y))
          print(log(X))
          QR <- rq(log(Y)~log(X),tau = t)
        } else if(input$transform == "Scale") {
          QR <- rq(scale(Y)~scale(X),tau = t)
        } else {
          QR <- rq(Y~X,tau = t)
        }
        
      }  
      
      )
        return(QR)
    
    })
    
    quantile_summary <- eventReactive(input$go, {
      withProgress(message = "Summarizing model...", {
        sumQR <- summary(quantile_regression(), se="boot")
        
        return(sumQR)
      })
    })
    
    output$quantile_plot <- renderPlot({
      print(quantile_summary())
      plot(quantile_summary())#, se = "boot")
    })
    
    output$quantile_plot2 <- renderPlot({
      
      
      t <- tabling()
      #plot(quantile_summary())#, se = "boot")
      p <- ggplot(t, aes(t[,2], t[,1])) +
        geom_point(size = 1) +
        geom_quantile(quantiles = as.numeric(input$quantiles), size = 2, aes(colour = ..quantile..)) +
        scale_alpha(range = c(0.3, 0.7)) +
        geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)
      
      p
      })
    
    output$ggpairsplot <- renderPlot({
      
      dat <- tabling()
      explained <- as.numeric(dat[,1])
      #quants <- quantile(dat[,1], input$quantiles)
     # dat$quant <- with(dat, factor(ifelse(dat[,1] < quants[1], 0, 
    #                                       ifelse(dat[,1] < quants[2], 1, 2))))
      
      #print(explained)
      print(quantile(as.numeric(explained),as.numeric(input$quantiles)))
      
      dat$quant <- cut(explained,quantile(as.numeric(dat$Y),as.numeric(input$quantiles)))
      
      print(names(dat))
      print(dat[, names(dat) != "quant"])
      
      q_plotting <- function(data, mapping){
        ggplot(data = data, mapping = mapping)+
          #geom_quantile(quantiles = as.numeric(input$quantiles),  aes(colour = quant)) +
          geom_point(aes(colour = quant)) +
          geom_smooth(method="lm",aes(colour = quant)) 
          #scale_x_continuous(limits = c(-1.5,1.5))+
          #scale_y_continuous(limits = c(-1.5,1.5))
      }
      
      pairs <- ggpairs(dat, 
                       columns = names(dat[, names(dat) != "quant"]),
                       #mapping=ggplot2::aes(colour = quant),
                       #mapping=ggplot2::aes(colour = cut(explained,quantile(explained,input$quantiles)),alpha=0.75),
                       upper=list(continuous=wrap(q_plotting)),#"smooth"),
                      #upper=list(continuous="blank"),
                       axisLabels="none", switch="both") + 
              theme_bw() #+ 
             # scale_color_manual(values=cbPalette) + 
              #scale_fill_manual(values=cbPalette)
     
      pairs$nrow <- 1
      pairs$yAxisLabels <- input$ycol
      return(pairs)
    })
    
    

 
    tabling <- eventReactive(input$go,{
        X <- selected_data()[ ,input$xcols]
        Y <- selected_data()[ ,input$ycol]
        return(data.frame(cbind(Y,X)))
     })
    
    # output$table <- DT::renderDataTable({
    #     tabling()
    # })
    
    output$intro_text <-  output$text2 <- renderUI({
      HTML(' <br>')
    })
    
    output$selected_var1 <- renderText({ 
      input$ycol
    })
    output$selected_var2 <- renderText({ 
      input$xcols
    })
    output$selected_var3 <- renderText({ 
      input$date_range
    })
    output$selected_var4 <- renderText({ 
      input$month_choice
    })
    output$selected_var5 <- renderText({ 
      input$hour_of_day
    })
    output$selected_var6 <- renderText({ 
      input$quantiles
    })

    
    output$plot1 <- renderPlot({
      if(algo_choice() == "K-Means Clustering"){
        fviz_cluster(clusters(), data=selectedData()) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_fill_manual(values=cbPalette) + scale_color_manual(values=cbPalette)
      } else if (algo_choice() == "Hierarchical Clustering"){
        print(clusters())
        fviz_dend(clusters(), k = cluster_ct(), labels_track_height = 2, palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) #+ scale_color_manual(values=cbPalette)
        #fviz_dend(clusters(), labels_track_height = 5, k_colors = cbPalette) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
     
        }
        #palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
         #         "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        #par(mar = c(5.1, 4.1, 0, 1))
        #plot(selectedData(),
        #     col = clusters()$cluster,
        #     pch = 20, cex = 3)
        #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    output$plot2 <- renderPlot({
        states <- tabling()
        states$state <- rownames(states)
        us_states_cluster <- full_join(us_states, states, by = c("region" = "state"))
        c_map <- ggplot() +
                 geom_polygon(data=us_states_cluster,
                              aes(x=long, 
                                  y=lat, 
                                  group = group, 
                                  fill=as.factor(cluster)),
                              color="grey50") +
                 labs(fill = "Cluster")+
                 ggtitle("Geographical Representation of Cluster Assignment")+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_rect(colour = "black", fill=NA, size=1))+
                 coord_map()+ 
                scale_fill_manual(values=cbPalette)
        c_map
        })
    
    output$plot3 <- renderPlot({
      states <- tabling()
      states$state <- rownames(states)
      us_states_cluster <- full_join(us_states, states, by = c("region" = "state"))
      c_map <- ggplot() +
        geom_polygon(data=us_states_cluster,
                     aes(x=long, 
                         y=lat, 
                         group = group, 
                         fill=us_states_cluster[,8]),
                     color="grey50") +
        labs(fill = colnames(us_states_cluster)[8])+
        ggtitle(paste("Geographical Distribution of", colnames(us_states_cluster)[8]))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = c(0.9, 0.2))+
        coord_map()
      c_map
    })
    
    output$plot4 <- renderPlot({
      states <- tabling()
      states$state <- rownames(states)
      us_states_cluster <- full_join(us_states, states, by = c("region" = "state"))
      c_map <- ggplot() +
        geom_polygon(data=us_states_cluster,
                     aes(x=long, 
                         y=lat, 
                         group = group, 
                         fill=us_states_cluster[,9]),
                     color="grey50") +
        labs(fill = colnames(us_states_cluster)[9])+
        ggtitle(paste("Geographical Distribution of", colnames(us_states_cluster)[9]))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = c(0.9, 0.2))+
        coord_map()
      c_map
    })
    
  
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(paste("emissionsClusters",input$xcol, input$ycol, sep = "_"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data.frame(cluster = clusters()$cluster, selectedData()), file, row.names = TRUE)
        }
    )
    
    output$selected_states <- renderPrint(input$state_choice)
    
    
}