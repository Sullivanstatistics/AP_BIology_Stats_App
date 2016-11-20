library(shiny)
library(MASS)
library(car)
library(rmarkdown)
library(plyr)
library(datasets)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

range <- function(x){
  max(x) - min(x)
}

shinyServer(function(input, output) {
 
  
    datasetInput <- reactive({
      switch(input$dataset,
             "Chick Weights by Feed Type" = chickwts,
             "Iris Data" = iris,
             "Student's Sleep Data" = sleep,
             "Vitamin-C and Teeth" = ToothGrowth)
    })
    
    
    output$mytable1 = renderDataTable({
      chickwts
    }, options = list(lengthMenu = c(5, 15,25, 50), pageLength = 10))
    output$mytable2 = renderDataTable({
      iris
    }, options = list(lengthMenu = c(5, 15,25, 50), pageLength = 10))
    output$mytable3 = renderDataTable({
      sleep
    }, options = list(lengthMenu = c(5, 15,25, 50), pageLength = 10))
    output$mytable4 = renderDataTable({
      ToothGrowth
    }, options = list(lengthMenu = c(5, 15,25, 50), pageLength = 10))
    

    
    output$chicks <- renderDataTable({
      DT::datatable(chickwts)
    })
    
   
    
    output$matrix <- renderTable({
      
      
      dataset <- datasetInput()
      
      
      
      dataset2 <- Filter(is.numeric, dataset)
      means <- round(apply(dataset2,2 , mean),2)
      median <- apply(dataset2, 2, median)
      mode <- apply(dataset2, 2, Mode)
      range <- apply(dataset2,2, range)
      sd <- round(apply(dataset2, 2, sd),2)
    names <- names(dataset2)
      
      
      matrix <- matrix(c( names, means, median,mode, range, sd), ncol=6)
      colnames(matrix) <- c("Names", "Mean", "Median", "Mode", "Range", "SD")
      matrix
    })
    
   
    output$tables <- renderTable({
      
      
      dataset <- datasetInput()
      dataset2 <- Filter(Negate(is.numeric), dataset)
      plyr::count(dataset2)
    })


    
 ### Graphs Page
### Sidebar Input Selector
    output$gtype1 <- renderUI({
      if(input$var_type==1){
        selectInput("gtype", label="What type of Graph do you want to make?", 
                    choices = list("Box-Whisker Plot" = 1, "Scatterplot"= 2), selected = 1)
      }
      })
    
    
    output$gtype2 <- renderUI({
      if(input$var_type==2){
        selectInput("gtype", label="What type of Graph do you want to make?", 
                    choices = list("Box-Whisker Plot" = 1), selected = 1)
      }
        
      })
    
    output$gtype3 <- renderUI({
      
      if (input$var_type==3){
        selectInput("gtype", label="What type of Graph do you want to make?", 
                    choices = list("Bar Plot" = 1, "Mosaic Plot"= 2), selected = 1)
        
      }
      
    })
    
    
### Graphs Page
### Variable Selectors
    
    output$var1_bw_x1 <- renderUI({
      if(input$var_type==1){
      if(input$gtype==1){
        selectInput("x1", "Pick the first Variable to make a Box-Whisker Plot with.",
                    names(datasetInput()), multiple =FALSE)}}
    })
    output$var1_bw_x2 <- renderUI({
      if(input$var_type==1){
      if(input$gtype==1){
        selectInput("x2", "Pick the second Variable to make a Box-Whisker Plot with.",
                    names(datasetInput()), multiple =FALSE)}}
    })
    
    output$var1_sp_x1 <- renderUI({
      if(input$var_type==1){
      if(input$gtype==2){
        selectInput("x1", "Pick the x-axis variable for the Scatterplot.",
                    names(datasetInput()), multiple =FALSE)}}
    })
    output$var1_sp_x2 <- renderUI({
      if(input$var_type==1){
      if(input$gtype==2){
        selectInput("x2", "Pick the y-axis variable for the Scatterplot.",
                    names(datasetInput()), multiple =FALSE)}}
    })
    
    output$var2_bw_x1 <- renderUI({
      if(input$var_type==2){
      if(input$gtype==1){
        selectInput("x1", "Pick the continuous variable to make the Box-Whisker Plot with.",
                    names(datasetInput()), multiple =FALSE)}}
    })
    output$var2_bw_x2 <- renderUI({
      if(input$var_type==2){
      if(input$gtype==1){
        selectInput("x2", "Pick the grouping (categorical) variable to make the Box-Whisker Plot with.",
                    names(datasetInput()), multiple =FALSE)}}
    })  
    
    
    output$var3_bp_x1 <- renderUI({
      if(input$var_type==3){
        if(input$gtype==1){
          selectInput("x1", "Pick the first Variable to make a Bar Plot with.",
                      names(datasetInput()), multiple =FALSE)}}
    })
    output$var3_bp_x2 <- renderUI({
      if(input$var_type==3){
        if(input$gtype==1){
          selectInput("x2", "Pick the second Variable to make a Bar Plot with.",
                      names(datasetInput()), multiple =FALSE)}}
    })
    
    output$var3_mp_x1 <- renderUI({
      if(input$var_type==3){
        if(input$gtype==2){
          selectInput("x1", "Pick the x-axis variable for the Mosaic Plot.",
                      names(datasetInput()), multiple =FALSE)}}
    })
    output$var3_mp_x2 <- renderUI({
      if(input$var_type==3){
        if(input$gtype==2){
          selectInput("x2", "Pick the y-axis variable for the Mosaic Plot.",
                      names(datasetInput()), multiple =FALSE)}}
    })

    
    
    #### Graphs Page
    #### Graph
    
output$displot <- renderPlot({
      dataset <- datasetInput()
 if(input$var_type==1){
   if(input$gtype==1){
     x1 <- dataset[,which(names(dataset)==input$x1)]
     x2 <- dataset[,which(names(dataset)==input$x2)]
     
     boxplot(x1,x2,data=dataset, main = paste(input$title), names=c(input$x1, input$x2), xlab=paste(input$xlab), ylab=paste(input$ylab))
     
   } else if(input$gtype==2){
     
     x1 <- dataset[,which(names(dataset)==input$x1)]
     x2 <- dataset[,which(names(dataset)==input$x2)]
    
     
     plot(x1,x2,data=dataset, main = paste(input$title), xlab=paste(input$xlab), ylab=paste(input$ylab))
    
   }
 } else if (input$var_type==2){

   x1 <- dataset[,which(names(dataset)==input$x1)]
   x2 <- dataset[,which(names(dataset)==input$x2)]
   
   boxplot(x1~x2,data=dataset, main = paste(input$title),  xlab=paste(input$xlab), ylab=paste(input$ylab))
   
   
 } else if (input$var_type==3){
   
   if(input$gtype==1){
     
     counts <- table(dataset[,which(names(dataset)==input$x1)],dataset[,which(names(dataset)==input$x2)] )
     
     barplot(counts,main = paste(input$title), xlab=paste(input$xlab), ylab=paste(input$ylab),
             legend = rownames(counts), beside=TRUE)
     
   } else if(input$gtype==2){
     
     counts <- table(dataset[,which(names(dataset)==input$x1)],dataset[,which(names(dataset)==input$x2)] )
     mosaicplot(counts)
  
   }
   
 }      
})
    
#### Statistical Testing   

### selections of tests


output$test1 <- renderUI({
  if(input$var_type==1){
    
    selectInput("ttype", label="What type of Statistical Test do you Want to Perform?", 
                choices = list("Paired t-test" = 1, "Independent t-test" = 2,
                                        "Regression"= 3), selected = 1)
    
    }
})

output$test2 <- renderUI({
  if(input$var_type==2){
    
    selectInput("ttype", label="What type of Statistical Test do you Want to Perform?", 
                choices = list("Paired t-test" = 1, "Independent t-test" = 2,
                               "ANOVA"= 3), selected = 1) 
    
  }
})


output$test3 <- renderUI({
  if(input$var_type==3){
    
    selectInput("ttype", label="What type of Statistical Test do you Want to Perform?", 
                choices = list("Chi-Square Test"=1), selected = 1)
    
  }
})
    output$testing <- renderTable({ 
      dataset <- datasetInput()
      x1    <- dataset[,which(names(dataset)==input$x1)]
      x2    <- dataset[,which(names(dataset)==input$x2)]
      
      
      if(input$var_type==1){
        if(input$ttype==1){
            t<- t.test(x1,x2, paired=TRUE)
            t.vec <- as.data.frame(cbind(t$statistic, t$p.value))
            names(t.vec)<- c("t-Statistic", "P-value")
           print(t.vec)
          }
         else if(input$ttype==2){
          t<- t.test(x1,x2)
          t.vec <- as.data.frame(cbind(t$statistic, t$p.value))
          names(t.vec)<- c("t-Statistic", "P-value")
          print(t.vec)
        } else if(input$ttype==3){
          
          mod <- lm(x2~x1)
          out <- paste(input$x2)
          pred <- paste(input$x1)
          estimate <- round(mod$coefficients[2],2)
          sums <- summary(mod)
          stat <- round(sums$coefficients[2,3],2)
          pval <- round(sums$coefficients[2,4],2)
          
          vec <- as.data.frame(cbind(out, pred, estimate, stat, pval))
          names(vec) <- c("Outcome", "Predictor", "Coefficient", "T-statistic", "P-Value" )
          print(vec)
          
          
        }
      } else if(input$var_type==2){
        if(input$ttype==1){
          t<- t.test(x1,x2, paired=TRUE)
          t.vec <- as.data.frame(cbind(t$statistic, t$p.value))
          names(t.vec)<- c("t-Statistic", "P-value")
          print(t.vec)
        }
        else if(input$ttype==2){
          t<- t.test(x1,x2)
          t.vec <- as.data.frame(cbind(t$statistic, t$p.value))
          names(t.vec)<- c("t-Statistic", "P-value")
          print(t.vec)
        } else if(input$ttype==3){
          a <- aov(x1~x2)
          f <- summary(a)
          fval <- f[[1]]$`F value`
          pval <- f[[1]]$`Pr(>F)`
          f.vec<-c(fval[1], pval[1])
          names(f.vec) <- c("F Statistic", "P-value")
          print(f.vec)
        }} else if (input$var_type==3){
          if (input$ttype==1){
            data <- table(x1, x2)
            x <- chisq.test(data)
            x.vec <- c(t$statistic, t$p.value)
            names(x.vec)<- c("Chi-Square-Statistic", "P-value")
            print(x.vec)
          }
        }
      })
    
    
    
    
    
    output$test_text <- renderText({
      
      if(input$var_type==1){
        if(input$ttype==1){
          paste("When using paired t-test, remember that you need to be comparing two 
                continuous variables that are correlated in some manner. You cannot trust the output 
                in any other situation.")
        } else if(input$ttype==2){
          paste("When using an independent t-test, remember that you need to be comparing two 
                continuous variables that are not correlated in any manner. You cannot trust the output 
                in any other situation.")
        } else if(input$ttype==3){
          paste("When using linear regression, remember that the outcome needs to be 
                continuous. The coefficient refers to the relationship the predictor has on the outcome.
                ")
        } } else if(input$var_type==2){
        if(input$ttype==1){
          paste("When using paired t-test, remember that you need to be a continuous
                  variable over 2 groups of correlated data. This will not work if your categorical variable has more than 2 groups. 
                  Use ANOVA if you have more than 2 groups")
        } else if(input$ttype==2){
          paste("When using independent t-test, remember that you need to be a continuous
                  variable over 2 groups. This will not work if your categorical variable has more than 2 groups. 
                Use ANOVA if you have more than 2 groups")
        } else if(input$ttype==3){
          paste("ANOVA is used to split a continuous variable up into 2 or more groups.
                The goal of this is to test and see if there are differences between these groups with  
                respect to the continuous variable.
                ")
        }} else if(input$var_type==3){
          if(input$ttype==1){
            paste("When using the chi-square test, we need to make sure that we are comparing  categorical variables.")
          } 
        
        }
        })
    
    ## download report
    output$downloadlab <- downloadHandler(
      filename = function() {
        paste0('my-lab', '.', 'docx')
      },
      
      content = function(file) {
        src <- normalizePath('lab.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'lab.Rmd', overwrite = TRUE)
        
        library(rmarkdown)
        out <- render('lab.Rmd', word_document()
        )
        file.rename(out, file)
      }
    )
    
})
