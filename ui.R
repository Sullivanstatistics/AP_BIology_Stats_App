library(shiny)
library(markdown)




shinyUI(
  
 
  
  navbarPage( "AP Biology Statistics Lab",
    
  tabPanel("Home",
           
           column(12, includeHTML("intitial.html")           )
    
    
  ),
  
  tabPanel("The Data",
       
           tabsetPanel(
             
             tabPanel('Chick Weights by Feed Type',
                      includeMarkdown("chickwts.Rmd"),
                  
                      dataTableOutput("mytable1")),
             tabPanel('Iris Data',
                      includeMarkdown("iris.Rmd"),
                      dataTableOutput("mytable2")),
             tabPanel('Student\'s Sleep Data',
                      includeMarkdown("sleep.Rmd"),
                      dataTableOutput("mytable3")),
             tabPanel('Vitamin-C and Teeth',
                      includeMarkdown("tooth.Rmd"),
                      dataTableOutput("mytable4"))
           ),
           textInput("text_data", label = "Use this space to describe what data you have chosen and why.", value = "Enter Text here ...", width="90%")
  )
  ,
  
  
  
  tabPanel("Data Summary",
           column(12,
                  sidebarPanel(
             selectInput("dataset", "Choose a dataset:", 
                         choices = c("Chick Weights by Feed Type", "Iris Data", "Student's Sleep Data", "Vitamin-C and Teeth"))
             
             , width=3),
             div(htmlOutput(outputId = "matrix"), align="center"),
           br(),div(htmlOutput(outputId = "tables"), align="center"),      
           textInput("text_summary", label = "Use this space to describe what you see in the summary.",
                     value = "Enter text here ...", width="90%"))),
  
  
  tabPanel("Question",
           column(12,
           includeHTML("hyp.html"),
           textInput("text_quest", label = "Use this space to write out your hypothesis.",
                     value = "Enter text here ...", width="90%")))
  ,
  tabPanel("Choosing Variables", column(12,
           includeHTML("vars.html"),
           fluidRow(
           column(width=4),
           column(width=4,
           selectInput("var_type", "Choose The variable Types:", 
                       choices = c("Continuous and Continuous"=1, "Continuous and Categorical"=2, 
                                   "Categorical and Categorical"=3), selected=1)),
           column(width=4)),
           textInput("text_vars", label = "Use this space to explain why you chose those variable combinations.",
                     value = "Enter text here ...", width="90%")
           
           
  )), 
  
  
  tabPanel("Graphs",
           
      fluidRow(     sidebarPanel(
             htmlOutput("gtype1"),
             htmlOutput("gtype2"),
             htmlOutput("gtype3")
             , width=3),
           column(width=4,
           htmlOutput("var1_bw_x1"),
           htmlOutput("var1_bw_x2"),
           htmlOutput("var1_sp_x1"),
           htmlOutput("var1_sp_x2"),
           htmlOutput("var2_bw_x1"),
           htmlOutput("var2_bw_x2"),
           htmlOutput("var3_bp_x1"),
           htmlOutput("var3_bp_x2"),
           htmlOutput("var3_mp_x1"),
           htmlOutput("var3_mp_x2")),
           br(),
           column(width=4,
                  textInput("title", label="What is the title of this graph?"),
                  textInput("xlab", label="What is the x-axis Label?"),
                  textInput("ylab", label="What is the y-axis Label?"))),
      
      fluidRow(
        column(width=2),
        column(width =8,
               title = "Graph",
               solidHeader = TRUE, status = "primary",
               plotOutput("displot")
               
               
      ),
      column(width=2))
      
      ,
      textInput("text_graphs", label = "Use this space to describe what you see in the plots", value = "Enter text here...", width="90%")
                  
  ), 
  
  tabPanel("Statistical Tests",
            sidebarPanel(
             htmlOutput("test1"),
             htmlOutput("test2"),
             htmlOutput("test3"),
             br(),
             helpText("Any errors that you find in red usually mean that you have chosen 
                      the wrong type of data for this test. Please go back to your notes
                      and pick the correct test for your data.")
             , width=3)
           
  , 
  mainPanel(
    fluidRow(div(htmlOutput(outputId = "test_text"),style="font-size:150%; align:middle")),
    br(),
    
  fluidRow(
    div(htmlOutput(outputId = "testing"),style="font-size:150%; align:middle"))
  
  ,
  br(),
  
  textInput("text_tests", label = "Use this space to interpret the results of this test. ", value = "Enter text here...", width="90%"))
  ), 
  
  tabPanel("Lab Report",
          
           includeHTML("lab_rep.html"),
           br(),
           
           fluidRow(
             column(width=4),
             column(width=4,
                    textInput("auth_names", "Names of Authors", value = "Enter Names Here ...")
                    ),
             column(width=4)
             
           ), 
           fluidRow(
             column(width=5),
             column(width=2,
                    #Download button
                    downloadButton('downloadlab')
             ),
             column(width=5)
             
           )
           
    
  )
    
    
    
    
  )
)