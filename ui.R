###############################################################################
# MoOSe : Monitoring Of Sequencer
# This is a shinydashboard web app designed ....
#
# Authors: 
# - Yahia Adnani (yahia.adnani@gustaveroussy.fr) : web  app ,databases

###############################################################################


# LIBRARIES -------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(rhandsontable)
library(shinyBS)
source("src/functions.R")
source("src/web_servicesTo_Rfunctions.R")

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page



header <- dashboardHeader(title = span(img(src = "logo.png", height = 45, width = 90), HTML("<b> DeViSE </b>")),uiOutput("user_profile"))

######################################### dashboardSidebar #######################################################
sidebar= dashboardSidebar(disable = T,
                          
                          develpped_by,
                          
                          sidebarMenu(id="tabs",
                                       # menuItem("Junction calling", tabName="m1", icon =  icon("chart-line")),
                                       menuItem("Runs/Analysis", tabName="m1", icon =  icon("chart-line")),
                                       menuItem("Post-analysis/visualization", tabName="m2", icon =  icon("eye"))
                                                )
                                      
                          )

#################################################################################################################

################################################## dashboardBody ################################################

body <-  dashboardBody(
  
  ### bs modal 
  tags$head(tags$style(HTML(
    ' .modal-lg {
    overflow-y: scroll;
    position: absolute;
    top: 50px;
    left: 20%;
    z-index: 1050;
    width: 60%;
    margin-left: 0px;
    padding-bottom: 80px;
    overflow: visible;

   }'))),
  
  
  ### laoding css -------------------------------------------------
  loading_css,
  
  
  bsModal(id="run_deviseModal",
         uiOutput("title_run_deviseModal"),
          "btn_viewIncidentals",
          size = "large",
          div(id="pipeline_UI",
              shinycssloaders::withSpinner(rhandsontable::rHandsontableOutput("design_devise",width = "100%")),
              actionButton(inputId = "run_analysis_btn","Submit",icon = icon("run")),
              tags$style("button#run_analysis_btn {background-color:#d85252; padding: 5px 25px;
                                       font-family:Andika, Arial, sans-serif; font-size:1.5em;  letter-spacing:0.05em; text-transform:uppercase ;color:#fff;
                                       text-shadow: 0px 1px 10px #000;border-radius: 15px;box-shadow: rgba(0, 0, 0, .55) 0 1px 6px;}"),
              helpText("Click on the button to run DeViSE analysis."),
              hidden(div(id="loading_modal",
                         loading_html
                         ))
          )),
  
  
  bsModal(id="samples_monitoringUI",
          uiOutput("title_samples_monitoringUI"),
          "btn_viewIncidentals",
          size = "large",

          div(id="samples_monitoringUI_div",
              shinycssloaders::withSpinner(DT::DTOutput("samples_monitoringDT")),
              hidden(
                    div(id="samples_re_run_div",
                          checkboxInput("re_runAll_analysis",label = "Resubmit all samples of this analysis."),
                          actionButton(inputId = "re_run_analysis_btn","Resubmit selected samples",icon = icon("redo"),class="btn-danger"),
                          helpText("Click on the button to re-run the analysis of selected samples.")
                        )
                 ),
              br(),
              shinycssloaders::withSpinner(uiOutput("log_files")),
              hidden(div(id="loading_modal2",
                         loading_html
              ))
 
          )
          
          
  ),
  
  
  ############ end modals- ---------------------------------------------------------------------------------------------
  
  useShinyjs(),
  extendShinyjs(text = jsResetCode),
  


  
  tabItems(
    
    
    tabItem(tabName = "m1",
            
          div(id="login_ui",  
            fluidRow(column(width=4, offset = 4,HTML('<h1 <b> <font color="red">DeViSE</font>: <font color="red">De</font>tection and <font color="red">Vi</font>sualization of <font color="red">S</font>plicing <font color="red"> E</font>vents</b> </h1>')),
                     column(width=4, offset = 4,h4("Sign in:"),
                            wellPanel(id = "login",
                                      textInput(".username", "Username:"),
                                      passwordInput(".password", "Password:"),
                                      div(actionButton(".login", "Log in",icon = icon("sign-in")), style="text-align: center;back"),
                                      textOutput("message"),
                                      tags$head(tags$style("#message{color: red;}")
                                      )
                            )
                     ))
            ),
              
              hidden(
                div(id="application",

                fluidRow(
                        box(icon=icon('list'),
                        title =HTML('<h1> <font color="black"><b> List of all runs/analysis </b> </font> </h1> '),
                        status="danger",
                        width = 12 ,
                        shinycssloaders::withSpinner(DT::dataTableOutput("DT_All_run"))
                    )
                    )
                
                
                
                ))
              ),
    
    tabItem(tabName = "m2",
            
            fluidRow( 
              box(icon=icon('eye'),
                  title =HTML('<h1> <font color="black"><b> View analysis </b> </font> </h1> '),
                  status="danger",
                  width = 12 ,
                  column(width =12,
                    column(width = 6,selectInput(inputId ="select_analysis",label = "Select one ore multiple analysis",multiple = T,choices = getAnalysisList())),
                    column(width = 6,selectInput(inputId ="type_junctions",label = "Select a type of results",multiple = F,choices =c("FILTERED","ALL")))
                  ),
                  column(width =12,
                  column(width = 6,selectInput(inputId ="select_samples_junc",label = "Select samples",multiple = T,choices = "")),
                  column(width = 6,numericInput(inputId ="cuttof_depth",label = "Minimum number of reads to keep a junction",value=25, min = 0, max = NA, step = 1))
                  ),       
                  
                  actionButton(inputId = "View_analysis_btn","Submit",icon = icon("run")),
                  tags$style("button#View_analysis_btn {background-color:#d85252; padding: 5px 25px;
                                   font-family:Andika, Arial, sans-serif; font-size:1.5em;  letter-spacing:0.05em; text-transform:uppercase ;color:#fff;
                                   text-shadow: 0px 1px 10px #000;border-radius: 15px;box-shadow: rgba(0, 0, 0, .55) 0 1px 6px;}"),
                  helpText("Click on the button to view the details of selected analysis."),
                  uiOutput("samples_invalid"),
                  
                  hidden(
                    div(id="results_output",
                       
                        box(icon=icon('eye'),
                        title = HTML('<h3> <font color="black"><b> Junctions table </b> </font> </h3> '),
                        status="danger",
                        collapsible = T,
                        width = 12 ,
                        HTML('<h4> Click <a href="https://regtools.readthedocs.io/en/latest/commands/junctions-annotate/#output" target="_blank">  here </a> to see the explanation of each column</h4>'),
                        br(),
                        
                        div(downloadButton(outputId = "download_all_junction",label = "Download")),
                        shinycssloaders::withSpinner(DT::dataTableOutput("DT_All_samples")),
                        div(downloadButton(outputId = "download_all_junction_bis",label = "Download"))
                        
                  ),
                  box(title = HTML('<h3> <font color="black"><b> Junctions vizualisation </b> </font> </h3> '),
                      status="danger",
                      collapsible = T,
                      width = 12,
                      column(width =12,
                             column(width = 6,selectInput(inputId ="select_gene",label = "Select a gene",choices = "")),
                             column(width = 6,selectInput(inputId ="select_samples",multiple = T,label = "Select one ore more samples",choices =""))
                      ),
                      column(width =12,
                             column(width = 6,selectInput(inputId ="select_principal_transcript",label = "Select principal transcript",choices ="")),
                             column(width = 6,selectInput(inputId ="select_transcript",multiple = T,label = "Select other transcripts",choices =""))
                             
                             
                      ),

                      textOutput("message_plot"),
                      tags$head(tags$style("#message_plot{color: red; font-size:1.5em;}")),
                      
                      box(icon=icon('eye'),
                          title = "Graphic settings",
                          status="danger",
                          collapsible = T,
                          solidHeader = T,
                          width = 12,
                          collapsed = T,
                          # column(width =12,
                          #        
                          #        column(width = 6,selectInput(inputId ="groupby_status",label = "Junctions groupBy:",choices = c("anchor","status (known/unknown)"),selected = "anchor")),
                          #        column(width = 6,numericInput(inputId ="cuttof_depth_plot",label = "Minimum number of reads to plot a junction",value=25, min = 0, max = NA, step = 1))
                          #        
                          # ),
                          column(width =12,
                                 
                                 column(width = 6,numericInput(inputId ="Plot_height",label = "Plot height",value=500, min = 200, max = NA, step = 10)),
                                 column(width = 6,numericInput(inputId ="cutoff_junc_plot",label = "Minimum number of reads to plot a junction",value=1, min = 1, step = 1))
                                 
                          )
                          ),
                      actionButton(inputId = "plot_junction_btn","Submit",icon = icon("run")),
                      tags$style("button#plot_junction_btn {background-color:#d85252; padding: 5px 25px;
                                   font-family:Andika, Arial, sans-serif; font-size:1.5em;  letter-spacing:0.05em; text-transform:uppercase ;color:#fff;
                                   text-shadow: 0px 1px 10px #000;border-radius: 15px;box-shadow: rgba(0, 0, 0, .55) 0 1px 6px;}"),
                      helpText("Click on the button to plot the junction graph."),
                      uiOutput("plot_junct")
                      
                  )
                  )
               ))
            )
    )

    ),
  hidden(div(id="loading",
             loading_html
  )),
  
  HTML("   <center>
           <img src= 'devise.svg'  height = '300'  width = '300' >
           <br/><h4 style='position: relative; bottom: 0; width:100%;'> <font color='black'> Please contact ADNANI Yahia <font color='blue'>  
           Yahia.ADNANI@gustaveroussy.fr </font> & bioinformatics team (BiGR) <font color='blue'>  
           pf-bioinfo@gustaveroussy.fr  </font> if you have any questions.</h4> </font> </center>")
    )
  
  



shinyUI(dashboardPage(skin = "red",header, sidebar, body,title = "DeViSE"))
