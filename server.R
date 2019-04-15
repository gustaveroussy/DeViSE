library(stringr)
library(plotly)
library(jsonlite)


#### config varibales (runs host, database params ...)------------------------------------------------------------------

# conf=yaml::yaml.load_file("data/appData/config.yaml")


########### END config varibale  ---------------------------------------------------------------------------------------

############ sources ---------------------------------------------------------------------------------------------------

options(shiny.maxRequestSize=1000*1024^2)
source("src/functions.R")
source("src/web_servicesTo_Rfunctions.R")


########### END sources  -----------------------------------------------------------------------------------------------

########### Reactive values --------------------------------------------------------------------------------------------

GTFs=reactiveValues(values=NULL)
RUNS=reactiveValues(values=NULL)
analyse=reactiveValues(name=NULL,junctions=NULL,current_transcript=NULL)
finished_analysis=reactiveValues(name=NULL)
hg19_exons=list(default=readRDS("data/appData/public_annotation/gencodeV19_exons_default.rds"),
                union=readRDS("data/appData/public_annotation/gencodeV19_exons_union.rds"))
###### End reactive Values ---------------------------------------------------------------------------------------------

###### observe docker --------------------------------------------------------------------------------------------------


SESSIONs<- reactiveValues(count= 0 )
SESSIONs$lastclosing<- as.numeric(Sys.time())
autoInvalidate <- reactiveTimer( 100000 )
t=30 # 30 seconde
# observe({
#   autoInvalidate()
#   # tuer le conteneur s'il n'y a aucune session ouverte apres t seconde
#   if (SESSIONs$count== 0 && (SESSIONs$lastclosing+ t )< as.numeric(Sys.time()) ){
#     
#     system(paste0( "docker rm -f ",system("hostname " ,intern = TRUE)))
#   }
# })

###### END observe docker --------------------------------------------------------------------------------------------------------




# SERVER FUNCTION ----------------------------------------------------------------------------------------------------------------

shinyServer(
  function(input, output, session) {
    
    isolate(SESSIONs$count <- SESSIONs$count + 1)
    session$onSessionEnded( function(){
      isolate(SESSIONs$count <- SESSIONs$count - 1)
      isolate(SESSIONs$lastclosing <- as.numeric(Sys.time()))
    })
    
############################### login --------------------------------------------------------------------------------------------
    
    observeEvent(input$.login,{
      
      logg=fromJSON(paste0("http://31.10.13.26:8080/Authentification/api/User?login=",input$.username,"&password=",input$.password))
      
      # if(logg$identification=="Successful identification"){
        if(T){
          
        shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        shinyjs::show(id = "application", anim = F)
        shinyjs::hide(id = "login_ui", anim = F)
        output$user_profile=renderUI(
          column(12,
          actionLink("user_",HTML(paste0("<b><font color='black'>",input$.username)," </font><b>"),icon=icon("user")),
          br(),
          actionLink("sign_out",HTML("<b><font color='black'> Sign out </font><b>"),icon=icon("sign-out-alt "))
          )
        )
      }else{
        output$message=renderText(logg$identification)
      }
      
    })
    
    observeEvent(input$sign_out,{
      updateTabItems(session, "tabs", selected = "m1")
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::hide(id = "application", anim = F)
      shinyjs::show(id = "login_ui", anim = F)
      output$message=renderText("")
      output$user_profile=renderUI("")
      
    })
    
  ######---------------------------------------------------------------------------------------------------------------
    
    
    

  ########## update annotations files ----------------------------------------------------------------------------------
    observe({
      
      gtf_list=list.files("data/appData/public_annotation",pattern = ".gtf",full.names = T)
      
      if(length(gtf_list)>0){
      val=paste0(getwd(),"/",gtf_list)
      names(val)=basename(val)
      GTFs$values=val
      print(GTFs$values)
      updateSelectInput(session,inputId = "annotation_public_gtf",choices = names(GTFs$values))
      }
      
    })
    
 ###-------------------------------run junction calling------------------------------------------------------------------
    

    
    output$DT_All_run=renderDataTable({
      
      input$run_analysis_btn
      ##------preparation des runs ----
      runlist=getRunList()
      analysislist=getAnalysisList()
      runs=data.frame(name=unique(c(runlist,analysislist)))
      runs$`Raw data`="non available"
      runs$`Raw data`[runs$name %in% runlist]="available"
      runs$Date=Sys.time() ##### à revoir
      runs$Analysis="Waiting"
      runs$Analysis[runs$name %in% analysislist]="launched"
      RUNS$values=runs
      class_btns=rep("btn-danger",nrow(runs))
      class_btns[runs$name %in% analysislist]="btn-info"
      label_btns=rep("Submit  the  analysis",nrow(runs))
      label_btns[runs$name %in% analysislist]="Details of analysis"
      icons=rep("eye",nrow(runs))
      icons[!runs$name %in% analysislist]="telegram-plane"
      ################################
      
      runs$Status=sapply(runs$name,FUN = getRunStatus)
      runs$Advanced=actionButtonInputs( len = nrow(runs),
                                        id = 'button_',
                                        label = label_btns,
                                        class_btns = class_btns,
                                        icons = icons,
                                        onclick = 'Shiny.onInputChange(\"select_button_advanced\",  this.id+"_"+Math.random())'
                                      )
      

      datatable(runs,
                selection = 'none',
                escape = FALSE,
                filter = list(position = 'top',
                              clear = FALSE),
                options = list(
                  scrollX = TRUE,
                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                  drawCallback = JS('function() {Shiny.bindAll(this.api().table().node()); } '),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#e0cee0', 'color': 'black'});",
                    "}"),
                  aLengthMenu = c(5,10,20,30,50,100,500),
                  iDisplayLength = 10, 
                  bSortClasses = TRUE
                ),
                rownames = F)%>%
        
        formatStyle(1,  color = 'black', backgroundColor = '#d5e5ef',fontWeight = 'bold')  %>%
        formatStyle(3,  color = 'black', backgroundColor = '#d5e5ef',fontWeight = 'bold')  %>%
        formatStyle(5,  color = 'black', backgroundColor = '#d5e5ef',fontWeight = 'bold')  %>%
        
        formatStyle("Raw data",color = "white",fontWeight = 'bold', backgroundColor = styleEqual(c("available","non available"), c("#83a89a","#ba6262")))  %>%
        formatStyle("Status",color = "black",fontWeight = 'bold', backgroundColor = styleEqual(c("100%","0%"), c("green","red")))  %>%
        formatStyle("Analysis",color = "white",fontWeight = 'bold', backgroundColor = styleEqual(c("launched","Waiting"), c("#83a89a","#ba6262"))) 
    })
    
    
    observeEvent(input$select_button_advanced,{
      
      updateCheckboxInput(session,inputId = "re_runAll_analysis",value = F)
      hide(id="samples_re_run_div")
      
      if(!is.null(input$select_button_advanced)){
        
      selectedRow <- as.numeric(strsplit(input$select_button_advanced, "_")[[1]][2]) 
      run_name=RUNS$values$name[selectedRow]
      
      RUNS$selected=as.character(run_name)
      
      if(RUNS$values$`Raw data`[selectedRow]!="non available" && RUNS$values$`Analysis`[selectedRow]=="Waiting"){
        
          progress <- shiny::Progress$new(session, min=1, max=100)
          on.exit(progress$close())
          progress$set(message = 'In progress ... ')
          progress$set( value = 100)
          enable("run_analysis_btn")
          design=getDesign(getFileListFromRun(run_name,".fastq.gz"))
          RUNS$design=design
          RUNS$type="new"
          RUNS$status=parserJobStatus(RUNS$selected)
          output$title_run_deviseModal=renderUI(HTML("<h3> <font color='black'><b>Submit a new analysis</b> </font> </h3>"))
          toggleModal(session, "run_deviseModal", toggle = "toggle")
          
      }else{
        RUNS$status=parserJobStatus(RUNS$selected)
        
        if(RUNS$values$`Raw data`[selectedRow]!="non available")
           show(id="samples_re_run_div")
        
           disable("re_run_analysis_btn")
           toggleModal(session, "samples_monitoringUI", toggle = "toggle")
        
      }
      }
      })
    
    output$samples_monitoringDT=renderDataTable({
      
      render_jobStatus(RUNS$status)
      
    })
    
    output$design_devise=renderRHandsontable({
      
      rhandsontable(RUNS$design,width = "100%") %>%
        
        hot_col("sample_id",type = "autocomplete", source = letters)
      
    }) 
    
    observe({


      updateActionButton(session = session,inputId = "re_run_analysis_btn","Resubmit selected samples",icon = icon("redo"))
      
      if(length(input$samples_monitoringDT_rows_selected)==0 && !input$re_runAll_analysis){
        
        disable("re_run_analysis_btn")
        
      }else{
        
        enable("re_run_analysis_btn")
        RUNS$samples_to_resubmit=RUNS$status$`Sample Name`[input$samples_monitoringDT_rows_selected]
        design=NULL
        try({design=getDesign(getFileListFromRun(RUNS$selected,".fastq.gz"))})
        
        if(!input$re_runAll_analysis){
         RUNS$design=design[design$sample_id %in% RUNS$samples_to_resubmit,]
        }else{
         updateActionButton(session = session,inputId = "re_run_analysis_btn","Resubmit all samples",icon = icon("redo"))
         RUNS$design=design
        }
        output$title_run_deviseModal=renderUI(HTML("<h3> <font color='black'><b>Resubmit  analysis</b> </font> </h3>"))
        RUNS$type="correction"
      }
      
    })
    
    observeEvent(input$re_run_analysis_btn,{
      enable("run_analysis_btn")
      toggleModal(session, "samples_monitoringUI", toggle = "close")
      toggleModal(session, "run_deviseModal", toggle = "toggle")
    })
    
    
    observeEvent(input$run_analysis_btn,{

      progress <- shiny::Progress$new(session, min=0, max=100)
      on.exit(progress$close())
      progress$set(message = 'In progress ...')
      progress$set(value = 100)
      disable("run_analysis_btn")
      design=hot_to_r(input$design_devise)
      
      if( RUNS$type=="correction"){
        
        old_jobs=readRDS(paste0("data/usersData/runs_jobs/",RUNS$selected))
        killJob(URLencode(paste0(old_jobs[as.character(design$sample_id)],collapse = " ")))
        job_ids=run_devise(RUNS$selected,design =design)
        old_jobs[names(job_ids)]=job_ids
        saveRDS(old_jobs,paste0("data/usersData/runs_jobs/",RUNS$selected))
        
      }else{
        
        job_ids=run_devise(RUNS$selected,design =design)
        saveRDS(job_ids,paste0("data/usersData/runs_jobs/",RUNS$selected))
      }
      
      RUNS$status=parserJobStatus(RUNS$selected)
      
      toggleModal(session, "run_deviseModal", toggle = "close")
      toggleModal(session, "samples_monitoringUI", toggle = "toggle")
      
    })
    
    
 ###------------------------------- View analysis------------------------------------------------------------------------
    
    
    
    
    
    
    
    
    
    
    
    
       observeEvent(input$View_analysis_btn,{
         
         progress <- shiny::Progress$new(session, min=0, max=100)
         on.exit(progress$close())
         progress$set(message = 'In progress ...')
         progress$set(value = 100)
         shinyjs::showElement("results_output")
         output$plot_junct=renderUI(br())
         analyse$name=input$select_analysis
         analyse$path=paste0("data/usersData/results/",input$select_analysis)
         analyse$junctions=aggregate_junctions(paste0(analyse$path,"/junctions_calling"),dir_analysis=analyse$path,cutoff = input$cuttof_depth)
         colnames(analyse$junctions)=str_replace_all(colnames(analyse$junctions),pattern = "_",replacement = " ")
         updateSelectInput(session,inputId = "select_gene",choices = unique(analyse$junctions$genes[!str_detect(analyse$junctions$genes,pattern = ",")]))
         updateSelectInput(session,inputId = "select_samples",choices = colnames(analyse$junctions)[13:ncol(analyse$junctions)])
         output$DT_All_samples=renderDataTable({
           
           datatable(analyse$junctions[,-12],
                     selection = 'single',
                     escape = FALSE,
                     filter = list(position = 'top',
                                   clear = FALSE),
                     options = list(
                       scrollX = TRUE,
                       preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                       drawCallback = JS('function() {Shiny.bindAll(this.api().table().node()); } '),
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#dbcfc9', 'color': 'black'});",
                         "}"),
                       aLengthMenu = c(5,10,20,30,50,100,500),
                       iDisplayLength = 10, 
                       bSortClasses = TRUE
                     ),
                     rownames = F) %>%
             
             formatStyle(1,  color = 'black', backgroundColor = 'rgb(244, 247, 249)')  %>%
             formatStyle(3,  color = 'black', backgroundColor = 'rgb(244, 247, 249)')  %>%
             formatStyle(5,  color = 'black', backgroundColor = 'rgb(244, 247, 249)')  %>%
             formatStyle(7,  color = 'black', backgroundColor = 'rgb(244, 247, 249)')  %>%
             formatStyle(9,  color = 'black', backgroundColor = 'rgb(244, 247, 249)')  %>%
             formatStyle(11, color = 'black', backgroundColor = 'rgb(244, 247, 249)')  %>%
             
             formatStyle("known junction",color = "white",fontWeight = 'bold', backgroundColor = styleEqual(c("known","unknown"), c("#83a89a","#ba6262"))) 
           
         })
         
       })
    
    observeEvent(input$select_gene,{
     
       if(input$select_gene!=""){
        
        transcripts=analyse$junctions$transcripts[analyse$junctions$genes==input$select_gene]
        transcripts=as.character(transcripts[!is.na(transcripts)])
        
        if(length(transcripts>0)){
          transcripts=unlist(strsplit(transcripts,","))
          updateSelectInput(session,
                            inputId = "select_principal_transcript",
                            choices = c("Merged_transcripts",transcripts),
                            selected = "")
        }
      }
    })
     
    observeEvent(list(input$select_principal_transcript,input$select_gene),{
      
      if(input$select_principal_transcript!=""){
        
        transcripts=analyse$junctions$transcripts[analyse$junctions$genes==input$select_gene]
        transcripts=as.character(transcripts[!is.na(transcripts)])
      
        if(length(transcripts>0)){
          
          transcripts=unlist(strsplit(transcripts,","))
          updateSelectInput(session,
                            inputId = "select_transcript",
                            choices = transcripts)
          analyse$current_transcripts=transcripts
        }
      }
    }) 
    
############## ----------------------------------------------------------------------------------------------------------
    
############## Download buttons -----------------------------------------------------------------------------------------
    
    output$download_all_junction <- downloadHandler(
      filename = function() {
        paste0("All_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions,file,row.names = F,sep="\t")
        
      }
    )
    
    output$download_known_junction <- downloadHandler(
      filename = function() {
        paste0("Known_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[analyse$junctions$`known junction`=="known",],file,row.names = F,sep="\t")
      }
    )
    
    output$download_unknown_junction <- downloadHandler(
      filename = function() {
        paste0("Unknown_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[analyse$junctions$`known junction`=="unknown",],file,row.names = F,sep="\t")
        
      }
    )
    output$download_all_junction_bis <- downloadHandler(
      filename = function() {
        paste0("All_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions,file,row.names = F,sep="\t")
        
      }
    )
    
    output$download_known_junction_bis <- downloadHandler(
      filename = function() {
        paste0("Known_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[analyse$junctions$`known junction`=="known",],file,row.names = F,sep="\t")
      }
    )
    
    output$download_unknown_junction_bis <- downloadHandler(
      filename = function() {
        paste0("Unknown_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[analyse$junctions$`known junction`=="unknown",],file,row.names = F,sep="\t")
        
      }
    )
    
    
#------------------------------------------------------------------------------------------------------------------------

##### Plotly ------------------------------------------------------------------------------------------------------------
    
    observeEvent(input$plot_junction_btn,{
       

      if(!is.null(input$select_samples) &&
         length(input$select_samples)!=0 &&
         input$select_samples!="" &&
         !is.null(input$select_principal_transcript) &&
         length(input$select_principal_transcript)!=0 &&
         input$select_principal_transcript!="" &&
         !is.null(input$select_transcript) &&
         length(input$select_transcript)!=0 &&
         input$select_transcript!=""
         
         ){
        
        showElement(id = "viz_plot",anim = T)
       
        output$plot_junct=renderUI(shinycssloaders::withSpinner(plotlyOutput("splice_graph",height = input$Plot_height)))
  
        if(is.null(input$select_transcript)){
          transcriptList=c()
        }else{
          transcriptList=input$select_transcript
        }
        
        output$message_plot=renderText("")
        
        p=viz_junctions(junctions =analyse$junctions,
                        samples =input$select_samples,
                        exonGTF = hg19_exons ,
                        gene = input$select_gene,
                        gene_transcript = analyse$current_transcripts[1],
                        principalTranscript =input$select_principal_transcript,
                        transcriptList = transcriptList,
                        groupJunctionsBy = input$groupby_status,
                        cutoff_depth = input$cuttof_depth_plot,
                        mutations =getMutationFromAnalysis(paste0("data/usersData/results/",analyse$name)), 
                        random_y = input$disp_level
          )
        
        output$splice_graph=renderPlotly({
           p
        })

      }else{
        output$message_plot=renderText("Oops !! One or more inputs above are not reported")
      }

    })

#####--------------------------------------------------------------------------------------------------------------------
  })