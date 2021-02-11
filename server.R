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
observe({
  autoInvalidate()
  # tuer le conteneur s'il n'y a aucune session ouverte apres t seconde
  if (SESSIONs$count== 0 && (SESSIONs$lastclosing+ t )< as.numeric(Sys.time()) ){

    system(paste0( "docker rm -f ",system("hostname " ,intern = TRUE)))
  }
})

###### END observe docker --------------------------------------------------------------------------------------------------------




# SERVER FUNCTION ----------------------------------------------------------------------------------------------------------------

shinyServer(
  function(input, output, session) {
      

    
    isolate({
      SESSIONs$count <- SESSIONs$count + 1
      if(SESSIONs$count>1)
        shinyjs::runjs(paste0("window.location.replace('http://phoenix.intra.igr.fr:3000/')"))
    })
     
      session$onSessionEnded( function(){
      isolate(SESSIONs$count <- SESSIONs$count - 1)
      isolate(SESSIONs$lastclosing <- as.numeric(Sys.time()))
    })
    
############################### login --------------------------------------------------------------------------------------------
    
    observeEvent(input$.login,{
      output$log_files=renderUI(br())
      logg=fromJSON(paste0("http://vls-odin.intra.igr.fr:8086/Authentification/api/User?login=",input$.username,"&password=",input$.password))
      
      if(logg$identification=="Successful identification"){
          
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
    # observe({
    #   
    #   gtf_list=list.files("data/appData/public_annotation",pattern = ".gtf",full.names = T)
    #   
    #   if(length(gtf_list)>0){
    #   val=paste0(getwd(),"/",gtf_list)
    #   names(val)=basename(val)
    #   GTFs$values=val
    #   updateSelectInput(session,inputId = "annotation_public_gtf",choices = names(GTFs$values))
    #   }
    #   
    # })
    # 
 ###-------------------------------run junction calling------------------------------------------------------------------
    

    
    output$DT_All_run=renderDataTable({
      
      input$run_analysis_btn
      ##------preparation des runs ----
      runlist=getRunList()
      analysislist=getAnalysisList()
      runs=data.frame(name=unique(c(runlist,analysislist)),Date="",stringsAsFactors = F)
      runs$`Raw data`="not available"
      runs$`Raw data`[runs$name %in% runlist]="available"
      runs$Analysis="Waiting"
      runs$Analysis[runs$name %in% analysislist]="launched"
      runs$Date[runs$name %in% analysislist]=sapply(runs$name[runs$name %in% analysislist],getTimestamp)
      runs=runs[order(runs$Date,decreasing = T),]
      runs=runs[order(runs$Analysis,decreasing = T),]
      RUNS$values=runs
      class_btns=rep("btn-success",nrow(runs))
      class_btns[runs$name %in% analysislist]="btn-info"
      label_btns=rep("Submit  the  analysis",nrow(runs))
      label_btns[runs$name %in% analysislist]="Details of analysis"
      icons=rep("info-circle",nrow(runs))
      icons[!runs$name %in% analysislist]="telegram-plane"
      
      ################################
      #runs$`Progress (%)`=NA#sapply(runs$name,FUN = getRunStatus)
      # runs$Advanced=actionButtonInputs( len = nrow(runs),
      #                                   id = 'button_',
      #                                   label = label_btns,
      #                                   class_btns = class_btns,
      #                                   icons = icons,
      #                                   onclick = 'Shiny.onInputChange(\"select_button_advanced\",  this.id+"_"+Math.random())'
      #                                 )
      indexs=which(runs$Analysis != "launched")
      
      class_btns=rep("",nrow(runs))
      runs$name=actionLinkInputs( len = nrow(runs),
                                        id = 'button2_',
                                        label = runs$name,
                                        class_btns = class_btns,
                                        icons = rep("eye",nrow(runs)),
                                        onclick = 'Shiny.onInputChange(\"select_button_viz\",  this.id+"_"+Math.random())'
      )
      runs$name[indexs]=RUNS$values$name[indexs]

      
      
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
                    "$(this.api().table().header()).css({'background-color': '#dbcfc9', 'color': 'black'});",
                    "}"),
                  aLengthMenu = c(5,10,20,30,50,100,500),
                  iDisplayLength = 10, 
                  bSortClasses = TRUE
                ),
                rownames = F)%>%
        
        # formatStyle(1,  color = 'black', backgroundColor = '#d5e5ef',fontWeight = 'bold')  %>%
        formatStyle(2,  color = 'black', backgroundColor = '#d5e5ef',fontWeight = 'bold')  %>%
        
        formatStyle("Raw data",color = "black",fontWeight = 'bold', backgroundColor = styleEqual(c("available","not available"), c("#83a89a","#ba6262")))  %>%
        formatStyle("Analysis",color = "black",fontWeight = 'bold', backgroundColor = styleEqual(c("launched","Waiting"), c("#9c96cc","#efbd7f"))) #%>% 
        # formatStyle("Progress (%)",
        #               background = styleColorBar(c(100,0), '#adffb9'),
        #               backgroundSize = '98% 88%',
        #               fontWeight = 'bold',
        #               color = "#c42b2b",
        #               backgroundRepeat = 'no-repeat',
        #               backgroundPosition = 'center')
    })
    
    
        output$samples_monitoringDT=renderDataTable({
          input$select_button_advanced
          render_jobStatus(RUNS$status)
          
        })
    
        output$design_devise=renderRHandsontable({
          input$select_button_advanced
           rhandsontable(RUNS$design,width = "100%",readOnly = T)
            # hot_col("sample_id",type = "autocomplete", source = letters)
          
        }) 
        
        observeEvent(input$select_button_viz,{
          selectedRow <- as.numeric(strsplit(input$select_button_viz, "_")[[1]][2]) 
          run_name=RUNS$values$name[selectedRow]
          updateSelectInput(session,inputId ="select_analysis",label = "Select one ore multiple analysis",choices = getAnalysisList(),selected = run_name )
          updateTabItems(session, "tabs", selected = "m2")
        })    
    
    observeEvent(input$select_button_advanced,{
      show("loading")
      
      output$log_files=renderUI(br())
      updateCheckboxInput(session,inputId = "re_runAll_analysis",value = F)
      hide(id="samples_re_run_div")
      input$select_button_advanced
      
      if(!is.null(input$select_button_advanced)){
        
      selectedRow <- as.numeric(strsplit(input$select_button_advanced, "_")[[1]][2]) 
      run_name=RUNS$values$name[selectedRow]
      
      RUNS$selected=as.character(run_name)
      
      if(RUNS$values$`Raw data`[selectedRow]!="not available" && RUNS$values$`Analysis`[selectedRow]=="Waiting"){
        
          enable("run_analysis_btn")
          design=getDesign(getFileListFromRun(run_name,".fastq.gz"))
          RUNS$design=design
          RUNS$type="new"
          output$title_run_deviseModal=renderUI(HTML(paste0("<h3> <font color='black'><b>Submit a new analysis: </b> </font>",RUNS$selected," </h3>")))
          toggleModal(session, "run_deviseModal", toggle = "toggle")
          
      }else{

          RUNS$status=parserJobStatus(RUNS$selected)
          output$title_samples_monitoringUI=renderUI(HTML(paste0("<h3> <font color='black'><b>Run/Analysis details: </b> </font>",RUNS$selected," </h3>")))
          
        if(RUNS$values$`Raw data`[selectedRow]!="not available")
        
           show(id="samples_re_run_div")
           disable("re_run_analysis_btn")
           toggleModal(session, "samples_monitoringUI", toggle = "toggle")
        
      }
      }
      hide("loading")
      })
    

    
    observe({


      updateActionButton(session = session,inputId = "re_run_analysis_btn","Resubmit selected samples",icon = icon("redo"))
      
      if(length(input$samples_monitoringDT_rows_selected)==0 && !input$re_runAll_analysis){
        
        disable("re_run_analysis_btn")
        
      }else{
        
        enable("re_run_analysis_btn")
        RUNS$samples_to_resubmit=RUNS$status$`Sample Name`[input$samples_monitoringDT_rows_selected]
        design=NULL
        try({design=getDesign(getFileListFromRun(isolate({RUNS$selected}),".fastq.gz"))})
        
        if(!input$re_runAll_analysis){
          isolate({RUNS$design=design[design$sample_id %in% RUNS$samples_to_resubmit,]})
        }else{
         updateActionButton(session = session,inputId = "re_run_analysis_btn","Resubmit all samples",icon = icon("redo"))
         isolate({RUNS$design=design})
        }
        output$title_run_deviseModal=renderUI(HTML(paste0("<h3> <font color='black'><b>Resubmit  analysis: </b> </font>",RUNS$selected," </h3>")))
        isolate({RUNS$type="correction"})
      }
      
    })
    
    observeEvent(input$re_run_analysis_btn,{
      show("loading_modal2")
      enable("run_analysis_btn")
      toggleModal(session, "samples_monitoringUI", toggle = "close")
      toggleModal(session, "run_deviseModal", toggle = "toggle")
      hide("loading_modal2")
      
    })
    
    
    observeEvent(input$run_analysis_btn,{

      show("loading_modal")
      
      disable("run_analysis_btn")
      design=hot_to_r(input$design_devise)
      output$log_files=renderUI(br())
      
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
      hide("loading_modal")
      
    })
    
    
    observeEvent(input$select_button_sample_name,{
      show("loading_modal2")
      
        selectedRow <- as.numeric(strsplit(input$select_button_sample_name, "_")[[1]][2])
        sample_name=as.character(RUNS$status$`Sample Name`[selectedRow])
        run_name=RUNS$selected
        logList=sort(getLogListFromSample(analysis = run_name,sample = sample_name),decreasing = T)
        
        output$log_files=renderUI({
          if(length(logList)==1){
            box(icon=icon('list'),
                title = HTML(paste0("<h3><b>No available log file for this sample </b>",sample_name,"</h3>")),
                width = 12 ,solidHeader = T,
                background = "orange" 
            )
            
          }else{

            box(icon=icon('list'),
                title = HTML(paste0("<h3><b>Log files: </b>",sample_name,"</h3>")),
                status="danger",
                width = 12 ,
                collapsible = T,
                lapply(logList, function(i){
                  column(12,
                         actionLink(paste0("key1_temp_",i),label=paste0(i),icon=icon("download"),onclick =downloadLogFile(run_name,sample_name,i,"shiny"))
                  )
                })            )
            
          }
          })
        hide("loading_modal2")
        
        })
    
 ###------------------------------- View analysis------------------------------------------------------------------------
        observe({
         updateSelectInput(session,
                           inputId ="select_samples_junc",
                           label = "Select samples",
                           choices = getlistsamplesFromRunList(input$select_analysis))  
          
        })
    
       observeEvent(input$View_analysis_btn,{
         
         show("loading")
         
         output$plot_junct=renderUI(br())

         if(length(input$select_samples_junc)==0){
           
           alert("Please select one/multiple samples from one/multiple analysis.")
           
         }else{
 
         junctions=aggregate_junctions(samplesList = input$select_samples_junc,type = "ALL")
         analyse$junctions=junctions$junctions.tab
         analyse$junctions=analyse$junctions[analyse$junctions$gene!="NA",]
         analyse$junctions=analyse$junctions[!is.na(analyse$junctions$gene),]
         
         if(!is.null(analyse$junctions)){
           
             analyse$junctions=analyse$junctions[analyse$junctions$maxReads_AllSamples>=input$cuttof_depth,]
             colnames(analyse$junctions)[1:11]=str_replace_all(colnames(analyse$junctions),pattern = "_",replacement = " ")[1:11]
             updateSelectInput(session,inputId = "select_gene",choices = unique(analyse$junctions$genes[!str_detect(analyse$junctions$genes,pattern = ",")]))
             updateSelectInput(session,inputId = "select_samples",choices = colnames(analyse$junctions)[13:(ncol(analyse$junctions)-1)])
             shinyjs::showElement("results_output")
             
         }else{
           shinyjs::hide("results_output")
         }
         
         if(length(junctions$samples_notValid)>0){
           output$samples_invalid=renderUI(
             box(HTML(paste0("<b><h4>Warning !! The results are not available for the following samples:</h4>",paste0(junctions$samples_notValid,collapse = "</br>"),"</b>")),background = "orange",solidHeader = T)
           )
         }else{
           output$samples_invalid=renderUI(br())
         }
         
         }
         
         hide("loading")
       })
    
       observeEvent(input$DT_All_samples_rows_selected,{
         
         updateSelectInput(session,inputId = "select_gene",choices = unique(analyse$junctions$genes[!str_detect(analyse$junctions$genes,pattern = ",")]),selected = analyse$junctions$genes[input$DT_All_samples_rows_selected])
         
         
       })
  
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
        paste0("aggregate_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[input$DT_All_samples_rows_all,],file,row.names = F,sep="\t")
        
      }
    )
    
   
    output$download_all_junction_bis <- downloadHandler(
      filename = function() {
        paste0("aggregate_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[input$DT_All_samples_rows_all,],file,row.names = F,sep="\t")
        
      }
    )
    
   
    
#------------------------------------------------------------------------------------------------------------------------

##### Plotly ------------------------------------------------------------------------------------------------------------
    
    observe({

      if(is.null(input$select_transcript)){
        transcriptList=c()
      }else{
        transcriptList=input$select_transcript
      }

      height=(length(input$select_samples)+length(transcriptList))*100+300
      updateNumericInput(session = session ,inputId = "Plot_height",value = height)
      
    })
    
    observeEvent(input$plot_junction_btn,{
      show("loading")
      
      
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
        
        if(is.null(input$select_transcript)){
          transcriptList=c()
        }else{
          transcriptList=input$select_transcript
        }
        
        showElement(id = "viz_plot",anim = T)
        output$plot_junct=renderUI(shinycssloaders::withSpinner(plotlyOutput("splice_graph",height = isolate(input$Plot_height))))
  

        
        output$message_plot=renderText("")
        junctions=analyse$junctions
        junctions$`known junction`=as.character(junctions$`known junction`)
        junctions$`known junction`[isolate(input$DT_All_samples_rows_selected)]=paste0(junctions$`known junction`[isolate(input$DT_All_samples_rows_selected)],"(selected)")
        mutations=NULL
        
        # try({mutations=hot_to_r(input$mutations_rhandontable)})
        
        analyse$plotly=viz_junctions(junctions = junctions,
                                      samples = input$select_samples,
                                      exonGTF = hg19_exons ,
                                      gene = input$select_gene,
                                      gene_transcript = analyse$current_transcripts[1],
                                      principalTranscript = input$select_principal_transcript,
                                      transcriptList = transcriptList,
                                      groupJunctionsBy = "status",
                                      cutoff_depth = input$cutoff_junc_plot,
                                      mutations = mutations
          )
        

      }else{
        output$message_plot=renderText("Oops !! One or more inputs above are not reported")
      }
      hide("loading")
      
    })
    
    output$splice_graph=renderPlotly({
      analyse$plotly
    })
    
    
    # output$mutations_rhandontable=renderRHandsontable({
    #   mutations=readRDS("data/usersData/mutations.rds")
    #   rownames(mutations)=mutations$sample
    #   mutations=mutations[input$select_samples,]
    #   print()
    #   if(nrow(mutations)==0){
    #     mutations=data.frame(sample=NA, chr=NA, start=NA, end=NA, ref=NA, alt=NA, gene=NA)
    #     mutations=mutations[1,]
    #   }else{
    #     rhandsontable(mutations,width = "100%",rowHeaders = NULL)
    #   }
    #   
    #     
    # })

    # observe({
    #   d <- event_data("plotly_click")
    #   print(d)
    # })
     
#####--------------------------------------------------------------------------------------------------------------------
  })
