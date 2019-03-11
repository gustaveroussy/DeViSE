library(stringr)
library(plotly)



#### config varibales (runs host, database params ...)------------------------------------------------------------------

# conf=yaml::yaml.load_file("data/appData/config.yaml")


########### END config varibale  ---------------------------------------------------------------------------------------

############ sources ---------------------------------------------------------------------------------------------------

options(shiny.maxRequestSize=1000*1024^2)
source("src/functions.R")


########### END sources  -----------------------------------------------------------------------------------------------

########### Reactive values --------------------------------------------------------------------------------------------

GTFs=reactiveValues(values=NULL)
analyse=reactiveValues(name=NULL,junctions=NULL)
finished_analysis=reactiveValues(name=NULL)
hg19_exons=list(default=readRDS("data/appData/public_annotation/gencodeV19_exons_default.rds"),
                union=readRDS("data/appData/public_annotation/gencodeV19_exons_union.rds"))
###### End reactive Values ---------------------------------------------------------------------------------------------

###### observe docker --------------------------------------------------------------------------------------------------



# observe({
#   autoInvalidate()
#   if (vals$count == 0 && (vals$lastclosing + 20) < as.numeric(Sys.time())){
#      system(paste0("docker rm -f ",system("hostname ", intern = TRUE)))
#    }
#  })

###### END observe docker ----------------------------------------------------------------------------------------------




# SERVER FUNCTION ------------------------------------------------------------------------------------------------------
shinyServer(
  function(input, output, session) {

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
    
    observeEvent(input$lunch_juncCalling,{
      
      message=message_JunCalling(input$input_bams,trim(input$analysis_name))
      
      if(message!=""){
        
        output$message_junction_calling=renderUI(message$html)
        alert(message$text)
      
      }else{
        finished_analysis$name= trim(input$analysis_name)
        nbr_bams=length(input$input_bams$datapath)
        progress <- shiny::Progress$new(session, min=0, max=nbr_bams)
        on.exit(progress$close())
        progress$set(message = 'Junction calling in  progress ...',
                     detail = '')
        progress$set(value = 1)
        
        t=format(Sys.time(),forma="%y-%m-%d_%H:%M:%S")
        dir_analysis=paste0("data/usersData/results/", trim(input$analysis_name))
        out_dir=paste0(dir_analysis,"/junctions_calling")
        dir.create(out_dir,recursive = T)
        
        for (i in 1:nbr_bams) {

          progress$set(message = paste0('Junction calling in  progress ...(',round(i/nbr_bams*100,0),'%)'),
                       detail = paste0(input$input_bams$name[i]," ..."))
          progress$set(value = i)
          out=paste0(out_dir,"/",unlist(strsplit(input$input_bams$name[i],".bam")))
          cmd=paste0(portcullis," full ",genomePath," ",input$input_bams$datapath[i]," -o ",out)
          print(cmd)
          system(cmd,wait = T,ignore.stdout = T)
          cmd=paste0(regtools,
                     ' junctions annotate ',
                     out,"/2-junc/portcullis_all.junctions.bed ",
                     genomePath," ",
                     GTFs$values[input$annotation_public_gtf],
                     " -o ", out,"/2-junc/portcullis_all.junctions_annotated.tab" )

          print(cmd)
          system(cmd,wait = T,ignore.stdout = T)
        }

        aggregate_junctions(out_dir,dir_analysis,cutoff = 0)
        output$message_junction_calling=renderUI({
                                                 column(12,HTML("<h2><b><font color='green'>
                                                           Your analysis is successfully completed
                                                           </b></font></h2>"),
                                                     actionButton(inputId = "go_to_analyse",
                                                                  label = HTML("<b>GO TO THE DETAILS OF THE ANALYZES</b>"),
                                                                  class="btn btn-success",
                                                                  icon = icon("arrow-alt-circle-right"))
                                                     
                                                     )
                                                 })
        
        
      }
 
       
    })
    
    observeEvent(input$go_to_analyse,{
      updateTabItems(session, "tabs", selected = "m2")
      updateSelectInput(session = session,inputId = "select_analysis",choices = getFinishedAnalysis(),selected = finished_analysis$name )
    })
    
############## ----------------------------------------------------------------------------------------------------------
 
 ###------------------------------- View analysis------------------------------------------------------------------------
    
       observeEvent(input$View_analysis_btn,{
         
         progress <- shiny::Progress$new(session, min=0, max=100)
         on.exit(progress$close())
         progress$set(message = 'In progress ...')
         progress$set(value = 100)
         shinyjs::showElement("results_output")
         shinyjs::showElement(id="vis-viz_plot")
         analyse$name=input$select_analysis
         analyse$path=paste0("data/usersData/results/",input$select_analysis)
         analyse$junctions=aggregate_junctions(paste0(analyse$path,"/junctions_calling"),dir_analysis=analyse$path,cutoff = input$cuttof_depth)
         colnames(analyse$junctions)=str_replace_all(colnames(analyse$junctions),pattern = "_",replacement = " ")
         updateSelectInput(session,inputId = "select_gene",choices = unique(analyse$junctions$genes))
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
             
             formatStyle("known junction",color = "white",fontWeight = 'bold', backgroundColor = styleEqual(c("knwon","unknwon"), c("#83a89a","#ba6262"))) 
           
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
        write.table(analyse$junctions[analyse$junctions$`known junction`=="knwon",],file,row.names = F,sep="\t")
      }
    )
    
    output$download_unknwon_junction <- downloadHandler(
      filename = function() {
        paste0("Unknown_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[analyse$junctions$`known junction`=="unknwon",],file,row.names = F,sep="\t")
        
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
        write.table(analyse$junctions[analyse$junctions$`known junction`=="knwon",],file,row.names = F,sep="\t")
      }
    )
    
    output$download_unknwon_junction_bis <- downloadHandler(
      filename = function() {
        paste0("Unknown_junctions-",analyse$name,'.tsv')
      },
      content = function(file) {
        write.table(analyse$junctions[analyse$junctions$`known junction`=="unknwon",],file,row.names = F,sep="\t")
        
      }
    )
    
    
#------------------------------------------------------------------------------------------------------------------------

##### Plotly ------------------------------------------------------------------------------------------------------------
    
    observeEvent(input$plot_junction_btn,{
       

      if(!is.null(input$select_samples) && length(input$select_samples)!=0 && input$select_samples!=""){
        showElement(id = "viz_plot",anim = T)
        output$plot_junct=renderUI(shinycssloaders::withSpinner(plotlyOutput("splice_graph",height = 800)))
        p=viz_junctions(junctions =analyse$junctions,
                        samples =input$select_samples,
                        exonGTF = hg19_exons ,
                        gene = input$select_gene,
                        gene_transcript = input$select_transcript[1],
                        principalTranscript =input$select_principal_transcript,
                        transcriptList = input$select_transcript,
                        groupJunctionsBy = "status",
                        cutoff_depth = input$cuttof_depth_plot 
                        # space_between_samples = space_between_samples,
                        # is.random_y =is.random_y 
          )
        
        output$splice_graph=renderPlotly({
           p
        })

      }else{
        alert("Oops !! Please select one or more samples .")
      }

    })

#####--------------------------------------------------------------------------------------------------------------------
  })