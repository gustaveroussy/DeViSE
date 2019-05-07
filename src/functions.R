
genomePath=paste0(getwd(),"/","data/appData/public_annotation/gencodeV19_dna.fa")


col.names_junc=c( "chrom" ,
                  "start",
                  "end",
                  "name",
                  "score" ,
                  "strand",
                  "splice_site",
                  "acceptors_skipped",
                  "exons_skipped",
                  "donors_skipped",
                  "anchor",
                  "known_donor",
                  "known_acceptor",
                  "known_junction",
                  "genes",
                  "transcripts")


develpped_by=HTML('
                               <body>
                               <p style="position: fixed; bottom: 0; width:100%;">
                                <br/>
                               <a href="https://www.gustaveroussy.fr/fr/content/plateforme-de-bioinformatique-activit%C3%A9s" target="_blank">Developed by the bioinformatics team,
                               <br/> Institut Gustave Roussy - B2M, <br/>
                               114 Rue Edouard Vaillant,<br/>
                               94800, Villejuif (France).
                               </a> 
                               </p>
                               </body>'
)


trim <- function (x) gsub("^\\s+|\\s+$", "", x)

shinyInput <- function(FUN, len, id,label, ...) {
  
  if(len == 0){
    return(integer(0))
  }
  singleShinyInput=function(args,funct, ...){
    as.character(funct(args[1],args[2], ...))
  }
  
  args=data.frame(id=paste0(id, seq(len)),label=label)
  inputs=apply(args,MARGIN = 1,FUN = singleShinyInput,funct=FUN, ...)
  inputs
  
}


actionButtonInputs=function(len, id,label, icons=NULL,class_btns, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(actionButton(paste0(id, i),label[i],icon=icon(icons[i]),class=class_btns[i],...))
  }
  inputs
}


actionLinkInputs=function(len, id,label, icons=NULL,class_btns, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(actionLink(paste0(id, i),label[i],icon=icon(icons[i]),class=class_btns[i],...))
  }
  inputs
}



getJunctionFromExons=function(bed){
  
  junctions=data.frame()
  
  for (i in unique(bed$Gene)) {
  
  exons=bed[bed$Gene==i,]
  junc=data.frame(Chrromosome=exons$Chromosome[1],
                       Start=exons$End[-length(exons$End)],
                       End=exons$Start[-1],
                       Gene=i,
                       Strand=sign(exons$Start[-1]-exons$End[-length(exons$End)]))
  
  junctions=rbind(junctions,junc)
  
  }
  
  junctions$Strand[junctions$Strand==1]="+"
  junctions$Strand[junctions$Strand==-1]="-"
  
  return(junctions)
}


get_bedJunctionsFrom_portcullis=function(path_junsction_tab){
  
  junction=read.delim(path_junsction_tab)
  junction=junction[,c(3,5,6,19)]
  colnames(junction)=c("chromosome","Start","End","Depth")
  return(junction)
}






message_JunCalling=function(input_bams,input_analysis_name){
  
  if(is.null(input_bams))
    return(list(html=HTML("<h2><b><font color='red'> Oops !! Please upload yor bams before launching the analyse. </b></font></h2>"),
                text="Oops !! Please upload yor bams before launching the analyse."))
  
  if(is.null(input_analysis_name) || input_analysis_name =="")
    return(list(html=HTML("<h2><b><font color='red'> Oops !! Please specify the name of the analysis. </b></font></h2>"),
                text="Oops !! Please specify the name of the analysis."))
  if(input_analysis_name %in% list.files("data/usersData/results"))
    return(list(html=HTML("<h2><b><font color='red'> Oops !! This analysis name is already exist, please specify another name. </b></font></h2>"),
                text="Oops !! This analysis name is already exist, please specify another name."))
    
  
  return("")
  
}




# aggregate_junctions=function(path_analysis,dir_analysis,cutoff_depth=25){
#   
# junctions=data.frame()
# files=list.files(path_analysis)
# 
# for (i in files) {
#   
#   junction=read.delim(paste0(path_analysis,"/",i,"/2-junc/portcullis_all.junctions_annotated.tab"))
#   junction$nb_raw_aln=junction$score
#   junction$sample=i
#   junction=junction[junction$nb_raw_aln>cutoff_depth,]
#   junctions=rbind(junctions,junction)
# }
# 
# junctions$junc_id=paste0(junctions$chrom,":",junctions$start,"-",junctions$end)
# 
# 
# junctions.tab=junctions[!duplicated(junctions$junc_id),]
# all_junctions=junctions.tab$junc_id
# 
# for (i in files) {
#  d=rep(0,length(all_junctions))
#  names(d)=all_junctions
#  junction=junctions[junctions$sample==i,]
#  depth=junction$nb_raw_aln
#  names(depth)=as.character(junction$junc_id)
#  d[names(depth)]=depth
#  junctions.tab[[i]]=d
# }
# 
# junctions.tab$known_junction[junctions.tab$known_junction==1]="known"
# junctions.tab$known_junction[junctions.tab$known_junction==0]="unknown"
# 
# junctions.tab=junctions.tab[,-which(colnames(junctions.tab) %in% c("name","score","nb_raw_aln","sample","junc_id","known_donor","known_acceptor"))]
# write.table(junctions.tab,paste0(dir_analysis,"/all_junctions_all_samples.tab"),sep = "\t",row.names = F)
# 
# 
# return(junctions.tab)
# 
# }


aggregate_junctions=function(samplesList,type="ALL"){
  
  junctions=data.frame()
  
  samples_valid=c()
  samples_notValid=c()
  
  for (i in samplesList) {
    
    r=strsplit(i,"/")[[1]][1]
    s=strsplit(i,"/")[[1]][2]
    
    junction=getJunctionsFromSample(sample = s,analysis = r,type = type)
    if(length(junction)!=1){
      junction=junction[,col.names_junc]
      junction$score=as.numeric(as.character(junction$score))
      junction$start=as.numeric(as.character(junction$start))
      junction$end=as.numeric(as.character(junction$end))
      junction$nb_raw_aln=junction$score
      junction$sample=i
      junctions=rbind(junctions,junction)
      samples_valid=c(samples_valid,i)
    }else{
      samples_notValid=c(samples_notValid,i)
    }
  }
  
  if(length(samples_valid)==0)
    return(list(junctions.tab=NULL,samples_notValid=samples_notValid))
  
  junctions$junc_id=paste0(junctions$chrom,":",junctions$start,"-",junctions$end)
  junctions.tab=junctions[!duplicated(junctions$junc_id),]
  all_junctions=junctions.tab$junc_id
  
  for (i in samples_valid) {
    d=rep(0,length(all_junctions))
    names(d)=all_junctions
    junction=junctions[junctions$sample==i,]
    depth=junction$nb_raw_aln
    names(depth)=as.character(junction$junc_id)
    d[names(depth)]=depth
    junctions.tab[[i]]=d
  }
  
  junctions.tab$known_junction[junctions.tab$known_junction==1]="known"
  junctions.tab$known_junction[junctions.tab$known_junction==0]="unknown"
  junctions.tab$known_junction=as.factor(junctions.tab$known_junction)
  junctions.tab$anchor=as.factor(junctions.tab$anchor)
  junctions.tab$genes=as.factor(junctions.tab$genes)
  junctions.tab=junctions.tab[,-which(colnames(junctions.tab) %in% c("name","score","nb_raw_aln","sample","junc_id","known_donor","known_acceptor"))]
  junctions.tab$maxReads_AllSamples=apply(as.matrix(junctions.tab[,samples_valid]),1,max)
  return(list(junctions.tab=junctions.tab,samples_notValid=samples_notValid))
  
}





getlistsamplesFromRunList=function(runList){
  res=c()
  for (i in runList) {
  samples=paste0(i,'/',getSampleListFromAnalysis(i))
  res=c(res,samples)
  }
  return(res)
}




get_exonFromGTF=function(gtf="data/appData/public_annotation/gencodeV19.gtf"){
  library(gread)
  gtf <- read_format(gtf)
  # extract exons, combine coordinates of overlapping exons
  exons <- extract(gtf, feature="exon",gene_id = "gene_name")
  # extract all exons within the gene, but combine overlapping exons
  # exons <- extract(gtf, feature="gene_exon", type="union")
  ## extract gene span (uses exon coordinates if feature='gene' doesn't exist)
  # genes <- extract(gtf, feature="gene", type="default")
  return(exons)
}

 getFinishedAnalysis=function(path="data/usersData/results"){
   
   analyses=list.files(path)
   res=c()
   for (i in analyses) {
     if("all_junctions_all_samples.tab" %in% list.files(paste0(path,"/",i)))
       res=c(res,i)
   }
   
   return(res)
   
 }
 
 
 gettextFromdataframe=function(data){
   
   res=c()
   
   for (i in 1:nrow(data)) {
     text=""
     for (col in colnames(data)) {
       text=paste0(text,"\n<b>",col,": </b>",data[[col]][i])
     }
     res=c(res,text)
   }
   
   return(res)
   
   
 } 
 
 getcolorFromgroup=function(x,by="anchor"){
   
   if(by=="anchor"){
     
     colors=rep("#999999",length(x))
     names(colors)=x
     colors[x=="A"]="#A65628"
     colors[x=="D"]="#984EA3"
     colors[x=="DA"]="#377EB8"
     colors[x=="N"]="#E41A1C"
     
   }else{
     colors=rep("green",length(x))
     names(colors)=x
     colors[x=="known"]="blue"
     colors[x=="unknown"]="red"
   }
   
   return(colors)
   
 }
 
 generate_colors=function(n=2){
   require(RColorBrewer)
   color = brewer.pal(9, "Set1")
   cols=c("#b300f9","#a0b25e","#5eb2aa","#b2745e")
   # color=rep(sample(color[-6],size = 4),n)
   color=rep(cols,n)
   return(color[1:n])
 }
 
 get_exonFrom_transcript=function(exons,transcript,gene){
   
   gene_id=exons$gene_id[str_detect(exons$transcript_id,transcript)]
   gene_id=gene_id[1]
   res=exons[str_detect(exons$gene_id,gene_id),]
   
   return(data.frame(chromosome=res$seqnames,
                     start=res$start,
                     end=res$end,
                     strand=res$strand,
                     gene=gene,
                     transcript=res$transcript_id
   ))
   
 }
 get_junctionFromExons=function(sample,junc,gene,cutoff){
   junctions=junc[junc$genes==gene,]
   junctions=junctions[junctions[[sample]]>=cutoff,]
   
   return(data.frame(chromosome=junctions$chrom,
                     Start=junctions$start,
                     End=junctions$end,
                     Depth=junctions[[sample]],
                     Transcript=junctions$transcripts,
                     status=junctions$`known junction`,
                     acceptors_skipped=junctions$`acceptors skipped`,
                     exons_skipped=junctions$`exons skipped`,
                     donors_skipped=junctions$`donors skipped`,
                     anchor=junctions$anchor)
          
   )
 }
 
 


 
 viz_junctions=function(
   junctions,
   # selected_junc=NULL,
   samples,
   exonGTF,
   gene,
   gene_transcript,
   principalTranscript,
   transcriptList,
   groupJunctionsBy="status",
   cutoff_depth=1,
   mutations=NULL,
   space_between_samples=5
 ){
   ####--------------------------------------------------------------------------------------
   
   ###Preparation --------------------------------------------------------------------------------
   exonsTranscripts=get_exonFrom_transcript(exonGTF$default,gene_transcript,gene)
   exonsUnion=get_exonFrom_transcript(exonGTF$union,gene_transcript,gene)
   exonsUnion$transcript="Merged_transcripts"
   exonsTranscripts=rbind(exonsTranscripts,exonsUnion)
   exonsTranscripts=exonsTranscripts[exonsTranscripts$transcript %in% c(transcriptList,principalTranscript), ]
   exonsTranscripts=exonsTranscripts[order(exonsTranscripts$start),]
   junctions=base::sapply(samples,get_junctionFromExons,junc=junctions[!is.na(junctions$genes),],gene=gene,cutoff=cutoff_depth,USE.NAMES = T,simplify = F)
   
   plot_bgcolor=''
   paper_bgcolor=''
   legende = list(
     y=.5,
     font = list(
       family = "sans-serif",
       size = 12,
       color = "#000"),
     bgcolor = "#ede3e3",
     bordercolor = "#FFFFFF",
     borderwidth = 2,
     orientation="v")
   
   ax <- list(
     showgrid = F,
     linewidth = 2,
     showticklabels = T,
     tickmode='array',
     title="Genomic position"
   )
   
   
   nbr_transcripts=length(transcriptList)
   if(nbr_transcripts==0){
     y_tickvals=c()
     y_names=c()
   }else{
     
     y_tickvals=2*seq(nbr_transcripts)
     y_ticktext=transcriptList
     y_names=transcriptList
   }
   
   my_max=function(x){
     if(length(x)==0)
       return(0)
     return(max(x))
   }
   
   for(s in samples){
     
     y_tickvals=c(y_tickvals,my_max(y_tickvals)+space_between_samples)
     y_ticktext=c(y_ticktext,paste0("<b>[",s,"]: </b>",principalTranscript))
     y_names=c(y_names,s)
   }
   
   names(y_tickvals)=y_names
   
   ay <- list(
     zeroline = F,
     showline = F,
     showgrid = F,
     linewidth = 2,
     tickvals = y_tickvals,
     ticktext = y_ticktext,
     showticklabels = T,
     tickmode='array'
   )
   
   ##########"----------------------------------------------------------------------------------------------------------------
   p <- plot_ly() %>% 
     layout(title=paste0("<b>","Selected gene: [ ",gene," ]</b>"),xaxis =ax , yaxis = ay)%>% 
     layout(plot_bgcolor=plot_bgcolor) %>% 
     layout(paper_bgcolor=paper_bgcolor) %>%
     layout(legend = legende)
   
   # if(is.random_y){
   #   random_y=0.5 
   # }else{
   #   random_y=1
   # }
   # 
   ################## plots transcripts Exons ------------------------------------------------------------------------------------------------------------------------
   
   exons_colors=generate_colors(length(y_ticktext))
   names(exons_colors)=c(transcriptList,samples)
   break_points=c()
   showlegend=c(T,T)
   names(showlegend)=c("known","unknown")
   showlegend_event=T
   for (t in rev(c(transcriptList,samples))) {
     
     if(t %in% transcriptList) {
       exons=exonsTranscripts[exonsTranscripts$transcript==t,]
       nm=t
       
     }else{
       exons=exonsTranscripts[exonsTranscripts$transcript==principalTranscript,]
       nm=paste0(t,": ",principalTranscript)
       
     } 
     
     break_points=c(break_points,exons$start,exons$end)
     
     p=add_segments(p ,
                    x = min(exons$start),
                    y = y_tickvals[t],
                    xend = max(exons$end),
                    yend = y_tickvals[t],
                    line = list(width = 2,dash=2,color=exons_colors[t]),
                    legendgroup=t,
                    showlegend = F)
     
    
     for (i in 1:nrow(exons)) {
       
       if(exons$strand[1]=="-"){
         num_exon=nrow(exons)-i+1
       }else{
         num_exon=i
         
       }
       
       
       p=add_polygons(p,x = c(exons$start[i],
                              exons$start[i],
                              exons$end[i],
                              exons$end[i]),
                      y= c(y_tickvals[t]-0.75,
                           y_tickvals[t]+0.75,
                           y_tickvals[t]+0.75,
                           y_tickvals[t]-0.75),
                      line=list(width = 1,color=exons_colors[t]),
                      fillcolor=toRGB(exons_colors[t]),
                      color=exons_colors[t],
                      name=nm,
                      legendgroup=t,
                      showlegend = F)
       
       p=add_text(p,x = (exons$start[i]+exons$end[i])/2,
                  y=y_tickvals[t],
                  textfont = list( size = 10,color="black"),
                  text=num_exon,
                  legendgroup=t,
                  showlegend = F)
       
       
     }
     
     if(t %in% names(junctions)) {
       
       i=t
       junction=junctions[[i]]
       
       if(nrow(junction)>0){
         
           random_y_pos=y_tickvals[i]+space_between_samples*0.8*log(junction$Depth)/max(log(junction$Depth))
           colors=getcolorFromgroup(junction$status,by = "status")
           group=names(colors)
           
         
         for (junc in 1:nrow(junction)) {
           
         
         p=add_segments(p ,
                        x = junction$Start[junc],
                        y = y_tickvals[i],
                        xend = (junction$Start[junc]+junction$End[junc])/2,
                        yend = random_y_pos[junc],
                        line = list(width = 2,color=colors[junc]),
                        color=colors[junc],
                        legendgroup=junction$status[junc],
                        name=junction$status[junc],
                        showlegend=showlegend[junction$status[junc]]
         )
         showlegend[junction$status[junc]]=F
   
         
         p=add_segments(p ,
                        x = (junction$Start[junc]+junction$End[junc])/2,
                        y =random_y_pos[junc],
                        xend = junction$End[junc],
                        yend = y_tickvals[i],
                        color=colors[junc],
                        line = list(width = 2,color=colors[junc]),
                        name=junction$status[junc],
                        legendgroup=junction$status[junc],
                        showlegend=F
         )
         
         
         
         p=add_text(p,x= (junction$Start[junc]+junction$End[junc])/2,
                    y=random_y_pos[junc],
                    textfont = list( size = 10,color=colors[junc]),
                    text=junction$Depth[junc],
                    color=colors[junc],
                    legendgroup=junction$status[junc],
                    showlegend = F)
         
         p=add_markers(p,
                       x = (junction$Start[junc]+junction$End[junc])/2,
                       y = random_y_pos[junc],
                       marker=list(symbol=14,
                                   size=2,
                                   color=colors[junc]),
                       legendgroup = junction$status[junc],
                       color=colors[junc],
                       name=junction$status[junc],
                       text=gettextFromdataframe(junction[,-5])[junc],
                       showlegend = F
         )
         
         }
           
       }
       
       if(!is.null(mutations)){
         
         events=mutations[t,]
         events=events[events$gene==gene,]
         if(nrow(events)>0){
           
           for (mut in 1:nrow(events)) {
             
             mutation=events[mut,]
             p=add_markers(p,x = c(mutation$start,mutation$end),y=y_tickvals[i],text=gettextFromdataframe(mutation),marker=list(color="#f9b300",symbol=20,size=15),legendgroup="event_mutations",name=paste0("DNA events (Mutations/Indels)"),showlegend=showlegend_event)
             showlegend_event=F
             if(mutation$start-mutation$end!=0){
               p=add_segments(p ,x = mutation$start, y = y_tickvals[i], xend =  mutation$end, yend = y_tickvals[i], line = list(width = 2,color="#f9b300"),legendgroup="event_mutations",name=paste0("DNA events (Mutations/Indels)"),showlegend=showlegend_event)
             }
             
           }
         } 
       }
       
     }
   }
   
   
   p=add_segments(p ,
                  x = unique(break_points),
                  y =min(y_tickvals),
                  xend = unique(break_points),
                  yend = max(y_tickvals),
                  line = list(width = 1,color="red",dash=1),
                  name="Projection-grid",
                  showlegend=T,
                  visible="legendonly"
   )
   
   p=add_segments(p ,
                  x = max(break_points),
                  y =min(y_tickvals)-1.5,
                  xend = unique(break_points),
                  yend = min(y_tickvals)-1.5,
                  line = list(width = 3,color="blue",dash=3),
                  name="Strand",
                  legendgroup = "Strand",
                  showlegend=T
   )
   
   if(exons$strand[1]=="-"){
     symbol_stand=7
   }else{
     symbol_stand=8
   }
   p=add_markers(p,
                 x = seq.int(from = min(break_points),to = max(break_points),by = (max(break_points)-min(break_points))/8),
                 y = min(y_tickvals)-1.5,
                 marker=list(symbol=symbol_stand,
                             size=15,
                             color="blue"),
                 legendgroup = "Strand",
                 showlegend = F
   )
   
   return(p)
 }
 

getMutationFromAnalysis=function(path){
  
  if("mutations.tab" %in% list.files(path)){
    return(read.delim(paste0(path,"/mutations.tab")))
  }else{
    return(NULL)
  }
}



parserJobStatus=function(analyse_name){
  
  job_ids=readRDS(paste0("data/usersData/runs_jobs/",analyse_name))
  res=c()
  for (i in 1:length(job_ids)) {
    
    text=getJobStatus(job_id = job_ids[i])$Status
    if(is.null(text)){
      
      res=rbind(res,c(names(job_ids)[i],job_ids[i],NA,NA,NA,"C",NA))
      
    }else{
      
      lines=unlist(strsplit(text,"\n"))
     
      l = unlist(strsplit(lines[3]," "))
      l=l[l!=""]
      res=rbind(res,c(names(job_ids)[i],l))
    }
    
    }
  
  colnames(res)=c("Sample Name","Job ID", "Name","User","Time Use", "Status", "Queue")
  res=as.data.frame(res)
  res$Status=as.character(res$Status)
  res$Status[res$Status=="R"]="Running"
  res$Status[res$Status=="Q"]="Waiting"
  res$Status[res$Status=="C"]="Complete"
  res$Results=sapply(res$`Sample Name`,getJunctionsFromSample,analysis=analyse_name)
  res$Status[paste0(res$Status,res$Results)=="CompleteNot available"]="Failed"
  return(res[,c(1,2,6,8)])
  
  
}

getRunStatus=function(run_name){
  
  if(!run_name %in% list.files("data/usersData/runs_jobs/"))
    return(0)
  
  Status=parserJobStatus(run_name)
  return(round(length(Status$Status[Status$Status=="Complete"])/nrow(Status),digits = 2)*100)
}



render_jobStatus=function(run_status){
  
  run_status$Advanced=shinyInput(actionButton,
                                 nrow(run_status),
                                 'button_',
                                 label = "Logs",
                                 onclick = 'Shiny.onInputChange(\"select_button_sample_name\",  this.id+"_"+Math.random())',
                                 icon=icon("info-circle"),
                                 class="btn-info")
  
  # run_status$Advanced=shinyInput(actionButton,
  #                                nrow(run_status),
  #                                'button_',
  #                                label = "Resubmit",
  #                                onclick = 'Shiny.onInputChange(\"select_button_sample_name_re_run\",  this.id+"_"+Math.random())',
  #                                icon=icon("redo") )
  
 return(datatable(run_status,
            selection = 'multiple',
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
          
          formatStyle(1,  color = 'black', backgroundColor = '#d5e5ef',fontWeight = 'bold')  %>%
          # formatStyle(4,  color = 'black', backgroundColor = '#d5e5ef',fontWeight = 'bold')  %>%
          formatStyle("Status",color = "black",fontWeight = 'bold', backgroundColor = styleEqual(c("Complete","Waiting","Running","Hold","Failed"),
                                                                                                 c("#83a89a","#ba6262","#9095ce","#ceb790","red"))) %>%
          formatStyle("Results",
                      color = "black",
                      fontWeight = 'bold',
                      backgroundColor = styleEqual(c("Available","Not available","Only filtered","Only not filtered"),
                                                   c("#83a89a","red","#9095ce","#ceb790"))) ) 

}



removeAnalysis=function(run_name){
  
  jobs=readRDS(paste0("data/usersData/runs_jobs/",run_name))
  killJob(URLencode(paste0(jobs,collapse = " ")))
  try(removeAnalysis_FS(run_name))
  file.remove(paste0("data/usersData/runs_jobs/",run_name))
  
  return(T)
  
}


loading_css=tags$head(tags$style(HTML('*{
    margin: 0;
                                       padding: 0;
                                       }
                                       
                                       body{
                                       background: #eee;
                                       }
                                       
                                       .circle{
                                       width: 180px;
                                       height: 180px;
                                       border: 10px inset rgb(237, 80, 184);
                                       display: block;
                                       position: fixed;
                                       top: 50%;
                                       left: 50%;
                                       margin-left: -100px;
                                       margin-top: -100px;
                                       border-radius: 200px;
                                       -moz-animation: rotate 5s infinitelinear;
                                       -webkit-animation: rotate 5s infinite linear;
                                       animation: rotate 5s infinite linear;
                                       box-shadow: 0 0 5px rgba(0,0,0,0.2);
                                       }
                                       
                                       .circle-small{
                                       width: 150px;
                                       height: 150px;
                                       border: 6px outset rgb(241, 133, 133);
                                       display: block;
                                       position: fixed;
                                       top: 50%;
                                       left: 50%;
                                       margin-left: -81px;
                                       margin-top: -81px;
                                       border-radius: 156px;
                                       -moz-animation: rotate-rev 3s infinite linear;
                                       -webkit-animation: rotate-rev 3s infinite linear;
                                       animation: rotate-rev 3s infinite linear;
                                       box-shadow: 0 0 5px rgba(0,0,0,0.2);
                                       }
                                       
                                       .circle-big{
                                       width: 210px;
                                       height: 210px;
                                       border: 4px dotted rgb(241, 133, 133);
                                       display: block;
                                       position: fixed;
                                       top: 50%;
                                       left: 50%;
                                       margin-left: -109px;
                                       margin-top: -109px;
                                       border-radius: 214px;
                                       -moz-animation: rotate-rev 10s infinite linear;
                                       -webkit-animation: rotate-rev 10s infinite linear;
                                       animation: rotate-rev 10s infinite linear;
                                       }
                                       
                                       .circle-inner{
                                       width: 200px;
                                       height: 200px;
                                       background-color: rgb(241, 133, 133);
                                       display: block;
                                       position: fixed;
                                       top: 50%;
                                       left: 50%;
                                       margin-left: -80px;
                                       margin-top: -80px;
                                       border-radius: 80px;
                                       -moz-animation: pulse 1.5s infinite ease-in;
                                       -webkit-animation: pulse 1.5s infinite ease-in;
                                       animation: pulse 1.5s infinite ease-in;
                                       opacity: 1;
                                       box-shadow: 0 0 5px rgba(0,0,0,0.2);
                                       }
                                       
                                       .circle-inner-inner{
                                       width: 100px;
                                       height: 100px;
                                       background-color: rgb(74,124,134);
                                       display: block;
                                       position: fixed;
                                       top: 50%;
                                       left: 50%;
                                       margin-left: -50px;
                                       margin-top: -50px;
                                       border-radius: 100px;
                                       -moz-animation: pulse 1.5s infinite ease-in;
                                       -webkit-animation: pulse 1.5s infinite ease-in;
                                       animation: pulse 1.5s infinite ease-in;
                                       box-shadow: 0 0 5px rgba(0,0,0,0.2);
                                       }
                                       
                                       
                                       /*==============ANIMATIONS=================*/
                                       
                                       /*==============ROTATE=====================*/
                                       
                                       @-moz-keyframes rotate{
                                       0% {-moz-transform: rotate(0deg);}
                                       100% {-moz-transform: rotate(360deg);}
                                       }
                                       
                                       @-webkit-keyframes rotate{
                                       0% {-webkit-transform: rotate(0deg);}
                                       100% {-webkit-transform: rotate(360deg);}
                                       }
                                       
                                       @keyframes rotate{
                                       0% {transform: rotate(0deg);}
                                       100% {transform: rotate(360deg);}
                                       }
                                       
                                       /*==============ROTATE-REV=================*/
                                       
                                       @-moz-keyframes rotate-rev{
                                       0% {-moz-transform: rotate(0deg);}
                                       100% {-moz-transform: rotate(-360deg);}
                                       }
                                       
                                       @-webkit-keyframes rotate-rev{
                                       0% {-webkit-transform: rotate(0deg);}
                                       100% {-webkit-transform: rotate(-360deg);}
                                       }
                                       
                                       @keyframes rotate-rev{
                                       0% {transform: rotate(0deg);}
                                       100% {transform: rotate(-360deg);}
                                       }
                                       
                                       /*==============PULSE======================*/
                                       
                                       @-moz-keyframes pulse{
                                       0% {
                                       -moz-transform: scale(0.1);
                                       opacity: 0.2;
                                       }
                                       50% {
                                       -moz-transform: scale(1);
                                       opacity: 0.8;
                                       }
                                       100% {
                                       -moz-transform: scale(0.1);
                                       opacity: 0.2;
                                       }
                                       }
                                       
                                       @-webkit-keyframes pulse{
                                       0% {
                                       -webkit-transform: scale(0.1);
                                       opacity: 0.2;
                                       }
                                       50% {
                                       -webkit-transform: scale(1);
                                       opacity: 0.8;
                                       }
                                       100% {
                                       -webkit-transform: scale(0.1);
                                       opacity: 0.2;
                                       }
                                       }
                                       
                                       @keyframes pulse{
                                       0% {
                                       transform: scale(0.1);
                                       opacity: 0.2;
                                       }
                                       50% {
                                       transform: scale(1);
                                       opacity: 0.8;
                                       }
                                       100% {
                                       transform: scale(0.1);
                                       opacity: 0.2;
                                       }
                                       }'
  )))




loading_html= HTML( '<div class="circle">  </div>
                                <div class="circle-small"></div>
                                <div class="circle-big"></div>
                                <div class="circle-inner-inner"></div>
                                <div class="circle-inner"> <h1> <b> Please wait <b> </h1></div>')