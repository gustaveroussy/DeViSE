

# write.table(mutations,"data/usersData/results/analyse_3pos_cltrs/mutations.tab",sep = "\t",row.names = F)

###----- Inputs -------------------------------------------------------------------------------

viz_junctions=function(
                      junctions,
                      samples=c("56531","42814","56970"),
                      exonGTF,
                      gene,
                      gene_transcript,
                      principalTranscript,
                      transcriptList,
                      groupJunctionsBy="anchor",
                      cutoff_depth=80,
                      mutations=NULL,
                      space_between_samples=5,
                      is.random_y=T
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
    layout(xaxis =ax , yaxis = ay)%>% 
    layout(plot_bgcolor=plot_bgcolor) %>% 
    layout(paper_bgcolor=paper_bgcolor) %>%
    layout(legend = legende)
  
  if(is.random_y){
    random_y=0.5 
  }else{
    random_y=1
  }
  
  ################## plots transcripts Exons ------------------------------------------------------------------------------------------------------------------------
  
  exons_colors=generate_colors(length(y_ticktext))
  names(exons_colors)=c(transcriptList,samples)
  break_points=c()
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
    
    showlegend=T
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
                     showlegend = showlegend)
      
      p=add_text(p,x = (exons$start[i]+exons$end[i])/2,
                 y=y_tickvals[t],
                 textfont = list( size = 10,color="black"),
                 text=num_exon,
                 legendgroup=t,
                 showlegend = F)
      showlegend=F
      
      
      
    }
    
    if(t %in% names(junctions)) {
      
      i=t
      junction=junctions[[i]]
      
      if(nrow(junction)>0){
        
        random_y_pos=y_tickvals[i]+space_between_samples*0.75*runif(nrow(junction),random_y,1)
        
        if(groupJunctionsBy=="anchor"){
          colors=getcolorFromgroup(junction$anchor)
          group=names(colors)
          
        }else{
          colors=getcolorFromgroup(junction$status,by = "status")
          group=names(colors)
          
        }
        
        p=add_segments(p ,
                       x = junction$Start,
                       y = y_tickvals[i],
                       xend = (junction$Start+junction$End)/2,
                       yend = random_y_pos,
                       line = list(width = 2,color=colors),
                       color=colors,
                       legendgroup=paste(group,i),
                       name=group,
                       showlegend=T
        )
        p=add_segments(p ,
                       x = (junction$Start+junction$End)/2,
                       y =random_y_pos,
                       xend = junction$End,
                       yend = y_tickvals[i],
                       line = list(width = 2,color=colors),
                       color=colors,
                       name=group,
                       legendgroup=paste(group,i),
                       showlegend=F
        )
        
        
        
        p=add_text(p,x= (junction$Start+junction$End)/2,
                   y=random_y_pos,
                   textfont = list( size = 10,color="black"),
                   text=junction$Depth,
                   name=colors,
                   legendgroup=paste(group,i),
                   showlegend = F)
        
        p=add_markers(p,
                      x = (junction$Start+junction$End)/2,
                      y = random_y_pos,
                      marker=list(symbol=14,
                                  size=2,
                                  color="blue"),
                      legendgroup = paste(group,i),
                      name=group,
                      text=gettextFromdataframe(junction[,-5]),
                      showlegend = F
        )
        
        
        
      }
      
      if(!is.null(mutations)){
        
          events=mutations[mutations$sample==t,]
          events=events[events$gene==gene,]
          showlegend=T
          
          if(nrow(events)>0){
            
            for (mut in 1:nrow(events)) {
            
              mutation=events[mut,]
              p=add_markers(p,x = c(mutation$start,mutation$end),y=y_tickvals[i],text=gettextFromdataframe(mutation),marker=list(color="red",symbol=20,size=10),name=paste0("DNA events (Mutations/Indels)"),showlegend=showlegend)
              showlegend=F
              if(mutation$start-mutation$end!=0){
                p=add_segments(p ,x = mutation$start, y = y_tickvals[i], xend =  mutation$end, yend = y_tickvals[i], line = list(width = 2,color="red"),name=paste0("DNA events (Mutations/Indels)"))
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
                            size=10,
                            color="blue"),
                legendgroup = "Strand",
                showlegend = F
  )
  
  return(p)
}



cutoff_depth=80
path="data/usersData/results/analyse_3pos_cltrs"
junctions=aggregate_junctions(paste0(path,"/junctions_calling"),dir_analysis=path,cutoff = cutoff_depth)
colnames(junctions)=str_replace_all(colnames(junctions),pattern = "_",replacement = " ")
junctions
exonGTF=list(default=readRDS("data/appData/public_annotation/gencodeV19_exons_default.rds"),union=readRDS("data/appData/public_annotation/gencodeV19_exons_union.rds"))
gene="BRCA1"
gene_transcript="ENST00000357654.3"
samples=c("56970","56531","42814")

transcriptList=c("ENST00000357654.3",
                 "ENST00000354071.3",
                 "ENST00000352993.3",
                 "ENST00000346315.3",
                 "ENST00000351666.3"
)
# transcriptList=c()
principalTranscript="Merged_transcripts"
space_between_samples=5
is.random_y=T
groupJunctionsBy="status"
mutations=data.frame(sample=c("56531","42814"),
                     chr=c("chr17","chr17"),
                     start=c(41226738,41267796),
                     end=c(41226738,41267805),
                     type=c("SNV","Del"),
                     ref=c('G','TAAGATGGTC'),
                     alt=c('A','T'),
                     gene="BRCA1")




p=viz_junctions(junctions =junctions,
                samples =samples,
                exonGTF = exonGTF ,
                gene = gene,
                gene_transcript = gene_transcript,
                principalTranscript =principalTranscript,
                transcriptList = transcriptList,
                groupJunctionsBy = groupJunctionsBy,
                cutoff_depth = cutoff_depth ,
                mutations=mutations,
                space_between_samples = space_between_samples,
                is.random_y =is.random_y )
              
  
  
 