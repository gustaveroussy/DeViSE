
genomePath=paste0(getwd(),"/","data/appData/public_annotation/gencodeV19_dna.fa")
portcullis="portcullis"
regtools="regtools"

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




aggregate_junctions=function(path_analysis,dir_analysis,cutoff_depth=25){
  
junctions=data.frame()
files=list.files(path_analysis)

for (i in files) {
  
  junction=read.delim(paste0(path_analysis,"/",i,"/2-junc/portcullis_all.junctions_annotated.tab"))
  depths=read.delim(paste0(path_analysis,"/",i,"/2-junc/portcullis_all.junctions.tab"))
  junction$nb_raw_aln=depths$nb_raw_aln
  junction$sample=i
  junction=junction[junction$nb_raw_aln>cutoff_depth,]
  junctions=rbind(junctions,junction)
}

junctions$junc_id=paste0(junctions$chrom,":",junctions$start,"-",junctions$end)


junctions.tab=junctions[!duplicated(junctions$junc_id),]
all_junctions=junctions.tab$junc_id

for (i in files) {
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

junctions.tab=junctions.tab[,-which(colnames(junctions.tab) %in% c("name","score","nb_raw_aln","sample","junc_id","known_donor","known_acceptor"))]
write.table(junctions.tab,paste0(dir_analysis,"/all_junctions_all_samples.tab"),sep = "\t",row.names = F)


return(junctions.tab)

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
     colors=rep("#e41a1c",length(x))
     names(colors)=x
     colors[x=="known"]="#999999"
   }
   
   return(colors)
   
 }
 
 generate_colors=function(n=2){
   require(RColorBrewer)
   color = brewer.pal(9, "Set1")
   color=rep(color[-6],n)
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
   random_y=0.3
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
         
         random_y_pos=y_tickvals[i]+space_between_samples*0.75*runif(nrow(junction),1-random_y,1)
         
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
 

getMutationFromAnalysis=function(path){
  
  if("mutations.tab" %in% list.files(path)){
    return(read.delim(paste0(path,"/mutations.tab")))
  }else{
    return(NULL)
  }
}
