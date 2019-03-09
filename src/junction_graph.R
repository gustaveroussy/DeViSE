
library(GenomicFeatures)
library(rtracklayer)
library(ggplot2)

sample_name="56531"
gene="BRCA2"
tool="Portcullis" ## Portcullis OR STAR
mutation=list(chr="chr9",start=21970901,end=21970901,ref='G',alt='A')
bed=read.delim("~/Documents/Projects_Etienne/splice/desing_bedRNAseq.bed")
dir.create(paste0("~/Documents/Projects_Etienne/splice/htmlReports/",sample_name),recursive = T)

if(tool=="STAR"){
junction=read.delim(paste0("~/Documents/Projects_Etienne/splice/STAR_analysis_2018-11-28/STAR_",sample_name,"/SJ.out.tab"),header = F)
junction=junction[,c(1,2,3,7)]
}else{
  junction=read.delim(paste0("~/Documents/Projects_Etienne/splice/portcullis_results/app_name=",sample_name,"/3-filt/portcullis_filtered.pass.junctions.tab"),header = T)
  junction=junction[,c(3,5,6,19)]
}
colnames(junction)=c("chromosome","Start","End","Depth")
exons=bed[bed$Gene==gene,]
exons=exons[order(exons$Start),]
html_output=paste0("~/Documents/Projects_Etienne/splice/htmlReports/",sample_name,"/",gene,".html")


class_junction=function(depth,fen=20,limit=100){
  if(depth >= limit){
    return(paste0("Junc-depth > ",limit))
  }else{
    return(paste0("Junc-depth: ",floor(depth/fen)*fen+1,"-",floor(depth/fen)*fen+fen))
  }
}

VizJunctions=function(exons,junction,mutation=NA,outputHtmlFile=NULL,gene="",sample_names=""){
  
  data=data.frame(Start=exons$Start,End=exons$End)
  junc=junction[junction$chromosome==as.character(exons$Chromosome[1]),]
  junc=junc[junc$Start>min(data),]
  junc=junc[junc$End < max(data),]
  
  mut=F
  
  if(!is.na(mutation) &&
     mutation$start > min(data) &&
     mutation$start < max(data) && 
     mutation$end > min(data) &&
     mutation$end < max(data))  {

    mut=T    
  }
  
  
  
  
  yend=c()
  y=c()
  for (i in 1:nrow(junc)) {
    
    if(i>1 && length(junc$Start[1:(i-1)][junc$Start[1:(i-1)]==junc$Start[i]])!=0 ){
      yend=c(yend,5)
      y=c(y,10)
    }else{
      yend=c(yend,15)
      y=c(y,10)
    }
  }   
  
  junc1=junc
  junc2=junc
  junc1$Start=(junc1$Start+junc1$End)/2
  junc2$End=(junc2$Start+junc2$End)/2
  junc_bis=rbind(junc1,junc2)
  y_bis=c(yend,y)
  yend_bis=c(y,yend)
  
  
  
  data=data.frame(Start=c(junc$Start,junc$End),Depth=c(junc$Depth,junc$Depth))
  

  library(plotly)
  library(ggplot2)
  
  ax <- list(
    zeroline = F,
    showline = T,
    tickcolor = toRGB("blue"),
    title="Genomic Position"
  )
    p=plot_ly()%>%
    layout(title =paste0("Splice graph [",sample_name,"] gene :",gene),xaxis = ax, yaxis =list(title="",range = c(0,20), zeroline = F),showlegend = T)
  
    
  for (i in 1:nrow(exons)) {
    
    p=add_polygons(p,x = c(exons$Start[i],exons$Start[i],exons$End[i],exons$End[i]), y= c(9.5,10.5,10.5,9.5),line=list(width = 1,color="blue"),color="blue", showlegend = F)
    p=add_text(p,x = (exons$Start[i]+exons$End[i])/2,
               y=10,
               textfont = list( size = 12,color="black"),
               text=paste0("E",exons$Exon[i]),showlegend = F)
    
    if(i!=nrow(exons)){
      p=add_segments(p ,x = exons$End[i], y = 10, xend = exons$Start[i+1], yend = 10, line = list(width = 4,dash=2,color="blue"),showlegend = F)
    }
    
  }
  

   p=add_text(p,x= (junc$Start+junc$End)/2,
              y=yend,
              textfont = list( size = 12,color="black"),
              text=junc$Depth,
              color=sapply(junc$Depth, class_junction),
              showlegend = T)
      

      
      p=add_segments(p ,x = junc_bis$Start, y = y_bis, xend =  junc_bis$End, yend = yend_bis, line = list(width = 2,color=sapply(junc_bis$Depth, class_junction)),color=sapply(junc_bis$Depth, class_junction))
      p=add_markers(p,x = data$Start,symbols = "x", y =10, marker=list(symbol=25,size=8),text =paste0("position: ",data$Start, "\nDepth:",data$Depth),color=sapply(data$Depth, class_junction) )

   if(mut){
     p=add_markers(p,x = c(mutation$start,mutation$end),y=9.95,marker=list(color="red",symbol=20,size=10),color=paste0("Mutation at: ",mutation$start),
                                                                                                       text=c(paste0("Chromosome: ",mutation$chr,
                                                                                                                      "\nStart: ",mutation$start,
                                                                                                                      "\nEnd: ",mutation$end,
                                                                                                                      "\nRef: ",mutation$ref,
                                                                                                                      "\nAlt: ",mutation$alt,
                                                                                                                      "\n\nthis position: ",mutation$start),
                                                                                                              paste0("Chromosome: ",mutation$chr,
                                                                                                                     "\nStart: ",mutation$start,
                                                                                                                     "\nEnd: ",mutation$end,
                                                                                                                     "\nRef: ",mutation$ref,
                                                                                                                     "\nAlt: ",mutation$alt,
                                                                                                                     "\n\nthis position: ",mutation$end)
                                                                                                                                    ) )
     if(mutation$start-mutation$end!=0){
       p=add_segments(p ,x = mutation$start, y = 9.95, xend =  mutation$end, yend = 9.95, line = list(width = 4,color="red"),color=paste0("Mutation at: ",mutation$start))
       
     }
     
      
   }
      # htmlwidgets::saveWidget(p,outputHtmlFile,selfcontained = F)
  
  return(p)
  
}

p=VizJunctions(exons,junction,mutation,gene,sample_name)








