x=readRDS("safire02.rds")
get_statFromJson=function(x,nova_dir="/mnt/seq1/Nova/"){

    library(jsonlite)
res.df=c()
for (i in unique(x$Run)) {
  
  ids=x$Name[x$Run==i]
  projects=x$Project[x$Run==i]
  js_res=NULL
  json_file=paste0(nova_dir,i,"/Data/Intensities/BaseCalls/Stats/Stats.json")
  try({js_res=remote_read_file(json_file,host,user,password,fromJSON, flatten = T)},silent = T)
  path=paste0("/mnt/seq1/Nova/",i,"/Data/Intensities/BaseCalls/")
  
   if(is.null(js_res)){
     json_file=paste0(nova_dir,i,"/Data/Intensities/BaseCalls/unaligned_barcodes8/Stats/Stats.json")
     try({js_res=remote_read_file(json_file,host,user,password,fromJSON, flatten = T)},silent = T)
     path=paste0("/mnt/seq1/Nova/",i,"/Data/Intensities/BaseCalls/unaligned_barcodes8/")
   }
    
  if(is.null(js_res)){
    json_file=paste0(nova_dir,i,"/Data/Intensities/BaseCalls/unaligned/Stats/Stats.json")
    try({js_res=remote_read_file(json_file,host,user,password,fromJSON, flatten = T)},silent = T)
    path=paste0("/mnt/seq1/Nova/",i,"/Data/Intensities/BaseCalls/unaligned/")
  }
  
  if(!is.null(js_res)){
  lane1=js_res$ConversionResults$DemuxResults[[1]]
  lane2=js_res$ConversionResults$DemuxResults[[2]]
  k=1
  for (j in ids) {
    NumberReads=lane1$NumberReads[lane1$SampleId==j]*2+lane2$NumberReads[lane2$SampleId==j]*2
    IndexSequence=lane1$IndexMetrics[lane1$SampleId==j][[1]][1]
    MismatchCounts.0 = paste0("[lane1: ",lane1$IndexMetrics[lane1$SampleId==j][[1]][2],", lane2: ",lane2$IndexMetrics[lane1$SampleId==j][[1]][2],"]")
    MismatchCounts.1 = paste0("[lane1: ",lane1$IndexMetrics[lane1$SampleId==j][[1]][3],", lane2: ",lane2$IndexMetrics[lane1$SampleId==j][[1]][3],"]")
    chemin=paste0(path,"/",projects[k],"/",j,"*fastq.gz")
    if(identical(NumberReads,numeric(0)))
      NumberReads=NA
    
    k=k+1
    
        res.df=rbind(res.df,data.frame(SampleID=j,
                                   run=i,
                                   IndexSequence=IndexSequence,
                                   MismatchCounts.0=MismatchCounts.0,
                                   MismatchCounts.1=MismatchCounts.1,
                                   NumberReads=NumberReads,
                                   path=chemin))
    
    
   
    
  }
  }else{
    res.df=rbind(res.df,data.frame(SampleID=ids,
                                   run=i,
                                   IndexSequence=rep(NA,length(ids)),
                                   MismatchCounts.0=rep(NA,length(ids)),
                                   MismatchCounts.1=rep(NA,length(ids)),
                                   NumberReads=rep(NA,length(ids)),
                                   path=rep(NA,length(ids)))) 
  }
  
  
}

counts=c()

for (i in res.df$SampleID) {
  counts=c(counts,length(which(res.df$SampleID==i)))
  
  
}

res.df=data.frame(res.df,`Number of occurrences`=counts )

return(res.df)
}
#res.df=res.df[order(res.df$SampleID),]

write.csv(res.df,"../samples_SAFIR02.csv",row.names = F)

x=get_statFromJson(x)

# 
# write(as.character(res.df[res.df$Number.of.occurrences==1,]$path),"../sample_unique.txt")
# 
# res.multipleSamples=res.df[res.df$Number.of.occurrences>1,]
# all_path=c()
# 
# for (i in unique(res.multipleSamples$SampleID)) {
# 
#   paths=res.multipleSamples$path[res.multipleSamples$SampleID==i]
#   p=paths[1]     
#     
#     for (j in paths[-1]) {
#       
#        p=paste0(p,";",j)
#   
#      }
# 
# all_path=c(all_path,p)
#     
# }
# 
# write(as.character(all_path),"../sample_multiple.txt")

write(as.character(res.df$path),"P28_PAKA_samples.txt",sep = "\t")



remote_read_file=function(chemin_file,host,user,password,FUN, ...){
  
  cmd=paste0("sshpass -p ",password," ssh ",user,"@",host," 'cat ",chemin_file,"'")
  FUN(pipe(cmd),...)  
  
}
library(jsonlite)
a=remote_read_file(json_file,host,user,password,fromJSON, flatten = T)





