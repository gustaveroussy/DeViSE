library(jsonlite)
library(httr)
library(yaml)
web_services=yaml.load_file("data/appData/web_services.yaml")

getRunList=function(){
  return(fromJSON(paste0(web_services$host,web_services$runList)))
}

getAnalysisList=function(){
  return(fromJSON(paste0(web_services$host,web_services$analysisList)))
}

getSampleListFromAnalysis=function(analysis){
  
    return(fromJSON(paste0(web_services$host,web_services$sample_listFromAnalysis,"?analysis=",analysis)))
  
}

getFileListFromRun=function(run,endsWith=""){
  
  return(fromJSON(paste0(web_services$host,web_services$fileList,"?run=",run,"&endwith=",endsWith)))
  
}

getLogListFromSample=function(analysis,sample,endsWith=""){
  
  return(fromJSON(paste0(web_services$host,web_services$logList,"?analysis=",analysis,"&sample=",sample,"&endwith=",endsWith)))
  
}

downloadLogFile=function(analysis,sample,logFile,from="rTerminal"){
  
  if(from=="rTerminal"){
    browseURL(paste0(web_services$host,web_services$download_log_file,"?analysis=",analysis,"&sample=",sample,"&logFile=",logFile))
    return(T)
  }
  
  if(from=="shiny"){
    return(paste0("window.open('",web_services$host,web_services$download_log_file,"?analysis=",analysis,"&sample=",sample,"&logFile=",logFile,"','_blank')"))
  }
  
  stop('"From" must be one of the folowing value: "shiny" or "rTerminal"')
  return(T)
}

getJunctionsFromSample=function(sample,analysis,type="STATUS"){
  
  junc=fromJSON(paste0(web_services$host,web_services$getJunctions,"?analysis=",analysis,"&sample=",sample))
  
  if(type=="ALL")
    return(junc$All_junctions)
  
  if(type=="FILTERED")
    return(junc$filtered_junctions)
  
  if(type=="STATUS"){
    
    if(junc$All_junctions!="all junctions file does not exist for this sample" && 
       junc$filtered_junctions!="filtered junctions file does not exist for this sample"){
      
      return("Available")
    }
    
    if(junc$All_junctions!="all junctions file does not exist for this sample"){
      return("Only not filtered junctions available")
    }
    
    if(junc$filtered_junctions!="filtered junctions file does not exist for this sample"){
      return("Only filtered junctions available")
    }
    
    return("Not available")
    
      }
  
  stop('"type" must be one of the folowing value: "FILTERED" or "ALL"')
  
}



# downloadLogFile=function(analysis,sample,from="rTerminal",type="ALL"){
#   
#   ws=NULL
#   
#   if(type=="ALL")
#     ws=web_services$download_all_junctionsFile
#   
#   if(type=="FILTERED")
#     ws=web_services$download_filtered_junctionsFile
#   
#   if(is.null(ws))
#     stop('"type" must be one of the folowing value: "ALL" or "FILTERED"')
#   
#   
#   if(from=="rTerminal"){
#     browseURL(paste0(web_services$host,ws,"?analysis=",analysis,"&sample=",sample))
#     return(T)
#   }
#   
#   if(from=="shiny"){
#     print(paste0("window.open('",web_services$host,ws,"?analysis=",analysis,"&sample=",sample,"','_blank')"))
#     shinyjs::runjs(paste0("window.open('",web_services$host,ws,"?analysis=",analysis,"&sample=",sample,"','_blank')"))
#     return(T)
#   }
#   
#   stop('"from" must be one of the folowing value: "shiny" or "rTerminal"')
#   return(T)
# }


getJobStatus=function(job_id){

    return(fromJSON(paste0(web_services$host,web_services$getJobStatus,"?job_id=",job_id)))
  
}

killJob = function(job_id) {
  try(fromJSON(paste0(web_services$host,web_services$killJob,"?job_id=",job_id)),silent = T)
  return(T)
  
}

removeAnalysis_FS=function(analysis){
 
   return(fromJSON(paste0(web_services$host,web_services$removeAnalysis,"?analysis=",analysis)))
  
}


getDesign=function(fastqList,endswith="_001.fastq.gz"){
 
  ids1=fastqList[str_detect(fastqList,paste0("_R1",endswith))]
  ids2=fastqList[str_detect(fastqList,paste0("_R2",endswith))]
  ids=unique(c(unlist(strsplit(ids1,paste0("_R1",endswith))),
               unlist(strsplit(ids2,paste0("_R2",endswith)))
               )
             )
  
  res=data.frame(sample_id=ids,upstream_file=NA,downstream_file=NA)

  for (i in ids) {
    
    R1=ids1[str_detect(ids1,i)]
    R2=ids2[str_detect(ids2,i)]
    
    if(identical(R1,character(0)))
      R1=NA
    
    if(identical(R2,character(0)))
      R2=NA
    
    res$upstream_file[ids==i]=R1
    res$downstream_file[ids==i]=R2
    
  }
  
  return(res)
  
  }


getTimestamp=function(analysis){
  
  return(fromJSON(paste0(web_services$host,web_services$getTimestamp,"?analysis=",analysis))$Timestamp)
  
}

run_devise=function(run_name,design){
  
design$run_name=run_name
body = toJSON(design)

res <- POST(paste0(web_services$host,web_services$run_devise_pipeline)
            , body = body
            ,encode = "json"
            , content_type("application/json"))

return(unlist(fromJSON(content(res,type = "text",encoding = "UTF-8"),simplifyDataFrame = F)))

}

