


Cheeck_SampleSheet_validator=function(samples,DB=NULL){
  check_index=cheecking_index(DB,samples$index,samples$I7_Index_ID,samples$index_source)
  
  is.dup_Sample_ID=duplicated(samples$Sample_ID,fromLast =T ) | duplicated(samples$Sample_ID,fromLast =F )
  is.dup_Index=check_index$duplicated_index
  is.warning_Index=check_index$warnings_mismatch
  is.exist_Index=check_index$exist_index
  
  is.barcode=is.DNAseq(samples$index)
  
  bilan=rep("OK",nrow(samples))
  reports=c()
  Allows_mismatch=T
  for (i in 1:nrow(samples)) {
    report=""
    
    
    
    if(is.warning_Index[i]){
      
      bilan[i]="warning"
      report=paste0(report,check_index$report[i],"; ")
      Allows_mismatch=F
      
    }
    
    if(!is.exist_Index[i]){
      
      indice=which(samples$index==samples$index[i])
      if(samples$index_source[i]!="UNKNOWN"){
        bilan[i]="not ok" 
        report=paste0(report,"[index not in the database or does not match the indicated I7_Index_ID & index_source]; ")
        
      }else{
        bilan[i]="warning"
        report=paste0(report,"[index not in the database ]; ")
        
        
        }
    }
    
    
    if(is.dup_Sample_ID[i]){
      
      bilan[i]="not ok"
      indice=which(samples$Sample_ID==samples$Sample_ID[i])
      indice=indice[-which(indice==i)]
      report=paste0(report,"[Duplicated Sample_ID  with line",paste_seq(indice,sep = ","),"; ")
      
    }
    
    
    if(is.dup_Index[i]){
      
      bilan[i]="not ok"
      indice=which(samples$index==samples$index[i])
      report=paste0(report,check_index$report[i],"; ")
      
    }
    
    
    if(!is.barcode[i] || is.null(samples$index[i]) || "" %in% samples$index[i] || is.na(samples$index[i]) ){
      bilan[i]="not ok"
      report=paste0(report,"[Format of Index is incorrect];")
      
    }
    
    if(is.null(samples$Sample_ID[i]) || "" %in% samples$Sample_ID[i] || is.na(samples$Sample_ID[i]) || !is.alpha_numeric(samples$Sample_ID[i])){
      bilan[i]="not ok"
      report=paste0(report,"[Format of Sample_ID is incorrect];")
      print(report)
    }
    
    if(is.null(samples$Sample_Project[i]) || "" %in% samples$Sample_Project[i] || is.na(samples$Sample_Project[i]) || !is.alpha_numeric(samples$Sample_Project[i])  ){
      
      bilan[i]="not ok"
      report=paste0(report,"[Format of Sample_Project is incorrect];")
      
    }
    
    
    
    
    if(report==""){
      
      report="OK"
      
    }
    
    reports=c(reports,report)
    
    
  }
  
  return(list(status=bilan,report=reports,Allows_mismatch=Allows_mismatch))
  
  
}


update_NA_Line_on_samplesheet_validator=function(df){
  
  df$Sample_ID[is.na(df$Sample_ID)]="new_sample"
  df$index[is.na(df$index)]="new_index"
  df$Sample_Project[is.na(df$Sample_Project)]="new_project"
  
  return(df)
}

reset_hot_table_samplesheet_validator=function(a,DB,auto_getIndexFromNum=F,separate_lane_mode=F){
  df=convert_rhandsontableObject_to_data.frame(a)
  
  if(!is.null(df$Lane))
    df=df[order(df$Lane),]
  
  
  
  if(nrow(df)<2)
    return(df)

if(auto_getIndexFromNum){
  indexs=getIndex_from_Index_id(DB,df$index ,df$I7_Index_ID, df$index_source )
  df$index=indexs$index_value
  df$I7_Index_ID=indexs$index_num
  
}
  
  if(!separate_lane_mode){
    print(df)
    checking_samples=Cheeck_SampleSheet_validator(df,DB)
    
  }else{
    
    if( all(df$Lane %in% c(1,2,"1","2")) ){
      
      checking_samples=Cheeck_SampleSheet_validator(df[df$Lane==unique(df$Lane)[1],],DB)
      for (i in unique(df$Lane)[-1]) {
        ch=Cheeck_SampleSheet_validator(df[df$Lane==i,],DB)
        checking_samples$status=c(checking_samples$status,ch$status)
        checking_samples$report=c(checking_samples$report,ch$report)
        checking_samples$Allows_mismatch=checking_samples$Allows_mismatch & ch$Allows_mismatch
        
      }
    }else{
      
      return("erreur")
    }
    
  }
  
  
  df$check=checking_samples$status
  df$report=checking_samples$report
print(df)
  return(data.frame(df))
}






