#!/usr/bin/env Rscript


library("optparse")

option_list = list(
  
  make_option(c("-d", "--design"), type="character", default=NULL, 
              help="path to design file", metavar="character"),
  
  make_option(c("-c", "--config"), type="character", default=NULL, 
              help="path to config file ", metavar="character"),
  
  make_option(c("-f", "--from"), type="character", default="fastq", 
              help="from bam/fastq", metavar="character"),
  
  make_option(c("-o", "--out"), type="character", default=NULL, 
              help="path to output dir", metavar="character")
  
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$design)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input design).n", call.=FALSE)
}

if (is.null(opt$out)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input out).n", call.=FALSE)
}

if (is.null(opt$config)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input config).n", call.=FALSE)
}


configs=yaml::yaml.load_file(opt$config)
try(dir.create(opt$out),silent = T)
analyse_name=normalizePath(opt$out)
design=read.delim(opt$design)

  
 run_single_devise=function(R1=NULL,R2=NULL,bam=NULL,sample_name,analyse_name,configs){
   
   mode = configs$mode
 ##### Preparation des directories de travail  ----------------------------------------------------------------------------------------
  
  sample_output=paste0(analyse_name,"/",sample_name)
  system(paste0("rm -r ",sample_output),ignore.stdout = T,ignore.stderr = T,wait = T)
  dir.create(sample_output,recursive = T)
  log_dir=paste0(sample_output,"/logs")
  dir.create(log_dir,recursive = T)
  out_file=paste0(log_dir,"/",sample_name,"_Step1_STAR.out") 
  # file.create(out_file)
  log_file=paste0(log_dir,"/",sample_name,"_Step1_STAR.log") 
  # file.create(log_file)
  
  # system(paste("echo '",timestamp() ,"' > ",log_file),intern = T)
  # system(paste("echo '",timestamp() ,"' > ",out_file),intern = T)
  
##### ---------------------------------------------------------------------------------------------------------------------------------  

##### STAR ----------------------------------------------------------------------------------------------------------------------------

  if(is.null(bam)){
  
  star_cmd=paste0(configs$STAR," --runThreadN 1 --genomeDir  ",
             configs$genome_dir," --readFilesIn ",R1," ", R2," --outFileNamePrefix ", sample_output, 
             "/ --outSAMtype BAM SortedByCoordinate --outSAMattributes All --twopassMode Basic   --readFilesCommand gunzip -c > ",
             out_file," 2> ", log_file
  )
  
  bam=paste0(sample_output,"/Aligned.sortedByCoord.out.bam")
  
  }else{
    
    star_cmd=""
    
  }
  
#### Portcullis ------------------------------------------------------------------------------------------------------------------------
  
  out_file=paste0(log_dir,"/",sample_name,"_Step2_PORTCULLIS.out") 
  # file.create(out_file)
  log_file=paste0(log_dir,"/",sample_name,"_Step2_PORTCULLIS.log") 
  # file.create(log_file)
  
  # system(paste("echo '",timestamp() ,"' > ",log_file),intern = T)
  # system(paste("echo '",timestamp() ,"' > ",out_file),intern = T)
  
  portcullis_cmd=paste0(configs$portcullis," full ",configs$genomePath," ",bam," ",configs$portcullis_param," -o ",sample_output," > ",
                        out_file," 2> ", log_file)
  
####--------------------------------------------------------------------------------------------------------------------------------------


#### Regtools ----------------------------------------------------------------------------------------------------------------------------

  out_file=paste0(log_dir,"/",sample_name,"_Step3_REGTOOLS_all_junc.out") 
  # file.create(out_file)
  log_file=paste0(log_dir,"/",sample_name,"_Step3_REGTOOLS_all_junc.log") 
  # file.create(log_file)
  
  # system(paste("echo '",timestamp() ,"' > ",log_file),intern = T)
  # system(paste("echo '",timestamp() ,"' > ",out_file),intern = T)
  
  regtools_cmd1=paste0(configs$regtools,
                       ' junctions annotate ',
                       sample_output,"/2-junc/portcullis_all.junctions.bed ",
                       configs$genomePath," ",
                       configs$GTF_path,
                       " -o ", sample_output,"/portcullis_all.junctions_annotated.tab",
                       " > ", out_file," 2> ", log_file)
  
  
  out_file=paste0(log_dir,"/",sample_name,"_Step4_REGTOOLS_filtered_junc.out") 
  # file.create(out_file)
  log_file=paste0(log_dir,"/",sample_name,"_Step4_REGTOOLS_filtered_junc.log") 
  # file.create(log_file)
  
  # system(paste("echo '",timestamp() ,"' > ",log_file),intern = T)
  # system(paste("echo '",timestamp() ,"' > ",out_file),intern = T)
  
  
  regtools_cmd2=paste0(configs$regtools,
             ' junctions annotate ',
             sample_output,"/3-filt/portcullis_filtered.pass.junctions.bed ",
             configs$genomePath," ",
             configs$GTF_path,
             " -o ", sample_output,"/portcullis_filtered.pass_junctions_annotated.tab",
             " > ", out_file," 2> ", log_file)
  
#########################################################################################################################################
  

  job_id=NULL
  
  if(mode=="cluster"){
    
    all_cmds=c(paste0(configs$samtools, " && ",star_cmd, " && ",portcullis_cmd),regtools_cmd1,regtools_cmd2)
    write(all_cmds, paste0(sample_output,"/",sample_name,"-cmds.sh"),sep = "\n")
    cluster_log=paste0(log_dir,"/cluster_log")
    dir.create(cluster_log,recursive = T)
    cmd_qsub=paste0(configs$qsub," -e ",cluster_log,"/error.txt -o ",cluster_log,"/output.txt ",configs$qsub_param," ",sample_output,"/",sample_name,"-cmds.sh")
    job_id=system(cmd_qsub,intern = T)
    print(job_id)
    
  }else{
    
    all_cmds=c(paste0(star_cmd, " && " ,portcullis_cmd),regtools_cmd1,regtools_cmd2)
    write(all_cmds, paste0(sample_output,"/",sample_name,"-cmds.sh"),sep = "\n")
    
    cmd_bash=paste0("bash ",sample_output,"/",sample_name,"-cmds.sh")
    print(paste0("###--------- Processing  of ",sample_name," in progress ..."))
    # system(cmd_bash,intern = T)
    print(paste0("###-----------",sample_name,"  done. "))
    
  }

 return(list(cmd=all_cmds,job_id=job_id))
  
 }
 
 
 
################ submit -----------------------------------------------------------------------------------------------------------------   
 
 jobs_ids=c()
 list_jobs=data.frame()
  for(i in 1:nrow(design) ){
    
    if(opt$from!="bam"){
    
     R1=design$Upstream_file[i]
     R2=design$Downstream_file[i]
     sample_name=design$Sample_id[i]
    
     if(is.null(R1)||is.null(R2)||is.null(sample_name))
       stop("Format of your  design file is incorrect.")
     
     run=run_single_devise(R1 = R1,
                           R2 = R2,
                           sample_name = sample_name,
                           analyse_name = analyse_name,
                           configs = configs )
    }else{
      
      bam=design$Bam[i]
      sample_name=design$Sample_id[i]
      
      if(is.null(bam)|| is.null(sample_name))
        stop("Format of your design file is incorrect.")
      
      run=run_single_devise(bam = bam,
                            sample_name = sample_name,
                            analyse_name = analyse_name,
                            configs = configs )
      
    }
    
     if(configs$mode == "cluster"){

          jobs_ids=c(jobs_ids,run$job_id)
          list_jobs=rbind(list_jobs,data.frame(s=sample_name,info=run$job_id))
          
     }else{

          list_jobs=rbind(list_jobs,data.frame(s=sample_name,info=" info not available "))
       
     }
     
  }
  
 
 if(configs$mode == "cluster"){
        
        write(paste0("qdel ", jobs_ids),paste0(analyse_name,"/kill_alljobs.sh"),sep="\n")   
   
 }
 
 write.table(list_jobs,paste0(analyse_name,"/submited_jobs.tsv"),sep="\t",col.names = F,row.names = F,quote = F)
 write.table(design,paste0(analyse_name,"/design.tsv"),sep="\t",row.names = F,quote = F)
 write(as.character(Sys.time()),paste0(analyse_name,"/timestamp_analysis.txt"),sep="/n")
###----------------------------------------------------------------------------------------------------------------------------------------



 
 
 
 
 
 
 
 
 
 
 
 
 
 
  
  
  
  
  
  
  
  


