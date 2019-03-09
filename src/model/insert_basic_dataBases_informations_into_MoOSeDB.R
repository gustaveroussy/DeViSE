library(RMySQL)
library(DBI)
user_db="moose_prod"
password_db="65841829b6a38f4cc7ac"
dbname="MoOSe"
host_db='31.10.13.42'
port=3306
#sock_db="/var/run/mysqld/mysqld.sock"
################  ------------------------------------------------------------------------------------------------------

###############  ----------------------------------------------------------------------------------

MoOSe_db <- dbConnect(RMySQL::MySQL() , dbname=dbname,user=user_db,password=password_db,host=host_db,port=port)  
DB=MoOSe_db

res=dbGetQuery(DB, paste0("SELECT *  FROM sample s
                            INNER JOIN application_type a  ON a.application_type_id=s.application_type_id
                            INNER JOIN species i  ON i.species_id=s.species_id
                            INNER JOIN project p  ON p.project_id=s.project_id
                            WHERE s.run_id=",141 )
)[,sample_sheet_attributes]

sample_sheet_attributes=c("sample_id","sample_name","sample_bar_code_source","sample_barcode_id","sample_bar_code","project_name","application_type_name","species_name","sample_description")
sample_sheet_attributes_new_names=c("InternID","Sample_Id","index_source","I7_Index_ID","Index","Sample_Project","Application","Species","Description")



################  ----------------------------------------------------------------

dbGetQuery(MoOSe_db, "delete from qc
           where qc_id < 1000000 ")
dbGetQuery(MoOSe_db, "delete from project
           where project_id < 5000 ")

dbGetQuery(MoOSe_db, "delete from monitoring

           where monitoring_id < 5000 ")
dbGetQuery(MoOSe_db, "delete from run
           where platform_id < 10 ")

dbGetQuery(MoOSe_db, "delete from project
           where project_id < 5000 ")

dbGetQuery(MoOSe_db, "delete from sample
           where sample_id < 100000 ")


a=data.frame(monitoring_statut="complet",monitoring_date=Sys.time(),run_id=311)
dbGetQuery(MoOSe_db,"ALTER TABLE sample
DROP FOREIGN KEY 'conversation_tags_ibfk_1',
ADD CONSTRAINT `fk_conversation_tags_tags` FOREIGN KEY (`tag_id`) REFERENCES `tags` (`id`);

")


runs=dbGetQuery(MoOSe_db,
                "SELECT m.run_id,r.run_name, m.monitoring_statut,p.platform_path , MAX(m.monitoring_date) as date FROM monitoring m
                      INNER JOIN run r  ON r.run_id=m.run_id
                      INNER JOIN platform p  ON p.platform_id=r.platform_id
                      WHERE m.monitoring_statut != 'moved'
                      GROUP BY m.run_id, m.monitoring_statut")

runs=runs[order(runs$date,decreasing = T),]
runs=runs[!duplicated(runs$run_id),]







run_id=dbGetQuery(DB,
                  paste0("SELECT run_id FROM run
                     WHERE run_name IN (",paste_seq(c("run_nova_3","run_nova_4","run_nova_4"),sep=", "),");"))



df=dbGetQuery(MoOSe_db, paste0("  SELECT *  FROM sample s
                                        INNER JOIN project p  ON s.project_id=p.project_id
                                        INNER JOIN application_type a  ON a.application_type_id=s.application_type_id
                                        WHERE s.run_id='1' "))



foreach(i=1:3) %do%
  sqrt(i)


runs=dbGetQuery(MoOSe_db, paste0("SELECT *  FROM run r
                                        INNER JOIN monitoring m  ON r.run_id=m.run_id
                                        INNER JOIN platform p  ON p.platform_id=r.platform_id
                                        WHERE r.run_isvalid=TRUE AND r.platform_id IN (",paste_seq(c(1,2),sep=", "),")")
)[,c(1:5,8,9,10,12,13)]
runs=runs[order(runs$monitoring_date,decreasing = T),]
run=runs[!duplicated(runs$run_id),]




dbGetQuery(DB,
           paste0("   SELECT *  FROM sample s
                        INNER JOIN project p  ON s.project_id=p.project_id
                         INNER JOIN run r  ON s.run_id=r.run_id
                           INNER JOIN application_type a  ON a.application_type_id=s.application_type_id
                              WHERE r.run_isvalid=TRUE AND  r.run_id IN (",paste_seq(run_ids,sep=", "),") 
                                  AND  p.project_name IN (",paste_seq(project_ids,sep=", "),") ")
)

run_ids=c("171130_A00255_0012_AH5NJGDMXX", "171205_A00255_0013_BH5NG3DMXX")
project_ids=c("P28_FAAN (SAFIR02")



samples=dbGetQuery(DB, paste0("SELECT *  FROM sample" )   )




SampleSheetIsValid(DB,682)


merge(t(data.frame("WES","WES","WGS","RNAseq","WES")),run_id, by.x=1,by.y ="application_type_name")


head(dbReadTable(MoOSe_db,"run"))



update_index_list=function(DB){

indexNextflex=read.csv("data/appData/bar_code_list_Nextflex.csv")
dbWriteTable(DB, name = "index",
                 value = index,
                 append = T, 
                 row.names = FALSE)

index=dbGetQuery(DB,"SELECT * FROM MoOSe.`index`")
write.table(index[,2],"data/appData/bar_code_list",row.names =  F)

}


indexNextflex$index_source=rep("NextFlex",96)


convert_rhandsontableObject_to_data.frame=function(rhandontable_object){
  
a=rhandontable_object
res.df=c()
for(i in 1:length(a$data)){
  
  a$data[[i]][sapply(a$data[[i]], is.null)]=""
  
  res.df=rbind(res.df,as.character(unlist(a$data[i])))
}
colnames(res.df)=names(a$params[3]$rColClasses)
return(res.df)

}


is.nulls=function(x){
  sapply(x, is.null)
}





































dbs=c("Genetics_cap" ,"Genetics_panelgenes",  "Genetics_v_6", "LRT"
      ,"PathMol_hemato","PathMol_tumeur_solide_v_2")

library(RMySQL)
library(DBI)
user_db="root"
password_db="bioinfo"
#dbname="Demande_Examen"
host_db='0.0.0.0'
port=3306
df=list()
for(dbname in dbs){
  DB=DBI::dbConnect(RMySQL::MySQL() , dbname=dbname,user=user_db,password=password_db,host=host_db,port=port)  
 # df[[dbname]]=colnames(dbReadTable(DB,"AllVariants"))
}
DBI:
  
  res=c()
for (i in colnames(df)) {
  
  res=rbind(res,c(i,paste(unique(df[[i]])[1:3],collapse=" || ")))
  
}

setwd("~/Documents/variant_database/Grio-dx_data/")

 l=c("RUN.csv",
  "library.csv",
"amplicons_DP_Normalize.csv",
  "amplicons_depth_tech.csv",
  "amplicons_depth.csv",
  "RUN.csv")

upsets=function(j){
 
  for(j in l){
 
a=list.files(pattern = j)
k=list()

for (i in a) {
  k[[i]]=colnames(read.csv(i))
}

lst=unique(unlist(k))
df=c()
for (i in 1:length(k)) {
  df=cbind(df, lst %in% k[[i]])
}
colnames(df)=unlist(strsplit(a,j))
rownames(df)=lst
dfs[[j]]=df
# set=fromList(k)
# # jpeg(paste0(unlist(strsplit(j,".csv")),".jpeg"),width = 800,height = 500)
# upset(set,sets.bar.color = "#56B4E9",nsets = 6,
#       text.scale = 2, group.by = "degree", order.by="freq")
# 
# #dev.off()


}

sapply(l,upsets)
