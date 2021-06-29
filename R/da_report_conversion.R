da_report_conversion <- function(name,start_date,end_date)
{
  name <- deparse(substitute(name))
  file_list <- normalizePath(enc2native(choose.files()))
  
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  raw <- foreach(i=seq_along(file_list), .combine=rbind, .packages=c('data.table')) %dopar%
    {
      data.table(Sitecode=fifelse(fread(cmd=paste0('findstr /r .Report.suite ','"',file_list[[i]],'"'),sep="-",header=F)$V2=='MST Global',
                                  fread(cmd=paste0('findstr /v /b # ','"',file_list[[i]],'"'),header=F,encoding='UTF-8')$V3[[1]],'US'),
                 fread(cmd=paste0('findstr /v /b # ','"',file_list[[i]],'"'),header=F,encoding='UTF-8'))
    }
  stopCluster(cl)
  raw <- distinct(raw[V1=='Natural Search' & str_detect(V2,'[0-9]-[0-9]'),.(dimension=V1,Date=V2,entries=V3,order=V4,revenue=V5,country=Sitecode)][order(country,Date,-(entries))],country,Date,.keep_all=TRUE)
  if(missing(start_date)) <- min(raw[['Date']])
  if(missing(end_date)) <- max(raw[['Date']])
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  raw <- raw[between(Date,start_date,end_date)]
  directory <- choose.dir('저장할 폴더를 선택하세요')
  fwrite(raw,enc2native(paste0(directory,'/',name,'.csv')),row.names=F)
}
