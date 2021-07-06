gdc_monthly_sid <- function(name,start_date,end_date)
{
  name <- deparse(substitute(name))
  file_list <- enc2native(choose.files(caption='RAW 자료를 선택하세요'))
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  raw <- foreach(i=seq_along(file_list), .combine=rbind, .packages=c('data.table','readxl')) %dopar%
    {
      data.table(read_excel(file_list[[i]],col_types=c('text','text','date','text','numeric')))
    }
  stopCluster(cl)
  colnames(raw) <- c('type','Country','Date','Url','Entries')
  if(missing(start_date)) start_date <- min(raw$Date)
  if(missing(end_date)) end_date <- max(raw$Date)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if(start_date>end_date) {
    temp_date <- end_date
    end_date <- start_date
    start_date <- temp_date
  }
  if(askYesNo('REF 기준 자료인가요? (MKT 기준이라면 아니오를 선택)')==TRUE) {
    raw <- raw[type=='referrer_type' & between(Date,start_date,end_date)]
  } else {
    raw <- raw[type=='marketing_channel' & between(Date,start_date,end_date)]
  }
  table <- suppressMessages(data.table(read_excel(enc2native(choose.files('기준 파일을 선택하세요')),sheet='URL List')))
  table <- table[-seq_len(which(table[,1]=='Country')),.(Country=`GDC URL List`,Model=`...4`,Url=`...5`)]
  table[,Url2:=substr(Url,1,100)]
  setkey(table,Url2)
  setkey(raw,Url)
  raw <- select(filter_all(raw[table],all_vars(!is.na(.))),Country,Model,`Entry Url`=Url,Date,Entries)
  write.csv(raw,paste0(choose.dir(caption='저장할 경로를 지정하세요'),"/",name,'.csv'),row.names=F)
}
