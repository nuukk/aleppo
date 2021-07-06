gdc_monthly_gsc <- function(name,start_date,end_date)
{
  name <- deparse(substitute(name))
  file_list <- enc2native(choose.files(caption='RAW 자료를 선택하세요'))
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  raw <- foreach(i=seq_along(file_list), .combine=rbind, .packages=c('data.table','readxl')) %dopar%
    {
      data.table(read_excel(file_list[[i]],col_types=c('date','text','numeric','numeric','numeric','numeric','text')))
    }
  stopCluster(cl)
  colnames(raw) <- paste0(toupper(substr(colnames(raw),1,1)),substr(colnames(raw),2,nchar(colnames(raw))))
  if(missing(start_date)) start_date <- min(raw$Date)
  if(missing(end_date)) end_date <- max(raw$Date)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if(start_date>end_date) {
    temp_date <- end_date
    end_date <- start_date
    start_date <- temp_date
  }
  raw <- raw[between(Date,start_date,end_date)]
  table <- suppressMessages(data.table(read_excel(enc2native(choose.files('기준 파일을 선택하세요')),sheet='URL List')))
  table <- table[-seq_len(which(table[,1]=='Country')),.(Country=`GDC URL List`,Model=`...4`,Url=`...5`)]
  setkey(table,Url)
  setkey(raw,Page)
  raw <- select(filter_all(table[raw],all_vars(!is.na(.))),Model,Country,Page=Url,Date,Clicks,Impressions,Ctr,Position,Url=i.Url)
  dir <- choose.dir(caption='파일을 저장할 경로를 지정하세요')
  write.csv(raw,paste0(dir,"/",name,'.csv'),row.names=F)
}
