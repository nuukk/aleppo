da_report_conversion_excel <- function(name,start_date,end_date)
{
  name <- deparse(substitute(name))
  file_list <- normalizePath(enc2native(choose.files()))
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  raw <- foreach(i=seq_along(file_list), .combine=rbind, .packages=c('readxl')) %dopar%
    {
      read_excel(file_list[[i]],col_types=c('text','text','date','text','text','text','text','numeric'))
    }
  stopCluster(cl)
  if(missing(start_date)) { start_date <- min(raw$week)}
  if(missing(end_date)) { end_date <- max(raw$week)}
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if(start_date>end_date) {
    temp_date <- end_date
    end_date <- start_date
    start_date <- temp_date
  }
  raw <- filter(raw,gbm_project=='DA' & between(week,start_date,end_date))
  write.csv(raw,paste0(choose.dir(caption='저장할 경로를 지정하세요'),"/",name,'.csv'))
}
