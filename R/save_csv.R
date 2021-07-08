save_csv <- function(...,filename)
{
  if(!missing(filename)) filename <- deparse(substitute(filename))
  if(missing(filename)) filename <- gsub(' |:','-',paste0(Sys.time()))
  data <- data.table(...)
  cn <- which(map_lgl(names(data),function(x) {is.character(data[,get(x)])}))
  dn <- which(map_lgl(names(data),function(x) {is.Date(data[,get(x)])}))
  data[,(names(data)[cn]):=lapply(.SD,enc2utf8),.SDcol=names(data)[cn]]
  if(length(dn)>=1) {
    data[,(names(data)[dn]):=lapply(.SD,as.Date),.SDcol=names(data)[dn]]
  } 
  dir <- choose.dir(caption='저장할 위치를 선택하세요')
  fwrite(data,enc2native(paste0(dir,'/',filename,'.csv')),bom=TRUE)
}
