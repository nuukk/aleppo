save_csv <- function(...,filename="")
{
  if(missing(filename)) filename <- gsub(' |:','-',paste0(Sys.time()))
  data <- data.table(...)
  cn <- which(map_lgl(names(data),function(x) {is.character(data[,get(x)])}))
  dn <- which(map_lgl(names(data),function(x) {is.Date(data[,get(x)])}))
  data[,(names(data)[cn]):=lapply(.SD,enc2utf8),.SDcol=names(data)[cn]]
  if(length(dn)>=1) {
    data[,(names(data)[dn]):=lapply(.SD,as.Date),.SDcol=names(data)[dn]]
  } 
  dir <- choose.dir(default=getwd(),caption='저장할 위치를 선택하세요')
  file_dir_name <- enc2native(paste0(dir,'/',filename,'.csv'))
  if(file.exists(file_dir_name)) {
    message('같은 이름의 파일이 존재해요. 파일 이름에 생성 시간이 추가됩니다!')
    file_dir_name <- enc2native(paste0(dir,'/',filename,'_',gsub(' |:','-',paste0(Sys.time())),'.csv'))
  }
  fwrite(data,file_dir_name,bom=TRUE)
}
