upload_table <- function(name,dbname,dbuser,dbpw,dbhost,port,append=c('Y','N'))
{
  x <- get(ls(.GlobalEnv)[menu(ls(.GlobalEnv),graphics=TRUE,title="업로드할 자료를 선택하세요")])
  name <- deparse(substitute(name))
  if(missing(append)) append <- 'Y'
  a <- dbConnect(dbDriver("MySQL"),
                 dbname=dbname,
                 user=dbuser,
                 password=dbpw,
                 host=dbhost,
                 port=port)
  dbSendQuery(a, 'set character set "euckr"')
  if(append=='Y') {
    dbWriteTable(a,name,x,row.names=F,append=T)
  } else {
    dbWriteTable(a,name,x,row.names=F,append=F)
  }
  dbDisconnect(a)
}
