drop_table <- function(name,id,pw,ip,port)
{
  a <- dbConnect(dbDriver("MySQL"),
                 dbname=name,
                 user=id,
                 password=pw,
                 host=ip,
                 port=port)
  dbSendQuery(a, 'set character set "euckr"')
  b0 <- dbListTables(a)[menu(dbListTables(a),graphics=T,title="삭제할 테이블을 고르세요")]
  if(askYesNo(msg=paste0(b0,"테이블을 정말 삭제하시겠습니까?"))==TRUE) {
    dbSendQuery(a,paste0("drop table ",b0,""))
    message(paste0(b0," 테이블을 삭제했습니다."))
  } else{
    stop("테이블 삭제 작업을 중단합니다")
  }
  dbDisconnect(a)
}
