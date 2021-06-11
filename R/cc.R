cc <- function(...,type=c('Y','N'))
{
  if(type=='Y') {
    write.table(...,file='clipboard',sep="\t",row.names=F,col.names=T)
  } else if(type=='N') {
    write.table(...,file='clipboard',sep="\t",row.names=F,col.names=F)
  }
}
