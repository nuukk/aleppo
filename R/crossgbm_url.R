crossgbm_url <- function(...,name,year,month)
{
  name <- deparse(substitute(name))
  year1 <- year-1
  year2 <- year
  raw  <- data.table(...)
  raw <- data[gbm=='CrossGBM' & month(Month)<=month & between(year(Month),year1,year2)]
  raw <- select(raw,-directory,-gbm)
  
  temp <- dcast(melt(data.table(summarize(group_by(raw,Sitecode,gbm_detail,year=year(Month),URL),across(where(is.numeric),sum))),
                     id=c('Sitecode','gbm_detail','year','URL')),Sitecode+gbm_detail+URL~variable+year,fun=sum)
  
  temp2sub <- select(mutate(temp2,
                     Entry_YoY:=get(paste0('Entry_',year2))-get(paste0('Entry_',year1)),
                     Order_YoY:=get(paste0('Order_',year2))-get(paste0('Order_',year1)),
                     Entry_Ratio=Entry_YoY/get(paste0('Entry_',year1))*100,
                     Order_Ratio=Order_YoY/get(paste0('Order_',year1))*100),
                     gbm_detail,Sitecode,URL,starts_with('Entry'),starts_with('Order'))
  
  temp2share <- select(mutate(group_by(temp2sub,Sitecode),
                       Entry_Share=prop.table(Entry_YoY)*100,
                       Order_Share=prop.table(Order_YoY)*100),gbm_detail,Sitecode,URL,starts_with('Entry'),starts_with('Order'))
  
  temp2total <- select(mutate(summarize(group_by(temp2,Sitecode),across(where(is.numeric),sum)),
                       gbm_detail='#TOTAL',URL='#TOTAL',
                       Entry_YoY:=get(paste0('Entry_',year2))-get(paste0('Entry_',year1)),
                       Order_YoY:=get(paste0('Order_',year2))-get(paste0('Order_',year1)),
                       Entry_Ratio=Entry_YoY/get(paste0('Entry_',year1))*100,
                       Order_Ratio=Order_YoY/get(paste0('Order_',year1))*100),
                  gbm_detail,Sitecode,URL,starts_with('Entry'),starts_with('Order'))

  raw <- bind_rows(temp2total,temp2share)
  raw <- arrange(raw,Sitecode,gbm_detail,URL,desc(get(paste0('Entry_',year2))))
  directory <- choose.dir()
  fwrite(raw,paste0(directory,'/',name,'.csv),row.names=F)
}
