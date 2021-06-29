trends_only <- function(name,fyear)
{
  name <- deparse(substitute(name))
  if(missing(fyear)) fyear <- 2015
  fyear <- as.numeric(fyear)
  
  keyword_info <- read_excel(normalizePath(enc2native(choose.files(caption='키워드 기준 파일을 선택하세요'))),range='B4:E1000')
  keyword_info <- data.table(keyword_info[complete.cases(keyword_info),])
  keyword_info$국가[keyword_info$국가=='UK'] <- 'GB'
  
  gen0 <- data.table(reduce(map2(keyword_info$General,keyword_info$국가,googletrendscrawling),plyr::rbind.fill))
  brn0 <- data.table(reduce(map2(keyword_info$Brand,keyword_info$국가,googletrendscrawling),plyr::rbind.fill))
  
  keyword_info$국가[keyword_info$국가=='GB'] <- 'UK'
  gen0$geo[gen0$geo=='GB'] <- 'UK'
  brn0$geo[brn0$geo=='GB'] <- 'UK'
  
  gen0[,product_type:=as.character(map2(gen0$geo,gen0$keyword,function(x,y) {keyword_info$제품군[keyword_info$국가==x & keyword_info$General==y]}))]
  brn0[,product_type:=as.character(map2(brn0$geo,brn0$keyword,function(x,y) {keyword_info$제품군[keyword_info$국가==x & keyword_info$Brand==y]}))]
  gen0 <- select(gen0,date,country=geo,product_type,trends_G=keyword,G_trends=hits,product_type)
  brn0 <- select(brn0,date,country=geo,product_type,kw_B=keyword,B_trends=hits,product_type)
  if(askYesNo('추가 General Raw 파일을 추가하실래요?')=='TRUE') {
    sec_gen_filelist <- normalizePath(enc2native(choose.files(caption='추가 General RAW를 선택하세요')))
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    sec_gen <- foreach(i=seq_along(sec_gen_filelist),.combine=rbind, .packages=c('data.table','stringr')) %dopar%
      {
        data.table(fread(sec_gen_filelist[1],skip=3,header=FALSE),
                   kw_G=str_split_fixed(fread(sec_gen_filelist[1],skip=1,nrow=1,encoding='UTF-8',header=FALSE)$V2,":",2)[[1]],
                   country=str_split_fixed(fread(sec_gen_filelist[1],skip=1,nrow=1,encoding='UTF-8',header=FALSE)$V2,":",2)[[2]])
      }
    stopCluster(cl)
    sec_gen[,country:=code[trimws(gsub("\\(|\\)","",country),'left')]]
    sec_gen <- sec_gen[,.(date=as.Date(paste0(V1,"-01")),country,kw_G,G_trends=as.numeric(V2))]
    sec_gen[,product_type:=as.character(map2(sec_gen$country,sec_gen$kw_G,function(x,y) {keyword_info$제품군[keyword_info$국가==x & keyword_info$General==y]}))]
    gen0 <- bind_rows(gen0,sec_gen)
  }
  gen0 <- filter(gen0,year(date)>=fyear)
  if(askYesNo('추가 Brand Raw 파일을 추가하실래요?')=='TRUE') {
    sec_brn_filelist <- normalizePath(enc2native(choose.files(caption='추가 Brand RAW를 선택하세요')))
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    sec_brn <- foreach(i=seq_along(sec_brn_filelist),.combine=rbind, .packages=c('data.table','stringr')) %dopar%
      {
        data.table(fread(sec_brn_filelist[1],skip=3,header=FALSE),
                   kw_B=str_split_fixed(fread(sec_brn_filelist[1],skip=1,nrow=1,encoding='UTF-8',header=FALSE)$V2,":",2)[[1]],
                   country=str_split_fixed(fread(sec_brn_filelist[1],skip=1,nrow=1,encoding='UTF-8',header=FALSE)$V2,":",2)[[2]])
      }
    stopCluster(cl)
    sec_brn[,country:=code[trimws(gsub("\\(|\\)","",country),'left')]]
    sec_brn <- sec_brn[,.(date=as.Date(paste0(V1,"-01")),country,kw_B,B_trends=as.numeric(V2))]
    sec_brn[,product_type:=as.character(map2(sec_brn$country,sec_brn$kw_B,function(x,y) {keyword_info$제품군[keyword_info$국가==x & keyword_info$General==y]}))]
    brn0 <- bind_rows(brn0,sec_brn)
  }
  brn0 <- filter(brn0,year(date)>=fyear)
  setkey(gen0,date,country,product_type)
  setkey(brn0,date,country,product_type)
  trends <- full_join(gen0,brn0)
  suppressWarnings(trends[,`:=`(date=as.Date(date),country=toupper(country),product_type=toupper(product_type),trends_G=as.numeric(G_trends),trends_B=as.numeric(B_trends))])
  trends <- select(trends,date,country,product_type,trends_G,trends_B)
  write_xlsx(trends,paste0(enc2native(choose.dir("저장할 위치를 선택하세요")),'/',name,'.xlsx'))
  assign(name,trends,.GlobalEnv)
}
