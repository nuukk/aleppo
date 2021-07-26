\name{trends_traffic}
\alias{trends_traffic}
\title{trends_traffic}
\usage{
  trends_traffic(name=myfile,dbname=mydb)
  trends_traffic(name=myfile)
  trends_traffic(myfile,mydb)
  trends_traffic(myfile)
}
\description{
  국가/월/제품군/키워드에 따라 구글 트렌드 추이와 트래픽 자료 정리를 도와주는 함수입니다.
}

\arguments{
  \item{name}{전처리된 파일을 로컬 컴퓨터에 저장할 때 파일명으로 사용되며, 반드시 입력해야 하는 필수 파라미터입니다. 지정한 경로에 같은 이름의 파일이 이미 존재할 경우, 이름 뒤에 시스템 현재 시각(yyyy-mm-dd-hh-mm-ss)이 추가됩니다.}
  \item{dbname}{전처리된 파일을 DB에 업로드할 때 테이블명으로 사용되며, 입력이 선택사항인 선택 파라미터입니다. 별도로 dbname을 지정하지 않을 경우 name 파라미터에 지정한 값을 사용합니다.}
}

\details{
  해당 국가/월/제품군에 트래픽 정보가 없는 경우에는 공란으로 출력됩니다.
  
  파일은 csv로 저장됩니다.
}
