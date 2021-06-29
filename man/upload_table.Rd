\name{upload_table}
\alias{upload_table}
\title{upload_table}
\usage{
  upload_table(name,dbname,dbuser,dbpw,dbhost,port,append='Y')
  upload_table(name,dbname,dbuser,dbpw,dbhost,port)

}
\description{
  MySQL DB 서버에 선택한 데이터셋을 업로드 하는 함수입니다.
}

\arguments{
  \item{name}{DB 서버에 데이터셋을 업로드할 때 이름으로 사용합니다.}
  \item{dbanme}{dbname}
  \item{dbuser}{dbuser}
  \item{dbpw}{dbpw}
  \item{dbhost}{dbhost}
  \item{port}{port}
  \item{append}{DB에 같은 이름의 테이블이 이미 존재할 경우 이어 업로드를 할 지, 새로운 데이터로 덮어 씌울지를 결정하는 선택 파라미터입니다. 'Y'는 기존 데이터에 이어서 업로드를 하며, 'N'은 새로운 데이터로 덮어 씌웁니다. 기본 값은 'Y'입니다.}
}

\details{
  구글 트렌드에서 정상적으로 트렌드 정보 수집이 불가능한 경우는 공란으로 표시됩니다.\n
  파일은 xlsx 파일로 저장됩니다.
}
