\name{trends_only}
\alias{trends_only}
\title{trends_only}
\usage{
  trends_only(name=mydata, fyear=2018)
  trends_only(mydata,2018)
  trends_only(mydata)
}
\description{
  국가/기간/제품군/키워드에 따라 구글 트렌드 추이 정리를 도와주는 함수입니다.
}

\arguments{
  \item{name}{전처리된 파일을 로컬 컴퓨터에 저장할 때 파일명으로 사용되며, 반드시 입력해야 하는 필수 파라미터입니다.}
  \item{fyear}{데이터의 기준 연도입니다. 입력하지 않으면 기본값인 2015가 적용되며, 2015년 1월 이후 데이터만 남겨 놓습니다.}
}

\details{
  구글 트렌드에서 정상적으로 트렌드 정보 수집이 불가능한 경우는 공란으로 표시됩니다.
  
  파일은 xlsx 파일로 저장됩니다.
}
