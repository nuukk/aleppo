\name{gdc_monthly_sid}
\alias{gdc_monthly_sid}
\title{gdc_monthly_sid}
\usage{
  gdc_monthly_sid(name=myfile,start_date='2021-01-01',end_date='2021-02-28')
  gdc_monthly_sid(name=myfile,start_date='2021-02-28',end_date='2021-01-01')
  gdc_monthly_sid(name=myfile)
  gdc_monthly_sid(myfile,'2021-01-01','2021-02-28')
  gdc_monthly_sid(myfile,start_date='2021-01-01')
  gdc_monthly_sid(myfile,end_date='2021-02-28')
}
\description{
  GDC 월간보고에 맞춰 전처리를 도와주는 함수입니다.
}

\arguments{
  \item{name}{전처리된 파일을 로컬 컴퓨터에 저장할 때 파일명으로 사용되며, 반드시 입력해야 하는 필수 파라미터입니다.}
  \item{start_date}{데이터 시작일로, 선택 파라미터입니다. 입력하지 않으면 데이터에서 가장 빠른 날짜가 자동으로 매핑됩니다. 시작일과 종료일의 순서가 바뀌어도 무방합니다.}
   \item{end_date}{데이터 종료일로, 선택 파라미터입니다. 입력하지 않으면 데이터에서 가장 최신 날짜가 자동으로 매핑됩니다. 시작일과 종료일의 순서가 바뀌어도 무방합니다.}
}

\details{
  파일은 CSV 파일로 저장됩니다.
}
