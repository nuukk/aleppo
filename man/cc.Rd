\name{cc}
\alias{cc}
\title{Copy & Paste}
\usage{
  cc(data-set,type='Y')
  cc(data-set,'Y')
  cc(data-set)
}
\description{
  데이터셋 등을 클립보드에 복사해주는 함수입니다.
}

\arguments{
  \item{type}{변수명(열 이름)의 저장 여부를 선택하는 함수입니다. 'Y'이면 열 이름이 포함되어 저장되고 'N'이면 열 이름은 저장되지 않습니다.}
}
\details{
  클립보드 용량을 초과하는 경우 잘림 현상이 발생할 수 있습니다.
}
