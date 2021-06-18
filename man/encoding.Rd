\name{encoding}
\alias{encoding}
\title{encoding}
\usage{
  encoding(...)
}
\description{
  문자열을 UTF-8 인코딩으로 변환해주는 함수입니다.
}

\arguments{
  \item{text}{문자열입니다.}
}

\examples{
  encoding('<U+0E23><U+0E32><U+0E04><U+0E32><U+0E40><U+0E04><U+0E23><U+0E37><U+0E48><U+0E2D><U+0E07><U+0E0B><U+0E31><U+0E01><U+0E1C><U+0E49><U+0E32> samsung')
  #결과: "ราคาเครื่องซักผ้า samsung"
}
