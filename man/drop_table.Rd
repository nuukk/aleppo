\name{drop_table}
\alias{drop_table}
\title{drop_table}
\usage{
  drop_table(name='abc',id='def',pw='zzz',ip='123.456.789.01',port=8080)
  drop_table('abc','def','zzz','123.456.789.01',8080)
}
\description{
  SQL 서버에서 특정 테이블의 삭제를 도와주는 함수입니다.
}

\arguments{
  \item{name}{dbname}
  \item{id}{dbuser name}
  \item{pw}{dbuser password}
  \item{ip}{host}
  \item{port}{host port}
}
