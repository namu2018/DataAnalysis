###user defined functions
##기본 함수 형태1
'''
함수명1 <- function(a,b) {
  실행문
  return 값 <- 생략가능 
}

함수명2 <- function(a,b="",c=0,d=0,e=0,f=FALSE,g=TRUE) {
  실행문
  return 값 <- 생략가능 
}
'''

함수명1()
val=함수명1(3,5)
val=함수명2()

sampleFnc <- function(){
  print("execute function")
}

sampleFnc1 <- function(a,b){
  c= a+b
  return(c)
}

res=sampleFnc1(3,5)
res

oper <- function(a,b,c='+', max=FALSE)
  {
  if (max==TRUE){ if(a>b){return(a)}
                   else {return(b)}}
  else{
  if (c=='+'){ return(a+b)}
  if (c=='-'){ return(a-b)}
  if (c=='*'){ return(a*b)}
  }
}

oper(3,5,max=TRUE)

###실습
##01. Mouda결측치 처리 함수를 만들어 보자

del_na <- function(data) {
  df <- na.omit(data)
  return(df)
}

del_na(hos)

del_col_na <- function(data, colm){
  if (data$colm == na) {}
}
  
##02. 자신만의 데이터 셋에 대한 기본 EDA함수를 만들어 보자

readfile <- function(filename, type='csv'){
  if(type=='csv'){ df <- read.csv(filename)}
  if(type=='txt'){ df <- read.txt(filename)}
  if(type=='spss'){df <- read.spss(filename)}
  return(df)
}