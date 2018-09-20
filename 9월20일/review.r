tr_a <- hos
test<- hos_test 

library(dplyr)

tr[[2]]


##함수만들기 : 결측 처리할 데이터 셋 
##wanteVal: mean, median, mode
NAProcess <- function ( df_NA, sel, wanteVal='mean'){
  for (i in sel){
    type=class(df_NA[[i]])
    
    if(wantedVal=='mean'){
      ##평균값을 구하고 위치를 찾고 치환한다
      val=mean(df_NA[[i]], na.rm=T)
      loc=which(is.na(df_NA[[i]]))
      }
    if(type == 'numeric'){
      df_NA[loc,i]=as.numeric(val)
    }else if (type =='integer'){
      df_NA[loc,i]==as.integer(val)
    }
  }
  
}



##함수만들기 : 결측 처리할 데이터 셋 
##wanteVal: mean, median, mode
NAProcess <- function ( df_NA, sel, wantedVal='mean'){
  for (i in sel){
    type=class(df_NA[[i]])
    
    if(wantedVal=='mean'){
      ##평균값을 구하고 위치를 찾고 치환한다
      val=mean(df_NA[[i]], na.rm=T)
      loc=which(is.na(df_NA[[i]]))
    } else if (wantedVal =='median'){
      val=median(df_NA[[i]], na.rm=T)
      loc=which(is.na(df_NA[[i]]))
    }
    if(type == 'numeric'){
      df_NA[loc,i]=as.numeric(val)
    }else if (type =='integer'){
      df_NA[loc,i]==as.integer(val)
    }
  }
  return(df_NA)
}


NAProcess(tr,7,'mean')
