Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_221") #java
library(RJDBC)
library(dplyr)
library(tidyr)

#DB데이터 추출부분
test <- dbGetQuery(conn, "select * from air_min where data_dt like '201904%'")
#추출된 DB에서 필요한 컬럼만 골라냄
tidy <- dplyr::select(test, DATA_DT, LOC_CODE, ITEM_CODE, DATA_CONCE)
#ITEM_CODE로 되어있는 데이터를 각 변수별로 해당시간의 해당 측정소 별 자료로 테이블 재배열
raw_201904 <- tidy %>% pivot_wider(names_from = ITEM_CODE, values_from = DATA_CONCE)

#추출된 DB데이터 컬럼명 rename
vars <- list('DATA_DT' = sym("DATA_DT"), 'LOC_CODE' = sym("LOC_CODE"), 'PM10'=sym("1"), 'PM2.5'=sym("2"), 'O3'=sym("3"), 'CO'=sym("4"), 'SO2'=sym("5"), 'NO2'=sym("6"), 'NOX'=sym("7"), 'NO'=sym("8"), 'WD'=sym("9"), 'WS'=sym("10"), 'TMP'=sym("11"), 'HUM'=sym("13"), 'INTMP'=sym("12"), 'SOL'=sym("14"), 'UV'=sym("15"), 'CH4'=sym("16"), 'N_CH4'=sym("17"), 'THC'=sym("18"))
raw_201904 <-select(raw_201904, !!!vars)
#rename후 csv파일로 다시쓰기
write.csv(raw_201904, file = "raw_201904.csv", row.names = F)

#1904부터 1910까지 데이터 합친 파일을 raw_1904_1910으로 이름붙이고 시작
raw_1904_1910 <- read.csv(file = "raw_1904_1910.csv", header = T) #파일 읽기
raw_1904_1910_2 <- raw_1904_1910 #test용 복사본
raw_1904_1910_2 <- dplyr::select(raw_1904_1910_2, DATA_DT:HUM) #hum컬럼까지 잘라냄

#이동차 혹은 777777같은 존재하지 않는 측정소 번호를 제외시켜야 하는데 아래 구문은 모두 걸러내지 못함
#longer object length is not a multiple of shorter object length 이런 warning이 뜨는데 정확한 이유는 모르겠음
raw_1904_1910_3 <- subset(raw_1904_1910_2, LOC_CODE != c(131000, 131001, 777777, 777778, 777779))
#대신 %in% 연산자를 사용하면 되는데 바로 적용가능한 함수는 없고 아래 구문을 통해 해당하는 논리연산자의 리스트를 작성
#not연산자(!)를 사용해서 해당 false에 해당하는 줄(row)만 남기는 방식을 적용했음
row_to_keep <- raw_1904_1910_2$LOC_CODE %in% c(131000, 131001, 777777, 777778, 777779)
raw_1904_1910_5 <- raw_1904_1910_2[!row_to_keep,] #[모든 row, 모든 column]

test_data <- raw_1904_1910_5 #다른 테스트를 위한 복사본
test_PM10 <- select(test_data, DATA_DT, LOC_CODE, PM10) #일자, 측정소번호, PM10만 남기고 잘라냄
#5분 단위의 시간이 row가 되고 각 측정소의 측정소번호가 column이 됨
test_PM10_2 <- pivot_wider(test_PM10, names_from = LOC_CODE, values_from = PM10)
test_PM2.5 <- pivot_wider(test_data, names_from = LOC_CODE, values_from = PM2.5)
test_etc <- pivot_wider(test_data, names_from = LOC_CODE, values_from = c(PM2.5, PM10))

#리스트가 문제가 아니라 그냥 다차원 배열을 만들면 되는것이었음 다만 이걸 파이썬에서 할지 여기서 해서 내보내야 하는지 고민
#일단 여기서 3차원 배열을 만들어보기로 함

pivot_PM10 <- select(test_data, DATA_DT, LOC_CODE, PM10) %>% pivot_wider(names_from = LOC_CODE, values_from = PM10)
pivot_PM2.5 <- select(test_data, DATA_DT, LOC_CODE, PM2.5) %>% pivot_wider(names_from = LOC_CODE, values_from = PM2.5)
pivot_O3 <- select(test_data, DATA_DT, LOC_CODE, O3) %>% pivot_wider(names_from = LOC_CODE, values_from = O3)
pivot_CO <- select(test_data, DATA_DT, LOC_CODE, CO) %>% pivot_wider(names_from = LOC_CODE, values_from = CO)
pivot_SO2 <- select(test_data, DATA_DT, LOC_CODE, SO2) %>% pivot_wider(names_from = LOC_CODE, values_from = SO2)
pivot_NO2 <- select(test_data, DATA_DT, LOC_CODE, NO2) %>% pivot_wider(names_from = LOC_CODE, values_from = NO2)
pivot_NOX <- select(test_data, DATA_DT, LOC_CODE, NOX) %>% pivot_wider(names_from = LOC_CODE, values_from = NOX)
pivot_NO <- select(test_data, DATA_DT, LOC_CODE, NO) %>% pivot_wider(names_from = LOC_CODE, values_from = NO)
pivot_WD <- select(test_data, DATA_DT, LOC_CODE, WD) %>% pivot_wider(names_from = LOC_CODE, values_from = WD)
pivot_WS <- select(test_data, DATA_DT, LOC_CODE, WS) %>% pivot_wider(names_from = LOC_CODE, values_from = WS)
pivot_TMP <- select(test_data, DATA_DT, LOC_CODE, TMP) %>% pivot_wider(names_from = LOC_CODE, values_from = TMP)
pivot_HUM <- select(test_data, DATA_DT, LOC_CODE, HUM) %>% pivot_wider(names_from = LOC_CODE, values_from = HUM)

#필요한건 pivot이 아님 pivot <- list(pivot_PM10, pivot_PM2.5, pivot_O3, pivot_CO, pivot_SO2, pivot_NO2, pivot_NOX, pivot_NO, pivot_WD, pivot_WS, pivot_TMP, pivot_HUM)
pivot_3d_array <- array(pivot_PM10, dim = c(12, 95, 61635))
pivot <- array(data = NA, dim = c(61635, 95, 12))

for(i in 1:61635){
  for(j in 1:95){
    pivot[i,j,1] <- pivot_PM10[i,j]
  }
}
#위의 형태로 데이터를 나타내야하는 이유는 전처리된 raw_1904_1910의 형태가 측정시간과 측정소번호에 따라 중복되는 시간 자료가 있기 때문
#모든 시간 자료는 하나만 있어야 함, 측정물질이 달라지면 다른 테이블의 형태로 다른 channel로 구성되야 한다고 판단됨
#따라서 이상치 처리를 제외한 데이터 구성의 형태는 시간과 측정소번호로 구성된 테이블이 각 물질별 갯수만큼 각각의 채널로 존재하는 형태

#target data 만들기 각 데이터의 24시간 후 데이터를 찾아서 뒷 컬럼에 붙임(PM10, PM2.5 대상)
#PM10 테스트자료에서 시간데이터가 5분간격으로 중간에 빠지거나 잘린곳이 없는지 점검
#but 시계열 데이터를 고려하여 딥러닝 시킬것이 아니면 현재 접근법은 단순 회귀이므로 일단 시계열 데이터가 빠졌는지 상관할 필요는 없음
#심지어 중간에 빠진 부분이 있어도 그 부분도 시계열 딥러닝에서 빠진 시간으로 고려될 것이므로 더욱 상관없을듯

#lag()를 사용하려고 고려했지만 중간에 빠진 시간이 있으면 그대로 시간이 밀려서 lag될 것이므로 포기
#DATA_DT에서 24시간 후를 더한 컬럼을 새로 만들어서 merge시켜보기로 함
test_PM10_3 <- test_PM10_2
test_PM10_3 <- dplyr::mutate(test_PM10_3, lag_DATA_DT = DATA_DT + 10000)
test_PM10_4 <- dplyr::select(test_PM10_3, lag_DATA_DT)
test_PM10_3 <- dplyr::select(test_PM10_3, -lag_DATA_DT)
test_PM10_4 <- dplyr::left_join(test_PM10_4, test_PM10_3, by = c("lag_DATA_DT" = "DATA_DT"))
test_PM10_3 <- filter(test_PM10_3, DATA_DT >= 201904020000) #타겟 데이터를 만들고나서 맞는 타겟 데이터가 없는 날짜범위를 지움
test_PM10_4 <- filter(test_PM10_4, lag_DATA_DT <= 201910312355)
test_PM10_4 <- test_PM10_4[-147,] #147번째 줄에서 5분단위가 아닌 시간 행이 발견되어 삭제 refactory할 때 5분단위 자료 검증코드도 넣을것

#위에서 PM10을 대상으로 시간 row와 측정소번호 column으로 pivot_wider를 할 수 있음을 확인
#문제는 각각의 항목을 대상으로 각각 pivot_wider를 해야 하는 것이 힘들어보임
#다시 처음으로 돌아와서 초기 raw데이터에서 바로 타겟 데이터를 작성하는 구문 작성 시도
raw_1904_1910_7 <- mutate(raw_1904_1910_6, t_PM10 = 0, t_PM2.5 = 0) #일단 타겟데이터 초기값을 0으로 설정해서 컬럼 선언

#loc_code도 맞아야함 but 시간이 너무 오래걸림
for(i in 1:length(raw_1904_1910_7$DATA_DT)){
  for(j in 1:length(raw_1904_1910_7$DATA_DT)){
    if(raw_1904_1910_7$DATA_DT[i] + 10000 == raw_1904_1910_7$DATA_DT[j]){
      raw_1904_1910_7$t_PM10[i] <- raw_1904_1910_7$PM10[j]
      raw_1904_1910_7$t_PM2.5[i] <- raw_1904_1910_7$PM2.5[j]
    }
  }
}

#https://rstudio.github.io/reticulate/ python in R 참고할것 