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

#위의 형태로 데이터를 나타내야하는 이유는 전처리된 raw_1904_1910의 형태가 측정시간과 측정소번호에 따라 중복되는 시간 자료가 있기 때문
#모든 시간 자료는 하나만 있어야 함, 측정물질이 달라지면 다른 테이블의 형태로 다른 channel로 구성되야 한다고 판단됨
#따라서 이상치 처리를 제외한 데이터 구성의 형태는 시간과 측정소번호로 구성된 테이블이 각 물질별 갯수만큼 각각의 채널로 존재하는 형태

