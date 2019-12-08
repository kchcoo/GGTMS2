rm(list=ls())

raw_1904_1910 <- read.csv(file = "raw_1904_1910.csv", header = T)
raw_1904_1910_2 <- raw_1904_1910
raw_1904_1910_2 <- dplyr::select(raw_1904_1910_2, DATA_DT:HUM) #hum컬럼까지 잘라냄



for(i in 1:length(raw_1904_1910_2$DATA_DT)){
  if(raw_1904_1910_2$PM10[i] < 0 & is.na(raw_1904_1910_2$PM10[i] != NA)){
    raw_1904_1910_2$PM10[i] <- NA
  }
  if(raw_1904_1910_2$PM2.5[i] < 0 & is.na(raw_1904_1910_2$PM2.5[i] != NA)){
    raw_1904_1910_2$PM2.5[i] <- NA
  }
  if(raw_1904_1910_2$CO[i] < 0 & is.na(raw_1904_1910_2$CO[i] != NA)){
    raw_1904_1910_2$CO[i] <- NA
  }
  if(raw_1904_1910_2$SO2[i] < 0 & is.na(raw_1904_1910_2$SO2[i] != NA)){
    raw_1904_1910_2$SO2[i] <- NA
  }
  if(raw_1904_1910_2$SO2[i] < 0 & is.na(raw_1904_1910_2$SO2[i] != NA)){
    raw_1904_1910_2$SO2[i] <- NA
  }
  if(raw_1904_1910_2$NO2[i] < 0 & is.na(raw_1904_1910_2$NO2[i] != NA)){
    raw_1904_1910_2$NO2[i] <- NA
  }
  if(raw_1904_1910_2$NOX[i] < 0 & is.na(raw_1904_1910_2$NOX[i] != NA)){
    raw_1904_1910_2$NOX[i] <- NA
  }
  if(raw_1904_1910_2$NO[i] < 0 & is.na(raw_1904_1910_2$NO[i] != NA)){
    raw_1904_1910_2$NO[i] <- NA
  }
}

write.csv(raw_1904_1910_2, file = "raw_1904_1910_2.csv", row.names = F)
#측정소번호 정리할것 이동차 등등 

sitelist <- unique(raw_1904_1910_2$LOC_CODE)
write.csv(sitelist, file = "sitelist.csv", row.names = F)
raw_1904_1910_3 <- subset(raw_1904_1910_2, LOC_CODE != c(131000, 131001, 777777, 777778, 777779))
#음수값들 결측처리 해야함

raw_1904_1910_2 <- read.csv(file = "raw_1904_1910_2.csv", header = T)
raw_1904_1910_4 <- raw_1904_1910_3
for(i in 1:length(raw_1904_1910_4$PM10)){
  if(is.na(raw_1904_1910_4$PM10[i])){
    next;
  }
  if(raw_1904_1910_4$PM10[i] < 0){
    raw_1904_1910_4$PM10[i] <- NA;
  }
}

test_data <- raw_1904_1910_3
test_PM10 <- select(test_data, DATA_DT, LOC_CODE, PM10)
test_PM10_2 <- pivot_wider(test_PM10, names_from = LOC_CODE, values_from = PM10)

row_to_keep <- raw_1904_1910_2$LOC_CODE %in% c(131000, 131001, 777777, 777778, 777779)
raw_1904_1910_5 <- raw_1904_1910_2[!row_to_keep,]