lot2date <- function(lot) {
  get1stYear <- function(y) floor(y / 20) * 20
  
  yearMap <- 0:19
  names(yearMap) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                      "A", "B", "C", "D", "E", "F", "G", "H", "W", "J")
  monthMap <- 1:12
  names(monthMap) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "X", "Y", "Z")
  
  origin <- get1stYear(year(today()))
  monthStr <- toupper(substr(lot, 1, 1))
  yearStr <- toupper(substr(lot, 2, 2))
  subSpec <- as.integer(substr(lot, 3, 3))
  subSpec <- if (is.na(subSpec)) 0 else subSpec
  
  y <- yearMap[yearStr] + origin
  #print(y)
  y <- if (y < year(today())) {
    if (month(today()) == 12 && monthStr == "1") y + 20 else y
  } else y
  m <- monthMap[monthStr]
  y * 10000 + m * 100 + subSpec
}

rcResolv <- function(ser, name) {
  seriesMap <- c("HOC", "BOC", "EOC", "POP", "DUO", "HTC", "HPC", "OIL", "HVC", "YUI", "YUJ")
  names(seriesMap) <- c("h", "b", "e", "p", "d", "t", "s", "o", "v", "i", "j")
  nameStripe <- regmatches(name, gregexpr("[[:upper:]]{1}[[:lower:]]*|\\d*", name))[[1]]
  #print(nameStripe)
  genRegExp <- function(x) paste0(paste(strsplit(x, "")[[1]], collapse = ".*"), ".*")

  #品種名の変換("h" -> "HOC")をする場合
  res <- rccode %>% filter(series == seriesMap[tolower(ser)])
  #しない場合
  #res <- rccode %>% filter(series == tolower(ser))
  res <- res[grep(paste0("^", genRegExp(nameStripe[1])), res$w1),]
  if (!is.na(nameStripe[2])) res <- res[grep(genRegExp(nameStripe[2]), res$w2),]
  else res <- subset(res, is.na(res$w2))
  
  if (!is.na(nameStripe[3])) res <- res[grep(genRegExp(nameStripe[3]), res$w3),]
  else res <- subset(res, is.na(res$w3))
  
  if (!is.na(nameStripe[4])) res <- res[grep(genRegExp(nameStripe[4]), res$w4),]
  else res <- subset(res, is.na(res$w4))
  
  res
}

#測色のデータ名を変換してコードと日付値を生成する。コードが複数または0になる場合
#エラーにする
convColName <- function(s) {
  sep <- strsplit(s, " ")[[1]]
  code <- rcResolv(sep[1], sep[2])
  if (nrow(code) == 0) {
    #print("illegal name")
    #print(sep[1:2])
    NA
  } else if (nrow(code) > 1) {
    #print("name ambiguous")
    #print(code$sname)
    NA
  } else {
    dayValue <- if (!is.na(sep[3])) lot2date(sep[3]) else NA
    data.frame(pcode = code$pcode, lot = sep[3], day = dayValue, stringsAsFactors = FALSE)
  }
}

convColObs <- function(df) {
  df <- df[, c(-1,-3)]
  names(df) <- c("name", "l", "a", "b")
  headings <- unlist(df[(0:(nrow(df) / 6 - 1)) * 6 + 1, 1])

  res <- data.frame()
  for (h in headings) {
    r <- convColName(h)
    #if (is.na(r)) stop("Name ERROR!")
    for (i in 1:6) res <- rbind(res, r)
  }
  wb <- rep(rep(c("w", "b"), each = 3), length(headings))
  res <- data.frame(res, bg = wb, df[, -1])
  
  res
}

#今回の測色データからpcodeとlotを抽出する
getPcodeLot <- function(df) {
  unique(convColObs(df)[, 1:2])
}


#測色データを読み込み、最新のデータだけ残して書き戻し、結果データを返す
recent_data <- function(fn){
  df <- read_tsv(fn, locale=locale(encoding = "MS932"))
  day <- (df[nrow(df),2])
  day <- str_sub(day, -20, -11)
  r <- df %>% transmute(r = str_detect(データ名, day))
  start_row <- min(which(unlist(r))) - 1
  df <- df[start_row:nrow(df),]
  write.table(df, fn, sep = "\t", quote=F, row.names=F, fileEncoding = "MS932")
  df
}

#読み込んだdfから不正な色名の一覧を表示する。不正な色名が無ければTrueを返す
check_names <- function(df) {
  #df <- df[, -1]
  names(df) <- c("X", "name", "l", "a", "b")
  headings <- df[(0:(nrow(df) / 6 - 1)) * 6 + 1, 1:2]
  r <- apply(headings, 1, FUN=function(h) convColName(h[2]))
  invalids <- which(is.na(r))
  if (length(invalids) > 0) {
    print("以下の色名は不正または曖昧です")
    print(headings[invalids,])
    FALSE
  } else TRUE
}

library(tidyverse)
library(lubridate)
#library(odbc)
#library(DBI)
#library(dbplyr)
library(RODBC)
#con <- dbConnect(odbc::odbc(), "qc")
#rccode <- dbGetQuery(con, "select * from rccode;")
con <- odbcConnect("qc")
rccode <- sqlQuery(con, "select * from rccode;")

df <- recent_data("col.txt")
if (!check_names(df)) stop()
dbdata <- convColObs(df)
#ロットがNAの場合、dayも文字のNAになるため、insertできなくなるが、他の行は記録され、エラーも発生しない
for (i in 1:nrow(dbdata)) {
  row <- dbdata[i,]
  val <- paste0(row$pcode, ", '", row$lot, "', ", row$day, ", '", row$bg, "', ",
                row$l, ", ", row$a, ", ", row$b)
  q <- paste("insert into colmeas values (", val, ");")
  sqlQuery(con, q)
}

#マクロ"colavg"相当のクエリを実行
sqlQuery(con, "delete from colavg;")
sqlQuery(con, "insert into colavg select * from colavgq;")
sqlQuery(con, "delete from colavgavg;")
sqlQuery(con, "insert into colavgavg select * from colavgavgq;")

#getPcodeLot()を実行し、結果をテーブルtに入れる

#addN2ColavgLot
sqlQuery(con, "UPDATE colavg AS c INNER JOIN t AS t ON ([c].pcode=[t].[pcode]) AND ([c].lot=[t].[lot]) SET c.lot = [c].lot+'n';
")
#addN2ColmeasLot
sqlQuery(con, "UPDATE colmeas AS c INNER JOIN t AS t ON ([c].lot=[t].[lot]) AND ([c].pcode=[t].[pcode]) SET c.lot = [c].lot+'n';")
#ins2newcol2
sqlQuery(con, "INSERT INTO newcol2 SELECT * FROM [select c.* from colavg c inner join t on c.pcode = t.pcode and c.lot = (t.lot + 'n')]. AS [%$##@_Alias];
")
