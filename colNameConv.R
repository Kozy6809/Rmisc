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
  print(nameStripe)
  genRegExp <- function(x) paste0(paste(strsplit(x, "")[[1]], collapse = ".*"), ".*")

  #品種名の変換("h" -> "HOC")をする場合
  res <- rccode %>% filter(series == seriesMap[ser])
  #しない場合
  #res <- rccode %>% filter(series == ser)
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
    print("illegal name")
    print(sep[1:2])
    NA
  } else if (nrow(code) > 1) {
    print("name ambiguous")
    print(code$sname)
    NA
  } else {
    dayValue <- lot2date(sep[3])
    data.frame(pcode = code$pcode, lot = sep[3], day = dayValue, stringsAsFactors = FALSE)
  }
}

convColObs <- function(df) {
  updaterccode()
  df <- df[, -1]
  names(df) <- c("name", "l", "a", "b")
  headings <- df[(0:(nrow(df) / 6 - 1)) * 6 + 1, 1]
  print(headings)
  res <- data.frame()
  for (h in headings) {
    r <- convColName(h)
    if (is.na(r)) stop("Name ERROR!")
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
