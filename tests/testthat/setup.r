library(data.table)
#setwd("tests/testthat")
PATH <- '../data/'
DESIRED_ROWS <- 5000

DAY <- seq(0, 366)
PERCENT <- seq(0, 100)
RADIANS <- seq(-360, 360, by=0.1) * pi/180
ZERO_OR_ONE <- list(0, 1)

ACCEL <- ZERO_OR_ONE
ASPECT <- RADIANS
BOOL <- c(TRUE, FALSE)
BUI <- seq(0, 1000, by=0.1)
BUIEFF <- ZERO_OR_ONE
CBH <- seq(0, 200, by=0.1)
CC <- PERCENT
CFB <- seq(-1, 2, by=0.01)
CFL <- seq(-10, 4000, by=0.1)
D0 <- DAY
DC <- seq(0, 1000, by=0.1)
DMC <- seq(0, 1000, by=0.1)
DJ <- DAY
ELV <- seq(0, 10000)
FC <- seq(-10, 20000)
FFMC <- seq(0, 101, by=0.1)
FMC <- seq(0, 500, by=0.1)
FRACTION <- seq(0, 1, by=0.05)
FUELTYPE=c("NF", "WA", "C1", "C2", "C3", "C4", "C5", "C6", "C7",
           "D1", "M1", "M2", "M3", "M4", "S1", "S2",
           "S3", "O1A", "O1B")
GFL <- seq(0, 100)
GS <- seq(0, 200)
HR <- seq(0, 366 * 24) * 60
HOURS <- seq(0, 23)
ISI <- seq(0, 300, by=0.1)
LAT <- seq(-90, 90, by=0.1)
LB <- seq(-1, 1.1, by=0.01)
# FIX: for some reason the original package just makes negatives positive
LONG <- abs(seq(-180, 360, by=0.1))
MON <- seq(1, 12)
PC <- PERCENT
PDF <- PERCENT
PREC <- seq(-10, 300, by=0.01)
RH <- seq(-10, 110, by=0.01)
ROS <- seq(0, 600, by=0.01)
SAZ <- RADIANS + pi
SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
SD <- unlist(append(list(-999), seq(0, 100)))
SFC <- seq(0, 20000)
SH <- seq(-10, 110)
TEMP <- seq(-30, 60, by=0.1)
WD <- RADIANS
WS <- seq(0, 300, by=0.1)
THETA <- seq(-360, 360, by=0.01)
WSV <- seq(-10, 500, by=0.1)
WAZ <- RADIANS + pi
WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)

FBP_ARGS <- list(data.table(ID=1),
                 data.table(FUELTYPE=FUELTYPE),
                 data.table(FFMC=FFMC),
                 data.table(BUI=BUI),
                 data.table(WS=WS),
                 data.table(WD=WD),
                 data.table(FMC=FMC),
                 data.table(GS=GS),
                 data.table(LAT=LAT),
                 data.table(LONG=LONG),
                 data.table(ELV=ELV),
                 data.table(DJ=DJ),
                 data.table(D0=D0),
                 data.table(SD=SD),
                 data.table(SH=SH),
                 data.table(HR=HR),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(GFL=GFL),
                 data.table(CC=CC),
                 data.table(THETA=THETA),
                 data.table(ACCEL=ACCEL),
                 data.table(ASPECT=ASPECT),
                 data.table(BUIEFF=BUIEFF),
                 data.table(CBH=CBH),
                 data.table(CFL=CFL),
                 data.table(ISI=ISI))

pickRows <- function(d1, num_rows=DESIRED_ROWS)
{
  d1 <- data.table(d1)
  #print(d1)
  #print(nrow(d1))
  #print(MAX_ROWS)
  old_names <- colnames(d1)
  while (nrow(d1) > num_rows)
  {
    #print('loop')
    #print(seq(1, nrow(d1), by=3))
    #print(d1[seq(1, nrow(d1), by=3), ])
    #print('assign')
    d1 <- data.table(d1[seq(1, nrow(d1), by=3), ])
    #print('end loop')
    #print(nrow(d1))
    stopifnot(!is.null(nrow(d1)))
    colnames(d1) <- old_names
  }
  #print('return')
  return(d1)
}

makeInput <- function(arguments)
{
  #print(arguments)
  d1 <- pickRows(arguments[[1]])
  if (1 < length(arguments))
  {
    for (n in 2:length(arguments))
    {
      #print(n)
      #print(arguments[[n]])
      d2 <- pickRows(arguments[[n]], ceiling(3 * DESIRED_ROWS / nrow(d1)))
      d1 <- pickRows(merge(data.frame(d1), data.frame(d2), by=NULL))
    }
  }
  return(data.table(d1))
}

makeData <- function(name, fct, arguments, split_args)
{
  i <- makeInput(arguments)
  if (!split_args)
  {
    stopifnot(is.data.table(i))
    values <- fct(i)
    i[, c(name)] <- values
    return(i)
  }
  n0 <- nrow(i)
  #i[, c(name)] <- do.call(fct, i)
  r <- list(do.call(fct, i[1, ]))
  isRow <- length(r[[1]]) > 1
  if (isRow)
  {
    r <- r[[1]]
    for (n in 2:nrow(i))
    {
      r2 <- do.call(fct, i[n, ])
      r <- rbind(r, r2)
    }
    stopifnot(nrow(i) == n0)
    return(r)
  }
  else
  {
    for (n in 2:nrow(i))
    {
      r <- append(r, do.call(fct, i[n, ]))
    }
    i[, c(name)] <- unlist(r)
    stopifnot(nrow(i) == n0)
    return(i)
  }
}


checkResults <- function(name, df1)
{
  # don't worry about what names are if this isn't actually a table
  ignore_names <- is.vector(df1)
  df1 <- data.table(df1)
  df2 <- data.table(read.csv(paste0(PATH, name, '.csv')))
  expect_equal(length(colnames(df1)), length(colnames(df2)))
  if (ignore_names)
  {
    colnames(df2) <- colnames(df1)
  }
  else
  {
    expect_equal(colnames(df1), colnames(df2))
  }
  for (n in sort(colnames(df1)))
  {
    test_that(paste0(name, '$', n), {
      actual <- unlist(df1[[n]])
      expected <- unlist(df2[[n]])
      # unsure if this will cause problems, but seems to fix when column is all NA
      class(actual) <- typeof(expected)
      expect_equal(actual, expected)
    })
  }
}

checkData <- function(name, fct, arguments, split_args=TRUE)
{
  df1 <- makeData(name, fct, arguments, split_args)
  df2 <- data.table(read.csv(paste0(PATH, name, '.csv')))
  #print(df1[[name]])
  #print(as.numeric(df1[[name]]))
  #print(df2[[name]])
  #print(as.numeric(df2[[name]]))
  #expect_equal(as.numeric(df1[[name]]), as.numeric(df2[[name]]))
  if (split_args)
  {
    actual <- df1[[name]]
    expected <- df2[[name]]
  }
  else
  {
    actual <- df1
    expected <- df2
  }
  expect_equal(actual, expected)
}
fctOnInput <- function(fct)
{
  return(function(ID, FUELTYPE, FFMC, BUI, WS, WD, FMC, GS, LAT, LONG, ELV, DJ, D0,
                  SD, SH, HR, PC, PDF, GFL, CC, THETA, ACCEL, ASPECT, BUIEFF,
                  CBH, CFL, ISI)
  {
    input <- data.frame(ID=ID,
                        FUELTYPE=FUELTYPE,
                        FFMC=FFMC,
                        BUI=BUI,
                        WS=WS,
                        WD=WD,
                        FMC=FMC,
                        GS=GS,
                        LAT=LAT,
                        LONG=LONG,
                        ELV=ELV,
                        DJ=DJ,
                        D0=D0,
                        SD=SD,
                        SH=SH,
                        HR=HR,
                        PC=PC,
                        PDF=PDF,
                        GFL=GFL,
                        CC=CC,
                        THETA=THETA,
                        ACCEL=ACCEL,
                        ASPECT=ASPECT,
                        BUIEFF=BUIEFF,
                        CBH=CBH,
                        CFL=CFL,
                        ISI=ISI)
    return(fct(input=input, output="S"))
  })
}
test_columns <- function(actual, expected)
{
  for (n in names(actual))
  {
    test_that(n, {
      a <- actual[[n]]
      e <- expected[[n]]
      if (is.numeric(a))
      {
        expect_equal(a, as.numeric(e))
      }
      else if(is.character(a))
      {
        expect_equal(a, as.character(e))
      }
      else
      {
        expect_equal(a, e)
      }
    })
  }
}
