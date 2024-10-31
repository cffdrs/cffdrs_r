library(data.table)
library(terra)

SIG_DIGS <- 4

DESIRED_ROWS <- 5000

DAY <- seq(0, 366)
PERCENT <- seq(0, 100)
RADIANS <- seq(-360, 360, by = 0.1) * pi / 180
ZERO_OR_ONE <- list(0, 1)

ACCEL <- ZERO_OR_ONE
ASPECT <- RADIANS
BOOL <- c(TRUE, FALSE)
BUI <- seq(0, 1000, by = 0.1)
BUIEFF <- ZERO_OR_ONE
CBH <- seq(0, 200, by = 0.1)
CC <- PERCENT
CFB <- seq(-1, 2, by = 0.01)
CFL <- seq(-10, 4000, by = 0.1)
D0 <- DAY
DC <- seq(0, 1000, by = 0.1)
DMC <- seq(0, 1000, by = 0.1)
DJ <- DAY
ELV <- seq(0, 10000)
FC <- seq(-10, 20000)
FFMC <- seq(0, 101, by = 0.1)
FMC <- seq(0, 500, by = 0.1)
FRACTION <- seq(0, 1, by = 0.05)
FUELTYPE <- c(
  "NF", "WA", "C1", "C2", "C3", "C4", "C5", "C6", "C7",
  "D1", "M1", "M2", "M3", "M4", "S1", "S2",
  "S3", "O1A", "O1B"
)
GFL <- seq(0, 100)
GS <- seq(0, 200)
HR <- seq(0, 366 * 24) * 60
HOURS <- seq(0, 23)
ISI <- seq(0, 300, by = 0.1)
LAT <- seq(-90, 90, by = 0.1)
LB <- seq(-1, 1.1, by = 0.01)
# FIX: for some reason the original package just makes negatives positive
LONG <- abs(seq(-180, 360, by = 0.1))
MON <- seq(1, 12)
PC <- PERCENT
PDF <- PERCENT
PREC <- seq(-10, 300, by = 0.01)
RH <- seq(-10, 110, by = 0.01)
ROS <- seq(0, 600, by = 0.01)
SAZ <- RADIANS + pi
SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
SD <- unlist(append(list(-999), seq(0, 100)))
SFC <- seq(0, 20000)
SH <- seq(-10, 110)
TEMP <- seq(-30, 60, by = 0.1)
WD <- RADIANS
WS <- seq(0, 300, by = 0.1)
THETA <- seq(-360, 360, by = 0.01)
WSV <- seq(-10, 500, by = 0.1)
WAZ <- RADIANS + pi
WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)

FBP_ARGS <- list(
  data.table(ID = 1),
  data.table(FUELTYPE = FUELTYPE),
  data.table(FFMC = FFMC),
  data.table(BUI = BUI),
  data.table(WS = WS),
  data.table(WD = WD),
  data.table(FMC = FMC),
  data.table(GS = GS),
  data.table(LAT = LAT),
  data.table(LONG = LONG),
  data.table(ELV = ELV),
  data.table(DJ = DJ),
  data.table(D0 = D0),
  data.table(SD = SD),
  data.table(SH = SH),
  data.table(HR = HR),
  data.table(PC = PC),
  data.table(PDF = PDF),
  data.table(GFL = GFL),
  data.table(CC = CC),
  data.table(THETA = THETA),
  data.table(ACCEL = ACCEL),
  data.table(ASPECT = ASPECT),
  data.table(BUIEFF = BUIEFF),
  data.table(CBH = CBH),
  data.table(CFL = CFL),
  data.table(ISI = ISI)
)

ROS_AT_THETA_INPUT = (function() {
  # HACK: specific input generation to not make so many invalid arguments
  # HACK: not using random numbers anywhere else and sample() is easier
  set.seed(123)
  input <- NULL
  sq <- floor(sqrt(DESIRED_ROWS))
  ros <- c(0, sort(unique(abs(round(rnorm(sq, 5, 20), 2)))))
  for (i in length(ros):1) {
    ros_cur <- ros[i]
    other_ros <- sort(unique(round(runif(sq, 0, ros_cur), 2)))
    input <- data.table(
      rbind(input,
            merge(data.frame(ROS=ros_cur, FROS=other_ros),
                  data.frame(BROS=other_ros),
                  by=NULL),
            # make some (usually) invalid results too
            data.frame(ROS=sample(other_ros, 1), FROS=ros_cur, BROS=sample(other_ros, 1)),
            data.frame(ROS=sample(other_ros, 1), FROS=sample(other_ros, 1), BROS=ros_cur)
      )
    )
  }
  if (nrow(input) > DESIRED_ROWS) {
    # can't sample directly from table so pick rows
    input <- input[sample(1:nrow(input), DESIRED_ROWS)]
  }
  # add rows we always want to check
  input <- rbind(
    input,
    data.frame(ROS=ros, FROS=ros, BROS=ros))
  input <- cbind(
    input,
    data.frame(THETA=sample(-361:361, nrow(input), replace=TRUE)))
  input <- setorder(unique(input), ROS, FROS, BROS)
})()

get_data_path <- function(name, suffix="csv") {
  return(test_path("data", sprintf("%s.%s", name, suffix)))
}

read_data <- function(name) {
  return(read.csv(get_data_path(name)))
}

get_raster_path <- function(name) {
  return(get_data_path(name, "tif"))
}

read_raster <- function(name) {
  return(rast(get_raster_path(name)))
}

pickRows <- function(d1, num_rows = DESIRED_ROWS) {
  d1 <- data.table(d1)
  # print(d1)
  # print(nrow(d1))
  # print(MAX_ROWS)
  old_names <- colnames(d1)
  while (nrow(d1) > num_rows) {
    # print('loop')
    # print(seq(1, nrow(d1), by=3))
    # print(d1[seq(1, nrow(d1), by=3), ])
    # print('assign')
    d1 <- data.table(d1[seq(1, nrow(d1), by = 3), ])
    # print('end loop')
    # print(nrow(d1))
    stopifnot(!is.null(nrow(d1)))
    colnames(d1) <- old_names
  }
  # print('return')
  return(d1)
}

makeInput <- function(arguments) {
  # print(arguments)
  d1 <- pickRows(arguments[[1]])
  if (1 < length(arguments)) {
    for (n in 2:length(arguments))
    {
      # print(n)
      # print(arguments[[n]])
      d2 <- pickRows(arguments[[n]], ceiling(3 * DESIRED_ROWS / nrow(d1)))
      d1 <- pickRows(merge(data.frame(d1), data.frame(d2), by = NULL))
    }
  }
  return(data.table(d1))
}

makeDataFromInput <- function(name, fct, input, split_args, with_input = FALSE) {
  if (!split_args) {
    stopifnot(is.data.table(input))
    values <- fct(input)
    input[, c(name)] <- values
    return(input)
  }
  n0 <- nrow(input)
  # input[, c(name)] <- do.call(fct, input)
  r <- list(do.call(fct, input[1, ]))
  isRow <- length(r[[1]]) > 1
  if (isRow) {
    r <- r[[1]]
    for (n in 2:nrow(input)) {
      r2 <- do.call(fct, input[n, ])
      r <- rbind(r, r2)
    }
    stopifnot(nrow(input) == n0)
    if (with_input) {
      r <- cbind(input, r)
    }
    return(r)
  } else {
    for (n in 2:nrow(input)) {
      r <- append(r, do.call(fct, input[n, ]))
    }
    input[, c(name)] <- unlist(r)
    stopifnot(nrow(input) == n0)
    return(input)
  }
}

makeData <- function(name, fct, arguments, split_args, with_input = FALSE) {
  return(makeDataFromInput(name, fct, makeInput(arguments), split_args, with_input))
}

# want to apply to each individual number
significant <- Vectorize(function(data) {
  # keep at least 2 decimal places
  return(signif(
    data,
    pmax(
      ceiling(log10(abs(data))) + 2,
      SIG_DIGS
    )
  ))
})

roundData <- function(data) {
  data <- as.data.table(data)
  for (col in names(data)) {
    # don't round integers
    if (is.numeric(data[[col]]) && !is.integer(data[[col]])) {
      data[[col]] <- significant(data[[col]])
    }
  }
  return(data)
}

roundRaster <- function(data) {
  return(signif(data, SIG_DIGS))
}

checkResults <- function(name, df1) {
  df1 <- roundData(df1)
  df2 <- as.data.table(read_data(name))
  expect_equal(df1, df2)
}

checkData <- function(name, fct, arguments, split_args = TRUE, with_input = FALSE) {
  checkResults(name, makeData(name, fct, arguments, split_args, with_input))
}

fctOnInput <- function(fct) {
  return(function(ID, FUELTYPE, FFMC, BUI, WS, WD, FMC, GS, LAT, LONG, ELV, DJ, D0,
                  SD, SH, HR, PC, PDF, GFL, CC, THETA, ACCEL, ASPECT, BUIEFF,
                  CBH, CFL, ISI) {
    input <- data.frame(
      ID = ID,
      FUELTYPE = FUELTYPE,
      FFMC = FFMC,
      BUI = BUI,
      WS = WS,
      WD = WD,
      FMC = FMC,
      GS = GS,
      LAT = LAT,
      LONG = LONG,
      ELV = ELV,
      DJ = DJ,
      D0 = D0,
      SD = SD,
      SH = SH,
      HR = HR,
      PC = PC,
      PDF = PDF,
      GFL = GFL,
      CC = CC,
      THETA = THETA,
      ACCEL = ACCEL,
      ASPECT = ASPECT,
      BUIEFF = BUIEFF,
      CBH = CBH,
      CFL = CFL,
      ISI = ISI
    )
    return(fct(input = input, output = "S"))
  })
}

test_raster <- function(name, input, fct) {
  # only comparing to significant digits specified
  actual <- roundRaster(fct(input))
  expected <- read_raster(name)

  out_cols <- setdiff(names(actual), toupper(names(input)))
  # we don't actually know the names of the columns from the file, so assign from output
  names(expected) <- names(actual)

  expect_equal(names(expected), names(actual))
  # # nc seems to prefer negative longitudes
  # ext(actual) <- ext(expected)
  m <- minmax(actual[[out_cols]] - expected[[out_cols]])
  expect_true(all(abs(m) < (10^-SIG_DIGS)))
}
