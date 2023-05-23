devtools::load_all()
data(test_fbp)

df <- data.table(test_fbp)
names(df) <- toupper(names(df))




r <- NULL
t <- test_fbp
names(t) <- toupper(names(t))
output <- "ALL"
for (i in 1:20) {
    print(paste0("t is length ", nrow(t)))
    print(strrep("*", 40))
    print("fire_behaviour_prediction(t)")
    x <- as.list(system.time({f_0 <<- data.table(fire_behaviour_prediction(t, output))}))
    # f_0$ID <- as.character(f_0$ID)
    setorder(f_0, ID)
    x$cores <- 0
    r_i <- as.data.frame(x)
    # n <- 1
    # while (n <= 48) {
    #     print(paste0("fbp(t, cores=", n, ")"))
    #     x <- as.list(system.time({f_n <<- fbp(t, output=output, cores=n)}))
    #     f_n$ID <- as.character(f_n$ID)
    #     setorder(f_n, ID)
    #     x$cores <- n
    #     r_i <- rbind(r_i, as.data.frame(x))
    #     stopifnot(all(f_0 == f_n))
    #     n <- ifelse(32 == n, 48, n * 2)
    # }
    print("fbp_v(t)")
    # x <- system.time({f_a <<- rbindlist(apply(t, 1, fire_behaviour_prediction_v))})
    x <- system.time({f_a <<- fbp_v(t, output)})
    # f_a$ID <- as.character(f_a$ID)
    setorder(f_a, ID)
    stopifnot(all(f_0 == f_a))
    x$cores <- -1
    r_i <- rbind(r_i, as.data.frame(x))
    r_i$rows <- nrow(t)
    setcolorder(r_i, c("cores", "rows"))
    print(r_i)
    r <- rbind(r, r_i)
    t <- rbind(t, t)
}

r <- terra::rast('C:/nrcan/cffdrs/inst/extdata/test_fbpRaster.tif')
test_fbpRaster <- rast(
  system.file("extdata", "test_fbpRaster.tif", package = "cffdrs")
)
input <- test_fbpRaster
# Stack doesn't hold the raster layer names, we have to assign
# them:
names(input) <- c(
  "FuelType", "LAT", "LONG", "ELV", "FFMC", "BUI", "WS", "WD", "GS",
  "Dj", "D0", "hr", "PC", "PDF", "GFL", "cc", "theta", "Accel", "Aspect",
  "BUIEff", "CBH", "CFL", "ISI"
)

cols <- c("ID",
"FUELTYPE_CODE",
"ACCEL",
"DJ",
"D0",
"ELV",
"BUIEFF",
"HR",
"FFMC",
"ISI",
"BUI",
"WS",
"WD",
"GS",
"ASPECT",
"PC",
"PDF",
"CC",
"GFL",
"CBH",
"CFL",
"LAT",
"LONG",
"FMC",
"THETA",
"SD",
"SH")

devtools::load_all()
r <- inp
 result <- lapp(
    x = r[[cols]],
    fun = Vectorize(fire_behaviour_prediction_v),
    output_code=1
  )

for(n in names(result)) {
    print(n)
    print(values(result[[n]]))
}


r <- terra::rast(system.file("extdata", "test_fbpRaster.tif", package = "cffdrs"))
names(r) <- c(
  "FuelType", "LAT", "LONG", "ELV", "FFMC", "BUI", "WS", "WD", "GS",
  "Dj", "D0", "hr", "PC", "PDF", "GFL", "cc", "theta", "Accel", "Aspect",
  "BUIEff", "CBH", "CFL", "ISI"
)
r$ID <- seq_len(ncell(r))
result <- lapp(
    x = r[["ID"]],
    fun = Vectorize(function(id, output_code) { return(c(id, id / output_code)) }),
    output_code=3
)