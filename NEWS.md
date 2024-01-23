# version 1.9.0

---

## Enhancements

### Updated all documentation

### The following functions have been deprecated:
- `.BROSCalc` -> `back_rate_of_spread`
- `.BEcalc` -> `buildup_effect`
- `.buiCalc` -> `buildup_index`
- `.C6calc` -> `intermediate_surface_rate_of_spread_c6`, 
    `crown_rate_of_spread_c6`, `crown_fraction_burned_c6`, `rate_of_spread_c6`
- `.CFBcalc` -> `critical_surface_intensity`, `surface_fire_rate_of_spread`,
    `crown_fraction_burned`
- -`.DCCalc` -> `drought_code`
- `.DISTtcalc` -> `distance_at_time`
- `.dmcCalc` -> `duff_moisture_code`
- `.ffmcCalc` -> `foliar_moisture_content`
- `.FBPcalc` -> `fire_behaviour_prediction`
- `.FIcalc` -> `fire_intensity`
- `fireSeason` -> `fire_season`
- `fwiCalc` -> `fire_weather_index`
- `.FROScalc` -> `flank_rate_of_spread`
- `.FMCcalc` -> `foliar_moisture_content`
- `.ISIcalc` -> `initial_spread_index`
- `.LBtcalc` -> `length_to_breadth_at_time`
- `.LBcalc` -> `length_to_breadth`
- `wDC` -> `overwinter_drought_code`
- `.ROSthetacalc` -> `rate_of_spread_at_theta`
- `.ROStcalc` -> `rate_of_spread_at_time`
- `.ROScalc` -> `rate_of_spread`
- `.SFCcalc` -> `surface_fuel_consumption`
- `.TFCcalc` -> `total_fuel_consumption`


#### Each function in this list has been replaced with a readable version. Some functions have had their pieces extracted and have become multiple functions.

## Fixes

### Remove dependencies on archived packages 'rgdal' and 'raster'

## New

### Added automated testing of all functions

### Improved readability of functions and subfunctions

### All changes documented in the git repo: https://github.com/cffdrs/cffdrs_r
