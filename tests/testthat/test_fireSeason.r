test_that("fireSeason", {
  library(cffdrs)
  #The standard test data:
  data("test_wDC")
  print(head(test_wDC))
  ## Sort the data:
  input <- with(test_wDC, test_wDC[order(id,yr,mon,day),])
  
  #Using the default fire season start and end temperature 
  #thresholds:
  a_fs <- fireSeason(input[input$id==1,])
  
  #Check the result:
  a_fs
  
  #    yr mon day fsdatetype
  #1 1999   5   4      start
  #2 1999   5  12        end
  #3 1999   5  18      start
  #4 1999   5  25        end
  #5 1999   5  30      start
  #6 1999  10   6        end
  #7 2000   6  27      start
  #8 2000  10   7        end
  
  #In the resulting data frame, the fire season starts 
  #and ends multiple times in the first year. It is up to the user #for how to interpret this.
  
  #modified fire season start and end temperature thresholds
  a_fs <- fireSeason (input[input$id==1,],fs.start=10, fs.end=3)
  a_fs
  #    yr mon day fsdatetype
  #1 1999   5   2      start
  #2 1999  10  20        end
  #3 2000   6  16      start
  #4 2000  10   7        end
  #select another id value, specify method explicitly
  b_fs <- fireSeason(input[input$id==2,],method="WF93")
  #print the calculated fireseason
  b_fs
  #   yr mon day fsdatetype
  #1 1980   4  21      start
  #2 1980   9  19        end
  #3 1980  10   6      start
  #4 1980  10  16        end
  #5 1981   5  21      start
  #6 1981  10  13        end
})
