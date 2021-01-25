test_that("spring_summary_works" ,
 # Generate Climate Data where there is a 2 years each with 2 months (so 4 months in total), and each month just has one day - and we can pick one to call it spring
  # Simple way to test because its 'easy' to see what the function should return if working correctly

         
          
          
{clim_data = as.data.frame(cbind(month=c(1:4), day=rep(1, times=4), year=c(2000,2000,2001,2001), 
                                 precip=rep(0, times=4), tavg=c(2,2,2,4)))

expect_equal(spring_summary(clim_data, spring_months=4)$Pavg_spring,0)
expect_equal(spring_summary(clim_data, spring_months=4)$Tavg_spring, 4)
expect_equal(spring_summary(clim_data, spring_months=2)$Tavg_spring, 2)
expect_equal(spring_summary(clim_data, spring_months=c(1:4))$warmest_spring, 2001)

})

