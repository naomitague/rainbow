test_that("spring_summary_works" ,
{clim_data = as.data.frame(cbind(month=c(1:4), day=rep(1, times=4), year=c(2000,2000,2001,2001), 
                                 precip=rep(0, times=4), tavg=c(2,2,2,4)))

expect_equal(spring_summary(clim_data, spring_months=4)$Pavg_spring,0)
expect_equal(spring_summary(clim_data, spring_months=4)$Tavg_spring, 4)
expect_equal(spring_summary(clim_data, spring_months=2)$Tavg_spring, 2)
expect_equal(spring_summary(clim_data, spring_months=c(1:4))$warmest_spring, 2001)

})

