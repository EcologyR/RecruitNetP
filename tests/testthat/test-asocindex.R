test_that("Study_site are the same in the two datasets of interactions and cover", {

  interp<-data.frame(Study_site=c("A","A","B"),Canopy=c("C1","C2","C2"),Recruit=c("R1","R2","R2"),Frequency=c(2,3,2))
  coverp<-data.frame(Study_site=c("A","A","B"),Canopy=c("C2","C1","C2"),Cover=as.numeric(c(NA,NA,3)),Plot=c("1_1","1_1","A"), Sampled_distance_or_area=c(2,4,3))
  expect_warning (asocindex(interp,coverp)) #expect warning if for some Study_site there is no information about the canopy of any species


})
