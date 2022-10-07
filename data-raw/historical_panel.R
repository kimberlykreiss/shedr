

shed_2013<-read_sheddata(2013)
shed_2013 <- shed_2013[c("CaseID", "weight", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h", "EF3_i")]
shed_2013$year <- 1
shed_2013 <- rename(shed_2013, final_weight = weight)

shed_2014<-read_sheddata(2014)
shed_2014 <- shed_2014[c("CaseID", "weight3", "ppethm", "ppmsacat", "B2", "B3", "ED0", "E3A_a",
                         "E3A_b", "E3A_c",  "E3A_d",  "E3A_e", "E3A_f", "E3A_g", "E3A_h", "E3A_i")]
shed_2014$year <- 2
shed_2014 <- rename(shed_2014, final_weight = weight3)
shed_2014 <- setnames(shed_2014, old =c("E3A_a", "E3A_b", "E3A_c",  "E3A_d",  "E3A_e", "E3A_f", "E3A_g", "E3A_h", "E3A_i"),
                      new =c("EF3_a","EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h", "EF3_i"))

shed_2015<-read_sheddata(2015)
shed_2015 <- shed_2015[c("CaseID", "weight3", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h", "EF3_i")]
shed_2015$year <- 3
shed_2015 <- rename(shed_2015, final_weight = weight3)

shed_2016<-read_sheddata(2016)
shed_2016 <- shed_2016[c("CaseID", "weight3b", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h", "EF3_i")]
shed_2016$year <- 4
shed_2016 <- rename(shed_2016, final_weight = weight3b)

shed_2017<-read_sheddata(2017)
shed_2017 <- shed_2017[c("CaseID", "weight3b", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h", "EF3_i")]
shed_2017$year <- 5
shed_2017 <- rename(shed_2017, final_weight = weight3b)

shed_2018<-read_sheddata(2018)
shed_2018 <- shed_2018[c("CaseID", "weight2b", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h", "EF3_i")]
shed_2018$year <- 6
shed_2018 <- rename(shed_2018, final_weight = weight2b)

shed_2019<-read_sheddata(2019)
shed_2019 <- shed_2019[c("CaseID", "weight_pop", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h")]
shed_2019$year <- 7
shed_2019 <- rename(shed_2019, final_weight = weight_pop)
shed_2019$EF3_i <- "."

shed_2020<-read_sheddata(2020)
shed_2020 <- shed_2020[c("CaseID", "weight_pop", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h")]
shed_2020$year <- 8
shed_2020 <- rename(shed_2020, final_weight = weight_pop)
shed_2020$EF3_i <- "."

shed_2021<-read_sheddata(2021)
shed_2021 <- shed_2021[c("CaseID", "weight_pop", "ppethm", "ppmsacat", "B2", "B3", "ED0", "EF3_a",
                         "EF3_b", "EF3_c",  "EF3_d",  "EF3_e", "EF3_f", "EF3_g", "EF3_h")]
shed_2021$year <- 9
shed_2021 <- rename(shed_2021, final_weight = weight_pop)
shed_2021$EF3_i <- "."



panel<-rbind(shed_2013, shed_2014)

panel<-rbind(shed_2013, shed_2014, shed_2015, shed_2016, shed_2017, shed_2018, shed_2019, shed_2020, shed_2021)
