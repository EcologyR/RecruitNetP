download.file("https://zenodo.org/record/6567608/files/Data_S1.zip?download=1",
              "data-raw/RNDB.v4.zip", mode = "wb")

unzip("data-raw/RNDB.v4.zip", exdir = "data-raw")
file.remove("data-raw/RNDB.v4.zip")


RecruitNet <- readr::read_csv("data-raw/RecruitNet.csv",
                              locale = locale(encoding = "ISO-8859-1"))
readr::write_csv(RecruitNet, "data-raw/RecruitNet.csv")
RecruitNet <- as.data.frame(readr::read_csv("data-raw/RecruitNet.csv"))
usethis::use_data(RecruitNet, overwrite = TRUE)


CanopyCover <- as.data.frame(readr::read_csv("data-raw/CanopyCover.csv"),
                             locale = locale(encoding = "ISO-8859-1"))
readr::write_csv(CanopyCover, "data-raw/CanopyCover.csv")
CanopyCover <- as.data.frame(readr::read_csv("data-raw/CanopyCover.csv"))
usethis::use_data(CanopyCover, overwrite = TRUE)

# data("RecruitNet")
# data("CanopyCover")

