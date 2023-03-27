download.file("https://zenodo.org/record/6567608/files/Data_S1.zip?download=1", "data-raw/RNDB.v4.zip", mode = "wb")

unzip("data-raw/RNDB.v4.zip", exdir = "data-raw")

RN <- as.data.frame(readr::read_csv("data-raw/RecruitNet.csv"))
RNCover <- as.data.frame(readr::read_csv("data-raw/CanopyCover.csv"))

usethis::use_data(RN, overwrite = TRUE)
usethis::use_data(RNCover, overwrite = TRUE)

# data("RN")
# data("RNCover")

