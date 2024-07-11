download.file("https://zenodo.org/record/6567608/files/Data_S1.zip?download=1",
              "data-raw/RNDB.v4.zip", mode = "wb")

unzip("data-raw/RNDB.v4.zip", exdir = "data-raw")
file.remove("data-raw/RNDB.v4.zip")

RecruitNet <- as.data.frame(readr::read_csv("data-raw/RecruitNet.csv"))
CanopyCover <- as.data.frame(readr::read_csv("data-raw/CanopyCover.csv"))

usethis::use_data(RecruitNet, overwrite = TRUE)
usethis::use_data(CanopyCover, overwrite = TRUE)

# data("RecruitNet")
# data("CanopyCover")

