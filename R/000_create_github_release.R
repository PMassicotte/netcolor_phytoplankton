# Create a GitHub release with the data and figures.

library(piggyback)

pb_releases()

pb_new_release(tag = "v1.0.0")

tmp <- "~/Desktop/data.zip"
zip(tmp, fs::dir_ls("data/", recurse = TRUE, type = "file"))
pb_upload(tmp, overwrite = TRUE, tag = "v1.0.0")

tmp <- "~/Desktop/figures.zip"
zip(tmp, fs::dir_ls("graphs/", recurse = TRUE, type = "file"))
pb_upload(tmp, overwrite = TRUE, tag = "v1.0.0")
