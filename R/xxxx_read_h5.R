library(hdf5r)
res <- hdf5r::h5file(here::here("data/raw/BGCP_clima2002_2015.h5"))

list.attributes(res)

res2 <- res$open("BGP_NWA")

res2 <- res[["BGP_NWA"]]

mat <- res2[1:1360, 1:1742]

plot(mat)

res$ls()


h5attr(res, "rownames")
