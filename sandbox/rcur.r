
data(STTm, package = "rCUR")
dim(STTm)

cur <- rCUR::CUR(STTm, c = 18, r = 6, k = 2)

sapply(c("C", "U", "R"), function(x) dim(attr(cur, x)))

devtools::load_all()
cur <- as_tbl_ord(cur)

apply(attr(cur, "C"), 2, norm, type = "2")
apply(attr(cur, "U"), 2, norm, type = "2")
apply(attr(cur, "U"), 1, norm, type = "2")
apply(attr(cur, "R"), 1, norm, type = "2")
