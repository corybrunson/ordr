data(UCBAdmissions)
ca::mjca(UCBAdmissions)

# http://www.carme-n.org/?sec=code2
plot(ca::mjca(ca::wg93[,1:4], lambda = "indicator"))
plot(ca::mjca(ca::wg93[,1:4], lambda = "Burt"))
plot(ca::mjca(ca::wg93[,1:4], lambda = "adjusted"))
plot(ca::mjca(ca::wg93[,1:4], lambda = "JCA"))
plot(ca::mjca(ca::wg93[,1:4], subsetcat = (1:20)[-seq(3,18,5)]))
plot(ca::mjca(ca::wg93, supcol = 5:7))
