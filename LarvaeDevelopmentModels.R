### possible models describing the density dependent death and growth rate of larave for the development project ----

hill<-function(L, h = 50)
{
  H<-L/(L+h)
  data.frame(L,H)
}

growth<-function(K=500, L )
{
  #G<-1 - 0.35*L/(K)
  G<-1-(L/(L+K))
  data.frame(L,G)
}

L<-seq(0,1000)

G<-growth(L=L)$G
total<-cbind(hill(L=L), G)

ggplot(total, aes(x = L, y = H, colour = "H")) + geom_line() +geom_line(aes(x = L, y = G, colour = "G")) + geom_line(aes(x = L, y = H*G, colour = "total"), size = 1.5) +theme_bw(base_size = 24) +ylab("growth rate / day") + xlab(expression(paste("Larvae density / 700 ml ",H[2],"O"))) +scale_color_manual(values = c("black", "orange", "darkblue"), labels = c("Competition", "Collaboration", "Total"))+ guides(color=guide_legend("")) 