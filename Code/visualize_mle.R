


# Plot simulated prevalence through time:

ggplot() +
  geom_line(data=simDat, aes(x=time, y=P))+
  geom_point(data= myDat, aes(x=time, y=sampPrev)) + 
  geom_errorbar(data= myDat, aes(x=time, ymin=lci, ymax=uci)
  ) +
  theme_bw() +
  labs(y="Prevalence", x="Days")