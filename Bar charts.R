install.packages("ggplot2")
library(ggplot2)

labels <- rep(c(2008, 2016), 4)
questions <- c("How likely unemployed and looking for work next 12 months?", "How likely not enough money for household necessities next 12 months?")

otherColumnData <- data.frame(
  x=labels,
  q = c(1.93, 1.58, 2.25, 2.33, 1.36, 0.994, 1.38, 1.07)
)
x.seq <- c(1,2,4,5,8,9,11,12)

colors <- rep(c("#f7bb3d","#f7bb3d","#cd5038","#cd5038"), 2)
ggplot(data=transform(otherColumnData, x=x.seq), aes(x=x, y=q, fill=colors)) +
  geom_histogram(stat = "identity") + labs(x="", y="") +
  scale_x_discrete(breaks = NULL) +
  geom_text(aes(x=x.seq, y=0, label=labels), vjust=2) +
  scale_x_continuous(breaks=c(mean(x.seq[1:4]), mean(x.seq[5:8])), labels=questions) +
  theme(text = element_text(size=15)) +
  scale_fill_discrete(name="Countries",
                    breaks=c("#f7bb3d", "#cd5038"),
                    labels=c("Hungary", "Poland"))