library(tidyverse)
library(ggpubr)


data1 <- read.table(file = "clipboard", header = F)
colnames(data1) <- c("Conc", "Abs", "Viability")
data1 <- data1 %>% mutate(logConc = log10(data1$Conc))

p_MA <- ggplot(data=data1, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Apigenin on Marble Goby MuSCs")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=15, y=0.55, 
           label=expression("LC50 = 11.97" ~ mu*"M"))+
  theme_classic()

p_MA

data_MC <- read.table(file = "clipboard", header = F)
colnames(data_MC) <- c("Conc", "Abs", "Viability")

p_MC <- ggplot(data=data_MC, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Chrysin on Marble Goby MuSCs")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=30, y=0.55, 
           label=expression("LC50 = 19.38" ~ mu*"M"))+
  theme_classic()

p_MC

data_MG <- read.table(file = "clipboard", header = F)
colnames(data_MG) <- c("Conc", "Abs", "Viability")

p_MG <- ggplot(data=data_MG, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Galangin on Marble Goby MuSCs")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=30, y=0.55, 
           label=expression("LC50 = 24.91" ~ mu*"M"))+
  theme_classic()



data_ML <- read.table(file = "clipboard", header = F)
colnames(data_ML) <- c("Conc", "Abs", "Viability")

p_ML <- ggplot(data=data_ML, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Luteolin on Marble Goby MuSCs")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=30, y=0.55, 
           label=expression("LC50 = 10.11" ~ mu*"M"))+
  theme_classic()

p_ML


figure <- ggarrange(p_MA, p_MC, p_MG, p_ML,
                    labels = c("A","B","C","D"),
                    ncol = 2, nrow = 2)

figure

data_bfA <- read.table(file = "clipboard", header = F)
colnames(data_bfA) <- c("Conc", "Abs", "Viability")

p_bfA <- ggplot(data=data_ML, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Apigenin on BF-2 cells")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=30, y=0.55, 
           label=expression("LC50 = 12.53" ~ mu*"M"))+
  theme_classic()

p_bfA


data_bfA <- read.table(file = "clipboard", header = F)
colnames(data_bfA) <- c("Conc", "Abs", "Viability")

p_bfA <- ggplot(data=data_bfA, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Apigenin on BF-2 cells")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=30, y=0.55, 
           label=expression("LC50 = 12.53" ~ mu*"M"))+
  theme_classic()

p_bfA


data_bfC <- read.table(file = "clipboard", header = F)
colnames(data_bfC) <- c("Conc", "Abs", "Viability")

p_bfC <- ggplot(data=data_bfC, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Chrysin on BF-2 cells")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=20, y=0.55, 
           label=expression("LC50 = 14.19" ~ mu*"M"))+
  theme_classic()

p_bfC



data_bfG <- read.table(file = "clipboard", header = F)
colnames(data_bfG) <- c("Conc", "Abs", "Viability")

p_bfG <- ggplot(data=data_bfG, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Galangin on BF-2 cells")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=30, y=0.55, 
           label=expression("LC50 = 13.31" ~ mu*"M"))+
  theme_classic()

p_bfG




data_bfL <- read.table(file = "clipboard", header = F)
colnames(data_bfL) <- c("Conc", "Abs", "Viability")

p_bfL <- ggplot(data=data_bfL, aes(x=Conc, y=Viability))+
  geom_point()+
  scale_x_log10(breaks=c(0, 0.5, 2.5, 5, 12.5, 25, 50, 100))+
  scale_y_continuous(labels = scales::percent)+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "LC50 of Luteolin on BF-2 cells")+
  geom_smooth(method='auto', se=FALSE, col='blue', linewidth=0.5)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  annotate("text", x=10, y=0.55, 
           label=expression("LC50 = 2.14" ~ mu*"M"))+
  theme_classic()

p_bfL

figure_bf_acgl <- ggarrange(p_bfA, p_bfC, p_bfG, p_bfL,
                    labels = c("A","B","C","D"),
                    ncol = 2, nrow = 2)

figure_bf_acgl



data_MB <- read.table(file = "clipboard", header = F)
colnames(data_MB) <- c("Conc", "Abs", "Viability")

data_MB$Conc <- as.character(data_MB$Conc) 
data_MB$Conc <- factor(data_MB$Conc, 
                       levels = c("0.5","2.5","5","12.5","25","50","100"))


p_MB <- ggplot(data = data_MB, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Baicalein on Marble Goby MuSCs")+
  theme_classic()

p_MB


data_MQ <- read.table(file = "clipboard", header = F)
colnames(data_MQ) <- c("Conc", "Abs", "Viability")

data_MQ$Conc <- as.character(data_MQ$Conc) 
data_MQ$Conc <- factor(data_MQ$Conc, 
                       levels = c("0.5","2.5","5","12.5","25","50","100"))


p_MQ <- ggplot(data = data_MQ, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Quercetin on Marble Goby MuSCs")+
  theme_classic()

p_MQ



data_M7 <- read.table(file = "clipboard", header = F)
colnames(data_M7) <- c("Conc", "Abs", "Viability")

data_M7$Conc <- as.character(data_M7$Conc) 
data_M7$Conc <- factor(data_M7$Conc, 
                       levels = c("0.5","2.5","5","12.5","25","50","100"))


p_M7 <- ggplot(data = data_M7, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Flavone on Marble Goby MuSCs")+
  theme_classic()

p_M7



data_M8 <- read.table(file = "clipboard", header = F)
colnames(data_M8) <- c("Conc", "Abs", "Viability")

data_M8$Conc <- as.character(data_M8$Conc) 
data_M8$Conc <- factor(data_M8$Conc, 
                       levels = c("0.5","2.5","5","12.5","25","50","100"))


p_M8 <- ggplot(data = data_M8, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Flavanone on Marble Goby MuSCs")+
  theme_classic()

p_M8

figure_SH_BQ78 <- ggarrange(p_MB, p_MQ, p_M7, p_M8,
                            labels = c("A","B","C","D"),
                            ncol = 2, nrow = 2)

figure_SH_BQ78


data_bfB <- read.table(file = "clipboard", header = F)
colnames(data_bfB) <- c("Conc", "Abs", "Viability")

data_bfB$Conc <- as.character(data_bfB$Conc) 
data_bfB$Conc <- factor(data_bfB$Conc, 
                       levels = c("0.5","2.5","5","12.5","25","50","100"))


p_bfB <- ggplot(data = data_bfB, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Baicalein on BF-2 cells")+
  theme_classic()

p_bfB


data_bfQ <- read.table(file = "clipboard", header = F)
colnames(data_bfQ) <- c("Conc", "Abs", "Viability")

data_bfQ$Conc <- as.character(data_bfQ$Conc) 
data_bfQ$Conc <- factor(data_bfQ$Conc, 
                        levels = c("0.5","2.5","5","12.5","25","50","100"))


p_bfQ <- ggplot(data = data_bfQ, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Quercetin on BF-2 cells")+
  theme_classic()

p_bfQ


data_bf7 <- read.table(file = "clipboard", header = F)
colnames(data_bf7) <- c("Conc", "Abs", "Viability")

data_bf7$Conc <- as.character(data_bf7$Conc) 
data_bf7$Conc <- factor(data_bf7$Conc, 
                        levels = c("0.5","2.5","5","12.5","25","50","100"))


p_bf7 <- ggplot(data = data_bf7, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Flavone on BF-2 cells")+
  theme_classic()

p_bf7


data_bf8 <- read.table(file = "clipboard", header = F)
colnames(data_bf8) <- c("Conc", "Abs", "Viability")

data_bf8$Conc <- as.character(data_bf8$Conc) 
data_bf8$Conc <- factor(data_bf8$Conc, 
                        levels = c("0.5","2.5","5","12.5","25","50","100"))


p_bf8 <- ggplot(data = data_bf8, aes(x=Conc, y=Viability))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = scales::percent(Viability)), vjust = 1.5, colour = "black")+
  labs(x=expression("Concentrations (" ~ mu*"M)"),
       y="Viability",
       title = "Cell viability of Flavanone on BF-2 cells")+
  theme_classic()

p_bf8

figure_bf2_BQ78 <- ggarrange(p_bfB, p_bfQ, p_bf7, p_bf8,
                            labels = c("A","B","C","D"),
                            ncol = 2, nrow = 2)

figure_bf2_BQ78

