#加载包和数据
library(readxl)
library(dplyr)
library(linkET)
library(RColorBrewer)
library(ggplot2)
library(cols4all)
datainfer=read_excel("datainfer.xlsx")
try=datainfer[,c(41:47)]
condition=datainfer[,c(2:40)]
#进行mantel test并新增连线数据
mantel <- mantel_test(condition,try,
                      spec_select = list(Reductions = 2:6,
                                         Score = 7:12,
                                         Word = 13:17,
                                         RepeatN = 18,
                                         WordCharacter=19,
                                         Topics=20:25,
                                         Description=26:31,
                                         Attitude=32:37,
                                         ReportNumber=38,
                                         HardMode=39)) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))


#画图
qcorrplot(correlate(try), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), 
              data = mantel, 
              curvature = nice_curvature()) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "BrBG")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))



library("dplyr")
library(linkET)
library(RColorBrewer)
library(ggplot2)
library(cols4all)




qcorrplot(correlate(try), type = "lower", diag = FALSE) +
  geom_square() +geom_mark(size = 2.8, #显著性标记大小
                           only_mark = T, #只显示显著性标记,逻辑值为F则会同时显示具体数值
                           sig_level = c(0.05, 0.01, 0.001), # 显著性水平
                           sig_thres = 0.05)+
  geom_couple(aes(colour = pd, size = rd), # 这行代码是关键
              data = mantel, 
              curvature = nice_curvature()) +scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "BrBG"))+
  
  # 下面就是各种颜色和名称设置
  
  scale_size_manual(values = c(0.5, 1.2, 2)) + #连线粗细
  scale_colour_manual(values = color_pal(3)) + #连线配色
  #修改图例：
  guides(size = guide_legend(title = "Mantel r",
                             override.aes = list(colour = "grey35"),
                             order = 2),
         colour = guide_legend(title = "Mantel p",
                               override.aes = list(size = 3),
                               order = 1),
         fill = guide_colorbar(title = "Pearson r", order = 3))+
  labs(title=paste("Correlation between word factor and number of guesses"))

