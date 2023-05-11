library(ahp)
library(data.tree)
setwd("D:\MGR\lab2\APU")
treeAhp <- Load("MacBooki.ahp")

print(treeAhp, filterFun = isNotLeaf)

Calculate(treeAhp)
print(treeAhp, priority = function(x) x$parent$priority["Total", x$name])
Visualize(treeAhp)
Analyze(treeAhp)
AnalyzeTable(treeAhp)
