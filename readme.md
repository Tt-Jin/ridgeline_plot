# 山脊图

###### 山脊图又称峰峦图、山峦图、嵴线图，英文常称为 ridgeline Plot, Joy Plot。

#### 1. 读入绘图数据

- 使用 gather() 函数将数据转换为长格式

- 使用 rename() 函数将修改键名

#### 2. 绘图

- 使用 geom_density_ridges() 函数绘制核心 RMDS 密度图。

- geom_signif() 函数用于在图上添加显著性标记，comparisons 参数指定了要进行比较的组合，map_signif_level 参数用于指定显著性水平对应的符号，p 值小于 0.001 的用 3个星号标记，p 值小于 0.01 的用 2个星号标记，p 值小于 0.05 的用1个星号标记。

- 在 theme() 函数中设置 Y 轴样式，margin = margin(0,-1.8,1,1,'cm') 这个参数控制文本标签与轴线之间的间距。margin() 函数用于指定上、右、下、左四个方向的间距值。

- ggarrange() 组合图形，ncol = 1表示以一列的形式排列。

#### 

### 最重要的是：

码代码不易，如果你觉得我的教程对你有帮助，请**小红书**(号**4274585189**)关注我！！！
