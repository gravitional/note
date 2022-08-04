#!/usr/bin/env python
# coding: utf-8

# ## 初始化

# In[ ]:


import xlrd # lib to read and write excel
from datetime import datetime as dt # lib for date and time
from datetime import date as dt2 # lib for date and time
from datetime import time as tm # generate time, e.g. 10:01:02
from datetime import timedelta


# In[ ]:


# excel 表格
book = xlrd.open_workbook("time.xls",encoding_override="GBK") # open ./time.xls, with encoding of GBK


# In[ ]:


# 打印 excel 表格的一些信息
print("The number of worksheets is {0}".format(book.nsheets))
print("Worksheet name(s): {0}".format(book.sheet_names()))
sheet = book.sheet_by_index(0)
print("{0} {1} {2}".format(sheet.name, sheet.nrows, sheet.ncols))
print("Cell A01 is {0}".format(sheet.cell_value(rowx=1, colx=1)))


# ### 节假日信息

# In[ ]:


hols=[
dt2(2022,7,2), dt2(2022,7,3),
dt2(2022,7,9), dt2(2022,7,10),
dt2(2022,7,16),dt2(2022,7,17),
dt2(2022,7,23),dt2(2022,7,24),
dt2(2022,7,30),dt2(2022,7,31)
]


# ## 从Excel表 解析数据

# In[ ]:


# 初始化字典，从第二行开始（程序中用1表示），读入信息，
dict1={}
for rowx in range(1,sheet.nrows):
    name=sheet.cell_value(rowx,0)
    number=sheet.cell_value(rowx,1)
    # 每个人的全名是 员工号_中文名
    fullname=number+"_"+name
    time=sheet.cell_value(rowx,2)
    if fullname not in dict1:
        dict1[fullname]=[]
        dict1[fullname].append(time)
    else:
        dict1[fullname].append(time)


# In[ ]:


# 解析 时刻记录 的 list, string ->datetime, 时间格式字符串参考 
# https://docs.python.org/zh-cn/3/library/datetime.html#strftime-and-strptime-format-codes
def fn_date(times):
    return map(lambda x: dt.strptime(x,"%Y/%m/%d %H:%M:%S"),times)
# dict 推导式，将每个人对应的 时间列表，解析成 datetime 对象的列表
dict2={name : list(fn_date(times))
       for name,times in dict1.items()}


# In[ ]:


# 提取出 每天所有的打卡时间
def fn_all(times):
    mydict={}
    # 以 year-month-day 为 key, 以 hour-minute-second 为 value, 保留所有时刻记录
    for t in times:
        a=t.date()
        b=t.time()
        if a not in mydict:
            mydict[a]=[b]
        else:
            mydict[a].append(b)
    return mydict
# 生成 {每个人...
#       {date...
#        [times...
dictAll={name:fn_all(times) for name,times in dict2.items()}


# In[ ]:


# 提取出 每天 最晚的时间
def fn_last(times):
    # 以 year-month-day 为 key, 以 hour-minute-second 为 value, 并只保留最晚（最大）的时间
    return {d:max(t) for d,t in times.items()}

#生成 每个人每天的最晚打卡时间 
dictLast={name:fn_last(times) for name,times in dictAll.items()}


# ## 节假日打卡间隔

# In[ ]:


dHol={}
for name,times in dictAll.items():
    # 最早打卡时间
    tmin=tm(8) #这里指定了默认的早上打卡时间，早上八点
    dMin={d:min(min(tl),tmin) for d,tl in times.items()}
    # 最晚打卡时间
    dMax={d:max(tl) for d,tl in times.items()}
    duration={}
    for da in dMax.keys():
        # 如果打卡记录在 节假日列表中
        if da in hols:
            # 计算 最早打卡与 最晚打卡 的时间差
            duration[da]=timedelta(
                days=0,
                seconds=dMax[da].second-dMin[da].second,
                microseconds=0,
                milliseconds=0,
                minutes=dMax[da].minute-dMin[da].minute,
                hours=dMax[da].hour-dMin[da].hour,
                weeks=0)
    dHol[name]=duration


# In[ ]:


# 节假日加班信息
dHolCnt={}
for name,times in dHol.items():
    dHolCnt[name]=[]
    cnt4=0;cnt8=0
    for da,delta in times.items():
        # 统计超过8小时的次数
        if delta >= timedelta(0,28800):
            cnt8+=1
        # 统计超过4小时的次数
        elif delta >= timedelta(0,14400):
            cnt4+=1
    dHolCnt[name].append(cnt4)
    dHolCnt[name].append(cnt8)


# In[ ]:


dHolCnt


# ## 统计时间在 20:00 之后的次数

# In[ ]:


def sum(x_dict):
    count=0
    tmpDict=x_dict.copy()
    for v in x_dict.values():
        if v >= tm(20):
            count=count+1
    tmpDict["count"]=count
    return tmpDict
# 生成统计数据
dCnt={name:sum(times) for name,times in dictLast.items()}


# In[ ]:


# 展示记录中的 count 信息，即超过 20:00 的次数
{name: record["count"] for name,record in sorted(dCnt.items())}

