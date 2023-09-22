#!/usr/bin/env python
# coding: utf-8

# In[21]:


from openpyxl import load_workbook
import smtplib
from email.mime.text import MIMEText
from email.header import Header
import datetime

'''
1.获取excel表的数据
2.编写邮件内容
3.发送邮件
'''


# In[22]:


# 发送邮件
class Test:
    def ck_log(self):
        pass

    def send_email(self, econtent, ename, mail):
        host = 'smtp.exmail.qq.com'
        user = 'mingyang.yang@ibe.cn'
        password = 'XXXXXX'
        receivers = [mail]
        subject = '员工工资表'
        msg = MIMEText(econtent, 'html', 'utf-8')
        msg['From'] = Header('有限公司')
        msg['To'] = Header(ename)
        msg['Subject'] = Header(subject, 'utf-8')

        try:
            obj = smtplib.SMTP_SSL(host, 465)
            obj.login(user, password)
            obj.sendmail(user, receivers, msg.as_string())
            print("邮件发送成功！")
        except smtplib.SMTPException as e:
            print("Error: 无法发送邮件")
            print(e)


# In[24]:


if __name__ == '__main__':
    wb = load_workbook('data.xlsx')
    o = Test()
    cnt = 0
    sheet = wb.active
    dt_now = datetime.datetime.now()
    duration = f'{dt_now.year}年{dt_now.month-1}月1日--{dt_now.year}年{dt_now.month}月1日'
    thead = '<thead>'
    #  1.获取excel表的数据
    for row in sheet:
        tbody = '<tr>'
        cnt += 1
        # 制作表头; 姓名, 邮箱, 年龄, 成绩
        if cnt == 1:
            for cell in row:
                thead += f'<th>{cell.value}</th>'
            thead += '</thead>'
        # 制作每一列; xiaoli, xiaoli@qq.com, 22, 22
        else:
            for cell in row:
                tbody += f'<td>{cell.value}</td>'
            tbody += '</tr>'
        name = row[0].value  # 姓名
        mail = row[1].value  # 第 n个人 的 邮箱地址
        #  2.编写邮件内容
        content = f'''
            <h3>{name},你好</h3>
            <p>请查收你在 {duration} 的工资</p>
            <table border='1px solid black'>
            {thead}
            {tbody}
            </table>
        '''
        #  3.发送邮件
        if cnt == 3:
            print('content:', content)
            print(name, mail)
            o.send_email(content, name, mail)
