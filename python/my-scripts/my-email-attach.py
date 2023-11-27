#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# 先导入相关的库和方法
import sys  # 命令行调用参数
from pathlib import Path  # 处理文件路径和文件名
import smtplib
import email  # 邮件发送和构造
from email.mime.text import MIMEText  # 负责构造文本
from email.mime.image import MIMEImage  # 负责构造图片
from email.mime.multipart import MIMEMultipart  # 负责将多个对象集合起来
from email.header import Header

# 定义一个彩色的打印函数


def echo2(x):
    print(
        f'+++++++++++++++++++++++\n\033[1;47m\033[1;32m{str(x)}\033[0;0m\n++++++++++++++++++++ '
    )


# 命令行参数, 第二个参数, 即 sys.argv[1], 是要发送的附件的位置, 支持 ~user
attach_file = (Path(str(sys.argv[1]))).expanduser()
echo2(attach_file)  # 打印出文件的路径
attach_name = attach_file.name
echo2(attach_name)  # 打印出文件的名字

## 设置邮箱域名, 发件人邮箱, 邮箱授权码, 收件人邮箱
# SMTP服务器,
mail_host = 'smtp.qq.com'
# 发件人邮箱
mail_sender = 'xxx@qq.com'
# 邮箱授权码,注意这里不是邮箱密码,如何获取邮箱授权码,请看本文最后教程
mail_license = 'xxx'
# 收件人邮箱, 可以为多个收件人
mail_receivers = [
    'xxx@qq.com',
]

# 构建MIMEMultipart对象代表邮件本身, 可以往里面添加文本, 图片, 附件等
mm = MIMEMultipart('related')

# 设置邮件头部内容
# 邮件主题
subject_content = """Python邮件测试"""
# 设置发送者,注意严格遵守格式,里面邮箱为发件人邮箱
mm["From"] = "tom<xxx@qq.com>"
# 设置接受者,注意严格遵守格式,里面邮箱为接受者邮箱
mm["To"] = "xxx<xxx@qq.com>"
# 设置邮件主题
mm["Subject"] = Header(subject_content, 'utf-8')

# 添加正文文本
# 邮件正文内容
body_content = """你好, 这是一个测试邮件!"""
# 构造文本,参数1: 正文内容, 参数2: 文本格式, 参数3: 编码方式
message_text = MIMEText(body_content, "plain", "utf-8")
# 向MIMEMultipart对象中添加文本对象
mm.attach(message_text)

# 添加图片
# # 二进制读取图片
# image_data = open('a.jpg','rb')
# # 设置读取获取的二进制数据
# message_image = MIMEImage(image_data.read())
# # 关闭刚才打开的文件
# image_data.close()
# # 添加图片文件到邮件信息当中去
# mm.attach(message_image)

## 如果存在的话, 就添加附件, 并发送邮件
if attach_file.exists():
    atta = MIMEText(open(str(attach_file), 'rb').read(), 'base64',
                    'utf-8')  # 构造附件
    # 设置附件信息
    atta["Content-Disposition"] = f'attachment; filename={str(attach_name)}'
    mm.attach(atta)  # 添加附件到邮件信息当中去
    # 发送邮件# 创建SMTP对象
    stp = smtplib.SMTP()
    stp.connect(mail_host, 587)  # 设置发件人邮箱的域名和端口, 端口地址为465
    stp.set_debuglevel(1)  # set_debuglevel(1)可以打印出和SMTP服务器交互的所有信息
    stp.login(mail_sender, mail_license)  # 登录邮箱, 传递参数1: 邮箱地址, 参数2: 邮箱授权码
    # 发送邮件, 传递参数1: 发件人邮箱地址, 参数2: 收件人邮箱地址, 参数3: 把邮件内容格式改为str
    stp.sendmail(mail_sender, mail_receivers, mm.as_string())
    echo2("邮件发送成功")
    stp.quit()  # 关闭SMTP对象
else:
    echo2(f'there is no such file {str(attach_name)}')
