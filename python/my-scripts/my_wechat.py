import itchat
import re


@itchat.msg_register(itchat.content.TEXT)
def text_reply(msg):
    if re.search('你好', msg['Text']):
        return '你好，我是自动回复机器人。'
    elif re.search('再见', msg['Text']):
        return '再见，祝你好运。'
    else:
        return '自动回复：我现在有事不在，稍后回复。'


itchat.auto_login(hotReload=True)
itchat.run()
