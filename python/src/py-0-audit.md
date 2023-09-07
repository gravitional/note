# python 审计事件 audit event

[How to set up and use python audit hooks](https://stackoverflow.com/questions/63350394/how-to-set-up-and-use-python-audit-hooks)
[Audit events table](https://docs.python.org/3/library/audit_events.html)
[释放Python 3.8 Walrus运算符的威力](https://zhuanlan.zhihu.com/p/87977676)

## 运行时审计钩子 Runtime audit hooks

[PEP 578 – Python Runtime Audit Hooks](https://peps.python.org/pep-0578/)

这显然是出于安全目的.
下面是一个简单的例子:

```python
import sys
import requests

def audit(event, args):
    if 'socket' in event:
        if event == 'socket.connect':
            raise RuntimeError('Alright, this is too much')
        print(f'Captain, some networking going on: event={event}, args={args}')
    else:
        print(f'Not very interesting event: {event}')

def do_something_nasty():
    return requests.get('https://soso.com')

sys.addaudithook(audit)
do_something_nasty()
```

让我们运行它:

```bash
Not very interesting event: import
...
Captain, some networking going on: event=socket.gethostbyname, args=('en.wikipedia.org',)
...
Traceback (most recent call last):
...
RuntimeError: Alright, this is too much
Not very interesting event: cpython._PySys_ClearAuditHooks
```

在写这篇文章的时候, CPython 运行时触发的内置事件还没有文档记录,
但这里有[文档框架](https://docs.python.org/3.11/library/audit_events.html).
PEP 578 提到了使用审计钩子的明显缺点:
一旦开发人员添加了审计钩子, 他们就明确选择了以性能换功能.

还可以使用自定义事件:

```python
import sys

def audit(event, args):
    if event == 'my-event':
        print(f'audit: {event} with args={args}')

sys.addaudithook(audit)
sys.audit('my-event', 'foo', 'bar', 123)

# Output:
# audit: my-event with args=('foo', 'bar', 123)
```

预期用例明显偏重于安全性.
一旦 3.8 版本发布, 我预计会有许多新的安全相关工具/库大量使用新的内置审计钩子功能.
也许一些现有的库也会为库本身触发的 审计事件 添加选择功能.
