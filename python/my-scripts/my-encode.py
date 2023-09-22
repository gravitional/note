print()
a = "我们的世界由原子组成".encode('utf-8') #编码成 utf8
print(f'To Bytes: {a}')  # a 的 bytes 序列
b = a.hex()  # 转换成 hex序列, 去掉 \x 标记
print(f'To pure hex sequence: {b}')
c = bytes.fromhex(b) # 转换成 byte, 即加上 \x 标记
print(f'To Bytes again: {c}') 
d = c.decode("utf-8") # 解码回 utf8
print(f'convert back: {d}')
