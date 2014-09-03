#!/usr/bin/env python3

import binascii
import base64
import sys

f,s = sys.argv[1:3]

if(len(f)!=len(s)):
    print("error")
    exit()

fb = binascii.a2b_hex(f)
sb = binascii.a2b_hex(s)
out = []

for i in range(0,len(fb)):
    out.append(fb[i] ^ sb[i])

print(binascii.b2a_hex(bytes(out)).decode())
