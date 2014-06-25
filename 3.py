#!/usr/bin/env python3

import binascii
import base64
import sys
import score

cipher = binascii.a2b_hex(sys.argv[1])
min_score = float('+inf')

for i in range(0,255):
    p = bytes(map(lambda c: i ^ c, cipher))
    # print(p, ".. from ", i)
    s = score.score(p)
    if s < min_score:
        min_score = s
        plaintext = p
        
print(plaintext)

