#!/usr/bin/env python3

import binascii
import base64
import sys
import score

min_error = float('+inf')
plaintext = b''

def try_decrypt(string):
    min_score = float('+inf')
    plaintext = b''
    for i in range(0,255):
        p = bytes(map(lambda c: i ^ c, string))
        s = score.score(p)
        if s < min_score:
            min_score = s
            plaintext = p
    return (min_score, plaintext)
    

for line in sys.stdin:
    err, plain = try_decrypt(binascii.a2b_hex(line.strip()))
    print(plain)
    if(err < min_error):
        err = min_error
        plaintext = plain

print(plaintext)
