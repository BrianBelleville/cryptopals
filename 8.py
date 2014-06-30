#!/usr/bin/env python3

import binascii
import base64
import sys

# split into 16 byte chunks and see if there are any duplicates
def has_duplicate(string):
    s = set()
    chunks = chunk_up(string)
    for c in chunks:
        if c in s:
            return True
        s.add(c)
    return False

def chunk_up(string):
    step = 16
    rval = []
    for i in range(0, len(string) - step, step):
        rval.append(string[i:i+step])
    return rval

in_file = open(sys.argv[1])
lines = in_file.read().split('\n')
print(list(map(binascii.b2a_hex, filter(has_duplicate, map(binascii.a2b_hex, lines)))))
