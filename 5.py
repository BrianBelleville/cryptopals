#!/usr/bin/env python3

import binascii
import base64
import sys
import score

# encrypt/decrypt the contents of FILE with KEY and print result to stdout
if(len(sys.argv) != 3):
    print("usage:", sys.argv[0], "KEY FILE")

key = sys.argv[1].encode('utf-8')
key_len = len(key)

in_file = open(sys.argv[2])
string = in_file.read().encode('utf-8')
in_file.close()
string_len = len(string)

out = []

for i in range(0, string_len):
    out.append(string[i] ^ key[i % key_len])

print(binascii.b2a_hex(bytes(out)))

