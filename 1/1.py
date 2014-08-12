#!/usr/bin/env python3

import binascii
import base64
import sys

t = sys.argv[1]

# convert hex string to bytes, then convert to base 64 and print
print(base64.b64encode(binascii.a2b_hex(t)).decode())
