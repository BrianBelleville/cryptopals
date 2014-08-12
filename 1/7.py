#!/usr/bin/env python3

import binascii
import base64
import sys
from Crypto.Cipher import AES

in_file = open(sys.argv[1])
string = base64.b64decode(in_file.read().replace("\n", ""))

cipher = AES.new(b'YELLOW SUBMARINE', AES.MODE_ECB)
print(cipher.decrypt(string).decode())
