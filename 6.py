#!/usr/bin/env python3

import binascii
import base64
import sys
import score


BitsSetTable256 = [0]
for i in range(1, 256):
    BitsSetTable256.append( (i & 1) + BitsSetTable256[i >> 1])

def byte_hamming_distance(b1, b2):
    if b1 > 255 or b1 < 0:
        raise Exception("b1 not a byte")
    if b2 > 255 or b2 < 0:
        raise Exception("b2 not a byte")
    return BitsSetTable256[b1 ^ b2]
    
def string_hamming_distance(s1, s2):
    return sum(map(byte_hamming_distance, s1, s2))

def find_key_size(string):
    min_distance = float('+inf')
    size = 0
    for i in range(2,40):
        s = 0
        for j in range(0,12,2):
            s += string_hamming_distance(string[i*j:i*(j+1)], string[i*(j+1):i*(j+2)])
        d = s / i
        if d < min_distance:
            min_distance = d
            size = i
    return size

def split_string(string, keysize):
    split = []
    for i in range(0, keysize):
        split.append([])
    for i in range(0, len(string)):
        split[i % keysize].append(string[i])
    return split

def solve_single_key(string):
    min_score = float('+inf')
    plaintext = ""
    for i in range(0,255):
        p = bytes(map(lambda c: i ^ c, string))
        s = score.score(p)
        if s < min_score:
            min_score = s
            plaintext = p
    return plaintext

in_file = open(sys.argv[1])
string = base64.b64decode(in_file.read().replace("\n", ""))
key_size = find_key_size(string)
# print("key size:", key_size)
work = split_string(string, key_size)
# print(work)
solve = list(map(solve_single_key, work))

tot_char = len(string)
out = bytes([x for t in zip(*solve) for x in t])
print(out.decode())
