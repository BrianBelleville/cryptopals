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
        d = (byte_hamming_distance(string[0:i], string[i:2*i]) +
        byte_hamming_distance(string[2*i:3*i])) / (2*i)
        if d < min_distance:
            min_distance = d
            size = i

def split_string(string, keysize):
    split = []
    for i in range(0, keysize):
        split.append([])
    for i in range(0, len(string)):
        split[i % keysize].append(string[i])
    return split

def solve_single_key(string):
    min_score = float('+inf')
    for i in range(0,255):
        p = bytes(map(lambda c: i ^ c, string))
        # print(p, ".. from ", i)
        s = score.score(p)
        if s < min_score:
            min_score = s
            plaintext = p
    return plaintext

