# give a score to an english plaintext

import math

# Got letter frequencys from http://www.cryptograms.org/letter-frequencies.php
letter_freq = dict()
letter_freq[b'a'[0]]= 	0.08167
letter_freq[b'b'[0]]= 	0.01492
letter_freq[b'c'[0]]= 	0.02782
letter_freq[b'd'[0]]= 	0.04253
letter_freq[b'e'[0]]= 	0.12702
letter_freq[b'f'[0]]= 	0.02228
letter_freq[b'g'[0]]= 	0.02015
letter_freq[b'h'[0]]= 	0.06094
letter_freq[b'i'[0]]= 	0.06966
letter_freq[b'j'[0]]= 	0.00153
letter_freq[b'k'[0]]= 	0.00772
letter_freq[b'l'[0]]= 	0.04025
letter_freq[b'm'[0]]= 	0.02406
letter_freq[b'n'[0]]= 	0.06749
letter_freq[b'o'[0]]= 	0.07507
letter_freq[b'p'[0]]= 	0.01929
letter_freq[b'q'[0]]= 	0.00095
letter_freq[b'r'[0]]= 	0.05987
letter_freq[b's'[0]]= 	0.06327
letter_freq[b't'[0]]= 	0.09056
letter_freq[b'u'[0]]= 	0.02758
letter_freq[b'v'[0]]= 	0.00978
letter_freq[b'w'[0]]= 	0.02360
letter_freq[b'x'[0]]= 	0.00150
letter_freq[b'y'[0]]= 	0.01974
letter_freq[b'z'[0]]= 	0.00074


# letter_freq['e']= 	0.12702
# letter_freq['t']= 	0.09056
# letter_freq['a']= 	0.08167
# letter_freq['o']= 	0.07507
# letter_freq['i']= 	0.06966
# letter_freq['n']= 	0.06749
# letter_freq['s']= 	0.06327
# letter_freq['h']= 	0.06094
# letter_freq['r']= 	0.05987
# letter_freq['d']= 	0.04253
# letter_freq['l']= 	0.04025

def add_or_increment(count_dict, letter):
    try:
        count_dict[letter]+=1
    except KeyError:
        count_dict[letter]=1
        
def to_freq_dict(count_dict, total):
    freq_dict = dict()
    for letter in count_dict:
        count = count_dict[letter]
        freq = count/total
        freq_dict[letter] = freq
    return freq_dict

# returns +inf if string contains characters outside of the printable
# ascii range, 0 otherwise
def contains_non_printable_error(string):
    for char in string:
        if char < 0x20 or char > 0x7e:
            return float('+inf')
    return 0.0

# give this the lowercase letters in plaintext
def letter_error(string):
    count_dict = dict()
    # only work on the letters
    work = filter(lambda char: char >= 0x61 and char <= 0x7a, string.lower())
    length = 0
    for char in work:
        add_or_increment(count_dict, char)
        length += 1
    freq_dict = to_freq_dict(count_dict, length)
    error = 0
    for key in letter_freq:
        freq = letter_freq[key]
        try:
            found_freq = freq_dict[key]
        except KeyError:
            found_freq = 0
        diff = abs(freq - found_freq)
        error += diff
    return error

# acording to wolfram alpha, average word length is 5, so about 1/6
# letters should be spaces
ideal_space = 1/6
def space_error(string):
    b = len(string)
    a = len(string.replace(b' ', b''))
    return ideal_space - (b-a)/b

# quantify the error in letter frequency, lower scores are better
def score(string):
    return letter_error(string) + space_error(string) + contains_non_printable_error(string)
