#!/usr/bin/env python2.7
#
#copyright (c) 2015 Ryan James Spencer
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
#
# A python translation of `haskdiff`
#

from itertools import count


def lcs(a, b):
    """longest common subsequence of sequences `a` and `b`"""
    ret_lcs = ''
    if len(a) < len(b):
        seqa, seqb = a, b
    else:
        seqa, seqb = b, a
    for i, ca in enumerate(seqa):
        cb = seqb[i]
        if ca == cb:
            ret_lcs += ca
    return list(ret_lcs)


def diff(a, b):
    a, b = list(a), list(b)

    def diff_do(a, b, c, n):
        if not (a or b or c):
            return []
        if a and not (b or c):
            return zip(map(lambda c: '- ' + c, a), count(n))
        elif b and not (a or c):
            return zip(map(lambda c: '- ' + c, b), count(n))
        else:
            ah = a[0]
            bh = b[0]
            if ah not in c:
                return ('- ' + ah, n) + diff_do(a[n:], b, c, (n+1))
            elif bh not in c:
                return ('+ ' + bh, n) + diff_do(a, b[n:], c, (n+1))
            else:
                return diff_do(a[n:], b[n:], c[n:], (n+1))
    return diff_do(a, b, lcs(a, b), 0)


if __name__ == '__main__':
    seqa = 'I am a'
    seqb = 'I am amtrack'
    print("To make `{}` look like `{}`".format(seqa, seqb))
    print(diff(seqa, seqb))
