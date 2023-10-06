#!/usr/bin/env python3
import bleach
import sys

print(bleach.clean(
    sys.stdin.read(),
    tags=['a', 'b', 'i', 'emph', 'strong', 'h1', 'h2', 'h3', 'h4',
          'img', 'p'],
    attributes={
        'a'   : ['href', 'rel'],
        'img' : ['alt',  'src'],
    }
))
