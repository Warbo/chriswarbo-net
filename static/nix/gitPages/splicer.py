#!/usr/bin/env python3
import os
import sys
pre, post = sys.stdin.read().split('READMESENTINEL')
print(pre)
print(open(os.getenv('READMEFILE'), 'r').read())
print(post)
