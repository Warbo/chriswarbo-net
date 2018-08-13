import os
import subprocess
import timeit

def time_eval_wholeSite():
    subprocess.check_call([os.getenv('eval_wholeSite')])

time_eval_wholeSite.timer   = timeit.default_timer
time_eval_wholeSite.timeout = 3600.0
