# -*- coding: utf-8 -*-
"""
Created on Wed Oct 30 21:11:36 2024

@author: mirko
"""

import multiprocessing
from refresh_app import start_observer

# Set start method as early as possible
multiprocessing.set_start_method('spawn', force=True)



if __name__ == '__main__':
    start_observer()
