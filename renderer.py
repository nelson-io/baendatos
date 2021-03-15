# -*- coding: utf-8 -*-
"""
Created on Mon Mar 15 09:07:32 2021

@author: u631286
"""

import os
import imageio
arr = [r'plots/' + file for file in os.listdir('plots')]
print(arr)



images = []
for filename in arr:
    images.append(imageio.imread(filename))
imageio.mimsave('gifs/render.gif', images)