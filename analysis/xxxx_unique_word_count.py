# -*- coding: utf-8 -*-

import re

def countWords(a_list):
    words = {}
    for i in range(len(a_list)):
        item = a_list[i]
        count = a_list.count(item)
        words[item] = count
    return sorted(words.items(), key = lambda item: item[1], reverse=True)

content = """
This is a test of the program, what it can do, and what it can't do.
"""

new_content = content.replace("'","")
cleanContent = re.sub('[^a-zA-Z]',' ', new_content)

wordList = cleanContent.lower().split()

result = countWords(wordList)

print(result)

print(len(result))

