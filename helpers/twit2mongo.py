#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pymongo
import json
from datetime import datetime

# conntect to database
con = pymongo.Connection('127.0.0.1', port=27017)
tatort = con.twitter_test_db.tatort
# clear data collection (if exists)
tatort.drop()

for file in set(['data/tatort_131117.json','data/tatort_131124.json',
				 'data/tatort_131201.json','data/tatort_131208.json',
				 'data/polizeiruf_131215.json','data/tatort_131222.json',
				 'data/tatort_131226.json','data/tatort_131229.json',
				 'data/tatort_140101.json','data/tatort_140105.json',
				 'data/polizeiruf_140112.json','data/tatort_140119.json',
				 'data/tatort_140126.json','data/tatort_140202.json',
				 'data/tatort_140209.json','data/tatort_140216.json',
				 'data/tatort_140223.json','data/tatort_140302.json',
				 'data/tatort_140309.json','data/tatort_140316.json',
				 'data/tatort_140330.json'
				  ]):
	with open(file,'r') as f:
		for line in f:
			# if line not empty
			if line.strip():
				# get rid of newlines and linebreaks within text
				line = line.replace("\\n"," ")
				line = line.replace("\\\\r","/r")
				line = line.replace("\\r"," ")
				# JSON object
				data = []
				try:
					line = json.loads(line)
					# change time format: string to datetime object
					this = line['created_at']
					this = datetime.strptime(this, "%a %b %d %X +0000 %Y")
					line['created_at'] = this
					line['time'] = int(datetime.strftime(this, "%H"))
					# add to JSON object
					data.append(line)
					for line in data:
						# save to database
						tatort.save(line)
				except:
					print("error! file: "+file)
					print("\tline: "+line)
con.disconnect()
