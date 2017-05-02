import argparse
import json
import csv
import os

parser = argparse.ArgumentParser(description="Convert JSON to CSV")
parser.add_argument('dir', help='The directory holding the JSON files.')
parser.add_argument('outfile', help='The file to write to.')
args = parser.parse_args()

attributes = ['scam','username','age','gender','ethnicity','occupation','location','country','latitude','longitude','fold','number', 'nullimage','nulldescription']

outhandle = csv.writer(open(args.outfile, 'w'))
outhandle.writerow(attributes)

for jsonfile in os.listdir(args.dir):
  profile = json.load(open(args.dir+os.sep+jsonfile,'r'))
  values = []
  for k in attributes:
    if k in profile:
      values.append(profile[k])
    elif k == 'number':
      values.append(jsonfile[0:jsonfile.rindex('.')])
    elif k == 'nullimage':
      values.append(profile['images'] == None or len(profile['images']) == 0 or (profile['images'] == ['images/fd6d0914b482e6c4aef6aa42df5eaf62.png']))
    elif k == 'nulldescription':
      values.append(profile['description'] == None)
    else:
      values.append(None)
  outhandle.writerow(values)
