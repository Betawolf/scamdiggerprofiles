import argparse
import json
import csv
import os

parser = argparse.ArgumentParser(description="Convert JSON to CSV")
parser.add_argument('dir', help='The directory holding the JSON files.')
parser.add_argument('outfile', help='The file to write to.')
args = parser.parse_args()

attributes = ['scam','username','age','gender','location','ethnicity','occupation','status','year_reported','month_reported','name','inet','phone','email','children','smoking','drinking','religion','orientation', 'match_age','intent']

scam = 1 if args.dir == 'scam' else 0

outhandle = csv.writer(open(args.outfile, 'w'))
outhandle.writerow(attributes)

for jsonfile in os.listdir(args.dir):
  profile = json.load(open(args.dir+os.sep+jsonfile,'r'))
  values = [scam]
  for k in attributes[1:]:
    if k in profile:
      values.append(profile[k])
    else:
      values.append(None)
  outhandle.writerow(values)
