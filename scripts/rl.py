import argparse
import json
import csv
import os

parser = argparse.ArgumentParser(description="Extract JSON field to username-mapped CSV")
parser.add_argument('dir', help='The directory holding the JSON files.')
parser.add_argument('outfile', help='The file to write to.')
parser.add_argument('var', help='The variable to select.')
args = parser.parse_args()

attributes = ['username',args.var]

outhandle = csv.writer(open(args.outfile, 'w'))
outhandle.writerow(attributes)

for jsonfile in os.listdir(args.dir):
  profile = json.load(open(args.dir+os.sep+jsonfile,'r'))
  if 'username' in profile and args.var in profile:
      v = profile[args.var]
#    for v in profile[args.var]:
      values = [profile['username'], v]
      outhandle.writerow(values)
