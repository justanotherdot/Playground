#!/usr/bin/env python2.7

"""
Simple definition lookup application for command line terminals.
Queries XML from services.aonaware.com
Usage: `define <word>` where <word> is a command line argument

Author:   Ryan James Spencer
Date:     7/3/15
Version:  0.1b
"""

import argparse
import re
import sys
import urllib2
import xml.etree.ElementTree as ET


def setup_args():
    argParser = \
        argparse.ArgumentParser(description="Commandline dictionary!")
    argParser.add_argument('word', metavar='w', type=str, nargs=1,
                           help='A word to define')
    args = vars(argParser.parse_args())
    query = ''.join(args['word'])
    return query


def construct_url(query):
    root = "http://services.aonaware.com"
    req_path = "/DictService/DictService.asmx/Define?word="
    url = "{0}{1}{2}".format(root, req_path, query)
    return url


def find_def(query):
    suffix = re.compile(r'.*\.py.*')
    if re.match(suffix, sys.argv[0]):
        print("You are attempting to run this script directly.")
        print("Please rerun with 'define <word>'")
    else:
        # Query the service and construct an ElementTree
        url = construct_url(query)
        response = urllib2.urlopen(url).read()
        root = ET.fromstring(response)
        dict_name = re.compile('.*Name')
        definition = re.compile('.*WordDefinition')
        # Regex the element tree for dictionary name and definition
        for item in root.findall('.//'):
            # Dictionary names are under the 'Name' tag.
            # Definitions are under 'WordDefinition'.
            # Care should be made because the root is also called 'WordDefinition'
            match = re.match(dict_name, item.tag) or re.match(definition, item.tag)
            if match:
                print(item.text)

if __name__ == '__main__':
    query = setup_args()
    find_def(query)
