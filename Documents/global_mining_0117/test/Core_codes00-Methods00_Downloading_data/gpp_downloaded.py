
from urllib.request import urlopen
from bs4 import BeautifulSoup
from urllib.request import ProxyHandler,build_opener
import urllib.parse
from urllib.request import urlretrieve
import urllib.request
import urllib.error
import re
import requests
from lxml import html
from urllib.request import urlretrieve
import time
from functools import wraps
import http.client



def timethis(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.perf_counter()
        r = func(*args, **kwargs)
        end = time.perf_counter()
        print('{}.{} : {}'.format(func.__module__, func.__name__, end - start))
        return r
    return wrapper
@timethis
def getLinkes(outputpath = 'E:\\Desktop\\'):
    url_ceda = 'http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/GeoTIFF/Monthly_MOD17A2/GeoTIFF_0.05degree/'
    #url_parent = "https://www.ncei.noaa.gov/data/avhrr-land-leaf-area-index-and-fapar/access/"
    html = urlopen(url_ceda)
    bsObj = BeautifulSoup(html,'lxml')

    parentlink = []
    for link in bsObj.findAll("a"):
        #print(link);
        if 'href' in link.attrs:
            temp_link = 'http://files.ntsg.umt.edu/'+link.attrs['href']
            print(temp_link)
            parentlink.append(temp_link)

    #parentlink = +parentlink
    #print(parentlink);

    files_name = []
    link = []
    output = []
    #print(parentlink[0:10])
    print(len(parentlink))
    parentlink = parentlink[3:195]
    #parentlink[3:195])
    #guide_file = 'https://docserver.gesdisc.eosdis.nasa.gov/public/project/GPM/3B4XRT_doc_V7.pdf'
    for temp in parentlink:
        print(temp);
        link.append(temp)
        pieces = [x.strip() for x in temp.split('/')]
        print(pieces)

        temp_file_name = pieces[10]
        # print(temp_file_name)
        temp_file_name = [x.strip() for x in temp_file_name.split('?')]
        print(temp_file_name)
        # temp_file_name = temp_file_name[0]+'_'+temp_file_name[1]+'_'+temp_file_name[2]+'.'+temp_file_name[3]
        temp_file_name = temp_file_name[0]
        temp_output = outputpath + temp_file_name
        print(temp_file_name)
        files_name.append(temp_file_name)
        output.append(temp_output)
        print(temp_output)

    print(link)
    for h in range(84,len(link)):
        # print(file_name[h])
        # print(output[h])
        print("Start Downloading!")
        #urllib.request.urlretrieve(link[h],output[h])

        request = urllib.request.Request(link[h])
        response = urllib.request.urlopen(request)
        data = response.read()
        with open(output[h], "wb") as code:
            code.write(data)
        # urlretrieve(data, output[h])
        print(h)
getLinkes()
