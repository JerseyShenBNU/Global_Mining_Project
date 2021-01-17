from urllib.request import urlopen
from bs4 import BeautifulSoup
import ssl
import urllib.request
from urllib.request import urlretrieve
import pandas as pd
import http.cookiejar as HC
import re
def getLinkes():
    url_parent = "https://e4ftl01.cr.usgs.gov/MOLT/MOD13C2.006/"
    ssl._create_default_https_context = ssl._create_unverified_context
    html = urlopen(url_parent)
    bsObj = BeautifulSoup(html)
    parentlink = []
    for link in bsObj.findAll("a"):
        if 'href' in link.attrs:
            parentlink.append(link.attrs['href'])

    parentlink_new = []

    pattern = re.compile('[0-9]+\.+[0-9]+\.+[0-9]+\/+')
    for a in parentlink:
        temp = re.findall(pattern,a)
        if(len(temp)!=0):
            parentlink_new.append(temp[0])

    link_deeper = []
    for a in parentlink_new:
        link = url_parent+a
        link_deeper.append(link)

    password_manager = urllib.request.HTTPPasswordMgrWithDefaultRealm()
    #UserName need to be registered at "https://urs.earthdata.nasa.gov"
    password_manager.add_password(None, "https://urs.earthdata.nasa.gov", 'UserName', 'Passwd')

    # Create a cookie jar for storing cookies. This is used to store and return
    # the session cookie given to use by the data server (otherwise it will just
    # keep sending us back to Earthdata Login to authenticate).  Ideally, we
    # should use a file based cookie jar to preserve cookies between runs. This
    # will make it much more efficient.

    cookie_jar = HC.CookieJar()

    # Install all the handlers.

    opener = urllib.request.build_opener(
        urllib.request.HTTPBasicAuthHandler(password_manager),
        # urllib2.HTTPHandler(debuglevel=1),    # Uncomment these two lines to see
        # urllib2.HTTPSHandler(debuglevel=1),   # details of the requests/responses
        urllib.request.HTTPCookieProcessor(cookie_jar))
    urllib.request.install_opener(opener)

    print(link_deeper)
    output = 'J:\\NDVI\\'
    for a in link_deeper:
        print('start download')
        temp = urlopen(a)
        temp_bsObj = BeautifulSoup(temp)

        name_list = []
        for name in temp_bsObj.findAll('a'):
            if 'href' in name.attrs:
                name_list.append(name.attrs['href'])
        print(name_list)
        pattern2 = re.compile('[A-Z]+[0-9]+[A-Z]+[0-9]+\.+[A-Z]+[0-9]+\.+[0-9]+\.+[0-9]+\.+hdf$')
        filename= []
        for b in name_list:
            temp_name = re.findall(pattern2,b)
            if(len(temp_name)!=0):
                filename.append(temp_name[0])
        link_download = a+filename[0]
        print(link_download)
        output2 = output+filename[0]

        urlretrieve(link_download, output2)

    print('end download')
getLinkes()