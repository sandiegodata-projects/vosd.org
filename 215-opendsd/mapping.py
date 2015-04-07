from xml.etree import ElementTree
import sys
sys.path.append('/usr/local/lib/python2.7/dist-packages/')
sys.path.append('/usr/local/lib/python2.7/dist-packages/keytree-0.2.1-py2.7.egg/')
import keytree
from shapely.geometry import Point, shape

doc = open("/home/damoncrockett/vosd.org/215-opendsd/Final.kml").read()
tree = ElementTree.fromstring(doc)
# kml namespace
kmlns = tree.tag.split('}')[0][1:]
# find all placemarks
placemks = tree.findall(".//{%s}Placemark" % kmlns)
# filter out those without polygon elements

with_polygons = []

for p in placemks:
    if p.findall(".//{%s}Polygon" % kmlns):
        with_polygons.append(p)

# create dataframe with polygons elements attached to their neighborhood names

import pandas as pd

polygons = []
names = []

for p in with_polygons:
    polygons.append(p.findall(".//{%s}Polygon" % kmlns)[0])
    names.append(p.getchildren()[0].text)
    
poly_names = pd.DataFrame(names)
poly_names['polygons'] = polygons

# send to csv
poly_names.to_csv('/home/damoncrockett/vosd.org/215-opendsd/poly_names.csv')

# import approvals

d = pd.read_csv('/home/damoncrockett/vosd.org/215-opendsd/approval_master_FULL.csv')
d = d[d.Latitude.notnull()]
d.reset_index(drop=True, inplace=True)

hood_polygons = []

n = len(d.index)

for i in range(n):
    point = Point(d.Longitude[i],d.Latitude[i])
    hood_polygon = filter(
        lambda x: shape(keytree.geometry(x)).contains(point),
        poly_names.polygons)
    if len(hood_polygon) == 0:
	hood_polygons.append('nan')
    else:
	hood_polygons.append(hood_polygon[0])
    print i
    if i == 100000:
	pd.DataFrame(hood_polygons).to_csv('/home/damoncrockett/vosd.org/215-opendsd/hood_polygons.csv')
    if i == 200000:
	pd.DataFrame(hood_polygons).to_csv('/home/damoncrockett/vosd.org/215-opendsd/hood_polygons.csv')	
    if i == 300000:
	pd.DataFrame(hood_polygons).to_csv('/home/damoncrockett/vosd.org/215-opendsd/hood_polygons.csv')

pd.DataFrame(hood_polygons).to_csv('/home/damoncrockett/vosd.org/215-opendsd/hood_polygons.csv')









