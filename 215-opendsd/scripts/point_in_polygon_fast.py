from xml.etree import ElementTree
import keytree
from shapely.geometry import Polygon, Point, shape

doc = open("/Users/damoncrockett/vosd.org/215-opendsd/Final.kml").read()
tree = ElementTree.fromstring(doc)
# kml namespace
kmlns = tree.tag.split('}')[0][1:]
# find all placemarks
placemks = tree.findall(".//{%s}Placemark" % kmlns)
# filter out those without polygon elements
placemks_with_polygons = []

for p in placemks:
    if p.findall(".//{%s}Polygon" % kmlns):
        placemks_with_polygons.append(p)
        
# func: extract kml LinearRings, convert to shapely LinearRings, make dict with cpa names

def coords_names(placemks):
  coords_names_dict = {}
  for placemk in placemks:
    name = placemk.getchildren()[0].text
    coord_text = placemk.findtext(".//{%s}coordinates" % kmlns)
    coords = []
    for elems in coord_text.split():
      points = elems.split(",")
      coords.append((float(points[0]), float(points[1])))
    coords_names_dict[Polygon(coords)] = name
  return coords_names_dict

# make dataframe from function
import pandas as pd

cpa_polygons = pd.DataFrame(coords_names(placemks_with_polygons).items(),
            columns=['Polygon','CPA'])

# import approvals

d = pd.read_csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/approval_master_FULL.csv',low_memory=False)
d = d[d.Latitude.notnull()]
d.reset_index(drop=True, inplace=True)

# collect all lat lon points from dataframe

n = len(d.index)
locations = []
for i in range(n):
    point = Point(d.Longitude[i],d.Latitude[i])
    locations.append(point)
    
# crucial step: build spatial index

from rtree import index
idx = index.Index()
count = -1
for item in cpa_polygons.Polygon:
    count +=1
    idx.insert(count, item.bounds)
    
# assign a cpa to each point

m = len(locations)
hoods = []
for i in range(m):
    print i
    tmp = 'nan'
    for j in idx.intersection((d.Longitude[i],d.Latitude[i])):
        if locations[i].within(cpa_polygons.Polygon.loc[j]):
            tmp = cpa_polygons.CPA[j]
            break
    hoods.append(tmp)
    
d['CPA'] = hoods
d.to_csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/approvals_CPA.csv', encoding='utf-8')







