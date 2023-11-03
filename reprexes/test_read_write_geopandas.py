# Aim: benchmark geopandas reading of gpkg and geojson files

file_path1 = "test.geojson"
file_path2 = "test.gpkg"

import geopandas as gpd

gdf1 = gpd.read_file(file_path1)
gdf2 = gpd.read_file(file_path2)

# timings and output to csv
import timeit
import csv

def read_gpkg():
    gpd.read_file(file_path2)

def read_geojson():
    gpd.read_file(file_path1)

def main():
    with open('benchmark.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile, delimiter=',')
        writer.writerow(['file', 'time'])
        writer.writerow(['test.geojson', timeit.timeit(read_geojson, number=10)])
        writer.writerow(['test.gpkg', timeit.timeit(read_gpkg, number=10)])

main()
