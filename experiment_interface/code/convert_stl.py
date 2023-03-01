#Function to read ASCII STL and write as binary STL

import numpy
from stl import mesh

def convert_stl(file, output_path):
  ascii_stl = mesh.Mesh.from_file(file)
  ascii_stl.save(output_path)

convert_stl('stl_files/id-03-Type1-Rep01.stl', 'test.stl')
