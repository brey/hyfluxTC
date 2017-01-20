import numpy as np
from bunch import bunchify
import sys
import csv

def read_header(filename):

     with open(filename, 'r') as f:
       names = f.readline().split()
     f.close()
     return names


def read_all(filename):

     with open(filename, 'r') as f:
         lines = [line.rstrip('\n') for line in f]
#        lines = [line for line in f]
     f.close()
     return lines 

def write_all(filename,l):

     with open(filename, 'w') as f:
        for lines in l:
            f.write(lines.strip('line'))
     f.close()


def readf(filename):

    var=read_header(filename)
    dat=np.loadtxt(filename,skiprows=1)
    d=dict(zip(var,dat.T))
    return bunchify(d)

def cvread(filename):

    with open(filename, 'r') as infile:
      cs = csv.reader(infile)
      header=cs.next()
      (n0,n1,n2,n3,n4)=([],[],[],[],[])
      for line in cs:
           lt=line[0].split('\t')
           (n0.append(lt[0]),n1.append(lt[1]),n2.append(lt[2]),n3.append(lt[3]),n4.append(lt[4]))

   #return [int(i) for i in n0],[int(i) for i in n1],n2,[int(i) for i in n3],[int(i) for i in n4]
    return [int(i) for i in n0],[float(i) for i in n1],n2,[int(i) for i in n3],[int(i) for i in n4]

def read_dia(filename,path=None):
    dat=read_all(filename)

    var=[]    
    value=[]
 
    line=dat[0].split()

    no=np.size(line)/2

    for i in range(no):
          var.append(line[no+i]) 
          value.append(line[i]) 

    value[2:4]=map(float, value[2:4])
    value[7:9]=map(float, value[7:9])
    value[1]=int(value[1])
    value[4:7]=map(int, value[4:7])

    var[:-1]=[x[:-1] for x in var[:-1]]

    for i in range(3):
          line=dat[i+1].split()

          var.append(line[4])
          value.append(map(int,line[0:4]))

    line=dat[4].split()
    
    no=np.size(line)/2

    for i in range(no):
          var.append(line[no+i][:-1]) 
          value.append(int(line[i])) 



    d=dict(zip(var,value))

    return d

def readtxt(filename):

    with open(filename, 'r') as infile:
      cs = csv.reader(infile)
      header=cs.next()
      var=header[0].split('\t\t')
      if np.size(var)==1: var=header[0].split('\t')
      var= [v.strip() for v in var]
      data= {k:[] for k in var}
      for line in cs:
           lt=line[0].split('\t')
           for i in range(np.size(var)):
             data[var[i]].append(lt[i])

      for key in data:
        try:
	  data[key]=[num(i) for i in data[key]]
        except: pass
  

      for key in data:
	data[key]=np.array(data[key])

      return bunchify(data)


def num(s):
     try:
	return int(s)
     except ValueError:
        return float(s)



def vplreadtxt(filename):

    with open(filename, 'r') as infile:
      cs = csv.reader(infile)
      header=cs.next()
      var=header[0].split('\t')
      var= [v.strip() for v in var]
      data= {k:[] for k in var}
      for line in cs:
           lt=line[0].split('\t')
           for i in range(np.size(var)):
             data[var[i]].append(lt[i])

      for key in data:
        try:
	  data[key]=[num(i) for i in data[key]]
        except: pass
  

      for key in data:
	data[key]=np.array(data[key])

      return bunchify(data)


def read_atcf(filename):


     with open(filename, 'r') as f:
         lines = [line.rstrip('\n') for line in f]
     f.close()

     data=np.array([w.split(',') for w in lines])

     for i in range(data.shape[0]):
      for j in range(data.shape[1]):
       try:
         data[i,j]=num(data[i,j]) 
       except:
            pass

     return  data
