import numpy as np
import os
import xml.etree.ElementTree as et


tree=et.parse('info.xml')

f=open('info.sh','w')

source=tree.findtext('source').strip()

f.write('#!/bin/bash -v\n')
f.write('export source="'+source+'"\n')
f.write('export hurName="'+tree.findtext('hurName')+'"\n')
f.write('export hurId='+tree.findtext('hurId')+'\n')
f.write('export basin="'+tree.findtext('basin')+'"\n')
f.write('export bulNo='+tree.findtext('bulNo')+'\n')
f.write('export bulDate="'+tree.findtext('bulDate')+'"\n')
f.write('export n='+tree.findtext('n')+'\n')
f.write('export fk='+tree.findtext('fk')+'\n')
#line export noaaID=$noaaID
f.close()
