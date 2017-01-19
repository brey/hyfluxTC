import xml.etree.ElementTree as et


tree=et.parse('info.xml')

with open('info1.xml','w') as f:
    f.write(et.tostring(tree.getroot(), encoding='utf8', method='xml'))

