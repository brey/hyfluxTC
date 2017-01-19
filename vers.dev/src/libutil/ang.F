c !DEC$ ATTRIBUTES DLLEXPORT:: libutilDll
c !DEC$ ATTRIBUTES ALIAS:'libutilDll' :: libutilDll
      function ang(x1,y1,x2,y2)
c
c     calcola l'angolo formato da una retta orientata che passa
c     da x1,y1 e x2,y2 ,con origine in x1,y1.
c     0.<ang<360.
c
      pigr=3.14159265
      if(x1-x2)12,7,12
 12   ang=abs(atan((y2-y1)/(x2-x1)))
      if(x1-x2)5,7,6
 5    if(y1-y2)1,1,4
 6    if(y1-y2)2,2,3
 7    if(y1-y2)8,9,10
 1    return
 2    ang=pigr-ang
      return
 3    ang=pigr+ang
      return
 4    ang=pigr*2-ang
      return
 8    ang=pigr*.5
      return
 9    ang=0.
      write (6,*)'punti coincidenti, ang=0.'
      return
 10   ang=pigr*1.5
      return
      end
