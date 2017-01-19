
      function numget (string,msg)
      implicit none
      character string*(*),str*254,buff(254)*1, msg*(*)
      equivalence (str,buff)
      integer i, j, ival, iret, lbsiz, length
      real*4 val, numget
*     real*4 undef /z'ffffffff'/
      real*4 undef /0./
c
      str   = string
      msg = ' '
      lbsiz = length(string)
        do i = 1,lbsiz
          if (buff(i).ne.' ') go to 1
        enddo
      i = lbsiz
1     continue
      call valnum1 (buff,i,ival,val,iret,lbsiz)
      go to (10,20,30,40), iret
10    numget = ival
      return
c
20    numget = val
      return
c
30    if (string(i:i+2).eq.'yes') then
      numget = 1.
      else if (string(i:i+1).eq.'no') then
 
      numget = 0.
 
      else
      msg =  'not converted strig: '//string(1:lbsiz)
      numget = undef
      endif
      return
c
40    continue
      msg = 'exponent number missing: '//string(1:lbsiz)
      numget = undef
      return
      end
c
      function valget (string)
      implicit none
      character string*(*),str*254,buff(254)*1
      equivalence (str,buff)
      integer i, j, ival, iret, lbsiz, length
      real*4 val, valget
      real*4 undef 

      call getmv(undef)

        if (string.eq.'NaN') then
        valget=undef
        return
        endif
c
      str   = string
      lbsiz = length(string)
        do i = 1,lbsiz
          if (buff(i).ne.' ') go to 1
        enddo
      i = lbsiz
1     continue
      call valnum1 (buff,i,ival,val,iret,lbsiz)
      go to (10,20,30,40), iret
10    valget = ival
      return
c
20    valget = val
      return
c
30    if (string(i:i+2).eq.'yes') then
      valget = 1.

      else if (string(i:i+1).eq.'no') then 
      valget = 0.

      else   !  error or string=NaN
      valget = undef
      endif

      return
c
40    continue
c  missing exponent number 
      valget = undef
      return
      end
c
      subroutine valnum1 (buff,i,ival,val,iret,lbsiz)
c     iret = 1  integer
c          = 2  real
c          = 3  character
c          = 4  exponent number missing
c
      character buff(*)*1
      logical num,first
      save first,izero
 
      real*8 dval, div
 
      data first/.true./
 
        if(first)then
        izero=ichar('0')
        first=.false.
        endif
c
      iret=3
      istor=i
      num=.false.
      isign=1
      ival=0
      dval=0.d0
      val=0.
 
      if(buff(i).eq.'-')then
      isign=-1
 
      else if(buff(i).eq.'.')then
      go to 30
 
      else if(buff(i).eq.'+')then
      isign=1
 
      else if(buff(i).ge.'0'.and.buff(i).le.'9')then
      ival=ichar(buff(i))-izero
      num=.true.
 
      else
      i=istor
      return
      endif
c
   10 i=i+1
      if(i.gt.lbsiz) go to 20
      if(buff(i).eq.' ') go to 20
      if(buff(i).eq.',') go to 20
      if(buff(i).eq.'.') go to 30
      if(buff(i).eq.'e') go to 40
      if(buff(i).eq.'d') go to 40
      if(buff(i).eq.'E') go to 40
      if(buff(i).eq.'D') go to 40
 
      if(buff(i).ge.'0'.and.buff(i).le.'9')then
      num=.true.
      ival=ival*10+(ichar(buff(i))-izero)
 
      else
      i=istor
      val = sngl(dval)
      return
      endif
 
      go to 10
c
20    continue
        if(num)then
        iret=1
        ival=isign*ival
        else
        i=istor
        endif
 
      val = sngl(dval)
      return
c
30    continue
      dval=dfloat(ival)
      div=1.d0
 
31    continue
      i=i+1
      if(i.gt.lbsiz) go to 32
      if(buff(i).eq.' ') go to 32
      if(buff(i).eq.',') go to 32
      if(buff(i).eq.'e') go to 41
      if(buff(i).eq.'d') go to 41
      if(buff(i).eq.'E') go to 41
      if(buff(i).eq.'D') go to 41
 
      if(buff(i).ge.'0'.and.buff(i).le.'9')then
      num=.true.
      div=div*10.d0
      dval=dval+dfloat(ichar(buff(i))-izero)/div
 
      else
      i=istor
      val = sngl(dval)
      return
      endif
 
      go to 31
c
32    continue
      if(num)then
      iret=2
      dval=isign*dval
      else
      i=istor
      endif
 
      val = sngl(dval)
      return
c
40    continue
      dval=dfloat(ival)
41    continue
 
      if(.not.num)then
      i=istor
      val = sngl(dval)
      return
      endif
c
      iexp=0
      num=.false.
      i=i+1
      if(i.gt.lbsiz) go to 60
 
      if(buff(i).eq.'-')then
      isexp=-1
 
      else if(buff(i).eq.'+')then
      isexp=1
 
      else if(buff(i).ge.'0'.and.buff(i).le.'9')then
      isexp=1
      iexp=ichar(buff(i))-izero
      num=.true.
 
      else
      i=istor
      val = sngl(dval)
      return
      endif
c
50    i=i+1
      if(i.gt.lbsiz) go to 60
      if(buff(i).eq.' ') go to 60
      if(buff(i).eq.',') go to 60
 
      if(buff(i).ge.'0'.and.buff(i).le.'9')then
      num=.true.
      iexp=iexp*10+(ichar(buff(i))-izero)
 
      else
      i=istor
      val = sngl(dval)
      return
      endif
 
      go to 50
c
60    continue
      if(num)then
      iret=2
        if (isexp.eq.1) then
        dval=(isign*dval)*10.d0**iexp
        else if (isexp.eq.-1) then
        dval=(isign*dval)/10.d0**iexp
        else
        write (6,*)'readbf: error in valnum1 '
        stop
        endif
 
      else
      iret=4
      dval=isign*dval
      i=istor
      endif
 
      val = sngl(dval)
      return
      end
