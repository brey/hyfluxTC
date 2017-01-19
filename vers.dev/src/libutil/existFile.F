      function existFile(file)
      implicit none
      logical*4 existFile
      character file*(*)
      integer length, ll
      existFile = .false.
      ll = length(file)
        if (file(1:ll).eq.' '.or.file(1:ll).eq.'none') return
      inquire (file=file(1:ll),exist=existFile)
      return
      end
