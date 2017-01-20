      subroutine fileopen(filename,unitnr)
      character filename*(*)
      integer unitnr
      call opnfree (unitnr,filename)
      return
      end
