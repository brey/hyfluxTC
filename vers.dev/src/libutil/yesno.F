	character*3 function yesno (flag)
        logical*4 flag
          if (flag) then
          yesno='yes'
          else
          yesno = 'no '
          endif
        return
        end
