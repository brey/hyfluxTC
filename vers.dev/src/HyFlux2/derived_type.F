c--------------------------------
      TYPE QuadStruct
      INTEGER*4, pointer :: f_ind(:,:)
      END TYPE QuadStruct


      TYPE (QuadStruct), pointer :: p1,p2

      allocate (p1%f_ind(10,10))
      allocate (p2%f_ind(10,10))

      p1%f_ind = 1
      p2%f_ind = 2


      stop
      end

      

        