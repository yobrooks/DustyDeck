MODULE ExtraFunc
  INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15, 307)
  
  CONTAINS
          REAL(KIND = DP) function trig (i,j)
                REAL(KIND = DP) :: x, y, z
                pi = acos(-1.0)
                x = dble(i) - dble(j)
                y = dble(i) + dble(j)
#ifdef YO3
                z = exp(sin(sqrt((x*x)+(y*y))*pi))
#else
                z = exp(sin(sqrt(x**2+y**2)*pi))
#endif
                trig = x + y + log10(abs(1+z+(x*y*z)))/ (abs(x)+abs(y))
                return
        end function trig



        REAL(KIND = DP) function conrand(seed)
                REAL(KIND = DP) :: seed
                REAL(KIND = DP) :: a, m, temp
                a = 16807.0D0
                m = 2147483647.0D0
                temp = a*seed
                seed = temp - m * int(temp/m)
                conrand = seed / m
                return
        end function conrand



        subroutine idcheck(N,check,AV,BV,ID)

                REAL(KIND = DP), TARGET :: AV(*), BV(*), ID(N,*)
                REAL(KIND = DP) :: l2, check, check2, a, b, c, d
#ifdef YO2
                do 10 i = 1, N  
                        do 20 j = 1, N
                                ID(i,j) =  cos(check+2.0*i*acos(-1.0)/N)+&
                                        2.0*sin(check+ 2.0*j*acos(-1.0)/N)
20                      continue
10              continue

                do 11 i = 1, N
                        ID(i,i) = 1
                        if(AV(i)*BV(i) < 0) then
                                ID(i,i) = -1
                        endif
11              continue
#else
                do 10 i = 1, N  
                        do 20 j = 1, N
                                if (i .eq. j) then 
                                        if ((AV(i) .lt. 0) .and. (BV(j) .lt. 0)) then
                                                ID(i,j) = 1.0
                                        elseif ((AV(i) .lt. 0) .and. (BV(j) .gt. 0)) then
                                                ID(i,j) = -1.0
                                        elseif ((AV(i) .gt. 0) .and. (BV(j) .lt. 0)) then
                                                ID(i,j) = -1.0
                                        else
                                                ID(i,j) = 1.0
                                        endif
                                elseif ( i .ne. j ) then 
                                        ID(i,j) =  cos(check+2.0*i*acos(-1.0)/N)+&
                                               2.0*sin(check+ 2.0*j*acos(-1.0)/N)
                                endif
20                      continue
10              continue
#endif


                l2 = 0.0
                do 30 i = 1, N
#ifdef YO3
                        l2 = l2 + AV(i)*AV(i)
#else
                        l2 = l2 + AV(i)**2
#endif
30              continue

                l2 = sqrt(l2)
                do 40 i = 1, N
                        AV(i) = AV(i) / l2
40              continue

                l2 = 0.0
                do 50 i = 1, N
#ifdef YO3
                        l2 = l2 + BV(i)*BV(i)
#else
                        l2 = l2 + BV(i)**2
#endif
50              continue
                l2 = sqrt(l2)
                do 60 i = 1, N
                        BV(i) = BV(i) / l2
60              continue
     
                a = 0.0D0
                b = 0.0D0
                c = 0.0D0
                d = 0.0D0
                do 70 i = 1, N
                        do 80 j = 1, N
                                do 90 k = 1, N
                                        goto ( 200, 300, 400, 500 ) int(mod(i+j+k,4)+1) 
200                                     a  = a +  AV(i) * BV(j) * ID(j,k) 
                                        check = check + a
                                        goto 100
300                                     b  = b +  AV(j) * BV(i) * ID(k,j) 
                                        check = check - b 
                                        goto 100
400                                     c  = c -  AV(i) * BV(j) * ID(k,j)
#ifdef YO3
                                         check = sqrt((b*b) + (c*c))
#else
                                        check = sqrt(b**2 + c**2)
#endif
                                        goto 100
500                                     d  = d -  AV(j) * BV(i) * ID(j,k) 
                                        check2 = a + b + c + d
100                             continue
90                      continue
80              continue
70       continue

        check = min(abs(check2),abs(check))/max(abs(check2),abs(check))           

        return
        end subroutine idcheck
END MODULE
