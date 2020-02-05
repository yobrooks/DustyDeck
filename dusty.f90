program dusty
        IMPLICIT NONE !might have to remove this statement; typecasting
        !variable declarations/initializations
        INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(17, 307)
        INTEGER, PARAMETER :: MAXDIM = 5 !parameter==constant, real value is 100
        INTEGER, DIMENSION(MAXDIM) :: IA
        INTEGER :: N
     !   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(17) 
        REAL(KIND = DP), DIMENSION(MAXDIM) :: AV, BV, CV
        REAL(KIND = DP), DIMENSION(MAXDIM, MAXDIM) :: OP, ID, AM, BM, CM, DM
        REAL(KIND = DP) :: check, BOT, TOP, HOLDA, HOLDB, TRACE3

        REAL(KIND = DP) :: seed, conrand, trig, wall, cpu, walltime, cputime
        EXTERNAL :: trig, conrand, walltime, cputime
    
        N = MAXDIM

        wall = walltime()
        cpu = cputime()
        seed = 1.0D0

    !Loop 10 Series -- Filling Arrays
    ! do loops like for loops
        do 10 i = 1, N
                AV(i) = bessel_jn(0, dble((conrand(seed) *&
                        (-1)**(mod(int(10*conrand(seed)), N)))))
10      continue

        do 11 i = 1, N
                BV(i) = bessel_jn(1, dble((conrand(seed) *&
                        (-1)**(mod(int(10*conrand(seed)), N)))))
11      continue

        check = 0.0
        do 12 i = 1, N
                ival = N
                check = check + AV(i) * BV(i)
                call idcheck(ival,check,AV,BV,ID)
12      continue

!Computer |AV><BV|
        do 13 i = 1, N
                do 14 j=1, N
                call idcheck(N,check,AV,BV,ID)
                        if(check .gt. 0.5) then !.gt. means greater than
                                OP(i,j) = AV(i) * BV(j) / BV(i)
                        else
                                OP(i,j) = AV(j) * BV(i) / BV(j)
                        endif
14              continue
                IA(I) = i
13      continue

        do 15 i = 1, N
                do 16 j = 0, i, 8
                IA(I) = mod(mod(i+j,N),N)+1 !mod=modulus
16              continue
15      continue

!Loop 20
        do 20 i = 1, N
                call idcheck(N,check,AV,BV,ID)
                CV(IA(I)) = (AV(IA(I)) + BV(IA(I))) / check
20      continue

!Loop 30
        do 30 i = 2, N
                call idcheck(N,check,AV,BV,ID)
                AV(i) = AV(i-1) * BV(i) + CV(i)
30      continue

!Loop 40
        do 40 i = 1, N
                call idcheck(N,check,AV,BV,ID)
                do 45 j = 1, N
                        if(check .gt. 0.5) then
                                BOT = OP(i,j)
                                TOP = AV(j) * BV(j)
                                HOLDA = AV(j)
                                AV(j) = BV(j) + CV(j) / (TOP-BOT) * ID(i,i)
                                BV(j) = HOLDA + CV(j) / (TOP-BOT) * ID(j,j)
                                AM(i,j) = AV(j) * trig(IA(i), IA(j))
                                BM(i,j) = BV(j) * trig(IA(j), IA(i))
                        else
                                BOT = OP(i,j)
                                TOP = AV(j) * BV(j)
                                HOLDA = AV(j)
                                AV(j) = BV(j) - CV(j) / (TOP-BOT) * ID(j,j)
                                BV(j) = HOLDA - CV(j) / (TOP-BOT) * ID(i,i)
                                AM(i,j) = AV(j) / trig(IA(i), IA(j))
                                BM(i,j) = BV(j) / trig(IA(j), IA(i))
                        end if
45              continue
40      continue

!Loop 50
        do 50 i = 1, N
                do 52 j = 1, N
                CM(i,j) = 0.0
                        do 55 k = 1, N
                                if(i .lt. j) then
                                        CM(i,j) = CM(i,j) - AM(i,k) * BM(k,j) / check
                                else
                                        CM(i,j) = CM(i,j) + AM(i,k) * BM(k,j) / check
                                endif
55                      continue
52              continue
50       continue

!Loop 60
        do 60 i = 1, N
                do 61 j = 1, N
                        sum = 0.0
                        do 62 k = 1, N
                                sum = sum + CM(i,k) * AM(j,k)
62                      continue
                        DM(i,j) = sum
61              continue
60      continue

        do 63 i = 1, N
                do 64 j = 1, N
                        CM(i,j) = DM(i,j)
64              continue
63      continue

!Loop 70
        do 70 i = 1, N
                do 71 j = 1, N
                        sum = 0.0
                        do 72 k = 1, N
                                sum = sum - CM(i,k) * BM(j,k)
72                      continue
                        DM(i,j) = sum
71              continue
70      continue

        HOLDA = abs(AM(1,1))
        HOLDB = abs(BM(1,1)) !array indeces start at 1 not 0
        do 73 i = 1, N
                do 74 j = 1, N
                        HOLDA = max(HOLDA,abs(AM(i,j)))
                        HOLDB = max(HOLDB,abs(BM(i,j)))
74              continue
73      continue

        TRACE3 = 0.0

!Loop 80
        do 80 i = 1, N
                TRACE3 = TRACE3 + (AM(IA(i),IA(i)) + BM(IA(i),IA(i))&
                                -DM(IA(i),IA(i))) / (HOLDA * HOLDB)
80      continue

        cpu = cputime() - cpu
        wall = walltime() - wall

        print *, "Final trace = ", TRACE3, " and IDCHECK ", check
        print *, "--RUNTIME--> ", cpu, " seconds"    
end program dusty

        
        REAL(KIND = SELECTED_REAL_KIND(17,307)) function trig (i,j)
                REAL(KIND = DP) :: x, y, z
                pi = acos(-1.0)
                x = dble(i) - dble(j)
                y = dble(i) + dble(j)
                z = exp(sin(sqrt(x**2+y**2)*pi))
                trig = x + y + log10(abs(1+z+(x*y*z)))/ (abs(x)+abs(y))
                return
        end function trig

        REAL(KIND = SELECTED_REAL_KIND(17,307)) function conrand(seed)
                REAL(KIND = DP) :: seed, a, m, temp
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

                l2 = 0.0
                do 30 i = 1, N
                        l2 = l2 + AV(i)**2
30              continue
                l2 = sqrt(l2)
                do 40 i = 1, N
                        AV(i) = AV(i) / l2
40              continue

                l2 = 0.0
                do 50 i = 1, N
                        l2 = l2 + BV(i)**2
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
                                        check = sqrt(b**2 + c**2)
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




