program dusty
        USE ExtraFunc
        IMPLICIT NONE
        !variable declarations/initializations
        !INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(17, 307)
        INTEGER, PARAMETER :: MAXDIM = 25
        INTEGER, DIMENSION(MAXDIM) :: IA
        INTEGER :: N, i, j, k, ival
        REAL(KIND = DP), DIMENSION(MAXDIM) :: AV, BV, CV
        REAL(KIND = DP), DIMENSION(MAXDIM, MAXDIM) :: OP, ID, AM, BM, CM, DM
        REAL(KIND = DP) :: check, BOT, TOP, HOLDA, HOLDB, TRACE3
        REAL(KIND = 4) :: sum

#ifdef YO4
#endif

        REAL(KIND = DP) :: seed, wall, cpu, walltime, cputime
        EXTERNAL :: walltime, cputime
    
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

        !Compute |AV><BV|
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

HOLDB = 0D0;


!Loop 50
#ifdef YO1
        do 50 i = 1, N
                do 52 j = i+1, N
                        CM(i,j) = 0.0
                        do 55 k = 1, N
                                CM(i,j) = CM(i,j) - AM(i,k) * BM(k,j) / check
55                      continue
52              continue
50       continue

do 51 i = 1, N
                do 53 j = i, 1, -1
                        CM(i,j) = 0.0
                        do 56 k = 1, N
                                CM(i,j) = CM(i,j) + AM(i,k) * BM(k,j) / check
56                      continue
53              continue
51       continue

#else
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
#endif
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
                        !write(*,*)"CM: ",CM(i,j)
64              continue
63      continue

!Loop 70
        do 70 i = 1, N
                do 71 j = 1, N
                        sum = 0.0
                        do 72 k = 1, N
                                sum = sum - CM(i,k) * BM(j,k)
                                !write(*,*)"sum at i,j: ",sum
72                      continue
                        DM(i,j) = sum
                        !write(*,*)"sum at k: ",sum
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
                                - DM(IA(i),IA(i))) / (HOLDA * HOLDB)
80      continue

        cpu = cputime() - cpu
        wall = walltime() - wall

        write(*,*), "Final trace = ", TRACE3, " and IDCHECK ", check
        write(*,*), "Runtime = ", cpu, " seconds"
end program dusty





