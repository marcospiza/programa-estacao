!PROGRAMA ARA LER DADOS DA ESTAÇÃO CECA

program estceca
implicit none
 
real,dimension(:),allocatable::chuva, ChuvaDV, U2, U2med, tmin,tmed, tmax, TmaxDV,TmedDV, TminDV, U2medDV, UR, URmax, URmed, URmin, URmaxDV,URmedDV, URminDV
integer,dimension(:),allocatable:: mesV,diaV,ano, dia, mes, hora, minut
integer:: Num_dia ! Num de dias do arquivo, calculado pela funcao ndia

call read_fileA("estceca_dados.txt")


contains

function ndia(vec)
integer,dimension(:),intent(in):: vec
integer ndia
integer i, ndata

ndata = size(vec)
i = 1
ndia = 1

!ini = 1

do while(i < ndata)
  do while(vec(i+1) == vec(i) .and. i< ndata)
     i = i +1  
  enddo
  !ifim = i
  !TmaxDV(ndia) = maxval(Tmed(ini,ifim))
  !inin = ifim + 1
  i = i + 1
  ndia = ndia + 1
enddo

if (vec(ndata) == vec(ndata-1)) ndia = ndia -1  
return
end function

subroutine read_fileA(fname)
character(*):: fname
integer::n,io,i,k,j
character(len=1):: lixo
character(len=20):: dat
real:: TmaxD, TmedD, TminD, ChuvaT, U2medD, URmaxD, URmedD, URminD
io=0


open(1,file=fname,status="old")

do i=1,4       !loop p/ pular as primeiras linhas
  read(1,*)
enddo

! Contar as linhas do arquivo
n=0
Do while (io == 0) 
	read (1, *, Iostat = io)
	n = n + 1   !Número de termos
enddo
n = n-1

close(1)


allocate(tmax(n)) 
allocate(tmin(n)) 
allocate(tmed(n))
allocate(U2(n))
allocate(chuva(n))
allocate(UR(n))
allocate(URmax(n))
allocate(URmin(n))
allocate(ano(n))
allocate(mes(n))
allocate(dia(n))
allocate(hora(n))
allocate(minut(n))

open(2,file="saida2",status="replace")
open(1,file=fname,status="old")


do i=1,4       !loop p/ pular as primeiras linhas
  read(1,*)
enddo

do i=1,n
read(1,*)dat ,lixo,lixo,chuva(i), U2(i), lixo, tmed(i), tmax(i), lixo, tmin(i),lixo, UR(i), URmax(I),lixo, URmin(I)

   read(dat(1:4),"(i4)") ano(i) !for Character converts to integer
   read(dat(6:7),"(i2)") mes(i)
   read(dat(9:10),"(i2)") dia(i)
   read(dat(12:13),"(i2)") hora(i)
   read(dat(15:16),"(i2)") minut(i)
enddo
close(1)
 

Num_dia = ndia(dia)
allocate(ChuvaDV(Num_dia))
allocate(TmaxDV(Num_dia))
allocate(TmedDV(Num_dia))
allocate(TminDV(Num_dia))
allocate(U2medDV(Num_dia))
allocate(URmaxDV(Num_dia))
allocate(URmedDV(Num_dia))
allocate(URminDV(Num_dia))
allocate(mesV(Num_dia))
allocate(diaV(Num_dia))



ChuvaT = 0

TmaxD = -99
TmedD = 0
TminD = 1000

U2medD = 0

URmaxD = -99
URmedD = 0
URminD = 1000


k = 0
j= 0

Do i=1,n
	    
	if (dia(i) == dia(i+1)) then
	 k = k + 1
	 chuvaT = chuvaT + chuva(i)
	
	 TmaxD = max(TmaxD,tmax(i))
	 TmedD = TmedD + tmed(i)
	 TminD = min(TminD,tmin(i))
	 
	 U2medD = U2medD + U2(i)
	 
	 URmaxD = max(URmaxD,URmax(i))
	 URmedD = URmedD + UR(i)
	 URminD = min(URminD,URmin(i))
	
	else
	 j = j + 1
	 diaV(j) = dia(i)
	 mesV(j) = mes(i)
	 
	 chuvaT = chuvaT + chuva(i)
	 
	 TmaxDV(j) = max(TmaxD,tmax(i))
	 TmedD = TmedD + tmed(i)
	 TmedDV(j) = TmedD / (k + 1)
	 TminDV(j) = min(TminD,tmin(i))
	 
	 U2medD = U2medD + U2(i)
	 U2medDV(j) = U2medD / (k + 1)
	
	 URmaxDV(j) = max(URmaxD,URmax(i))
	 URmedD = URmedD + UR(i)
	 URmedDV(j) = URmedD / (k + 1)
	 URminDV(j) = min(URminD,URmin(i))
	 
	 
	ChuvaDV(j) = ChuvaT
	ChuvaT = 0

	TmaxD = -99
	TmedD = 0
	TminD = 1000

	U2medD = 0

	URmaxD = -99
	URmedD = 0
	URminD = 1000

	k = 0
	end if
		
enddo

	10 FORMAT( 'linha', T8, 'Dia', T12, 'Mes', T20, 'Chuva', T29, 'VVmed', T37, 'Tmax', T47, 'Tmed', T57, 'Tmin', &
		& T67, 'URmax', T77, 'URmed', T87, 'URmin'/ 14('-'), T20, '(mm)', T29, '(m/s)', T36, '(graus)', T46, '(graus)', &
		& T56, '(graus)', T68, '(%)', T78, '(%)', T88, '(%)'/ 91('-') / )
    20 FORMAT( I3 T8, I2 T12, I2 T16, F8.2 T25, F8.2 T34,F8.2 T44,F8.2 T54, F8.2 T64, F8.2 T74, F8.2 T84, F8.2 T94 )

	write(2,10)
do i=1,j
	write(2,20)i, diaV(i), mesV(i),ChuvaDV(i),U2medDV(i), TmaxDV(i),TmedDV(i), TminDV(i), URmaxDV(i),URmedDV(i), URminDV(i)
enddo
	
	
close (1)
end subroutine

end program