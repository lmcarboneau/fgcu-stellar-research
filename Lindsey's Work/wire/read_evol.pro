FUNCTION read_evol, filename
;This function will read a Formatted Data File 
;Produced by the program write_evol  
;into a structure 
;
;The files are located at:
;/ai43/tbn/isokron/hans/*.txt
; HB: fra Z til Fe/H: alog10((1D*z/1000.)/0.02 )
;e.g.
;IDL>filename='mass1000_metal06.txt'  m=10 solmasser og metallicitet=-0.52
;IDL>evol10=read_evol(filename)
;IDL>help,  evol10, /struc
;   LUM             FLOAT     Array[1498]
;   MMSUN           FLOAT     Array[1498]
;   QC              FLOAT     Array[1498]
;   RADIUS          FLOAT     Array[1498]
;   TEFF            FLOAT     Array[1498]
;   XC              FLOAT     Array[1498]
;   Z               FLOAT        0.00600000
;   X               FLOAT          0.730000
;   ALPHA           FLOAT           1.80000
;   TAGE            FLOAT           3.20000
;   FEH             FLOAT         -0.523000
;
;IDL>plot, evol10.teff, evol10.lum
;
;
;Units 
;LUM: Bolemetric luminosities in solar units
;MMsun: Mass in solar mass
;QC: Mass of the convective core 
;Radius: radius in solar radius(6.9599E10)
;Teff: in Kelvin
;XC: Central Hydrogen
;ALPHA: Mixing Length
;TAGE: Age in Gyr
;FEH: [Fe/H]
;
;
;
;Reply to Henrik R. Jensen or Tine Bjørn Nielsen
;          hrj@phys.au.dk      tbn@phys.au.dk
 on_error,2

 if n_elements(filename) eq 0 then begin
  message,'Call with: Structurename=read_evol(filename)' ,/info
  return, '0'
 endif

dummy=' '
z=0.000
x=0.00
alpha=0.0
mmsun=0.00
feh=0.0
n=1

;lum=:reform(lum), MMsun:reform(MMsun), qc:reform(qc),$;
;     Radius:reform(radius),Teff:reform(teff),xc:reform(xc), $
 ;    z:z, x:x,alpha:alpha,tage:tage,feh:feh}

openR, lun, filename, /Get_lun
readf,lun, mmsun
readf,lun, z
readf,lun, x
readf,lun, alpha
readf,lun, feh
readf,lun, dummy
readf,lun, n
readf,lun, dummy
readf,lun, dummy
array=fltarr(6,n)
readf,lun, array, format='(6(F12.5, 2x))'

Free_lun, lun


age=reform(array(0,*))
radius=reform(array(1,*))
teff=reform(array(2,*))
lum=reform(array(3,*))
xc=reform(array(4,*))
qc=reform(array(5,*))

return, {lum:reform(lum), age:reform(age), qc:reform(qc),$
     Radius:reform(radius),Teff:reform(teff),xc:reform(xc), $
     z:z, x:x,alpha:alpha,mmsun:mmsun,feh:feh} 
end


pro  write_evol,  savfile, filename
;This program will write a Formatted Data file
;From Tine B. Nielsen save file
;     tbn@phys.au.dk
;
;Reply to Henrik R. Jensen
;          hrj@phys.au.dk


;write: write_evol,.sav,.txt

if n_elements(savfile) eq 0 then begin  
savfile=' '
read, 'The .sav file: ', savfile 
endif

if n_elements(filename) eq 0 then begin
filename=' '
read, 'The new .txt file: ', filename 
endif

rsun=6.9599E10

restore, savfile

n=n_elements(lum)
str={lum:reform(lum), age:reform(age), qc:reform(qc),$
     Radius:reform(radius)/rsun,Teff:reform(teff),xc:reform(xc), $
     z:z, x:x,alpha:alpha,MMsun:mmsun,feh:feh}
  

formatvar='(4(F12.5, 2x),2(E12.5,2x))'

n=n_elements(str.lum)
strarrvar=['Age','Radius','Teff','Lum','xc','qc']
strarrunit=['Gyr','R/(R_sun)','Kelvin','L/(L_sun)','Central H','M conv core']
streg=replicate('-',82)

openw, lun, filename, /get_lun

printf, lun,str.MMsun, 'Mass', 'M/(M_sun)', format='(F12.5,2x,A12,2x,A13)'
printf, lun,str.z/100., 'Z value','1=Z+X+Y ', format='(F12.5,2x,A12,2x,A13)'
printf, lun,str.x, 'X value', ' ', format='(F12.5,2x,A12,2x,A13)'
printf, lun,str.alpha, 'alpha','Mixing length ', format='(F12.5,2x,A12,2x,A13)'
printf, lun,str.feh, '[Fe/H]', ' ', format='(F12.5,2x,A12,2x,A13)'

printf, lun,streg,  format='(82(A1))'

printf, lun, N, 'Number of values', format='(I12,2x,A20)'

printf, lun,strarrunit, format='(6(A12,2x))'
printf, lun,strarrvar, format='(6(A12,2x))'
for i=0,n-1 do begin
printf, lun, str.age[i], $
             str.radius[i],  $
             str.teff[i],   $
             str.lum[i],$
             str.xc[i],    $
             str.qc[i],    format=formatvar
endfor
Free_lun, lun
end


