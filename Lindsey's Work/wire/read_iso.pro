FUNCTION read_evol, filename
;This function will read a Formatted Data File 
;Produced by the program write_iso  
;into a structure 
;
;e.g.
;IDL>filename='iso2345.txt'
;IDL>iso10=read_iso(filename)
;IDL>help,  iso10, /struc
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
;original file:
;/usr/users/hrj/GIRAFFE/data/Reduced/Normering/logLTeff/Clem/read_iso.pro
;
;Replay to Henrik R. Jensen
;          hrj@phys.au.dk 
 on_error,2

 if n_elements(filename) eq 0 then begin
  message,'Call with: Structurename=read_iso(filename)' ,/info
  return, '0'
 endif

dummy=' '
z=0.000
x=0.00
alpha=0.0
tage=0.00
feh=0.0
n=1

;lum=:reform(lum), MMsun:reform(MMsun), qc:reform(qc),$;
;     Radius:reform(radius),Teff:reform(teff),xc:reform(xc), $
 ;    z:z, x:x,alpha:alpha,tage:tage,feh:feh}

openR, lun, filename, /Get_lun
readf,lun, z
readf,lun, x
readf,lun, alpha
readf,lun, tage
readf,lun, feh
readf,lun, dummy
readf,lun, n
readf,lun, dummy
readf,lun, dummy
array=fltarr(6,n)
readf,lun, array, format='(6(F12.5, 2x))'

Free_lun, lun


mmsun=reform(array(0,*))
teff=reform(array(1,*))
lum=reform(array(2,*))
radius=reform(array(3,*))
xc=reform(array(4,*))
qc=reform(array(5,*))

return, {lum:reform(lum), MMsun:reform(MMsun), qc:reform(qc),$
     Radius:reform(radius),Teff:reform(teff),xc:reform(xc), $
     z:z, x:x,alpha:alpha,tage:tage,feh:feh} 
end

pro  write_evol,  savfile, filename
;This program will write a Formatted Data file
;From Tine B. Nielsen save file
;     tbn@phys.au.dk
;
;Replay to Henrik R. Jensen
;          hrj@phys.au.dk

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
str={lum:reform(lum), MMsun:reform(MMsun), qc:reform(qc),$
     Radius:reform(radius)/rsun,Teff:reform(teff),xc:reform(xc), $
     z:z, x:x,alpha:alpha,tage:tage,feh:feh}
  

formatvar='(6(F12.5, 2x))'

n=n_elements(str.lum)
strarrvar=['Mmsun','Teff','Lum','Radius','xc','qc']
strarrunit=['M/(M_sun)','Kelvin','L/(L_sun)','R/(R_sun)','Central H','M conv core']
streg=replicate('-',82)

openw, lun, filename, /get_lun

printf, lun,str.z/1000., 'Z value','1=Z+X+Y ', format='(F12.5,2x,A12,2x,A12)'
printf, lun,str.x, 'X value', ' ', format='(F12.5,2x,A12,2x,A12)'
printf, lun,str.alpha, 'alpha','Mixing length ', format='(F12.5,2x,A12,2x,A12)'
printf, lun,str.tage, 'Age', 'Gyr', format='(F12.5,2x,A12,2x,A12)'
printf, lun,str.feh, '[Fe/H]', ' ', format='(F12.5,2x,A12,2x,A12)'

printf, lun,streg,  format='(82(A1))'

printf, lun, N, 'Number of values', format='(I12,2x,A20)'

printf, lun,strarrunit, format='(6(A12,2x))'
printf, lun,strarrvar, format='(6(A12,2x))'
for i=0,n-1 do begin
printf, lun, str.mmsun[i], $
             str.teff[i],  $
             str.lum[i],   $
             str.radius[i],$
             str.xc[i],    $
             str.qc[i],    format=formatvar
endfor
Free_lun, lun
end


