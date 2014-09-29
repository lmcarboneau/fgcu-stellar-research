PRO wire_brightwater_rename,wildcard

; Example:
; wire_brightwater_rename, '/export/brixx1/bruntt/wire/psicen/groundbased/brightwater/test_16APR2006/*.0*'

; Struture with RA / DEC positions of target stars:
position = replicate({name:'',ra:0.,dec:0.},3)
position(0).name = 'psicen' & position(0).ra = 15.*ten(14,20,33.4316) & position(0).dec = ten(-37,53,07.061)
position(1).name = 'hr5357' & position(1).ra = 15.*ten(14,19,23.8793) & position(1).dec = ten(-37,00,10.464)
position(2).name = 'hr5471' & position(2).ra = 15.*ten(14,41,57.5909) & position(2).dec = ten(-37,47,36.595)

spawnrob,'ls -1 ' + wildcard, l
n = n_elements(l)
if n eq 0 then begin
 print,' %%% No images with wildcard: ' + wildcard
 RETURN
endif

for i=0,n-1 do begin

 fil = l(i)
; x = strsplit(fil,'/',/extract) & nx = n_elements(x)
; y = strsplit(x(na-1),'.',/extract)
; command = 'mv ' + fil + ' ' + fil2
; print,command

 img = float(readfits(fil,head,/silent))


 date = head(6) ; UT date
 time = head(8) ; UT at start
 texp = head(15) ; exp. time in seconds
 filter = head(27) ; filter

 x1 = strsplit(filter,"'",/extract) 
 usefilter = strcompress(x1(1),/remove_all)
 

 x1 = strsplit(date,"'",/extract)

 x2 = strsplit(x1(1),"-",/extract)
 year = float(x2(0)) & month = float(x2(1)) & day = float(x2(2))
 x1 = strsplit(time,"'",/extract)
 x2 = strsplit(x1(1),":",/extract)
 hour = float(x2(0)) & minute = float(x2(1)) & sec = float(x2(2))
 x1 = strsplit(texp," ",/extract)
 texposure = float(x1(2))

; Minutes at mid-exposure:
 min_x = minute + (sec+texposure*0.5)/60. 

; Extracted time values:
; print,year,month,day,hour,minute,sec,texposure, min_x

 juldate, [year, month, day, hour, min_x], jd_temp ; Julian Date
; help,jd_temp

 nam = strsplit(fil,'/',/extract) & nnam = n_elements(nam)
 obj = strsplit(nam(nnam-1),'_',/extract)
 w = where(strmatch(position.name,'*'+obj(0)+'*',/fold) eq 1,c)
 ;print,' %%% Julian date: ', jd_temp,format='D16.8'
 if c eq 1 then begin
   ;print,' %%% Object found: ' + nam(nnam-1) + ' ---> ' , position(w)
   hjd_temp = helio_jd( jd_temp, position(w).ra, position(w).dec) ; Heliocentric Julian Date, ra/dec in degrees!
   ;print,' %%% Heliocentric julian date: ',hjd_temp

   sxaddpar, head, 'HJD', hjd_temp, 'Heliocentric Julian Date - wire_brightwater_rename.pro'

 endif else print,' %%% RA/DEC not found: ' + obj(0)
   sxaddpar, head, 'JD', jd_temp, 'Julian Date - wire_brightwater_rename.pro',format='D16.8'

; The JD times agree to the 5th significant digit with US Naval
; Observatory (1 second):
; http://aa.usno.navy.mil/data/docs/JulianDate.html

 z = strsplit(fil,'.',/extract)
 fil2 = z(0) + '_' + z(1) + '_' + usefilter + '.fit'

 writefits,fil2,img,head

endfor

END
