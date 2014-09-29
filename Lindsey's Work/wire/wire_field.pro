print,' %%%%%%% '
print,' %%%%%%% '
print,' %%%%%%% USE wire_auto_field.pro INSTEAD !!! %%%%%%% '
print,' %%%%%%% '
print,' %%%%%%% '
stop









; Identify the neighbours to Altair in the WIRE data


; field = 'HD113226'
; field = 'alphaLeo' ; one object
; field = 'AlphaPav' ; three objects
; field = 'alphaUMa' ; 1
; field = 'AltairA7V'
; field = 'AlphaBoo'
; field = 'HR5698F8V' ; merge file missing in aarhus & usafa
; field = 'GammaEqu'  ; only one object
; field = 'epsilonUMa' ; only one object
; field = 'NSV4058' ; one object
; field = 'epsilonCyg' ; one object
; field = 'EpsilonPeg' ; one object
; field = 'KappaSco'
; field = 'kappaVel' ; two stars only ... secondary star has no valid x,y pos!

; field = 'SigmaSgr' ; no good match
; field = 'HR6596F5V' ; no good match
; field = 'betaPeg' ; main target does not fit.... M giant!
; field = 'AlphaPav' ; very few objects. Central star fits. Perhaps it's time to make a new calibr.a
; field = 'ProcyonF5IV-V' ; GOOD FIT
; field = 'HD113226' ; perfect field match ... 

use_buzasi_calibr = 0B ; Derek's old count rate calibration

device,set_Graphics_function = 3 ; ordinary plot
window,0,xsize=550,ysize=550,title='Wire Identification / Secondary Targets'
plot,[0,1],/nodata


fac = 10.0
fac2 = 20.0 ; governs plot symbol size ... 5 - 40

;fac = 4.5
;fac2 = 8.0 ; governs plot symbol size

scale = 1.03
flipx = -1.0
flipy = 1.0

ang = 0.0 ; start rotation angle in degrees
if field eq 'Altair'   then ang = 279.0 
if field eq 'HD113226' then ang =  72.0
if field eq 'alphaPav' then ang =  -72.0


sp = 1.0 ; size of plot window
dops = 0 ; always zero!

;case field of
; 'Altair':   nfile = '~/bruntt/wire/altair_ident.txt'
; 'HD113226': nfile = '~/bruntt/wire/hd113226_ident.txt'
;
nfile = '~/wire/ident/'+field+'_ident.txt'
spawnrob,'ls -1 ' + nfile,a
if a(0) eq '' then begin
 print,'' & print,' *** Ident file not found: '+nfile & print,''
 stop
endif 




col = getcolor(/load)
czz = 1.0 ; characther thickness !

nlin,nfile,nn

if nn le 10 then stop

ra  = fltarr(nn)
dec = fltarr(nn)
vv  = fltarr(nn)
bb  = fltarr(nn)
nam = strarr(nn)
all = strarr(nn)
vv(*) = -99.9
bb(*) = -199.99
typ = strarr(nn)


dd = ''
get_lun,uu
openr,uu,nfile

for i=0L,nn-1 do begin
 readf,uu,dd
 a = strsplit(dd,'|',/extract)

 typ(i) = strcompress(a(1),/remove_all)

 b = strsplit(a(2),' ',/extract)
  nb = n_elements(b)

  if nb gt 6 or nb le 5 then goto,failed
  ra(i)  = float(b(0)) + float(b(1))/60. + float(b(2)) / (3600.) ; hours
  ra(i)  = ra(i) * 360. / 24. ; degrees
  dec(i) = float(b(3)) + float(b(4))/60. + float(b(5)) / (3600.) ; degrees
  nam(i) = strcompress(a(0))
  all(i) = dd

 mag = strsplit(a(3),' ',/extract)
 nm = n_elements(mag)

 bb(i) = 99.
 vv(i) = 150.

 if nm ge 3 then begin
    if strmatch(a(3),'*V*') eq 1 or strmatch(a(3),'*:*') eq 1 then begin
       bb(i) = float(mag(0))
       vv(i) = float(mag(2))
    endif
 endif 

 if nm eq 2 then begin
    if strmatch(a(3),'*V*') ne 1 and strmatch(a(3),'*:*') ne 1 then begin
      bb(i) = float(mag(0))
      vv(i) = float(mag(1))
    endif
 endif

failed:

endfor
close,uu & free_lun,uu

bv = bb-vv
w = where(abs(bv) gt 50 or bb lt -50. or vv lt -50.,c)
w2 = where(abs(bv) lt 50 and bb gt -50. and vv gt -50.,c)
w2 = where(abs(bv) lt 3. and vv gt -2. and vv lt 8.5 and bb gt -3. and bb lt 12.,c)

w2 = where(typ eq '*' and nam ne '',c)

bv = bv(w2)
vv = vv(w2)
bb = bb(w2)
ra = ra(w2)
dec = dec(w2)
nam = nam(w2)
all = all(w2)
typ = typ(w2)

ra = ra - median(ra)

ra_proj = ra / cos(dec * !PI / 180. ) ; sky projection will change RA! DEC is unchanged! Stars at high dec. will be further apart (in degrees) on the REAL sky.
; plot,dec,ra,psym=4,ysty=3,yr=[292,310]
; oplot,dec,ra_proj,psym=2,col=col.green

mra  = ra_proj(0)  ; median(ra)
mdec = dec(0) ; median(dec)

; Count rate for a star with V=0.0 in one second: (534400 + 246230*bv+ 976300*bv^2)
; For stars B-V > 0.72 the count rate is 1.22e6

; cr = (534400 + 246230*bv+ 976300*bv^2) * 0.5 * (10.^(-0.4*vv))

   cr_temp = 11.3967 + 1.09573*vv - 0.986342 * bv
   cr      = 10.^((25.0-cr_temp)/2.5)

 ; Calibration on 6th of April 2004 ;
 ; 18 main targets from WIRE --- sigma = 0.78 mag
   cr_temp = 11.7777 +      0.948206 * vv    -0.644600 * bv   +   0.179909 * bv^2
   cr      = 10.^((25.0-cr_temp)/2.5)
   cr_err0 = 10.^((25.0-cr_temp + 0.78)/2.5)
   cr_err1 = 10.^((25.0-cr_temp - 0.78)/2.5)

; Mean counts in 0.5 secs for the five stars in the altair field


mergefile = '/data2/bruntt/wire/dat/'+field+'/'+field + '_merged_allslots_31.idl'
spawnrob,'ls -1 ' + mergefile,merg
if merg(0) eq '' then begin
 mergefile = '/data2/bruntt/wire/dat/'+field+'/data/'+ '*_merged_allslots_31.idl'
 spawnrob,'ls -1 ' + mergefile,merg
 if merg(0) eq '' then begin
  print,'' & print,' *** LC file not found: '+mergefile & print,''
  stop
 endif
endif 
 merg = merg(0)
 print,' %%% Merge file: ' + merg
 restore,merg

nstar = n_elements(wireult(0).x)
if nstar eq 1 then begin
 print,'' & print,' *** Only one object observed for: '+field & print,''
 stop
endif
dx0 = fltarr(nstar) & dy0 = dx0 & ct = dx0
for ss=0,nstar-1 do begin
 wg = where(wireult.mag(ss) gt 5. and wireult.mag(ss) lt 25. and $
            wireult.x(ss) gt .5 and wireult.x(ss) lt 510.5 and $
            wireult.y(ss) gt .5 and wireult.y(ss) lt 510.5,cg)
 if cg ge 1000 then begin
  dx0(ss) = median(wireult(wg).x(ss))
  dy0(ss) = median(wireult(wg).y(ss))
   ct(ss) = median(wireult(wg).mag(ss))
   ct(ss) = 10.^( (25.0 - ct(ss)) / 2.5 )
 endif
endfor

wok = where(ct gt 10,cok)
dx0 = dx0(wok) & dy0 = dy0(wok) & ct = ct(wok)
if abs(dx0(0) - 260) gt 10. or abs(dx0(0) - 260) gt 10. then stop
dx0 = dx0 - dx0(0)
dy0 = dy0 - dy0(0)

print,' %%% # Input stars: ' + string(nstar,format='(I3)')
print,' %%% # Valid stars: ' + string(cok  ,format='(I3)')


;case field of

; 'Altair': begin
;    ct = [ 57816.8    ,  3269.00  ,    15210.0,      3591.97  ,    173176]
;   dx0 = [  114 , -124 , -155 ,  -41  ,   0] 
;   dy0 = [  -49 ,  180 ,   43 ,  145  ,   0]
; endcase

; 'HD113226': begin
;   ct = 10^((25.-[      13.630, 16.681, 18.629, 16.749, 16.025, 15.962])/2.5)
;  dx0 = [      263.291, 416.483, 436.475, 282.420, 180.485, 42.439]
;  dy0 = [      261.147, 454.431, 280.418, 146.477, 99.412, 102.465]
;  dx0 = dx0 - dx0(0) ; reference star pos = 0,0
;  dy0 = dy0 - dy0(0)
; endcase

;endcase


nt = n_elements(ct)

plot_io,vv,cr,psym=3,yr=[1e2,3e6],ysty=3,xsty=3,xr=[0,12]
 for i=0,n_elements(ct)-1 do plots,!x.crange,ct(i),thick=2



; NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD
;  1 2048 2061    10.0 62000.0   25.00    2.00    1.80    4.40    2.50;;

;     4 1366.780  534.881    9.731    0.038   32.068       5.    0.800    0.000
;     5 1086.329 1652.433    9.478    0.051   28.551       5.    0.800    0.000
get_lun,u
openw,u,'wire.als'
printf,u,' NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
printf,u,'  1 2048 2061    10.0 62000.0   25.00    2.00    1.80    4.40    2.50'
printf,u,''
for i=0,nt-1 do printf,u,i+1,dx0(i)/60.,dy0(i)/60.,25. - 2.5*alog10(ct(i)),0.010, 50.000, 5., 0.8, 0.0,$
 format='(I6,5F9.3,F9.0,2F9.3)'
close,u & free_lun,u

get_lun,u
openw,u,'simbad.als'
printf,u,' NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
printf,u,'  1 2048 2061    10.0 62000.0   25.00    2.00    1.80    4.40    2.50'
printf,u,''
for i=0,c-1 do begin
 vv1 = vv(i) & bv1 = bv(i)

; Buzasi's calibration
   cr1_buzasi = (534400 + 246230*bv1+ 976300*bv1^2) * 0.5 * (10.^(-0.4*vv1))

; Bruntt's calibration, 26 JAN 2004
   cr_temp = 11.3967 + 1.09573*vv1 - 0.986342 * bv1
   cr1 = 10.^((25.0-cr_temp)/2.5)

; Bruntt's calibration, 06 APR 2004

   cr_temp = 11.7777 +      0.948206 * vv1    -0.644600 * bv1 +     0.179909 * bv1^2
   cr1      = 10.^((25.0-cr_temp)/2.5)
   cr_err00 = 10.^((25.0-cr_temp + 0.78)/2.5)
   cr_err11 = 10.^((25.0-cr_temp + 0.78)/2.5)

if use_buzasi_calibr eq 1 then cr1 = cr1_buzasi

 mg1 = 25. - 2.5 * alog10(cr1)
 printf,u,i+1,ra_proj(i)-mra,dec(i)-mdec,mg1,0.010, 50.000, 5., 0.8, 0.0,$
 format='(I6,5F9.3,F9.0,2F9.3)'
endfor
close,u & free_lun,u

; stop

agg:
if dops eq 0 then device,set_Graphics_function = 3 ; ordinary plot

dy = dy0 * flipy
dx = dx0 * flipx

dx = dx * scale
dy = dy * scale

dx1 = dx / 60. & dy1 = dy / 60. ; offsets in degrees

 plot,ra_proj-mra,dec-mdec,psym=3,xsty=3,ysty=3,/nodata,$
  xtit='Proj-RA [Deg]',ytit='DEC [Deg]',tit='WIRE Field: ' + field,$
  xr= [-5,5]*sp,yr=[-5,5]*sp,xthick=3,ythick=3,charsi=1.3,$
  charthick=2.0
 plotsym,0

for i=0,nt-1 do begin
 rmax = sqrt( dx1(i)^2. + dy1(i)^2. ) & rmin = rmax
 tvellipse, rmax, rmin, 0, 0, 0.,thick=2,/data,linestyle=2 
endfor

for i=0,c-1 do begin
 vv1 = vv(i) & bv1 = bv(i)

  cr1_buzasi = (534400 + 246230*bv1+ 976300*bv1^2) * 0.5 * (10.^(-0.4*vv1))

; Bruntt, JAN 2004:
   cr_temp = 11.3967 + 1.09573*vv1 - 0.986342 * bv1
; Bruntt's calibration, 06 APR 2004
   cr_temp = 11.7777 +      0.948206 * vv1    -0.644600 * bv1 +     0.179909 * bv1^2
   cr1 = 10.^((25.0-cr_temp)/2.5)

   cr_temp = 11.7777 +      0.948206 * vv1    -0.644600 * bv1 +     0.179909 * bv1^2
   cr1      = 10.^((25.0-cr_temp)/2.5)
   cr_err00 = 10.^((25.0-cr_temp + 0.78)/2.5)
   cr_err11 = 10.^((25.0-cr_temp + 0.78)/2.5)

 if use_buzasi_calibr eq 1 then cr1 = cr1_buzasi

 mg1 = 25. - fac * alog10(cr1)
 sz1 = (25.- mg1)/fac2

 if abs(ra_proj(i)-mra) lt 5.*sp and abs(dec(i)-mdec) lt 5.*sp then begin
 if vv1 lt -10 or vv1 gt 20 or abs(bv1) lt -30 then $
     plots,ra_proj(i)-mra,dec(i)-mdec,psym=8,symsize=.5,col=col.red else $ 
     plots,ra_proj(i)-mra,dec(i)-mdec,psym=8,symsize=sz1 ; SIMBAD stars!

 endif


 
endfor

plotsym,0,thick=3


; Rotate the diagram
ff = 1.

!MOUSE.BUTTON = 1
if dops eq 0 then device,set_Graphics_function = 6 ; temporary plot
dx2 = dx1 & dy2 = dy1
dr = 4./60 ; positional error

nz = n_elements(dx1)
 for i=0,nz-1 do begin
   mg2 = 25. -fac * alog10(ct(i))
   sz2 = (25.-mg2) / fac2
   if dops eq 0 then $
    plots,dx2(i),dy2(i),psym=8,col=col.green,symsize = sz2

 endfor

repeat begin

; Erase old plot
 for i=0,nz-1 do begin
   mg2 = 25. - fac * alog10(ct(i))
   sz2 = (25.-mg2) / fac2
   if dops eq 0 then $
    plots,dx2(i),dy2(i),psym=8,col=col.green,symsize = sz2
 endfor

 angr = ang * !PI / 180.
 dx2 = dx1 * cos(angr) - dy1 * sin(angr)
 dy2 = dx1 * sin(angr) + dy1 * cos(angr)

; New plot
 for i=0,nz-1 do begin
   mg2 = 25. - fac * alog10(ct(i))
   sz2 = (25.-mg2) / fac2
   if dops eq 0 then $
    plots,dx2(i),dy2(i),psym=8,col=col.green,symsize = sz2
 endfor

; cursor,colnew,rownew,/down,/normal
; ang = ang + ff * 4.

limdis = .5 ; .5 ... 1.5

print,' -------------------------------  --------------------------- ----------'

; Detect the best match!
for k=0,nz-1 do begin

; Only distance has to match:
  dis = sqrt( (dx2(k) - (ra_proj-mra))^2. + ( dy2(k) - (dec-mdec) )^2. )
  wmat = where(dis lt limdis,cmat)
; Both count rate and position must match:
;  wmat = where(abs( (ct(k)-cr)/ct(k) ) lt 2.8 and dis lt limdis,cmat)

; Print the match to the user
  if cmat ge 1 and cmat lt 25 then begin
    nam9 = nam(wmat)
    print,' %%% Count rate (B,V) = '+string(cr(wmat),format='(I7)') + $
     '  ' + string(ct(k),format='(I7)') + ' = Obs C.R. -- ' + $
     ' Name = ' + nam9 + ' B,V: ' + $
      string(bv(wmat),format='(F5.2)') + ' ' + string(vv(wmat),format='(F5.2)')
  endif

  if cmat ge 1 then begin

;   for i=0,cmat-1 do xyouts,ra_proj(wmat(i))-mra,dec(wmat(i))-mdec,$
;     strcompress(string(bv(wmat(i)),format='(F5.1)'),/remove_all),charsi=1

  
     a = where(dis(wmat) eq min(dis(wmat)),ca)
     if ca eq 1 then a = a(0)

     if dops eq 1 then $
        print,strcompress(all(wmat(a)))+$
         string(bv(wmat(a)),format='(F5.2)')+' '+$
         string(cr(wmat(a)),format='(I7)')+' '+$
         string(ct(k),format='(I7)')
     c5 = col.sky & if dops ge 1 then c5 = 0

     nam5 = nam(wmat(a)) 
      if strmatch(nam5,'NAME*') eq 1 then begin ; remove "NAME"
       hh = strsplit(nam5,' ',/extract)
       nam5 = hh(1)
       ; if nam5 eq 'TARAZED' then stop
      endif

   if dops ge 1 then czz = 3.0

     if dops le 1 then $       
     xyouts,ra_proj(wmat(a))-mra,dec(wmat(a))-mdec,nam5,$
             charsi=1.1,col=c5,charthick=czz
     

   if dops ge 1 then czz = 1.0

      vv1 = vv(wmat(a)) & bv1 = bv(wmat(a))

; Derek B's count rate calibration:
;      cr1 = (534400 + 246230*bv1+ 976300*bv1^2) * 0.5 * (10.^(-0.4*vv1))

; Bruntt's calibr:

      cr_temp = 11.3967 + 1.09573*vv1 - 0.986342 * bv1
; Bruntt's calibration, 06 APR 2004
      cr_temp = 11.7777 +      0.948206 * vv1    -0.644600 * bv1 +     0.179909 * bv1^2
      cr1 = 10.^((25.0-cr_temp)/2.5)


      mg1 = 25. -fac * alog10(cr1)
      sz1 = (25.-mg1)/fac2
      if dops eq 1 then $
       plots,ra_proj(wmat(a))-mra,dec(wmat(a))-mdec,col=c5,psym=8,symsi=sz1*1.1
  endif

endfor


if dops ge 1 then begin
 dops = 0
 device,/close
 set_plot,'x'
endif

cc = get_kbrd(1)
case cc of
 'a': ang = ang + 1. ; rotation angles
 's': ang = ang - 1.
 'A': ang = ang + 12.
 'S': ang = ang - 12.
 'q': scale = scale + 0.05 ; apply different scale
 'Q': scale = scale + 0.15
 'w': scale = scale - 0.05
 'W': scale = scale - 0.15
 'E': scale = 1.0 ; reset scale
 'f': flipx = flipx * (-1.0)
 'F': flipy = flipy * (-1.0)
 'p': sp = sp * 0.9
 'P': sp = sp * 1.1
 'O': dops = 2 ; do not overplot the matching stars!
 'o': dops = 1 
 'i': begin 
       print,'Name','Cnt Simbad','Cnt Obs',format='(A12,A11,A11)'
       nmatch = n_elements(wmat)
       if wmat(0) ne -1 then begin
       for mev=0,nmatch-1 do begin
;          cr0 = 11.3967 + 1.09573*vv(wmat(mev)) - 0.986342 * bv(wmat(mev))
          cr0= 11.7777 +      0.948206 * vv(wmat(mev))    -0.644600 * bv(wmat(mev)) +     0.179909 * bv(wmat(mev))^2.
          cr1 = 10.^((25.0-cr0)/2.5)

          print, nam(wmat(mev)), $
                 cr1, $
                 ct(mev+1), $
                 format='(A12,F11.1,F11.1)'          
       endfor
       endif
      endcase
  'x': begin
        device,set_Graphics_function = 3 ; ordinary plot
        stop 
       endcase
 else: ax = 5 ; do nothing - print,ang,scale,flipx,flipy
endcase
 if scale le 0.05 then scale = 0.05 

if dops ge 1 then begin
 device,set_Graphics_function = 3 ; ordinary plot
  ; outn = '~/wire/wire_match_'+string(dops,format='(I1)')+'.ps'
  outn = '~bruntt/wire/wire_match_'+field+'_'+string(dops,format='(I1)')+'.ps'
 a4,x=18,y=17,name=outn
 print,'Output ps file:    $gv '+outn+' & '
endif

goto,agg

endrep until cc eq 'x' ;
; endrep until !MOUSE.BUTTON ne 1 ; until left mouse button is not pressed

;; ff = -1. * ff
;scale = scale + 0.1
;goto,agg

device,set_Graphics_function = 3 ; ordinary plot

       ;plot,vv,25. -2.5 * alog10(cr),psym=3,ysty=3,xsty=3,xr=[0,12]

end


