; Identify the neighbours to Altair in the WIRE data

device,set_Graphics_function = 3 ; ordinary plot
window,0,xsize=450,ysize=450,title='Wire Identification / Seconday Targets'
plot,[0,1],/nodata


fac = 10.0
fac2 = 25.0 ; governs plot symbol size

;fac = 4.5
;fac2 = 8.0 ; governs plot symbol size

scale = 0.95
flipx = 1.0
flipy = 1.0
ang = 5.5 ; start rotation angle in degrees

nfile = '/ai38/bruntt/wire/data/altair_neigh4.dat'
col = getcolor(/load)

nlin,nfile,nn

ra  = fltarr(nn)
dec = fltarr(nn)
vv  = fltarr(nn)
bb  = fltarr(nn)
vv(*) = -99.9
bb(*) = -199.99


dd = ''
openr,1,nfile

for i=0,nn-1 do begin
 readf,1,dd
 a = strsplit(dd,'|',/extract)

 b = strsplit(a(2),' ',/extract)
  nb = n_elements(b)
  if nb gt 6 or nb le 5 then goto,failed
  ra(i)  = float(b(0)) + float(b(1))/60. + float(b(2)) / (3600.) ; hours
  ra(i)  = ra(i) * 360. / 24. ; degrees
  dec(i) = float(b(3)) + float(b(4))/60. + float(b(5)) / (3600.) ; degrees

 mag = strsplit(a(3),' ',/extract)
 nm = n_elements(mag)

 if nm le 1 or nm ge 3 or strmatch(a(3),'*V*') eq 1 or strmatch(a(3),'*:*') eq 1 then begin
;  print,mag
 endif else begin
  bb(i) = float(mag(0))
  vv(i) = float(mag(1))
 endelse

failed:

endfor
close,1

bv = bb-vv
w = where(abs(bv) gt 50 or bb lt -50. or vv lt -50.,c)
w2 = where(abs(bv) lt 50 and bb gt -50. and vv gt -50.,c)
w2 = where(abs(bv) lt 3. and vv gt -2. and vv lt 8.5 and bb gt -3. and bb lt 12.,c)

bv = bv(w2)
vv = vv(w2)
bb = bb(w2)
ra = ra(w2)
dec = dec(w2)

ra_proj = ra / cos(dec * !PI / 180. ) ; sky projection will change RA! DEC is unchanged!

mra  = ra_proj(0)  ; median(ra)
mdec = dec(0) ; median(dec)

; Count rate for a star with V=0.0 in one second: (534400 + 246230*bv+ 976300*bv^2)
; For stars B-V > 0.72 the count rate is 1.22e6

cr = (534400 + 246230*bv+ 976300*bv^2) * 0.5 * (10.^(-0.4*vv))

; Mean counts in 0.5 secs for the five stars in the altair field
ct =[ 57816.8    ,  3269.00  ,    15210.0,      3591.97  ,    173176]
nt = n_elements(ct)

plot_io,vv,cr,psym=3,yr=[1e2,3e6],ysty=3,xsty=3,xr=[0,12]
 for i=0,n_elements(ct)-1 do plots,!x.crange,ct(i),thick=2

dx0 = [  114 , -124 , -155 ,  -41  ,   0] ; offsets relative to altair in arc minutes
dy0 = [  -49 ,  180 ,   43 ,  145  ,   0]


; NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD
;  1 2048 2061    10.0 62000.0   25.00    2.00    1.80    4.40    2.50;;

;     4 1366.780  534.881    9.731    0.038   32.068       5.    0.800    0.000
;     5 1086.329 1652.433    9.478    0.051   28.551       5.    0.800    0.000
openw,1,'wire.als'
printf,1,' NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
printf,1,'  1 2048 2061    10.0 62000.0   25.00    2.00    1.80    4.40    2.50'
printf,1,''
for i=0,4 do printf,1,i+1,dx0(i)/60.,dy0(i)/60.,25. - 2.5*alog10(ct(i)),0.010, 50.000, 5., 0.8, 0.0,$
 format='(I6,5F9.3,F9.0,2F9.3)'
close,1

openw,1,'simbad.als'
printf,1,' NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
printf,1,'  1 2048 2061    10.0 62000.0   25.00    2.00    1.80    4.40    2.50'
printf,1,''
for i=0,c-1 do begin
 vv1 = vv(i) & bv1 = bv(i)
 cr1 = (534400 + 246230*bv1+ 976300*bv1^2) * 0.5 * (10.^(-0.4*vv1))
 mg1 = 25. -2.5 * alog10(cr1)
 printf,1,i+1,ra_proj(i)-mra,dec(i)-mdec,mg1,0.010, 50.000, 5., 0.8, 0.0,$
 format='(I6,5F9.3,F9.0,2F9.3)'
endfor
close,1

; stop

agg:
device,set_Graphics_function = 3 ; ordinary plot

dy = dy0 * flipy
dx = dx0 * flipx

dx = dx * scale
dy = dy * scale

dx1 = dx / 60. & dy1 = dy / 60. ; offsets in degrees

 plot,ra_proj-mra,dec-mdec,psym=3,xsty=3,ysty=3,/nodata,$
  xtit='Projected RA / Degrees',ytit='DEC / Degrees',$
  xr= [-5,5],yr=[-5,5]
 plotsym,0

for i=0,nt-1 do begin
 rmax = sqrt( dx1(i)^2. + dy1(i)^2. ) & rmin = rmax
 tvellipse, rmax, rmin, 0, 0, 0.,thick=2,/data,linestyle=2 
endfor

for i=0,c-1 do begin
 vv1 = vv(i) & bv1 = bv(i)
 cr1 = (534400 + 246230*bv1+ 976300*bv1^2) * 0.5 * (10.^(-0.4*vv1))
 mg1 = 25. -fac * alog10(cr1)
 sz1 = (25.-mg1)/fac2
 plots,ra_proj(i)-mra,dec(i)-mdec,psym=8,symsize=sz1
 
endfor

plotsym,0,thick=3


; Rotate the diagram
ff = 1.

!MOUSE.BUTTON = 1
device,set_Graphics_function = 6 ; temporary plot
dx2 = dx1 & dy2 = dy1
dr = 4./60 ; positional error

nz = n_elements(dx1)
 for i=0,nz-1 do begin
   mg2 = 25. -fac * alog10(ct(i))
   sz2 = (25.-mg2) / fac2
   plots,dx2(i),dy2(i),psym=8,col=col.green,symsize = sz2
; Approx. error box
    oplot,[dx2(i)-dr,dx2(i)+dr],[dy2(i)+dr,dy2(i)+dr],col=col.red
    oplot,[dx2(i)-dr,dx2(i)+dr],[dy2(i)-dr,dy2(i)-dr],col=col.red
    oplot,[dx2(i)-dr,dx2(i)-dr],[dy2(i)-dr,dy2(i)+dr],col=col.red
    oplot,[dx2(i)+dr,dx2(i)+dr],[dy2(i)-dr,dy2(i)+dr],col=col.red
 endfor

; agg:

repeat begin

;print,dx1,dy1
;print,dx2,dy2
;print,''

; Erase old plot
 for i=0,nz-1 do begin
   mg2 = 25. - fac * alog10(ct(i))
   sz2 = (25.-mg2) / fac2
   plots,dx2(i),dy2(i),psym=8,col=col.green,symsize = sz2
; Approx. error box
    oplot,[dx2(i)-dr,dx2(i)+dr],[dy2(i)+dr,dy2(i)+dr],col=col.red
    oplot,[dx2(i)-dr,dx2(i)+dr],[dy2(i)-dr,dy2(i)-dr],col=col.red
    oplot,[dx2(i)-dr,dx2(i)-dr],[dy2(i)-dr,dy2(i)+dr],col=col.red
    oplot,[dx2(i)+dr,dx2(i)+dr],[dy2(i)-dr,dy2(i)+dr],col=col.red
 endfor

 angr = ang * !PI / 180.
 dx2 = dx1 * cos(angr) - dy1 * sin(angr)
 dy2 = dx1 * sin(angr) + dy1 * cos(angr)

; New plot
 for i=0,nz-1 do begin
   mg2 = 25. - fac * alog10(ct(i))
   sz2 = (25.-mg2) / fac2
   plots,dx2(i),dy2(i),psym=8,col=col.green,symsize = sz2
; Approx. error box
    oplot,[dx2(i)-dr,dx2(i)+dr],[dy2(i)+dr,dy2(i)+dr],col=col.red
    oplot,[dx2(i)-dr,dx2(i)+dr],[dy2(i)-dr,dy2(i)-dr],col=col.red
    oplot,[dx2(i)-dr,dx2(i)-dr],[dy2(i)-dr,dy2(i)+dr],col=col.red
    oplot,[dx2(i)+dr,dx2(i)+dr],[dy2(i)-dr,dy2(i)+dr],col=col.red
 endfor

 cursor,colnew,rownew,/down,/normal
 ang = ang + ff * 4.

endrep until !MOUSE.BUTTON ne 1 ; until left mouse button is not pressed

; ff = -1. * ff
scale = scale + 0.1
goto,agg

device,set_Graphics_function = 3 ; ordinary plot

       ;plot,vv,25. -2.5 * alog10(cr),psym=3,ysty=3,xsty=3,xr=[0,12]

end


