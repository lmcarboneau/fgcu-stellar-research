stop

if n_elements(wire) eq 0 then $
 restore,'/data1/bruntt/wire/AlphaUMi/wire_AlphaUMi_048.idl'

;  restore,'/data1/bruntt/wire/AlphaUMi/wire_AlphaUMi_056.idl1x'

magn_limit = 14.

ap = 4
col = getcolor(/load)
colx = [col.white, col.green, col.yellow, col.red, col.sky]

nstar = n_elements(wire(0).d(*,0,0))
d1 = 0L ; 10000L
d2 = n_elements(wire.hjd(0))-1 ; d1 + 50000L - 1
st = 25 ; take ever 25'th data point (fast!)

 plot,[0,1],/nodata,xr=[-.3,.1],yr=[10,18],ysty=1,xsty=1

 ndat = (d2-d1 + 1) / st

 datuse = d1 + findgen(ndat) * st

 flux = fltarr(nstar,ndat)
 back = flux
 me = fltarr(2,nstar)
 times = dblarr(nstar,ndat)
 mag = flux

 a = sort(wire.hjd(0))
 wire = wire(a) ; sort HJD by primary stars

 w0 = where(wire.d(0,3,3) gt 50,c0)
 t0 = double(long(median(wire(w0).hjd(0))*10.))/ 10.

for star=0,nstar-1 do begin
 
 ccd = reform(wire(datuse).d(star,*,*))
 print,' Star = '+string(star,format='(I3)')+': ', format='(A12,$)'

 for i=0L,ndat-1 do begin
  i2 = i ; * st
  if i2 mod (5000.) eq 0 then print,i2,format='(I7,$)'
  dat = reform(ccd(*,*,i2))
  back_temp = [ dat(0,0), dat(0,7), dat(7,0), dat(7,7) ] 
  resistant_mean,back_temp,3,backgr,sd,nr
  flux(star,i) = total(dat(2:5,2:5)) -  backgr * 16. ; 
  back(star,i) = backgr
 endfor

 print,''

 ; Good data ...
 w = where(flux(star,*) gt 50. and back(star,*) lt 1000.,c)

 mag(star,w) = -2.5 * alog10( flux(star,w) ) + 25.
 me(0,star) = median(mag(star,w))
 me(1,star) = robust_sigma(mag(star,w))

 tt    = wire(datuse).hjd(star) - t0
 times(star,w) = tt(w) 

 oplot,times(star,w),mag(star,w), psym=3, col = colx(star)

 ; print,' Hit any key ' & s = get_kbrd(1)
 
endfor







stop




nmax = 300
nmax_obs = 0
tjump = fltarr(nstar,nmax)
tjump(*,*) = -1e4

; Detect jumps in magnitude ---> WIRE offset


for star=0,nstar-1 do begin

m = reform(mag(star,*))
w = where( m gt 5. and m lt 25.,c)
t = reform(times(star,w))
m = m(w)

s = sort(t) ; sort by times
m = m(s)
t = t(s)

m = smooth(m,11,/edge_truncate)

; POint to points difference in flux:
dm = m(1:c-1) - m(0:c-2)

m2 = m
ptp_robust_fin, m2, ptp, 0
jump = ptp * 8.0

wj = where(abs(dm) gt jump,cj)

;plot,t(0:c-2),dm,psym=3,yr=[-1,1]*jump*3., $
; xtit='Time [hjd]',ytit='Delta Mag'
;plots,!x.crange, jump,col=col.green,line=2
;plots,!x.crange,-jump,col=col.green,line=2

; Make the detected significant jumps in magn.
; for j=0,cj-1 do plots,t(wj(j)),!y.crange,col=col.red

 if median(m) lt magn_limit and ptp lt 0.05 then begin ; no bad stars pls.
 if cj ge 1 then begin
  jj = long(t(wj) * 200.) / 200. ; + t0
  uj = uniq(jj)
  nj = n_elements(uj)
  tjump(star,0:nj-1) = jj(uj) ; only store times w/ unique jumps in magn.

  if nj gt nmax_obs then nmax_obs = nj
 endif
 endif

endfor
tjump = tjump(*,0:nmax_obs-1) ; remove unused entries

tjump2 = fltarr(nstar * nmax_obs)

cnt = 0L
for s=0,nstar-1 do begin
 w = where(tjump(s,*) gt -500.,c)
 if c ge 1 then begin
   tjump2(cnt:cnt+c-1) = tjump(s,w)
   cnt = cnt + c
 endif
endfor
tjump2 = tjump2(0:cnt-1)
a = sort(tjump2) & tjump2 = tjump2(a)
uu = uniq(tjump2) & tjump2 = tjump2(uu)
nj = n_elements(tjump2)

for j=0,nj-1 do $
 plots,tjump2(j),!y.crange,col=col.red,line=2



magn = fltarr(nstar,nj)

for star=0,nstar-1 do begin
 for j=0,nj-1 do begin

  tt = wire.hjd(star) - t0

  if j ge 1 then $
   w = where(tt gt tjump2(j-1) and tt le tjump2(j) and $
             wire.d(star,3,3) gt 50. and $
             wire.d(star,0,0) lt 1000. and $
             wire.d(star,7,0) lt 1000. and $
             wire.d(star,0,7) lt 1000. and $
             wire.d(star,7,7) lt 1000.,c) else $
   w = where(tt gt -1e9 and tt le tjump2(j) and $
             wire.d(star,3,3) gt 50. and $
             wire.d(star,0,0) lt 1000. and $
             wire.d(star,7,0) lt 1000. and $
             wire.d(star,0,7) lt 1000. and $
             wire.d(star,7,7) lt 1000.,c)

      if c ge 10 then begin

         bac = wire(w).d(star,0,0) + $
               wire(w).d(star,7,7) + $
               wire(w).d(star,7,0) + $
               wire(w).d(star,0,7) 
         bac = bac / 4.0

         dat   = wire(w).d(star,2,2) + $
                 wire(w).d(star,2,3) + $
                 wire(w).d(star,2,4) + $
                 wire(w).d(star,2,5) + $
                 wire(w).d(star,3,2) + $
                 wire(w).d(star,3,3) + $
                 wire(w).d(star,3,4) + $
                 wire(w).d(star,3,5) + $
                 wire(w).d(star,4,2) + $
                 wire(w).d(star,4,3) + $
                 wire(w).d(star,4,4) + $
                 wire(w).d(star,4,5) + $
                 wire(w).d(star,5,2) + $
                 wire(w).d(star,5,3) + $
                 wire(w).d(star,5,4) + $
                 wire(w).d(star,5,5)

         dat2 = -2.5 * alog10(dat - bac * 16.) + 25.

        ; Round to nearest 0.1 magn
         magn(star,j) = long( median( dat2 ) * 15.) / 15.

      endif

 endfor
endfor


; Find unique magnitudes
magn2 = fltarr(300)
cnt = 0L
for j=0, nj-1, 2 do begin
 w = where(magn(*,j) gt 5.,c)
 if c ge 1 then begin
   magn2(cnt:cnt+c-1) = magn(w,j)
   cnt = cnt + c
  endif    
endfor

magn2 = magn2(0:cnt-1)
ss = sort(magn2)
magn2 = magn2(ss)
uu = uniq(magn2) & magn2 = magn2(uu)
nm = n_elements(magn2)

for i=0,nm-1 do plots,!x.crange,magn2(i),col=col.green








stop

np = n_elements(wire.hjd(0))
wire1 = replicate( {x:fltarr(5), y: fltarr(5), hjd:dblarr(5), $
                    stamp:strarr(5), d: fltarr(5, 8, 8)}, np)

cnt = 0L
for star=0,nstar-1 do begin
 for j=0,nj-1,2 do begin
  tt = wire.hjd(star) - t0

  if j ge 1 then $
   w = where(tt gt tjump2(j-1) and tt le tjump2(j) and $
             wire.d(star,3,3) gt 50. and $
             wire.d(star,0,0) lt 30000. and $
             wire.d(star,7,7) lt 30000.,c) else $
   w = where(tt gt -1e9 and tt le tjump2(j) and $
             wire.d(star,3,3) gt 50. and $
             wire.d(star,0,0) lt 30000. and $
             wire.d(star,7,7) lt 30000.,c)

   if c ge 10 then begin
      wire1(cnt:cnt+c-1).x(star)     = wire(w).x(star)    
      wire1(cnt:cnt+c-1).y(star)     = wire(w).y(star)    
      wire1(cnt:cnt+c-1).hjd(star)   = wire(w).hjd(star)    
      wire1(cnt:cnt+c-1).stamp(star) = wire(w).stamp(star)    
      wire1(cnt:cnt+c-1).d(star,*,*) = wire(w).d(star,*,*)    
      cnt = cnt + c
   endif  

 endfor
endfor 

wire1 = wire1(0:cnt-1) ; remove unused entries
plot,wire1.hjd(0)-t0,wire1.d(0,3,3),psym=3,xr=[-.3,.3],min_value = 1e3,ysty=3
oplot,wire.hjd(0)-t0,wire.d(0,3,3),psym=3,col=col.red

END
