PRO wire_weights, wire3, star, wei2

; Compute weights for the wire data:

nstar = n_elements(wire3(0).mag) ; number of stars
np = n_elements(wire3.mag(0)) ; number of data points

t0 = 51480D
times = wire3.hjd - t0
a = sort(times)
times2 = times(a)

tstep = median(times2(1:np-1) - times2(0:np-2) ) ; typical time step

limt = tstep * 2.5

dk = fltarr(nstar,np)
dk(*,*) = -99.9
ff = findgen(np)

; for l=0,nstar-1 do begin ; comp. star
l = star

mag = wire3.mag(l)
mag = mag - median(mag)
ptp_robust_fin,mag,noise,1 ; PTP noise

 for k=0L,np-1 do begin   ; data points

  w = where( (abs(times(k)-times) lt limt) and (ff ne k),c)
  if c ge 1 then $
   dk(l,k) =  ( total( abs(mag(w)-mag(k)) ) / float(c) ) / noise

 endfor
; endfor

wire_stetson,dk(0,*),wei2
w = where(dk(0,*) lt 1.0,c)
wei2(w) = max(wei2)
wei2 = wei2/total(wei2)


goto,skiprest

a = max(wei2)
ss = -2.5 * alog10(wei2 + a) ; + 25.0
symmag,1.0/ss,0.1,0.7,pz,con
; magnitude,minsym,maxsym,pz,con ; get symbol sizes

mag = wire3.mag(0) & mag = mag - median(mag)
plotsym,0,/fill
plot,times,mag,xr=[0,.3]-6.5,/nodata,yr=[-1,1]*0.02
for i=0L,np-1 do plots,times(i),mag(i),symsi=pz(i),psym=8

plot,times,dk(0,*),psym=3,yr=[0,5],ytit='Offset in sigmas',xtit='HJD'
oplot,times,dk(1,*),psym=3,col=col.red

plot,dk(0,*),psym=3,yr=[0,5],ytit='Offset in sigmas',xtit='HJD'
oplot,dk(1,*),psym=3,col=col.red

plot,dk(0,*),dk(3,*),psym=3,xr=[0,5],yr=[0,5]

dks = fltarr(2,np)

dks(*,*) = -9.9

wuse = [0,4]

for k=0L,np-1 do begin
 wx = where(dk(wuse,k) gt 0.,cx)

 if cx ge 2 then begin
  resistant_mean,dk(*,k),3,me,sd,nr
  dks(0,k) = me
  dks(1,k) = sd
 endif

 if cx eq 1 then begin
  dks(0,k) = dk(wx,k)
  dks(1,k) = 99.9
 endif

endfor


plot,dks(0,*),dks(1,*),psym=3,xr=[0,5],yr=[0,2]

plot,dks(0,*),dk(0,*),psym=3,xr=[0,5],yr=[0,5]
plot,dks(0,*),dk(4,*),psym=3,xr=[0,5],yr=[0,5]

plot,dk(0,*),dk(4,*),psym=3,xr=[0,5],yr=[0,5]

wire_stetson,dks(0,*),wei
; plot,wei,wei2,psym=3,ysty=3,xsty=3

plot,dks(0,*),wei
oplot,dk(0,*),wei2,psym=3,col=col.red


plot,dk(0,*),wei2,psym=3,xr=[0,5]

plot,mag,wei2,psym=3,xr=[-1,1]*0.02

plot,dk(0,*),wei2,psym=3,xr=[0,5]
; plot,dk(4,*),wei2,psym=3,xr=[0,3]

skiprest:

END
