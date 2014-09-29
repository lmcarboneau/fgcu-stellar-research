; Examine the peak distribution of an amplitude spectrum

addyear = '1999'
addyear = '2000'

if n_elements(p99novM5) eq 0 then begin
 restore,'~/wire/wire_amp/wire_lc_Procyon_1999_mer5.amp.idl'
 restore,'~/wire/wire_amp/wire_lc_Procyon_2000_mer5.amp.idl'
endif

if addyear eq '1999' then begin
 f = p99novM5.f
 a = p99novM5.a



plot_oo,p99novM5.f2,p99novM5.d2,yr=[.7,50]
oplot,p99novM5.f2,p99novM5.d2fit,col=col.sky

plot_oo,p00novM5.f2,p00novM5.d2,yr=[.7,50],xr=[100,10000],xsty=1,ysty=1
oplot,p00novM5.f2,p00novM5.d2fit,col=col.sky


forb = 173.68056D
wire_exclude_orbital,f, a, fny, dny,$
 forb=forb,df=11.,inc=.001,fskip=173.68+[-1,1]*50.,skip2=39.+[-1,1]*12.

 wg = where(fny gt 1e-3,cg)
 f1 = fny(wg) & d1 = dny(wg)

 x2 = 10.0
endif

if addyear eq '2000' then begin
 f = p00novM5.f
 a = p00novM5.a

forb = 174.23611D
wire_exclude_orbital,f, a, fny, dny,$
 forb=forb,df=11.,inc=.001

 wg = where(fny gt 1e-3,cg)
 f1 = fny(wg) & d1 = dny(wg)

 x2 = 10
endif


col=getcolor(/load)

plot,f,a,xr=[0,1000]
oplot,fny,dny,col=col.sky

; Get the median amplitude in some reference frequency interval


amax = 16.
amin = 1e-5

nm = 120.
lx2 = alog10(amax) & lx1 = alog10(amin)
ai1 = lx1 + ((lx2 - lx1) * findgen(nm)) / (nm-1.)
ai = 10.^(ai1)

ai = amin + (amax - amin) * findgen(nm) / nm


nf = 10 & step = 200
f1 = 350. + findgen(nf) * step
f2 = f1 + step
colx = (findgen(nf)/(nf-1)) * 100. + 50.

plot_io,[0,1],xr=[0,x2],yr=[.001,1],/nodata,xsty=1

for int = 0,nf-1 do begin

wuse = where(fny gt 1e-3 and fny gt f1(int) and fny lt f2(int),cx)

g = where(fny gt f1(int) and fny lt f2(int),cg)
 resistant_mean, dny(g), ma, sd, nr
; ma = median(dny(g))

amp = dny / ma ; normalize amplitudes by mean in the freq range!

res = fltarr(2,nm)

; plot,fny(wuse),amp(wuse)
; plots,!x.crange,1.0,thick=2,line=5,col=250

for i=0,nm-1 do begin
 wg = where(amp(wuse) gt ai(i),cg)
 res(0,i) = ai(i)
 res(1,i) = cg / float(cx)
endfor

oplot,res(0,*),res(1,*),psym=-6,symsi=.4, col=colx(int)

txtout = $
 strcompress( string( (f1(int) + f2(int))*0.5, format='(I10)'),/remove_all)

p = where(res(1,*) gt .0001,ccp)

xyouts,res(0,p(ccp-1))-0.05,res(1,p(ccp-1))*.9,txtout, $
 orientation=270.,alignment=0.0

endfor

END
