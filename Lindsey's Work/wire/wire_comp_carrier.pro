; Run this after wire_plot_ampl.pro

outps = 'procyon_carrier_power0-n.ps'
setpl, 18,7, outps, 2, 2

marklin = 0
marknoise = 1

colx = [col.black,col.green, col.yellow, col.red, col.sky]
colx(*) = [col.black]
!P.charsize = 1.0


readcol,'/ai40/bruntt/wire/procyon/carrier_procyon_freqs.dat', $
 f,l,sn,format='F, I, F'
nf = n_elements(f)

; plot,[0,1],/nodata,xr=[500,2000],yr=[0,1],xsty=1,ysty=1

 wg = where(sn gt 3.5,c)
;for i=0,nf-1 do oplot,f(i)*[1,1.],[.5,1],col=colx(4)

wp = where(fc.f gt 0. and sigma_at_freq gt 3.0,cp)
;for i=0,cp-1 do oplot,fc(wp(i)).f*[1.,1],[0,.5],col=colx(3)


match = 1.5
cnt = 0L & gem = fltarr(100) & val = gem
siglimit2 = 2.5 ; limit for HB's detected peaks

for i=0,cp-1 do begin

  if sigma_at_freq(i) lt siglimit2 then goto,tooweak

  dev = abs(fc(wp(i)).f - f)
  ww = where(dev lt match,cc)

  if cc ge 1 then begin
   wm = where(dev(ww) eq min(dev(ww)),cm)

   gem(cnt) = fc(wp(i)).f ; f(ww(wm(0)))

   wx = where(abs(fc(i).freq*fac*fac2 - gem(cnt)) lt 2.0,cx)
   val(cnt) = max((fc(0).amp(wx)/1e6)^2.0) * 1.02
   

   cnt = cnt + 1
  endif

  tooweak:

endfor

gem = gem(0:cnt-1)
val = val(0:cnt-1)

; for i=0,cnt-1 do oplot,gem(i)*[1,1],[val(i),1],line=2,col=colx(1)


; wait,2

f1 = 575.
f2 = 1625.

nn = 0 ; 26 ; 26

w = where(fc(nn).freq gt .1,c)
plot,fc(nn).freq(w)*fac*fac2,(fc(nn).amp(w)/1e6)^2.0,$
 ysty=1,xr=[f1,f2],xsty=1,yr=[0,.6]
if marknoise ge 1 then $
 oplot,noise2(0,*),noise2(1,*)*siglimit,col=colx(3),psym=-6,symsi=.4

if marklin ge 1 then $
for i=0,cnt-1 do oplot, gem(i)*[1.,1], [val(i), 1.], $
 line=2,col=colx(1),thick=1

whb = where(fc.f gt 0. and sigma_at_freq gt siglimit2,nhb)
if marklin ge 2 then $
for i=0,nhb-1 do $
 if fc(whb(i)).f gt f1 and fc(whb(i)).f lt f2 then $
 oplot, fc(whb(i)).f*[1,1], [.3,.5] , $
 line=1,col=colx(1),thick=3

wp  = where(sn gt 3.5,c) ; significant peaks acc. to Carrier paper (2004)

if marklin ge 3 then $
for i=0,c -1 do oplot,f(wp(i))*[1,1], [.55,.7],line=2,col=colx(4),thick=3


device,/close & set_plot,'x' & print,' $ gv ' + outps + ' & ' 



end


