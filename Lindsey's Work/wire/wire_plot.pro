
dops = 0

col = getcolor(/load)
colx = col.red

if dops eq 1 then begin
 outfile = '/ai39/bruntt/wire/altair/altair_plot.ps'
 a4,y=24,x=18,name=outfile
 colx = 90
endif

!P.charsize = 2.0

restore,'/ai39/bruntt/wire/altair/altair_merged_allslots.idl'
; restore,"/ai39/bruntt/wire/altair/altair_merged2.idl"
  w = where(wire3.hjd gt 51475.,c1)
 w2 = where(wire3.hjd lt 51475.,c2)

for star = 0,4 do begin
!P.multi = [0,2,3]

titl = '!14Altair!3' + ' -- Slot ' + string(star,format='(I1)')
if star eq 0 then titl = '!14Altair!3'

 plot,wire3(w).hjd-51460.,wire3(w).gc(0),psym=3,ysty=3,$
  xr=[8,32],xsty=3,yr=[2.9,4.1],xtit='HJD',ytit='!8x!3 position',tit=titl
 oplot,wire3(w2).hjd-51460.,wire3(w2).gc(0),psym=3,col=colx

 plot,wire3(w).hjd-51460.,wire3(w).gc(1),psym=3,ysty=3,$
  xr=[8,32],xsty=3,yr=[2.9,4.1],xtit='HJD',ytit='!8y!3 position',tit=titl
 oplot,wire3(w2).hjd-51460.,wire3(w2).gc(1),psym=3,col=colx

 sig = robust_sigma(wire3(w).mag(star))

 plot,wire3(w).hjd-51460.,wire3(w).mag(star),psym=3,ysty=3,$
  xr=[8,32],xsty=3,yr=[-1,1]*sig*6.,xtit='HJD',ytit='!4D!3m',tit=titl
 oplot,wire3(w2).hjd-51460.,wire3(w2).mag(star),col=colx,psym=3

 plot,wire3(w).mag(star),wire3(w).gc(1),psym=3,ysty=3,$
  xr=[-1,1]*sig*6.,xsty=3,yr=[2.9,4.1],xtit='!4D!3m',ytit='!8y!3 position',tit=titl
 oplot,wire3(w2).mag(star),wire3(w2).gc(1),psym=3,col=colx
 
 plot,wire3(w).mag(star),wire3(w).gc(0),psym=3,ysty=3,$
  xr=[-1,1]*sig*6.,xsty=3,yr=[2.9,4.1],xtit='!4D!3m',ytit='!8x!3 position',tit=titl
 oplot,wire3(w2).mag(star),wire3(w2).gc(0),psym=3,col=colx
 
 w = where(abs(wire3.mag(star)) lt 5.,cc)

 plot,[0,1],/nodata
 xyouts,.5,.5,alignment=0.5,$
  string(mmag(0,star),format='(F5.2)') + ' ('+ $
  string(sig,format='(F6.4)') + ') ' + $
  'n='+string(cc,format='(I6)')

 xyouts,.5,.3,alignment=0.5,$
  string(1e6*sqrt(!PI/cc)*sig,format='(F5.1)') + ' ppm'

  


if dops eq 0 and star ne 4 then begin
 print,'Hit any key ...'
 s = get_kbrd(1)
endif

!P.multi = 0

endfor

if dops eq 1 then begin
device,/close
set_plot,'x'

print,''
print,' $gv '+outfile + ' & '
print,' $lpr '+outfile + ' & '
print,''
endif

END
