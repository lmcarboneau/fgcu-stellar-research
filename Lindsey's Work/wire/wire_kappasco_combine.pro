
if n_elements(freqA) eq 0 then begin

restore,'~/wire/bstars/kappasco/sim9900/kappasco_oct99_mar00_001.amp.idl'
freq9900 = freq & amp9900 = amp & freq9900_2 = freq2 & amp9900_2 = amp2

restore,'~/wire/bstars/kappasco/sim99/wire_lc_KappaSco_1999_s0_HD160578_B1_001.amp.idl'
freq99 = freq & amp99 = amp & freq99_2 = freq2 & amp99_2 = amp2

restore,'~/wire/bstars/kappasco/sim/kappasco_mar04_okt04_001.amp.idl'
freq0404 = freq & amp0404 = amp & freq0404_2 = freq2 & amp0404_2 = amp2

restore,$
 '~/wire/bstars/kappasco/sim2/wire_lc_LambdaSco_Mar2004_s3_HD160578_B1_001.amp.idl'
freq04 = freq & amp04 = amp & freq04_2 = freq2 & amp04_2 = amp2

restore,$
 '~/wire/bstars/kappasco/sim99000404/kappasco_oct99_mar00_mar04_okt04_001.amp.idl'
freqA = freq & ampA = amp & freqA_2 = freq2 & ampA_2 = amp2

endif



aerts = [4.8678381D , 5.0042461D ]

dops = 1

colx = col.green
colm = col.magenta

easyps, keywords, kk, dops=dops, /prepare,  dim = [28,15,5,29], $
 /landscape, $
 fil = 'kappasco_amp.ps', dir = '/home/bruntt/wire/wire_eps/'
col=getcolor(/load)

plot,freq04,amp04,xr=5. + [-1,1] * .15,thick=2,yr=[0,5500],$
 xtit='Frequency [c/day]',ytit='WIRE amplitude [ppm]',tit='Kappa Sco w/ WIRE (B1.5-II)'
; oplot,freqA,ampA, col=colx,thick=1
oplot,freq99,amp99, col=col.yellow,thick=2
oplot,freq0404,amp0404, col=col.sky,thick=2
oplot,freq9900,amp9900, col=col.red,thick=2

p=0 & oplot,aerts(p)*[1.,1.],[0,1200],col=colm,thick=4
p=1 & oplot,aerts(p)*[1.,1.],[0,4200],col=colm,thick=4

xt = [' ','',' ','', ' ','']


plot,freq04,amp04,xr=5.004 + [-1,1] * .01,thick=2,yr=[0,5500],$
 position=[.75,.6,.95,.9],/noerase,/nodata, $
 xtickname = xt
oplot,freqA,ampA, col=colx,thick=1
; oplot,freq99,amp99, col=col.yellow,thick=1
oplot,freq0404,amp0404, col=col.sky,thick=1
oplot,freq9900,amp9900, col=col.red,thick=1

p=1 & oplot,aerts(p)*[1.,1.],[0,4200],col=colm,thick=4
p=1 & oplot,aerts(p)*[1.,1.]+0.0000001*[3,3],[0,4200],col=colm,thick=4,line=2
p=1 & oplot,aerts(p)*[1.,1.]-0.0000001*[3,3],[0,4200],col=colm,thick=4,line=2


xyouts,4.998, 4900, 'F1', charsi=1.5,charthick=2
xyouts, 5.01, 4950, 'F2'  , charsi=1.5,charthick=2       


; plot,freq04,amp04,xr=[4.86,4.91],thick=2,yr=[0,2000],$

plot,freq04,amp04,xr=4.892+[-1,1.]*0.01,thick=2,yr=[0,2000],$
 position=[.75,.6,.95,.9]-[1,0,1,0.]*.55,/noerase,/nodata, $
 xtickname = xt
oplot,freqA,ampA, col=colx,thick=1
; oplot,freq99,amp99, col=col.yellow,thick=1
oplot,freq0404,amp0404, col=col.sky,thick=2
oplot,freq9900,amp9900, col=col.red,thick=2

p=0 & oplot,aerts(p)*[1.,1.],[0,1200],col=colm,thick=4
p=1 & oplot,aerts(p)*[1.,1.],[0,4200],col=colm,thick=4


easyps, keywords, kk, dops=dops, /close



END
