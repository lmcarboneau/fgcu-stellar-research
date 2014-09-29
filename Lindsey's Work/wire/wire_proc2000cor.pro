; Correct offset problem in WIRE 2000 light curve


; ===========================================================================
; Wire 2000 light curve: nmerge = 31
; ===========================================================================
readcol,'/home/bruntt/wire/wire_lc/wire_lc_Procyon_2000_s0_mer5.dat',$
  t,d,w,f,format='D,D,D,F'

file = "/data2/bruntt/wire/dat/ProcyonF5IV-V/data/2000/Procyon_2000_wire5.idl"
restore,file
; ===========================================================================

; ===========================================================================
; Wire 2000 light curve: nmerge = 31
; ===========================================================================
;readcol,'~/wire/wire_lc/wire_lc_Procyon_2000_star_0.dat',$
;  t,d,w,f,format='D,D,D,F'

;file = '/data2/bruntt/wire/dat/ProcyonF5IV-V/data/2000/Procyon_2000_31.idl'
;restore,file
; ===========================================================================

; plot,t,d,psym=3,yr=[-1,1]*.1,xsty=1

t0 = 51816D ; Procyon 2000 - Reduced Nov 2004
m0 = 11.3792

; plot,t,f,psym=3,yr=[1.7,2.0]

plot,f,d,psym=3,xr=[1.7,1.9],yr=[-1,1]*0.02

col=getcolor(/load)
wg = where(abs(f-1.8232) lt .01 and abs(d-0.0088) lt 0.001,c)
oplot,f(wg),d(wg),col=col.red,psym=3
wgg = where(abs(f-1.8232) lt .005 and abs(d-0.0004209) lt 0.0003,cgg)
oplot,f(wgg),d(wgg),col=col.orchid,psym=3
wx = where(abs(f-1.8355) lt .005 and abs(d-0.00346) lt 0.0007,cx)
oplot,f(wx),d(wx),col=col.green,psym=3

hitme, s9

offset = median(d(wgg)) - median(d(wg))

wg2 = where(abs(wireult.fwhm(0)-1.8232) lt 0.01 and $
            abs(wireult.mag(0)-m0-0.0088) lt 0.001,c2)

plot,wireult.fwhm(0),wireult.mag(0)-m0,psym=3,xr=[1.7,2.0],yr=[-1,1]*.02
oplot,wireult(wg2).fwhm(0),wireult(wg2).mag(0)-m0,psym=3,col=col.red
wireult(wg2).mag(0) = wireult(wg2).mag(0) + offset

wx2 = where(abs(wireult.fwhm(0)-1.8355) lt 0.005 and $
            abs(wireult.mag(0)-m0-0.00346) lt 0.0007,cx2)

wx3 = where(abs(wireult.fwhm(0)-1.8232) lt 0.008 and $
            (wireult.mag(0)-m0) gt .0015,cx3)


oplot,wireult(wg2).fwhm(0),wireult(wg2).mag(0)-m0,$
 psym=3,col=col.cyan
 oplot,wireult(wx2).fwhm(0),wireult(wx2).mag(0)-m0,psym=3,col=col.green
 oplot,wireult(wx3).fwhm(0),wireult(wx3).mag(0)-m0,psym=3,col=col.magenta

hitme, s9

; Debug bad orbits:
; plot,wireult.hjd-t0,wireult.mag(0)-m0,psym=3,yr=[-1,1]*.01,xr=[0,2]
; plot,wireult.hjd-t0,wireult.mag(0)-m0,psym=3,yr=[-1,1]*.01,xr=[-.2,.2]
; plot,wireult.hjd-t0,wireult.mag(0)-m0,psym=3,yr=[-1,1]*.01,xr=[-2.2,-1.8]
; oplot,wireult(wb).hjd-t0,wireult(wb).mag(0)-m0,psym=1,col=col.sky

; Bad orbits:
wb = where(abs(wireult.hjd-t0-1.095) lt 0.02,cb)
oplot,wireult(wb).fwhm(0),wireult(wb).mag(0)-m0,psym=1,col=col.sky,symsi=.3
wireult(wb).mag(0) = wireult(wb).mag(0) -6.99 ; bad data

wb = where(abs(wireult.hjd-t0+2.025) lt 0.023,cb)
oplot,wireult(wb).fwhm(0),wireult(wb).mag(0)-m0,psym=1,col=col.sky,symsi=.3
wireult(wb).mag(0) = wireult(wb).mag(0) -6.99 ; bad data

wb = where(abs(wireult.hjd-t0) lt 0.06,cb)
oplot,wireult(wb).fwhm(0),wireult(wb).mag(0)-m0,psym=1,col=col.sky,symsi=.3
wireult(wb).mag(0) = wireult(wb).mag(0) -6.99 ; bad data

wireult(wx2).mag(0) = wireult(wx2).mag(0) -9.99 ; bad data
wireult(wx3).mag(0) = wireult(wx3).mag(0) -7.99 ; bad data

fileout = file + '.fwhmcor'
save,filename=fileout,wireult,t0,mmag, err ; as in wire_merge_fin.pro

print,' %%% Saved file: ' + fileout

END


