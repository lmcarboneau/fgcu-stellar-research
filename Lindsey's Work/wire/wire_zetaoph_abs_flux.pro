; Feb 2007: See if absolute flux of Zeta Oph has
; changed from Feb 2004 to Sep 2006 (2.5 years)

;2004:
; 0: 149757
; 1: 151769
; 2: 148367
; 3: 150052
; 4: 148349
;
;2005:
; 0: 149757
; 1: 150052
; 2: 148367
; 3: 151769
; 4: 148349
;
;2006:
; 0: 149757   09V     V=2.6  B-V = 0.02
; 1: 150052   K5        6.5  B-V = 1.9
; 2: 148367   A3m       4.6  B-V = 0.17  **
; 3: 151769   F7IV      4.6  B-V = 0.44
; 4: 148349   M3III     5.3  B-V = 1.7



c = [0,0,0] & mz=13.95 & rry = .30 & mf = 1.88   & titt = 'Zeta Oph (O9V)'
; c = [2,2,2] & mz=16.2  & rry = .30 & mf = 1.9  & titt = 'HD 148367 (A3m)'  ;  2006 is 0.25 mag fainter. But perhaps a smaller aperture?
; c = [1,3,3] & mz=16.0  & rry = .15 & mf = 1.9  & titt = 'HD 151769 (F7IV)' ; 2004 is 0.2 mag fainter, smaller aperture?

restore,'ZetaOph_Feb2004_31.idl.hjd'
 w04 = transpose([ [wireult.hjd],[wireult.mag(c(0))] ])
restore,'ZetaOph_Sep2005_wire31.idl'
 w05 = transpose([ [wireult.hjd],[wireult.mag(c(1))] ])
restore,'ZetaOph_Aug2006_wire31.idl.t-1.5.cut.hjd'
 w06 = transpose([ [wireult.hjd],[wireult.mag(c(2))] ])

x = strsplit(titt,' ',/extract)

easyps, keywords, kk, dops=dops, /prepare, dim = [22,26.0,-1,1], $
 fil = 'zeta_oph_abs_flux_'+x(1)+'.ps', dir = '/export/brixx1/bruntt/papers/wire/zetaoph/'

 col=getcolor(/load)
 colarr = col.sky

!P.multi=[0,1,2]

smsub = 1. & smfac = 5
plot,w04(0,*)-min(w04(0,*)),w04(1,*)-mz,psym=3,yr=[1,-1]*rry+mz,$
 tit=titt,/nodata,xtit='!6Time [d]',ytit='!4D!6mag' ; ,xr=[2,5]

 oplot,w04(0,*)-min(w04(0,*)),smooth(w04(1,*),smfac,/edge),psym=3,col=col.red
 oplot,w05(0,*)-min(w05(0,*)),smooth(w05(1,*),smfac,/edge),psym=3,col=col.aqua
 oplot,w06(0,*)-min(w06(0,*)),smooth(w06(1,*),smfac,/edge),psym=3,col=col.sky 


; Zeta Oph -- absolute flux: change by 0.04 mag. 
; This could very likely be due to changes is the FWHM?

restore,'ZetaOph_Feb2004_31.idl.hjd'
 w04 = transpose([ [wireult.hjd],[wireult.fwhm(c(0))] ])
restore,'ZetaOph_Sep2005_wire31.idl'
 w05 = transpose([ [wireult.hjd],[wireult.fwhm(c(1))] ])
restore,'ZetaOph_Aug2006_wire31.idl.t-1.5.cut.hjd'
 w06 = transpose([ [wireult.hjd],[wireult.fwhm(c(2))] ])

plot,w04(0,*)-min(w04(0,*)),w04(1,*)-mf,psym=3,yr=[1,-1]*0.6+mf,/nodata,xtit='!6Time [d]',ytit='!6FWHM [pixels]'
 oplot,w04(0,*)-min(w04(0,*)),smsub*smooth(w04(1,*),smfac,/edge),psym=3,col=col.red
 oplot,w05(0,*)-min(w05(0,*)),smsub*smooth(w05(1,*),smfac,/edge),psym=3,col=col.aqua
 oplot,w06(0,*)-min(w06(0,*)),smsub*smooth(w06(1,*),smfac,/edge),psym=3,col=col.sky 

 easyps, keywords, kk, dops=dops, /close

END
