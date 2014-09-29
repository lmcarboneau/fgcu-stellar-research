; Examine the effect of having two epochs of WIRE data

; Beta Cep star: HD 158427 : alpha Ara

; wireg,'AlphaAra_Feb2004_31.idl.hjd'
; wireg,'AlphaAra_Sep2004_31.idl.hjd'


;  wire_lc_AlphaAra_Feb2004_s0_HD158427_B2Vne.dat
;  wire_lc_AlphaAra_Sep2004_s0_HD158427_B2Vne.dat
;  
;  cp wire_lc_AlphaAra_Feb2004_s0_HD158427_B2Vne.dat alphaara.dat
;  cat wire_lc_AlphaAra_Sep2004_s0_HD158427_B2Vne.dat >> alphaara.dat

wire_mix_lc,$
  '/home/bruntt/wire/wire_lc/wire_lc_AlphaAra_Feb2004_s0_HD158427_B2Vne.dat',$
  '/home/bruntt/wire/wire_lc/wire_lc_AlphaAra_Sep2004_s0_HD158427_B2Vne.dat',$
  '/home/bruntt/wire/wire_lc/alphaara.dat',$
53068,53254


  
  readcol,'/home/bruntt/wire/wire_lc/wire_lc_AlphaAra_Feb2004_s0_HD158427_B2Vne.dat',$
   t,d,w,format='d,d,d'
  
  tt2 = t & dat2 = d & wei2 = w  & fac = 1e6/86400D
  minfreq = .1/fac & maxfreq = 500./fac & highres = 3.5
  minfreq = .1 & maxfreq = 50. & highres = 3.5
  ampl_spec_calc_wire_rv,tt2,dat2,wei2,$
   minfreq,maxfreq,freq,amp,phase,highres=highres
  
  freq_feb = freq & amp_feb = amp
  
  
  readcol,'/home/bruntt/wire/wire_lc/alphaara.dat',$
   t,d,w,format='d,d,d'
  
  tt2 = t & dat2 = d & wei2 = w  & fac = 1e6/86400D
  minfreq = .1/fac & maxfreq = 500./fac & highres = 3.5
  minfreq = .1 & maxfreq = 15. & highres = 3.5
  ampl_spec_calc_wire_rv,tt2,dat2,wei2,$
   minfreq,maxfreq,freq,amp,phase,highres=highres
  
  freq_tot = freq & amp_tot = amp
  
  save,filename='/home/bruntt/wire/bstars/spectra/alphaara.idl',$
   freq_tot,amp_tot, freq_feb, amp_feb

restore,'/home/bruntt/wire/bstars/spectra/alphaara.idl'

readcol,'/home/bruntt/wire/bstars/spectra/alphaara_feb.per',a,f1,a1,p1,format='a,f,f,f'
readcol,'/home/bruntt/wire/bstars/spectra/alphaara_tot.per',a,f2,a2,p2,format='a,f,f,f'

n1 = n_elements(f1)
n2 = n_elements(f2)

slack = 0.3
slack = 0.0

x1 = 1.9 & x1z = 2.0
x2 = 2.2 & x2z = 2.07

x1 = 3.9 & x1z = 4.05
x2 = 4.3 & x2z = 4.15


plot,freq_feb,amp_feb,xr=[x1,x2]+slack*[-1,1],yr=[0,12000]
oplot,freq_tot,amp_tot,col=col.green
for i=0,n1-1 do oplot,[1.,1]*f1(i), [0.,a1(i)]*1e6, thick=1,col=col.red
for i=0,n2-1 do oplot,[1.,1]*f2(i), [0.,a2(i)]*1e6, thick=1,col=col.cyan

plot,freq_feb,amp_feb,xr=[x1z,x2z],yr=[4000,12000],/noerase,position=[.7,.7,.9,.9]
oplot,freq_tot,amp_tot,col=col.green
for i=0,n1-1 do oplot,[1.,1]*f1(i), [0.,a1(i)]*1e6, thick=1,col=col.red
for i=0,n2-1 do oplot,[1.,1]*f2(i), [0.,a2(i)]*1e6, thick=1,col=col.cyan


plot,freq_feb,amp_feb,xr=[3.5 , 4.5],yr=[0,12000]
oplot,freq_tot,amp_tot,col=col.green

Finder en masse freq. omkring 4 c/d freq. Er de rigtige?
Prov en simulering... hvor der KUN er to modes?

Jeg tror det er rigtigt. Men hvis cleanede spekre ...
JEg tror det er VIGTIGT at tage to epoker!


END
