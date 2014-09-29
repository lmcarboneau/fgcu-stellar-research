; Compare simulations of Procyon with WIRE data

dops = 0

;  epoch = '99'
 epoch = '00'
; epoch = 'MOST'

; Which generation of simulations:
; batch = ''  ; 17th of June 2004
batch = '2' ; 22nd of June

 datamode = '.dat'   ; ganulation + p modes --> batch = ''
; datamode = '.pmode' ; pmodes only --> batch = '2'
; datamode = '.gran'  ; granulation only --> batch = '2'

; if batch eq '' then datamode = '.dat'
; if batch eq '2' then datamode = '.gran'

print,''
print,' %%% EPOCH = ' + epoch
print,' %%% DATAMODE = ' + datamode
print,' %%% BATCH OF SIMULATIONS: ' + batch
print,''

fmax = 3700. ; freq. max in c/day max is about 5700
; Nykvist frekens: 1/delta_t(point to point) = 1e6/(11.57*15) c/d

nmax = 300000L ; 140000L ; 500000L
s = replicate( {fo:-1., ao:-1., $
                fs:-1., as:-1., $
                fn:-1., an:-1., $
                fsn:-1, asn:-1., $
                tob:-99., dob:-2., wob:0., $
                ts:-99., ds:-2., ws:0., $
                tn:-99., dn:-2., wn:0., $
                tsn:-99., dsn:-2., wsn:0.}, nmax)
 
col=getcolor(/load)

w9 = '/mnt/WinC/linux/wire/'
spawnrob,'hostname',host
if strmatch(host,'*amalthea*') then w9 = '/ai40/bruntt/wire/'
if strmatch(host,'*phys*') then w9 = '/ai40/bruntt/wire/'

minfreq = 0.005 & maxfreq = fmax
epoch2 = epoch
if epoch eq 'MOST' then epoch2 = '99'
readcol,w9+'/wire_lc/wire_procyon_sept'+epoch2+'_22jun04.dat',tt2,dat2,wei2 
; readcol,w9+'/wire_lc/wire_procyon_sept'+epoch2+'.dat',tt2,dat2,wei2 

nobs = n_elements(tt2)
s(0:nobs-1).tob = tt2 & s(0:nobs-1).dob = dat2 & s(0:nobs-1).wob = wei2

ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,$
 freq2,amp2,phase2,highres=1.0,nmax=nmax
freq = reform(freq2)*1e6/86400D & amp = reform(amp2)

nobs2 = n_elements(freq)
s(0:nobs2-1).fo = freq
s(0:nobs2-1).ao = amp

if epoch eq '99' or epoch eq '00' then $
 readcol,$
   w9 + 'wire_simul/WIRE'+epoch+'_hansk_granul'+batch+datamode,$
    t,d,w else $
 readcol,w9 + 'wire_simul/MOST_granul.dat',t,d,w
 

tt2 = t / 86400D & dat2 = d/1e6
np_sim = n_elements(tt2)
wei2=fltarr(np_sim) & wei2(*)=1.
minfreq=0.002 & maxfreq=fmax

s(0:np_sim-1).ts = tt2 & s(0:np_sim-1).ds = dat2 & s(0:np_sim-1).ws = wei2

ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,$
 freq2,amp2,phase2,highres=1.0,nmax=nmax
freq_sim = reform(freq2)*1e6/86400D & amp_sim = reform(amp2)

nsim2 = n_elements(freq_sim)
s(0:nsim2-1).fs = freq_sim
s(0:nsim2-1).as = amp_sim

sigma = 500e-6 ; Matthews paper
sigma = sigma / sqrt(245665. / 27648) ; points in Matthews / HansK simulation

if epoch eq '00' then sigma = 0.000135
if epoch eq '99' then sigma = 0.000135

n = randomn(seed,np_sim) * sigma
tt2 = t/86400D & dat2 = n & wei2 =fltarr(np_sim) & wei2(*)=1.

nnoi = n_elements(tt2)
s(0:nnoi-1).tn = tt2 & s(0:nnoi-1).dn = dat2 & s(0:nnoi-1).wn = wei2

minfreq=0.002 & maxfreq=fmax
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,$
 freq2,amp2,phase2,highres=1.0,nmax=nmax
freq_noi = reform(freq2)*1e6/86400D & amp_noi = reform(amp2)

nnoi2 = n_elements(freq_noi)
s(0:nnoi2-1).fn = freq_noi
s(0:nnoi2-1).an = amp_noi

; ============= ADD SIMULATION AND WHITE NOISE =============

tt2  = s(0:np_sim-1).ts
dat2 = s(0:np_sim-1).ds + s(0:nnoi-1).dn & dat2 = dat2-median(dat2)
wei2 = fltarr(np_sim) & wei2(*) = 1.

minfreq=0.002 & maxfreq=fmax
ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,$
 freq2,amp2,phase2,highres=1.0,nmax=nmax
freq_noisim = reform(freq2)*1e6/86400D & amp_noisim = reform(amp2)

nnoisim = n_elements(freq_noisim)
s(0:nnoisim-1).fsn = freq_noisim
s(0:nnoisim-1).asn = amp_noisim

; ================== FINAL PLOTS =============================

ff= 35 ; smooth factor

outps = '~/wire/wire_eps/wire_procyon_'+epoch+'.ps'
if dops eq 1 then setpl,19,8,outps

; plot_oo,[0,1],xr=[500,5000],/nodata,yr=[.4,200],ysty=1,xsty=3, $
;  xtit='log (Frequency)',ytit='log (Power)',tit='WIRE '+epoch
; oplot,freq,smooth(amp^2.0,ff),col=col.sky
; oplot,freq_sim,smooth(amp_sim^2.0,ff),col=col.red
; oplot,freq_noi,smooth(amp_noi^2.0,ff),col=col.magenta


 plot_oo,[0,1],xr=[50,5000],/nodata,yr=[.4,800],ysty=1,xsty=3, $
  xtit='log (Frequency)',ytit='log (Power)',tit='WIRE '+epoch
 oplot,s.fo,smooth(s.ao^2.0,ff),col=col.sky
 oplot,s.fs,smooth(s.as^2.0,ff),col=col.red
 oplot,s.fn,smooth(s.an^2.0,ff),col=col.magenta
; oplot,s.fsn,smooth(s.asn^2.0,ff),col=col.cyan


oplot,[1,1]*2000.,[.4,200],thick=3,line=2

if dops eq 1 then begin
 device,/close
 set_plot,'x'
endif

outsave = $
 '/ai40/bruntt/wire/wire_simul/wire_granul_spec_'+epoch+'_'+batch+datamode+'.idl'
save,filename=outsave,s,epoch

print,' %%% Save file: '+outsave

end
