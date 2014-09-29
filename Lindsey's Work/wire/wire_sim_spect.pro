PRO wire_sim_spect, lc1, $
 tgran=tgran, agran=agran, amode=amode, lifet=lifet, wn1=wn1, $
 seed=seed, relamp=relamp, outdir = outdir, debug=debug


; Make granulation + p mode simulation
; and calculate the power density spectrum

; Example:
; wire_sim_spect,'~/wire/wire_lc/wire_lc_Procyon_1999_s0_mer5.dat',lifet=50

  

default9, tgran, 900.
default9, agran, 0.80
default9, amode, 15.
default9, lifet, 4.0
default9, wn1, 285.
default9, seed, 10000
default9, relamp, [1.00,1.15,0.60] ; relative amplitude of l=0,1,2

default9, outdir, '~/wire/sim/wire_components/septest99_relamp1/'
default9, debug, 0B

granulation, lc1, tgran, agran, amode, lifet, wn=wn1,$
  outdir=outdir,seed=seed, $
  relamp = relamp

; Get the newest light curve:
spawnrob,'ls -1tr ' + outdir, a & na = n_elements(a)
lcfile = outdir + '/' + a(na-1)

; Import the light curve:
readcol,lcfile,t,d,w

; Calculate amplitude spectrum
x=t & y=d & y_weight=w & min_freq=0.2 & max_freq=700.
ampl_spec_calc_wire_rv, $
 x, y, y_weight, min_freq, max_freq, freq, ampl, fase

; >>>> analysis:

; Power density spectrum: (factor from 1999 light curve)
dens = ( (ampl/1.086)^2.) / 6.864 ; convert to ppm, divide by 6.864 microHz
fac = 1e6 / 86400D
freq = freq * fac

restore,'/home/bruntt/wire/sim/Procyon_1999_m5_nomodes.idl'

if debug then begin
 plot_oo,freq,dens,xr=[100,1e4],yr=[.1,150],/nodata,$
  xthick=2,ythick=2,charsi=1.2,charthick=2,xsty=1,ysty=1
 oplot,freq,dens,thick=2,col=50
 oplot,merg.f,merg.d,thick=2 

 hitme,s9
endif

wg = where(freq gt 100. and freq lt max(merg.f) and dens gt 1e-3,cg)
ps = interpol(merg.d, merg.f, freq(wg) )

if debug then begin
 ; plot_oo,freq(wg), dens(wg) / ps,xr=[100,25000],xsty=1
 plot_oo,freq(wg), smooth(dens(wg) / ps,21,/edge) ,xr=[100,25000],xsty=1
 hitme, s9
endif

ff  = freq(wg) & dd  = dens(wg) / ps
ff2 = freq(wg) & dd2 = dens(wg) / ps

; Remove orbital signal
forb = 173.68056
wire_exclude_orbital,ff,dd,fny,dny,$
 forb=forb,df=13.,inc=.001,$
 fskip=173.68+[-1,1]*50.,skip2=39.+[-1,1]*12.


plot,ff,dd,yr=[0,15],ysty=1,min_value=1e-3,xr=[0,5000]
oplot,fny,dny,col=100,min_value=1e-3

wg2 = where(fny gt 1e-3,cg2)
f1 = fny(wg2) & d1 = dny(wg2)

f1 = ff2 & d1 = dd2

g = strsplit(lcfile,'.',/extract) & ng = n_elements(g)
outfile = ''
for l=0,ng-2 do outfile = outfile + g(l) + '.'
outfile = outfile + 'idl'

save,filename=outfile, f1,d1

print,' %%% Saved file: ' + outfile

END
