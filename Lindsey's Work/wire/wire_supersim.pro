; Make N simulations of the best WIRE 2000 time series:

nsim = 100

; mode = 'purewhite2000'
; mode = 'bestfit2000'
mode = 'mostfirst10'
mode = 'mostfirst10white'

mode = 'procyonWIRE2005'


base = 'undefined'

; IF MOST DID BETTER?

if mode eq 'procyonWIRE2005' then begin
 ag = 0.7         ; = 32.9 ppm ; 32.9 / 47.  = 0.7000
 ts = 750.        ; time scale for granulation
 pp = 9.9*sqrt(2.); p-mode noise ---> too high ampl.
 pp = 8. * sqrt(2.)
 base = '~/wire/procyon/simul/procyon2005sim100/'
 wnlev = 110.      ; white noise level as for WIRE
 lcpro00 = '/home/bruntt/wire/wire_lc/' + $
   'wire_lc_Procyon_Mar2005_s0_HD61421_F5IV-V_scatfit_sublow.dat'
endif

if mode eq 'bestfit2000' then begin
 ag = 0.7         ; = 32.9 ppm ; 32.9 / 47.  = 0.7000
 ts = 750.        ; time scale for granulation
 pp = 9.9*sqrt(2.); p-mode noise
 base = '~/wire/procyon/simul/peakdist2000/'
 wnlev = 99.      ; white noise level as for WIRE
 lcpro00 = '~/wire/procyon/power//wire_lc_Procyon_2000_s0_HD61421_F5IV-V_sublow.dat'
endif

if mode eq 'purewhite2000' then begin
 ag = 0.         ; = 32.9 ppm ; 32.9 / 47.  = 0.7000
 ts = 7.         ; time scale for granulation
 pp = 0.         ; p-mode noise
 base = '~/wire/procyon/simul/purewhite2000/'
 wnlev = 99.      ; white noise level as for WIRE
 lcpro00 = '~/wire/procyon/power//wire_lc_Procyon_2000_s0_HD61421_F5IV-V_sublow.dat'
endif

if mode eq 'mostfirst10' then begin
 ag = 0.7          ; = 32.9 ppm ; 32.9 / 47.  = 0.7000
 ts = 750.         ; time scale for granulation
 pp = 9.9*sqrt(2.) ; p-mode noise
 base = '~/wire/procyon/simul/mostfirst10/'
 wnlev = 99.      ; white noise level as for WIRE
 lcpro00 = '~/wire/procyon/most32d_smoo_first10.dat' ; 10 days from most at high cadence
endif

if mode eq 'mostfirst10white' then begin
 ag = 0.          ; = 32.9 ppm ; 32.9 / 47.  = 0.7000
 ts = 7.         ; time scale for granulation
 pp = 0. ; p-mode noise
 base = '~/wire/procyon/simul/mostfirst10white/'
 wnlev = 99.      ; white noise level as for WIRE
 lcpro00 = '~/wire/procyon/most32d_smoo_first10.dat' ; 10 days from most at high cadence
endif




baseout = base + 'density/'

; Remove old data
print,' %%% Removing dir: ' + base + ' ... OK?  (press "y" to proceed) '

s = get_kbrd(1) & if s ne 'y' then stop

spawnrob,'rm -fr ' + base, oo

spawnrob,'mkdir ' + base,oo
spawnrob,'mkdir ' + baseout,oo

for i=0,nsim-1 do begin

if i le 9 then              addn = '00' + strcompress(i,/remove_all)
if i ge 10 and i le 99 then addn =  '0' + strcompress(i,/remove_all)
if i ge 100 then            addn =        strcompress(i,/remove_all)

granulation, lcpro00, [ts], [ag], [pp], 1.0,wn=wnlev,outdir=base, addname = 'it' + addn + '_'

endfor

print,''
print,' %%% Computing the amplitude spectra!'
print,''

wire_density_all,base + '/*.dat',outdir = baseout,fmax=10000.,highres=3.5

print,' %%% Try:   .r wire_peakdist.pro '


end
