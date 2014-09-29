PRO granulation, timefile, i_timeG, i_ampG, i_ampP, i_dtime, $
 wn=wn, outdir=outdir, seed=seed, $
 relamp=relamp, cnter_start=cnter_start, starname=starname, $
 addname = addname

; i_timeG: time scale for granulation
; i_ampG:  scaled amplitude of granulation 1.0 = Procyon
; i_ampP:  amplitude of p-modes (Procyon ~ 9) [ppm]
; i_dtime: decay time for p-modes (equal for all modes)
; >>> Input parameters can be arrays! (added 24th of Nov 2004 by HB)
; wn:    white noise level in ppm

; Example:
; granulation,
; '~/wire/wire_lc/wire_lc_Procyon_2000_s0_HD61421_F5IV-V_scat.dat', $
; 500., 1., [7,9,11], wn = 100., outdir='~/wire/wire_sim/'

; (c) Hans Kjeldsen & Hans Bruntt 2004
; Original version by HK. This version by HB: it includes more
; features like making several simulations at once.

; /usr/local/bin/idl << EOF > output.dat
; /usr/local/rsi/idl_6.0/bin/idl << EOF > output.dat
;
; The following cshell program is made for simulating the
; WIRE data on Procyon (alpha CMi).
;
; The routine is setup as a cshell file in order to allow
; the routine to run using IDL-demo (where no file-output
; is allowed). The output file "output.dat" is therefore
; created by "> output.dat"
;
; NOTE: you will have to give the full name of the idl
; executable on your local system.. e.g. type "which idl"
; to find this.
; Correct "/usr/local/bin/idl" above to your local idl location.
;
; In order to use this routine you will need:
;
; 1. A file which contain only the time of the observing (in sec).
;    The name of this file should be:   "time.inp"
;
; 2. Your should know:
;    a. The number of lines in this file (e.g. n=27648)
;    b. The amplitude of the p-mode oscillations (e.g. ampP = 9.10)
;    c. The relative amplitude of the granulation (ampG = 1.00)
;    d. The time-scale of granulation (timeG = 550) in sec.
;    e. The level of white noise (xW = 0.00). SET TO ZERO here
;
; 3. Change the numbers below
;
; 4. Remeber to set the seed number (first line below), if you
;    do want to run different simulations.
;
; 5. change mode of this file (do.WIRE) by "chmod 755 do.WIRE"
;
; 6. and run the program:  "do.WIRE" or "./do.WIRE"
;    Output will be in file "output.dat"
;
; Hans Kjeldsen - 23.11.2004
;

default9, relamp, [1.00,1.15,0.60] ; relative amplitude of l=0,1,2
default9, starname,'procyon'

g = findfile(timefile,Count=cnt)
if cnt ne 1 then begin
 print,' %%% Time file not found: ' + timefile
 RETURN
endif

if n_elements(seed) eq 0 then $
 seed = ceil(randomu(seed2)*600000L) ; pick a random seed number
org_seed = seed ; store the seed number (to be used for White Noise also)

default9, i_ampP, 9.1; amplitude of p-modes in Procyon in ppm
default9, i_ampG, 1.0  ; amplitude of granulation
default9, i_timeG, 550.  ; time-scale of granulation in sec.
default9, i_dtime, 1.0 ; life time in days for ALL modes
default9, cnter_start, 0B ; start counter if prg. crashes (no space left on Device?)
xW = 0.00  ; level of white noise

cnter = 0L ; counter


;  SIMULATION SOFTWARE 2004
;
xn0=xW ; White Noise per data point
; NOT USED
xn1=0. ; Drift-noise scale
; NOT USED
dd99 = 20000. ; Drift-noise time scale (sec)
dd99 = 1. - 100./dd99

; In case your running several simulations:
nnp  = n_elements(i_ampP)  & nng  = n_elements(i_ampG)
nntg = n_elements(i_timeG) & nndt = n_elements(i_dtime)

print,' %%% Number of simulations to do: ' + $
 strcompress(nnp * nng * nntg * nndt,/remove_all)

for n1=0,nnp-1 do begin  ; all amplitudes of P modes
for n2=0,nng-1 do begin  ; all amplitudes of granulation
for n3=0,nntg-1 do begin ; all time scales of granulation
for n4=0,nndt-1 do begin ; all life times of modes

; Select the parameters to be used:
ampP  = i_ampP(n1)  & ampG  = i_ampG(n2)
timeG = i_timeG(n3) & dtime = i_dtime(n4)


xn2=47.0*ampG  ;  Procyon Granulation

if cnter lt cnter_start then goto,skip_simulation

print,' %%% Sim = ' + strcompress(cnter+1,/remove_all) + $
 ' Gran ampl: ' + $
 strcompress(string(ampG,format='(F9.2)'),/remove_all) + ' (rel) ' + $
 ' Gran tscl: ' + $
 strcompress(string(timeG,format='(F9.1)'),/remove_all) + ' s '



; LIFE TIME OF PROCYON OSCILLATIONS: set to 1 d in this simulation
; dtime=1.0  ; Mode lifetime (days)
; dtime_use=exp(-100./(86400./dtime)) ; Så er det helt korrekt formelt (HK)
dtime_use=exp(-100./86400./dtime) ; Så er det helt korrekt formelt (HK)
; ERROR CORRECTED 10TH OF DEC 2004: ---> dtime_use=exp(-100./86400./dtime)


ampX = ampP/sqrt(2.) ; Mode peak amplitude (ppm)

print,' ... Peak mode ampl: ' + $
 strcompress(string(ampX,format='(F9.2)'),/remove_all) + ' ppm ' + $
 ' Life time factor: ' + $
 strcompress(string(dtime_use,format='(F12.7)'),/remove_all)


readcol, timefile,timeinp,mag,weight, /silent, $
 format='D,D,D' & n = n_elements(timeinp)
s = sort(timeinp) & timeinp = timeinp(s) ; sort by times
tzero = min(timeinp)
timeinp = timeinp - tzero ; min time == 0
timeinp = timeinp * 86400D ; convert time to seconds!
data = fltarr(n)

np = 30000
data2 = fltarr(np)
data3 = fltarr(np)
data4 = fltarr(np)

; Granulation simulation
;
Gcoef = exp(-100./timeG)
for i=1L,np-1 do data2(i)=Gcoef*data2(i-1)+randomn(seed,1)*xn2
data2(0)=data2(np-1)
for i=1L,np-1 do data2(i)=Gcoef*data2(i-1)+randomn(seed,1)*xn2
data2(0)=data2(np-1)
for i=1L,np-1 do data2(i)=Gcoef*data2(i-1)+randomn(seed,1)*xn2


goto,not_used1
;
; NOT USED
;
for i=1L,np-1 do data3(i)=0.998*data3(i-1)+randomn(seed,1)*xn2/7.1*0.
data2(0)=data3(np-1)
for i=1L,np-1 do data3(i)=0.998*data3(i-1)+randomn(seed,1)*xn2/7.1*0.
data2(0)=data3(np-1)
for i=1L,np-1 do data3(i)=0.998*data3(i-1)+randomn(seed,1)*xn2/7.1*0.
;
; NOT USED
;
for i=1L,np-1 do data4(i)=dd99*data4(i-1)+randomn(seed,1)*xn1
data2(0)=data4(np-1)
for i=1L,np-1 do data4(i)=dd99*data4(i-1)+randomn(seed,1)*xn1
data2(0)=data4(np-1)
for i=1L,np-1 do data4(i)=dd99*data4(i-1)+randomn(seed,1)*xn1
;

not_used1:

; Rescaling of granulation.
;
for i=0L,n-1 do data(i)=data2(fix(timeinp(i)/100.+1.60))+randomn(seed,1)*xn0
for i=0L,n-1 do data(i)=data(i)+data2(fix(timeinp(i)/100.+1.20))+randomn(seed,1)*xn0
for i=0L,n-1 do data(i)=data(i)+data2(fix(timeinp(i)/100.+0.85))+randomn(seed,1)*xn0
for i=0L,n-1 do data(i)=data(i)+data2(fix(timeinp(i)/100.+0.50))+randomn(seed,1)*xn0
for i=0L,n-1 do data(i)=data(i)+data2(fix(timeinp(i)/100.+0.15))+randomn(seed,1)*xn0
for i=0L,n-1 do data(i)=data(i)+data2(fix(timeinp(i)/100.-0.20))+randomn(seed,1)*xn0
for i=0L,n-1 do data(i)=data(i)+data2(fix(timeinp(i)/100.-0.60))+randomn(seed,1)*xn0
for i=0L,n-1 do data(i)=data(i)/7.
;
; NOT USED
;
;for i=0L,n-1 do data(i)=data(i)+data3(fix(timeinp(i)/100.+0.5))+randomn(seed,1)*xn1
;for i=0L,n-1 do data(i)=data(i)+data4(fix(timeinp(i)/100.+0.5))+randomn(seed,1)*xn1

amp1 = fltarr(np)
if strmatch(starname,'*procyon*',/fold_case) then $
 granulation_get_pmodes, 60, ampX, frq, amp, pha, relamp=relamp

if strmatch(starname,'*sun*',/fold_case) then $
 granulation_get_pmodes, 75, ampX, frq, amp, pha, relamp=relamp,$
 df=135.,d02=12.,envelope_fcen=3200. ; ,/debug

if strmatch(starname,'*singlefreq*',/fold_case) then begin
 amp = fltarr(60) & amp(0) = ampX
 frq = fltarr(60) & frq(*)=1. & frq(0) = 1000D
 pha = fltarr(60) & frq(0) = randomu(seeder) * 2 * !DPI
 ; pha = 0..2PI
endif


 
;print, frq
;print, amp
;print, pha

; frq = frq*2.*3.14159265 / 1000. / 1000.
frq = frq * 2D * (!DPI / 1e6)
j=0
;
; Simulating the excitation of modes  (j=0,59)
;
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)

;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampC = amp1/sqrt(mean(amp1*amp1))
;print, ampC
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
amp1(0)=amp1(np-1)
for i=1L,np-1 do amp1(i)=amp1(i-1)*dtime_use+randomn(seed,1)
;amp1 = amp1-mean(amp1)
ampS = amp1/sqrt(mean(amp1*amp1))
;print, ampS
amp1(0)=amp1(np-1)
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampC(fix(timeinp(i)/100.+0.5))*cos(frq(j)*timeinp(i)+pha(j))
for i=0L,n-1 do data(i)=data(i)+amp(j)*ampS(fix(timeinp(i)/100.+0.5))*sin(frq(j)*timeinp(i)+pha(j))
;print, j+1
j=j+1
data2 = -1.*data
data3 = 0.*data-1.

print,''
print,' ****** final j value: ', j
print,''

;
;  NOT USED
;
xtime=1000. ; filter time-scale (sec)
;for i=0L,n-1 do for j=0,n-1 do data2(i)=data2(i)+data(j)*exp(-1.*(timeinp(i)-timeinp(j))^2/xtime^2)
;for i=0L,n-1 do for j=0,n-1 do data3(i)=data3(i)+exp(-1.*(timeinp(i)-timeinp(j))^2/xtime^2)
data99 = dblarr(3,n)
data99(0,*)=timeinp
;data99(1,*)=data-data2/data3
data99(1,*)=data
data99(2,*)=1.
;data99(1,*)=data2
;data99(2,*)=data3
;
;  Output til screen (file: output.dat)
;

; ===============================================================
; Add white noise (Hans Bruntt 23rd of November 2004):
; ===============================================================
if n_elements(wn) eq 1 then begin
 seed_wn = org_seed ; make sure you use the same seed number
 ndd = n_elements(data99(0,*))
 for i=0L,ndd-1 do $
  data99(1,i) = data99(1,i) + randomn(seed_wn) * wn ; wn in ppm
endif
; ===============================================================

data99(0,*) = data99(0,*) / 86400D ; transform back to days
data99(0,*) = data99(0,*) + tzero  ; with the same zero point as input!
data99(2,*) = data99(2,*) / total(data99(2,*)) ; sum of weights == 1.0
data99(1,*) = data99(1,*) * (1.086D / 1e6) ; convert ppm to magnitudes!

outfile = timefile + '.sim'

add_timeG = strcompress(string(timeG,format='(F9.1)'),/remove_all)
add_ampG = strcompress(string(ampG,format='(F9.2)'),/remove_all)
add_ampP = strcompress(string(ampP,format='(F9.1)'),/remove_all)
add_lifetime = strcompress(string(dtime,format='(F9.2)'),/remove_all)


if n_elements(wn) eq 0 then add_wn = '_WN0.0' else $
 add_wn = '_WN' + $
  strcompress(string(wn,format='(F10.1)'),/remove_all)

add_name = 'gt' + add_timeG + '_aG' + $
 add_ampG + '_aP' + add_ampP + '_dT' + add_lifetime + $
 add_wn

s = strsplit(timefile,'.',/extract) & s = s(0)
outfile = s(0) + '.' + add_name + '.dat'

;if n_elements(addname) ne 0  and $
;   n_elements(i_ampP) eq 1 and n_elements(i_ampG) eq 1 and $
;   n_elements(i_timeG) eq 1 and n_elements(i_dtime) eq 1 then begin
; outfile = addname + '_' + outfile  
; print,' %%% Outfile name: ' + outfile
;endif

if n_elements(outdir) ne 0 then begin
 if cnter eq 0 then spawnrob,'mkdir ' + outdir ; create the output dir!
 s5 = strsplit(timefile,'/',/extract) & ns5 = n_elements(s5)
 n5 = s5(ns5-1)
 s6 = strsplit(n5,'.',/extract)
 n6 = s6(0)
 default9, addname, ''
 outfile = outdir + '/' + addname + n6 + add_name + '.dat'
endif

 print,' %%% Outfile name: ' + outfile


openw,1,outfile
nout = n_elements(data99(0,*))

for i=0L,nout-1 do $
 printf,1,data99(*,i), $
  format='(D15.8,D20.12,D15.8)'
close,1

print,' %%% File: ' + outfile

skip_simulation:

cnter = cnter + 1

   endfor
  endfor
 endfor
endfor



; print, data99
;;;print, ampC, ampS
; exit
; EOF

seed = org_seed ; added 2 DEC 2004 by HB

END
