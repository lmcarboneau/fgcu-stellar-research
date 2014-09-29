PRO ampl_spec_calc_wire_rv, $
 x, y, y_weight, min_freq, max_freq, freq, ampl, fase, highres=highres, $
 nmax=nmax,silent=silent,$
 dont_sub_median=dont_sub_median,sub_mean_time=sub_mean_time,datamode=datamode,$
 spectral=spectral 

; Don't mess with datamode
; spectral = calc. spectral window
default9, spectral, 0B

; SIMPLE EXAMPLE:
;  ampl_spec_calc2,lc(0,*),lc(1,*),0.,1,15,ff,aa,bb & plot,ff,aa

; YOU NEED TO COMPILE H. Kjeldsens fortran code:
;  f77 -o four_trans_hk_RV four_trans_hk_RV.f
;  f77 -o four_trans_hk    four_trans_hk.f

;PURPOSE: Calculate the amplitude spectrum of a chosen time series, 
;         with a given max, min and resolution of frequency.
;INPUT  : 1: The x-array (time in days) and
;         2: the y-array of the timeseries of which the amplitude 
;            spectrum should be calculated.
;         3: Two options: 
;             a) If input is a scalar, eg. "0" = No stat. weights.
;             b) The array of weights for the individual data points in 
;                the y-array.
;         4: Minimum frequency value checked in Fourier-transform; 
;            Unit: CYCLES PR. DAY
;         5: Maximum frequency value checked in Fourier-transform; 
;            Unit: CYCLES PR. DAY
;         6: Variable name for the out-put frequency. Unit: CYCLES PR. DAY
;         7: Variable name for the out-put amplitude. Unit: ppm
;         8: Variable name for the out-put fase. Unit: Radians

;OUTPUT: Three arrays; 1) the "frequency" in 1e-6Hz, 2) the "amplitude" in ppm,
;         and 3) the fase in radians. Also the scalar variable weight_noise 
;         is given.
;         Additionally, an input file for the Fortran Fourier program
;         four_trans_hk.f and four_trans_hk_RV.f
;         (which calculates the amplitude spectrum) is created in
;         the directory 'dir' and is called spec_input.asc. 
;         Furthermore the Fortran Fourier program makes an output file
;         containing the amplitude spectrum which is also written in the
;         directory 'dir' and is called spec.dat. 
;         (The spec.dat file has dimensions 4*n_points where 
;         1.col is the data point number, 
;         2.col is the frequency,  
;         3.col is the amplitude, and 
;         4.col is the fase). 
;
; 23/10-01 by Dennis Stello.
; 25/11-01 by Dennis Stello. 
;  Changed to include an array of weights for every data point 
;  in y-array, to be used by the Fortran Fourier program
;  calculating the amplitude spectrum.
; 06.02.02: Simplifications by Hans Bruntt ...
;***********************************************************************

; ======================================================================
; ADDED BY H. BRUNTT (TO SIMPLIFY CODE !?):
; ======================================================================
; ALWAYS USE MODE 0 WITH "DIFF-MAGN" DATA
 
default9, datamode, 0

if datamode eq 1 then begin ; spectral window, data avg. ne 0.000 !
  mode = 1 ; data = y / mean(y) - 1. ?
endif
if datamode eq 0 then begin ; normal differential photometry ?
  mode = 0 ; data = y - mean(y)
endif

default9, dont_sub_median, 0B ; do not subtract mean y-value?
default9, sub_mean_time, 1B   ; subtract mean time in data set?

if spectral then y(*) = 1D ; calc. spectral window

if n_elements(silent) eq 0 then silent = 0

if n_elements(nmax) eq 0 then nmax = 5e6
if n_elements(highres) eq 0 then highres = 1.0

; sort timeseries by the time!
a = sort(x) & x = x(a) & y = y(a) 
 if n_elements(y_weight) ge 2 then y_weight = y_weight(a)
y = y + 100.0 ; on purpose: offset the light curve (avoid div. by zero error!)
x = reform(x)
y = reform(y)
 if n_elements(y_weight) ge 2 then y_weight = reform(y_weight)

conv = double(1e6 / 86400D ) ; conversion of freq. from "c/day" to "micro Hertz" !
res = conv / (max(x) - min(x)) ; resolution in micro Hz
  ; print,res,conv,(max(x) - min(x))

 min_freq = min_freq * conv 
 max_freq = max_freq * conv

spawnrob,'pwd',dir  ; get the present dir, files are saved here!
stat_weight = 1  ; default: use stat. weights!
weight_exp  = 1. ; default for HK program (range 0.1 - 2.0)

if n_elements(y_weight) eq 1 then begin ; no weights given
 nn = double(n_elements(x))
 y_weight = fltarr(nn) & y_weight(*) = double(1. / nn)
 stat_weight = 0 ; do not use stat. weights !
endif

 y_weight = y_weight / total(y_weight) ; normalize weights!

; IMPORTANT: RESOLUTION OF SPECTRUM !
; N > 2 * delta(COMPUTED FREQ RANGE) / delta(freq. resolution)
;  delta(freq. resolution) = 1. / delta(time) 
;   ---> Remember: freq. have same unit!

n_points = (max_freq - min_freq) * 2.5 / res
if n_points lt 5 then begin
 print,' *** n_points too low: ', n_points
 stop
endif

n_points = n_points * highres
n_points = max([1,long(n_points / 1000.)]) * 1000. + 1.
; At least 1001 points ...

if silent eq 0 then $
 print,' %%% Number of points in ampl. spectrum: ', n_points

if n_points ge nmax then begin
 print,' *** Too many data points in calculation !!'
 stop
endif

; From wire_clean.pro:
; np = 5.0 * (f2 - f1) * ffac / f_resolution
; np = ceil(np / 1000.) * 1000. + 1.

; ======================================================================
; END OF ADDED BY H. BRUNTT:
; ======================================================================


nframes   = N_ELEMENTS(x)  ; Number of points in the input arrays.
ew   = DBLARR(nframes)     ; Making the input ascii data file for the 
ew   = y - avg(y)          ; Flux, eqw. or similiar data
time = DBLARR(nframes)                 
time = x - sub_mean_time * LONG(MEAN(x))   ; Subtr. mean to get time=0 in center of datasets.

if dont_sub_median eq 1 then ew(*) = 1 ; window function calc?

IF stat_weight eq 0. THEN BEGIN ; set up the data array for HK's FFT prg.
  dataset      = DBLARR(2,nframes)
ENDIF ELSE BEGIN 
  dataset      = DBLARR(3,nframes)
  dataset(2,*) = y_weight
if silent eq 0 then $
  print,' %%% Applying statistical point weights ... ! '
ENDELSE

dataset(0,*) = time
dataset(1,*) = ew

if n_elements(dir) ge 2 then begin
 print,' %%% Problem with your directory: '+dir
 RETURN
endif

datafile  = dir+'/dataset.asc'
datafile2 = dir+'/spec_input.asc'
if silent eq 0 then begin
 print,' %%% Data file for fft : '
 print,' %%% ' + datafile
 print,' %%% ' + datafile2
endif

OPENW, 1, datafile  ; input ascii file for HK's FFT prg.
 np = n_elements(ew)
 if stat_weight eq 0. then begin
   for ii=0L,np-1 do $
     PRINTF,1,dataset(*,ii),format='(D17.8,X,D18.8)' 
 endif else begin
   for ii=0L,np-1 do $
     PRINTF,1,dataset(*,ii),format='(D17.8,X,D18.8,X,D10.7)'
 endelse
CLOSE, 1

OPENW, 1, datafile2          ;Writing the input file for the Fourier program,

PRINTF,1,strcompress('dataset.asc',/remove_all)  ; 'name of input ascii data file',
PRINTF,1,$ ;'choice of statistical weights'
 strcompress(string(stat_weight,format='(F5.3)'),/remove_all)  
PRINTF,1,$ ;'choice of Weight-exponent'
 strcompress(string(weight_exp,format='(F5.3)'),/remove_all)               
PRINTF,1,$ ;'choice of Min. frequency in microHz'
 strcompress(string(min_freq ,format='(F9.3)'),/remove_all)                   
PRINTF,1,$ ;'choice of Max. frequency in microHz'.
 strcompress(string(max_freq ,format='(F15.3)'),/remove_all)                    
PRINTF,1,$ ;'Number of points in spectrum'.
 strcompress(string(n_points,format='(I16)'),/remove_all)                    
 
CLOSE, 1

;PRINT,$
; 'If the mean of the time series equals 0 (exact), type 0; if not, type 1:'
;mean = get_kbrd(1)

; IMPORTANT !!! mean = 1 if light curves is offset (div. by Zero problem)
if mode eq 1 then mean = 1 ; EG. USE FOR CALC. SPECTRAL WINDOW 
if mode eq 0 then mean = 0 ; ALWAYS USE MODE 0 WITH "DIFF-MAGN" DATA

; print,' MODE : ',mode

; if abs(avg(ew)) le 1e-7 then mean = 0

; January 2005:
; cd ~/bin
; g77 -o four_trans_hk_promepis_150000 /home/hanxxor/idl/wire/four_trans_hk_wire_150000.f
; g77 -o four_trans_hk_RV_promepis_150000 /home/hanxxor/idl/wire/four_trans_hk_RV_wire_150000.f
; g77 -o four_trans_hk_promepis /home/hanxxor/idl/wire/four_trans_hk_wire.f
; g77 -o four_trans_hk_RV_promepis /home/hanxxor/idl/wire/four_trans_hk_RV_wire.f

spawnrob,'hostname',host

; January 2005:
if strmatch(host,'*taurus*') then begin
 fft_prg1rv = '~/bin/four_trans_hk_RV_nbi'
 fft_prg2rv = '~/bin/four_trans_hk_RV_nbi_150000'
 fft_prg1 = '~/bin/four_trans_hk_nbi'
 fft_prg2 = '~/bin/four_trans_hk_nbi_150000'
endif

; January 2005:
if strmatch(host,'*leo*') then begin
 fft_prg1rv = '~/bin/four_trans_hk_RV_leo'
 fft_prg2rv = '~/bin/four_trans_hk_RV_leo_150000'
 fft_prg1 = '~/bin/four_trans_hk_leo'
 fft_prg2 = '~/bin/four_trans_hk_leo_150000'
endif

; Feb 2005:
if strmatch(host,'*lynx*') then begin
 fft_prg1rv = '~/bin/four_trans_hk_RV_lynx'
 fft_prg2rv = '~/bin/four_trans_hk_RV_lynx_150000'
 fft_prg1 = '~/bin/four_trans_hk_lynx'
 fft_prg2 = '~/bin/four_trans_hk_lynx_150000'
endif

; January 2005:
if strmatch(host,'*manowar*') then begin
 fft_prg1rv = '~/bin/four_trans_hk_RV_promepis'
 fft_prg2rv = '~/bin/four_trans_hk_RV_promepis_150000'
 fft_prg3rv = '~/bin/four_trans_RV_promepis_550000'
 fft_prg1 = '~/bin/four_trans_hk_promepis'
 fft_prg2 = '~/bin/four_trans_hk_promepis_150000'
 fft_prg3 = '~/bin/four_trans_hk_promepis_550000'
endif

; February 2006: Sydney
if strmatch(host,'brixx.physics.usyd.edu.au') then begin
 fft_prg1rv = '~/bin/four_trans_hk_RV_promepis'
 fft_prg2rv = '~/bin/four_trans_hk_RV_promepis_150000'
 fft_prg3rv = '~/bin/four_trans_RV_promepis_550000'
 fft_prg1 = '~/bin/four_trans_hk_promepis'
 fft_prg2 = '~/bin/four_trans_hk_promepis_150000'
 fft_prg3 = '~/bin/four_trans_hk_promepis_550000'
 fft_prg_spectral_window = '~/bin/four_trans_hk_spectral'
endif


if strmatch(host,'*brunttlt*') or $
   strmatch(host,'*amalthea*') then begin
 fft_prg1rv = '~/bin/four_trans_hk_RV_wire_mepis'
 fft_prg2rv = '~/bin/four_trans_hk_RV_wire_mepis_150000'
 fft_prg1 = '~/bin/four_trans_hk_wire_mepis'
 fft_prg2 = '~/bin/four_trans_hk_wire_mepis_150000'
endif
if strmatch(host,'*phys.au.*') or $
   strmatch(host,'*amalthea*') then begin
 fft_prg1rv = '/ai40/bruntt/idl/venus/mylib/four_trans_hk_RV_wire'
 fft_prg2rv = '/ai40/bruntt/idl/venus/mylib/four_trans_hk_RV_wire_150000'
 fft_prg1 = '/ai40/bruntt/idl/venus/mylib/four_trans_hk_wire'
 fft_prg2 = '/ai40/bruntt/idl/venus/mylib/four_trans_hk_wire_150000'
endif
if strmatch(host,'*.eso.*') then begin
 fft_prg1rv = '~/bruntt/idl/venus/mylib/four_trans_hk_RV_wire_eso'
 fft_prg2rv = '~/bruntt/idl/venus/mylib/four_trans_hk_RV_wire_eso_150000'
 fft_prg1 = '~/bruntt/idl/venus/mylib/four_trans_hk_wire_eso'
 fft_prg2 = '~/bruntt/idl/venus/mylib/four_trans_hk_wire_eso_150000'
endif
if strmatch(host,'*usafa*') then begin
 fft_prg1rv = '~bruntt/idl/venus/mylib/four_trans_hk_RV_wire_usafa'
 fft_prg2rv = '~bruntt/idl/venus/mylib/four_trans_hk_RV_wire_usafa_150000'
 fft_prg1 = '~bruntt/idl/venus/mylib/four_trans_hk_wire_usafa'
 fft_prg2 = '~bruntt/idl/venus/mylib/four_trans_hk_wire_usafa_150000'
endif


; To compile Hans Kjeldsen's fortran77 programs:
; f77 -o four_trans_hk_RV_wire_usafa_150000 four_trans_hk_RV_wire_150000.f
; f77 -o four_trans_hk_RV_wire_usafa        four_trans_hk_RV_wire.f
; f77 -o four_trans_hk_wire_usafa_150000    four_trans_hk_wire_150000.f
; f77 -o four_trans_hk_wire_usafa           four_trans_hk_wire.f

spawnrob,'pwd',orgdir & orgdir = orgdir(0)
cd, dir

if silent eq 0 then $
 if mean eq 0 then print,' ****** RV MODE! ****** '

; ------------------------------------------------ 
IF spectral then begin ; NEW MARCH 2006
 SPAWNROB,fft_prg_spectral_window + ' < '+datafile2, oo1
 print,' %%% SPECTRAL WINDOW CALC. w/ prg. ' + fft_prg_spectral_window
ENDIF ELSE BEGIN
; ------------------------------------------------ 

IF mean EQ 0 THEN BEGIN   ; wire mode ... no offset !

 if nframes gt 550000 then begin
  print,' *** Too few points will be allocated in fourier calc. program'
  stop
 endif 

 if nframes lt 80000 then begin
 SPAWNROB,fft_prg1rv + ' < '+datafile2, oo1 ;Run the Fourier program.
 endif else begin

 if nframes lt 150000 then begin
   SPAWNROB,fft_prg2rv + ' < '+datafile2, oo1 ;Run the Fourier program.
 endif else begin
   SPAWNROB,fft_prg3rv + ' < '+datafile2, oo1 ;Run the Fourier program.
 endelse 
 endelse

ENDIF ELSE BEGIN
 if nframes lt 80000 then begin
 SPAWNROB,fft_prg1 + ' < '+datafile2, oo1
 endif else begin
 SPAWNROB,fft_prg1 + ' < '+datafile2, oo1
endelse

 if nframes gt 150000 then begin
  print,' *** Too few points will be allocated in fourier calc. program'
  stop
 endif 

ENDELSE

; ------------------------------------------------ 
ENDELSE
; ------------------------------------------------ 
; SPECTRAL WINDOW or NORMAL AMPL. CALCULATION?
; Specal = do not subtr. mean data value !
; ------------------------------------------------ 

workfile = dir+'/spec.dat'
workfile2 = dir + '/spec.dat'
if silent eq 0 then begin
 print,' Work file : '
 print, workfile
endif

spawnrob,'wc ' + workfile,oo
a = strsplit(oo,' ',/extract)
npp = long(a(0)) - 1 ; last line = comment!

cd,orgdir

if npp le 1 then begin
 print,' %%% Something went wrong with the FFT calculation ...'
 RETURN
endif

spec_arr = FLTARR(4,npp)            ;Dimensions of 'spec.dat'.

print,' %%% Output file from HK code: ' + workfile

; 1-line input ... Changed by Bruntt in July 2004:
readcol, workfile, dummy, freq, ampl, fase, format='F, F, F, F',/silent

freq = freq / conv ; cycles pr. day please!

END
