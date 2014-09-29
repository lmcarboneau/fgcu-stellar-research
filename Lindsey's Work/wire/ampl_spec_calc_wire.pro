PRO ampl_spec_calc_wire, $
 x, y, y_weight, min_freq, max_freq, freq, ampl, fase

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
; mode = 1 ; data = y / mean(y) - 1.
  mode = 0 ; data = y - mean(y)

; sort timeseries by the time!
a = sort(x) & x = x(a) & y = y(a) 
 if n_elements(y_weight) ge 2 then y_weight = y_weight(a)
y = y + 5.0 ; on purpose: offset the light curve (avoid div. by zero error!)
x = reform(x)
y = reform(y)
 if n_elements(y_weight) ge 2 then y_weight = reform(y_weight)

conv = double(1e6 / 86400.) ; conversion of freq. to "micro Hertz" !
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
;resolution = 10. ; recommended range 5-100. 
                 ; res = 15 is ok, I guess
; resolution = 100. ; recommended range 5-100. =========== needed for LARGE SEP. CALC!
                  ; res = 15 is ok, I guess
resolution = 15.
; Recommended range for LARGE SEP. SEARCH: 200-400.
n_points = long((max_freq - min_freq + 1.) * resolution) 

; ======================================================================
; END OF ADDED BY H. BRUNTT:
; ======================================================================


nframes   = N_ELEMENTS(x)  ; Number of points in the input arrays.
ew   = DBLARR(nframes)     ; Making the input ascii data file for the 
ew   = y - avg(y)          ; Flux, eqw. or similiar data
time = DBLARR(nframes)                 
time = x - LONG(MEAN(x))   ; Subtr. mean to get time=0 in center of datasets.

IF stat_weight eq 0. THEN BEGIN ; set up the data array for HK's FFT prg.
  dataset      = DBLARR(2,nframes)
ENDIF ELSE BEGIN 
  dataset      = DBLARR(3,nframes)
  dataset(2,*) = y_weight
ENDELSE

dataset(0,*) = time
dataset(1,*) = ew

if n_elements(dir) ge 2 then begin
 print,' %%% Problem with your directory: '+dir
 RETURN
endif

datafile = dir+'/dataset.asc'
datafile2 = dir+'/spec_input.asc'
print,' Data file for fft : '
print,datafile
print,datafile2

OPENW, 1, datafile  ; input ascii file for HK's FFT prg.
 np = n_elements(ew)
 if stat_weight eq 0. then begin
   for ii=0L,np-1 do $
     PRINTF,1,dataset(*,ii),format='(D17.8,X,D12.8)' 
 endif else begin
   for ii=0L,np-1 do $
     PRINTF,1,dataset(*,ii),format='(D17.8,X,D12.8,X,D10.7)'
 endelse
CLOSE, 1

OPENW, 1, datafile2          ;Writing the input file for the Fourier program,

; PRINTF,1,strcompress(datafile,/remove_all)  ; 'name of input ascii data file',
PRINTF,1,strcompress('dataset.asc',/remove_all)  ; 'name of input ascii data file',
PRINTF,1,$ ;'choice of statistical weights'
 strcompress(string(stat_weight,format='(F5.3)'),/remove_all)  
PRINTF,1,$ ;'choice of Weight-exponent'
 strcompress(string(weight_exp,format='(F5.3)'),/remove_all)               
PRINTF,1,$ ;'choice of Min. frequency in microHz'
 strcompress(string(min_freq ,format='(F9.3)'),/remove_all)                   
PRINTF,1,$ ;'choice of Max. frequency in microHz'.
 strcompress(string(max_freq ,format='(F9.3)'),/remove_all)                    
PRINTF,1,$ ;'Number of points in spectrum'.
 strcompress(string(n_points,format='(I12)'),/remove_all)                    
 
CLOSE, 1

;PRINT,$
; 'If the mean of the time series equals 0 (exact), type 0; if not, type 1:'
;mean = get_kbrd(1)

; IMPORTANT !!! mean = 1 if light curves is offset (div. by Zero problem)
if mode eq 1 then mean = 1         
if mode eq 0 then mean = 0 ; ALWAYS USE MODE 0 WITH "DIFF-MAGN" DATA

; print,' MODE : ',mode

; if abs(avg(ew)) le 1e-7 then mean = 0

; cd /ai39/bruntt/idl/venus/mylib
; f77 -o four_trans_hk_RV four_trans_hk_RV.f
; f77 -o four_trans_hk four_trans_hk.f
; f77 -o four_trans_hk_RV_wire four_trans_hk_RV_wire.f
; f77 -o four_trans_hk_wire four_trans_hk_wire.f
; cd /ai39/bruntt/idl/venus/mylib
;  f77 -o four_trans_hk_wire_150000 four_trans_hk_wire_150000.f
; cd /ai39/bruntt/idl/venus/mylib
;  f77 -o four_trans_hk_RV_wire_130000 four_trans_hk_RV_wire_130000.f


spawnrob,'hostname',host

if strmatch(host,'*phys.au.*') then begin
 fft_prg1rv = '/ai39/bruntt/idl/venus/mylib/four_trans_hk_RV_wire'
 fft_prg2rv = '/ai39/bruntt/idl/venus/mylib/four_trans_hk_RV_wire_150000
 fft_prg1 = '/ai39/bruntt/idl/venus/mylib/four_trans_hk_wire'
 fft_prg2 = '/ai39/bruntt/idl/venus/mylib/four_trans_hk_wire_150000
endif
if strmatch(host,'*.eso.*') then begin
 fft_prg1rv = '~/bruntt/idl/venus/mylib/four_trans_hk_RV_wire_eso'
 fft_prg2rv = '~/bruntt/idl/venus/mylib/four_trans_hk_RV_wire_eso_150000
 fft_prg1 = '~/bruntt/idl/venus/mylib/four_trans_hk_wire_eso'
 fft_prg2 = '~/bruntt/idl/venus/mylib/four_trans_hk_wire_eso_150000
endif
if strmatch(host,'*usafa*') then begin
 fft_prg1rv = '~bruntt/idl/venus/mylib/four_trans_hk_RV_wire_usafa'
 fft_prg2rv = '~bruntt/idl/venus/mylib/four_trans_hk_RV_wire_usafa_150000
 fft_prg1 = '~bruntt/idl/venus/mylib/four_trans_hk_wire_usafa'
 fft_prg2 = '~bruntt/idl/venus/mylib/four_trans_hk_wire_usafa_150000
endif

; To compile Hans Kjeldsen's fortrans programs:
; f77 -o four_trans_hk_RV_wire_usafa_150000 four_trans_hk_RV_wire_150000.f
; f77 -o four_trans_hk_RV_wire_usafa        four_trans_hk_RV_wire.f
; f77 -o four_trans_hk_wire_usafa_150000    four_trans_hk_wire_150000.f
; f77 -o four_trans_hk_wire_usafa           four_trans_hk_wire.f

spawnrob,'pwd',orgdir & orgdir = orgdir(0)
cd, dir
 
IF mean EQ 0 THEN BEGIN   ; wire mode ... no offset !
 if nframes lt 80000 then begin
 SPAWNROB,fft_prg1rv + ' < '+datafile2, oo1 ;Run the Fourier program.
 endif else begin
 SPAWNROB,fft_prg2rv + ' < '+datafile2, oo1 ;Run the Fourier program.
 endelse
 if nframes gt 150000 then begin
  print,' *** Too few points will be allocated in fourier calc. program'
  stop
 endif 

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

; SPAWNROB,'mv spec.dat '+dir+'/spec.dat'   ;Move output 'spec.dat' to the right directory.

workfile = dir+'/spec.dat'
workfile2 = dir + '/spec.dat'
print,' Work file : '
print, workfile


spawnrob,'wc ' + workfile,oo
a = strsplit(oo,' ',/extract)
npp = long(a(0)) - 1 ; last line = comment!

cd,orgdir

;spec_arr = FLTARR(4,n_points)            ;Dimensions of 'spec.dat'.

if npp le 1 then begin
 print,' %%% Something went wrong with the FFT calculation ...'
 RETURN
endif

spec_arr = FLTARR(4,npp)            ;Dimensions of 'spec.dat'.
OPENR, 1, workfile
READF, 1, spec_arr, weight_noise
CLOSE, 1

freq = spec_arr(1,*)                     ;The out-put data.
ampl = spec_arr(2,*)
fase = spec_arr(3,*)

freq = freq / conv ; cycles pr. day please!

END
