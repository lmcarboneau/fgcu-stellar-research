PRO wire_windowfunc, file, outfile, amp

; Purpose:
; ------------
; Calculate the window function for a light curve

; Example
; ------------
; wire_windowfunc,$
;  '~/wire/procyon/power//wire_lc_Procyon_2000_s0_HD61421_F5IV-V_sublow.dat',$
;  'temp.dat', amp

readcol,file,t,d,w,format='D,D,D'

d(*) = 1.0
n = n_elements(d)

openw,1,outfile
for i=0L,n-1 do $
 printf,1,t(i),d(i),w(i),format='(D15.8,D15.8,D15.8)'
close,1

; wire_density_analysis,outfile,wwin,fmax=25000, fmin=20.,$
;  forb=164.344, df=1e-6, density=5., /skip_smooth, /dont_sub_median

tt2 = t & dat2 = d & wei2 = w
fac = 1e6/86400D
minfreq = 2./fac & maxfreq = 5000./fac
highres = 3.5

fac = 1e6/86400D
f = 1e3 / fac ; 1 milli Hz is the input frequency
dat2(*) = 0. & dat2 = 1.0 * sin(2.*!DPI*(f*tt2 + .5))


ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq,maxfreq,freq,amp,phase,$
 highres=highres


f = freq*fac ; microHz
a = amp/1e6 ; ppm
n2 = n_elements(f)

;openw,1,outfile
;for i=0L,n2-1 do $
; printf,1,f(i),a(i),format='(D15.8,D15.8)'
;close,1

print,' %%% Amplitude spectrum saved as file: ' + outfile

amp = replicate( {f:0., a:0}, n2)
amp.f = f
amp.a = a

save,filename=outfile,amp

END
