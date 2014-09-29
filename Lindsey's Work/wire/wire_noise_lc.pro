PRO wire_noise_lc, v, b, days, duty, range=range, titl=titl

; duty : duty cycle in percent, eg. 30
; days ; Tobs total, eq 14 days
; v, b ; v and b magnitude, eg. from SIMBAD

default9, gain, 15. ; gain: electrons pr. ADU
default9, range, 0  ; plot a range of observing times?
default9, titl, '' ; title 
default9, dops, 1B

wire_counts, b, v, counts, /out

n = days * (duty/100.) * 86400. * 2. ; number of data points

noise_ptp  = sqrt(1D /(counts * gain))
noise_ptp2 = noise_ptp / sqrt(30.)
noise_amp  = noise_ptp * sqrt(!PI / n)

print,' %%% Observing for ' + string(days,format='(I4)') + $
      ' days w/ duty cycle of ' + string(duty,format='(I4)') + $
      '% - data points = ' + string(n,format='(I8)')

print,' %%% In amplitude spectrum at "high" frequencies: ' + $
 string(noise_amp*1e6,format='(F8.2)') + ' ppm'
print,' %%% Noise pr. data point: ' + $
 string(noise_ptp*1e6,format='(F8.2)') + ' ppm'

print,' %%% Noise pr. data 31 points (time sampling 15s): ' + $
 string(noise_ptp2*1e6,format='(F8.2)')  + ' ppm'

if range then begin

 days2 = 2. + findgen(120)/3.
 n2 = days2 * (duty/100.) * 86400. * 2. ; number of data points
 noise_amp2  = noise_ptp * sqrt(!PI / n2)

; Frequency resolution: 3/(2 * Tobs)
; Loumos, G., Deeming, T. J., 1978, ApSS 56, 285
 resolution = (3./2) / days2
 resolution_mhz = (1e6/86400.) * resolution
 resolution_mhz2 = (1e6/86400.) * (3/2.) / (days2*2. + 160.)

 tt = titl + ' ' + $
  'V = ' + strcompress(string(v,format='(F5.1)'),/remove_all) + $
  '; B-V = ' + strcompress(string(b-v,format='(F5.2)'),/remove_all) 

 plotsym,0,/fill

 maxy1 = max(noise_amp2*1e6)*1.1
 miny1 = min(noise_amp2*1e6)*0.8

 !P.multi=[0,1,2]

if titl eq '' then titl2 = 'wire_target' else titl2 = titl
easyps, keywords, kk, dops=dops, /prepare, dim = [12,15.0,-1,1], $
 fil = titl2 + '.ps', dir = '/home/bruntt/wire/wire_eps/targets/'


 plot_io,days2,noise_amp2*1e6,$
  xtit='Observing time in days',ytit='Noise in ampl. spec. [ppm]',$
  tit=tt,symsi=.5,xr=[0,max(days2)+2.], $
  charsi=1.2, yr=[miny1,maxy1]
 ; yr=[.5,100], yr=[0,maxy]


 maxy = max(resolution_mhz)*1.1
 miny = min(resolution_mhz2)*0.8

 plot_io,days2, resolution_mhz, xr=[0,max(days2)+2.], $
  xtit='Observing time in days',ytit='Resolution in !4l!3Hz', $
 yr=[miny,maxy], charsi=1.2

 oplot,days2, resolution_mhz2,line=2

endif

easyps, keywords, kk, dops=dops, /close


END
