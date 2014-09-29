PRO wire_spline, hjd, dat, period, phase, dat2, dat3

ndat = n_elements(dat)
dat2 = fltarr(ndat)

phase = (hjd mod period) / period

; stop

;a = sort(phase)
;phx  = phase(a)
;datx = dat(a)

ph_lim = 0.005
cnt = 0L

phase_0 = -1.0
phase_start = -999.

while phase_start lt 1.0 do begin
 phase_start = phase_0 + ph_lim * cnt
 phase_slut  = phase_0 + ph_lim * (cnt + 1 )

 w = where(phase ge phase_start and phase lt phase_slut,c)
 if c ge 5 then begin
  resistant_mean,dat(w),3,me,sd,nr
  dat2(w) = me
 endif

 cnt = cnt + 1.0
 ; debug: print,cnt,phase_start, phase_slut,format='(I6,2F7.3)'
 
endwhile

; plot,phase,dat,psym=3
; oplot,phase,dat2,psym=1,col=col.red

dat3 = dat - dat2 ; subtract spline


END

