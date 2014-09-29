PRO wire_get_every, x, every, dops, ev

; Speed up the plotting in wirep_plot

;print,' EVERY : ' + strcompress(every)

n = float(n_elements(x))
if (dops ge 1) or (every eq 1) then begin
 ev = findgen(n) ; plot all points
endif else begin
 ev = (findgen(floor(n/every))) * float(every)
; print,' %%% Plotting only every ' + strcompress(every) + ' data point!'
endelse

END
