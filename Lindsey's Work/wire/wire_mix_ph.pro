PRO wire_mix_ph, input_p98, output_p98, time_in, time_out

; Example:
; wire_mix_ph, $
; '~/wire/wire_periods/AlphaUMi_Feb2004_s0_HD8890_F7:Ib-IIva.per',$
; '~/wire/wire_periods/AlphaUMi_July2004_s0_HD8890_F7:Ib-IIva.per',$
; 53038, 53211

; Purpose: Correct the phases of one list of freq+amp+phases to
; another zero point in time

g = findfile(input_p98,Count=cnt)
if cnt ne 1 then begin
 print,' %%% File not found: ' + input_p98
 RETURN
endif

readcol, input_p98, nam1, f1, a1, p1, format='A,D,D,D'

; Get the needed precission
t1 = double(time_in)
t2 = double(time_out)
delta_t = t2 - t1

nf = n_elements(f1)

; Frequency and amplitude are the same
f2 = f1
a2 = a1
nam2 = nam1

; Phases will be shifted depending on delta_t
p2init = f1 * delta_t + p1
sub = floor(p2init)
p2 = p2init - sub

get_lun,u
openw,u,output_p98
 for i=0,nf-1 do $
  printf,u, nam2(i), f2(i), a2(i), p2(i), $
   format='(A5, D22.12, D20.9, D20.9)'

close,u
free_lun,u

print,' %%% Wrote ' + strcompress(nf,/remove_all) + $
 ' frequencies to file: ' + output_p98


END
