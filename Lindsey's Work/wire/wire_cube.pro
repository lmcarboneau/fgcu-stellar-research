PRO wire_cube, fil, cub, pa

; Take output from wire_density_compare.pro and put data in a
; "cube" = 3D array for easier plotting

; Example:
; wire_cube,'~/wire/wire_sim99/density/Procyon_sim_comp_1999.idl', cub,pa
; surface,cub(1,*,*),charsi=2.0

g = findfile(fil,Count=cnt)
if cnt ne 1 then begin
 print,' %%% File not found: ' + fil
 RETURN
endif

restore,fil



tgran = comp.tgran & s = sort(tgran) & tgran = tgran(s)
agran = comp.agran & s = sort(agran) & agran = agran(s)
amode = comp.amode & s = sort(amode) & amode = amode(s)
lifet = comp.lifet & s = sort(lifet) & lifet = lifet(s)

u_tgran = uniq(tgran) & tgran2 = tgran(u_tgran)
u_agran = uniq(agran) & agran2 = agran(u_agran)
u_amode = uniq(amode) & amode2 = amode(u_amode)
u_lifet = uniq(lifet) & lifet2 = lifet(u_lifet)

n_tgran = n_elements(tgran2)
n_agran = n_elements(agran2)
n_amode = n_elements(amode2)
n_lifet = n_elements(lifet2)

print,tgran2, agran2, amode2, lifet2
print,' %%% Number of T-Gran, A-Gran, A-Mode, T-Life: '
print,n_tgran, n_agran, n_amode, n_lifet



cub = fltarr(4,n_tgran, n_agran, n_amode  , n_lifet)
;; pa = fltarr(3,n_tgran, n_agran, n_amode   , n_lifet)

pa = fltarr(4,n_tgran, n_agran, n_amode , n_lifet)

for i=0,n_tgran -1 do begin
 for j=0,n_agran-1 do begin
  for k=0,n_amode-1 do begin
   for l=0,n_lifet-1 do begin

    w = where(comp.tgran eq tgran2(i) and $
              comp.agran eq agran2(j) and $
              comp.amode eq amode2(k) and $
              comp.lifet eq lifet2(l), c)
    if c eq 1 then begin
      cub(0,i,j,k,l) = comp(w).spreadr ; spread
      cub(1,i,j,k,l) = comp(w).chi2r   ; chi sqr
      cub(2,i,j,k,l) = comp(w).dif     ; difference
      cub(3,i,j,k,l) = comp(w).ratio   ; sum of ratios

      pa(0,i,j,k,l) = tgran2(i)
      pa(1,i,j,k,l) = agran2(j)
      pa(2,i,j,k,l) = amode2(k)
      pa(3,i,j,k,l) = lifet2(l)

;     cub(i,j,k,l) = comp(w).spreadr
;     pa(0,i,j,k,l) = tgran2(i)
;     pa(1,i,j,k,l) = agran2(j)
;     pa(2,i,j,k,l) = amode2(k)
;     pa(3,i,j,k,l) = lifet2(l)
    endif

   endfor
  endfor
 endfor
endfor


END
