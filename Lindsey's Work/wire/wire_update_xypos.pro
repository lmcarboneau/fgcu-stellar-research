goto,korr2


restore,'~/wire/wire_essential/xy_positions3_8000.idl'
wireinfo8000 = wireinfo
xyinfo8000 = xyinfo
xyinfo = 0B & wireinfo = 0B

restore,'~/wire/wire_essential/xy_positions3.idl'

wireinfousafa = wireinfo
xyinfousafa = xyinfo
xyinfo = 0B & wireinfo = 0B

nmax = 500
nstar = 40

cnt = 0L

wireinfo = replicate( {t0:-100D,dir:'NA',nstars:0, object:''}, nmax)
xyinfo   = replicate( {xy:fltarr(2,nstar), exy:fltarr(2,nstar), $
                       xy2:fltarr(2,nstar), flux: fltarr(2,nstar), $
                       fwhm:fltarr(2,nstar), angle: fltarr(10), $
                       distcen: fltarr(10), xc: fltarr(2), $
                       extra: fltarr(6,nstar), $
                       object: strarr(nstar), $
                       aperture: intarr(nstar)}, nmax)


w = where(wireinfo8000.object ne '',c)
for i=0,c-1 do begin

 obj8000 = wireinfo8000(w(i)).object

 g = strsplit(obj8000,'_',/extract)
 g2 = g(0)  

 out = ''
 store8000 = 0B

 w2 = where(strmatch(wireinfousafa.object,'*' + g2 + '*') eq 1,c2)
 if c2 eq 1 then out = ' >>> USAFA >>> ' + wireinfousafa(w2).object
 if c2 ge 2 then stop

; ============================================
; Insert the 8000 = Aarhus information
; ============================================

  wireinfo(cnt) = wireinfo8000(w(i))
  xyinfo(cnt)   = xyinfo8000(w(i))
  store8000 = 1B

 if obj8000 eq 'AlphaAra' then begin
  wireinfo(cnt).object = 'AlphaAra_Mar2004'
  print,' %%% AlphaAra ---> AlphaAra_Mar2004'
 endif

 if obj8000 eq 'Polaris' then begin
  wireinfo(cnt).object = 'AlphaUMi_Feb2004'
  print,' %%% Polaris ---> AlphaUMi_Feb2004'
 endif

  print,' > Fin1Arr > ' + wireinfo(cnt).object


  ; gunder = where(strmatch(wireinfo(cnt).object,'*_*') eq 1,cunder)

  cnt = cnt + 1

; ============================================

; ============================================
 if c2 ge 1 then object = wireinfousafa(w2).object
; ============================================

 if c2 eq 1 then begin
  if object ne 'Polaris' and $
     object ne 'AlphaUMi' and $
     object ne 'AlphaAra' and $
     object ne 'KsiHya' and $
     object ne 'AlphaInd' and $
     object ne 'EtaCen' and $
     object ne 'BetaCep' and $
     object ne 'EpsilonCep' then begin

    print,' %%%  I stored : ' + wireinfo(cnt-1).object, ' t0 = ',wireinfo(cnt-1).t0
    print,' %%% Now store : ',wireinfousafa(w2).object, ' t0 = ',wireinfousafa(w2).t0
    s = get_kbrd(1)
  
    if s eq 'y' then begin
     print, ' >>> Storing USAFA info ... '
     wireinfo(cnt) = wireinfousafa(w2)
     xyinfo(cnt)   = xyinfousafa(w2)
     print,' > FinArr > ' + wireinfo(cnt).object
     cnt = cnt + 1
    endif

  endif ; target does not have *better* information from Aarhus?
 endif ; one object found?

 outfin = ' > 8000 > ' + obj8000 + out

 print,outfin


endfor

korr:

wu = where(strmatch(wireinfousafa.object,'EpsilonEri*') eq 1,cu)
if cu eq 1 then begin
     wireinfo(cnt) = wireinfousafa(wu)
     xyinfo(cnt)   = xyinfousafa(wu)
     print,' > Fin2Arr > ' + wireinfo(cnt).object
     cnt = cnt + 1
endif

wu = where(strmatch(wireinfousafa.object,'AlphaLup*') eq 1,cu)
if cu eq 1 then begin
     wireinfo(cnt) = wireinfousafa(wu)
     xyinfo(cnt)   = xyinfousafa(wu)
     print,' > Fin2Arr > ' + wireinfo(cnt).object
     cnt = cnt + 1
endif

wu = where(strmatch(wireinfousafa.object,'KappaOph*') eq 1,cu)
if cu eq 1 then begin
     wireinfo(cnt) = wireinfousafa(wu)
     xyinfo(cnt)   = xyinfousafa(wu)
     print,' > Fin2Arr > ' + wireinfo(cnt).object
     cnt = cnt + 1
endif

wu = where(strmatch(wireinfousafa.object,'AlphaOri*') eq 1,cu)
if cu eq 1 then begin
     wireinfo(cnt) = wireinfousafa(wu)
     xyinfo(cnt)   = xyinfousafa(wu)
     wireinfo(cnt).object = 'AlphaOri_Sep2004'
     print,' > Fin2Arr > ' + wireinfo(cnt).object
     cnt = cnt + 1
endif

; There was a mistake for this object:
wu = where(strmatch(wireinfousafa.object,'KappaSco*') eq 1,cu)
if cu eq 1 then begin
     wireinfo(cnt) = wireinfousafa(wu)
     xyinfo(cnt)   = xyinfousafa(wu)
     wireinfo(cnt).object = 'AlphaAra_Sep2004'
     print,' > Fin2Arr > ' + wireinfo(cnt).object
     cnt = cnt + 1
endif

; ========================================================
; Add approximate date to object name !!
; ========================================================

korr2:

wn = where(strmatch(wireinfo.object,'AlphaInd') eq 1,cn)
if cn eq 1 then begin
     wireinfo(wn).object = 'AlphaInd_May2004'
     print,' > Change name > ' + wireinfo(wn).object
endif

wn = where(strmatch(wireinfo.object,'EtaCen') eq 1,cn)
if cn eq 1 then begin
     wireinfo(wn).object = 'EtaCen_July2004'
     print,' > Change name > ' + wireinfo(wn).object
endif

wn = where(strmatch(wireinfo.object,'BetaCep') eq 1,cn)
if cn eq 1 then begin
     wireinfo(wn).object = 'BetaCep_Jun2004'
     print,' > Change name > ' + wireinfo(wn).object
endif

wn = where(strmatch(wireinfo.object,'EpsilonCep') eq 1,cn)
if cn eq 1 then begin
     wireinfo(wn).object = 'EpsilonCep_Jun2004'
     print,' > Change name > ' + wireinfo(wn).object
endif


wn = where(strmatch(wireinfo.object,'BetaVol') eq 1,cn)
if cn eq 1 then begin
     wireinfo(wn).object = 'BetaVol_Apr2004'
     print,' > Change name > ' + wireinfo(wn).object
endif



outfile = '~/wire/wire_essential/xy_positions4.idl'
save,filename=outfile, wireinfo, xyinfo

wp = where(wireinfo.object ne '',c_p)
for i=0,c_p-1 do $
 print,wireinfo(wp(i)).object

END
