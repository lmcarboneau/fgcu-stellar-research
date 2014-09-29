PRO wirep_update_info, info, wirep_struct, field, decor=decor, fac=fac

; Update info struct when importing a wirep analyis file

; Debugging:
print,info.fiton,info.fitsub,$
      info.dec.remscat,info.dec.xyfit,info.dec.appxyfit

default9, fac, -99

info.d        = wirep_struct.d
info.lc       = wirep_struct.lc
info.sp       = wirep_struct.sp
info.fil      = wirep_struct.fil
info.starname = wirep_struct.starname
info.nstar    = wirep_struct.nstar
info.t0       = wirep_struct.t0
info.t1       = wirep_struct.t1
info.t2       = wirep_struct.t2
info.per1     = wirep_struct.per1
*info.fit2d    = wirep_struct.fit2d  ; POINTERS !!
*info.fit2t    = wirep_struct.fit2t
*info.fit      = wirep_struct.fit
info.freq1    = wirep_struct.freq1
info.freq2    = wirep_struct.freq2
info.fac      = wirep_struct.fac
info.specmax  = wirep_struct.specmax
info.dec      = wirep_struct.dec
info.obj      = wirep_struct.obj
info.fiton    = wirep_struct.fiton
info.fitsub   = wirep_struct.fitsub
info.d(*).orb_freq   = wirep_struct.orb_freq

if n_elements(fac) ge 0. then begin
 info.fac = fac
 wirep_struct.fac = fac
endif

; print,' %%% Orbital freq. imported: ', wirep_struct.orb_freq

;; if info.freq1 eq info.freq2 

;Widget_Control, field.button_fit_onoff,   Set_Button=info.fiton
;Widget_Control, field.button_fit_subfit,  Set_Button=info.fitsub
;Widget_Control, field.button_scat3,       Set_Button=info.dec.appscatfit
;Widget_Control, field.button_dec_xy1,     Set_Button=info.dec.xyfit
;Widget_Control, field.button_dec_xy2,     Set_Button=info.dec.appxyfit

if decor then begin
 Widget_Control, field.button_fit_onoff,   Set_Button=wirep_struct.fiton
 Widget_Control, field.button_fit_subfit,  Set_Button=wirep_struct.fitsub

 Widget_Control, field.button_scat2,       Set_Button=wirep_struct.dec.appscatfit
 Widget_Control, field.button_scat3,       Set_Button=wirep_struct.dec.remscat
 Widget_Control, field.button_dec_xy2,     Set_Button=wirep_struct.dec.appxyfit

 Widget_Control, field.button_timefit,     Set_Button=wirep_struct.dec.apptimefit
 Widget_Control, field.button_backfit,     Set_Button=wirep_struct.dec.appbackfit
 Widget_Control, field.button_fwhmfit,     Set_Button=wirep_struct.dec.appfwhmfit

; Fields: Orbital frequency + Zero Phase
 Widget_Control, field.field_orb_freq,     Set_Value=wirep_struct.orb_freq
 Widget_Control, field.field_zero_phase,   Set_Value=wirep_struct.d(0).add_phase

; help,field,/str
; help,wirep_struct.dec,/str
; Search for dec = { in wirep.pro !!

endif

; print,' %%% Orb freq set to: ',wirep_struct.orb_freq

;    field.button_scat2   = button_scat2    ; app scat fit == dec.appscatfit
;    field.button_scat3   = button_scat3    ; dec.remscat
;    field.button_dec_xy2 = button_dec_xy2  ; app xy fit = dec.appxyfit

; Debugging:
print,wirep_struct.fiton,wirep_struct.fitsub,$
      wirep_struct.dec.remscat,wirep_struct.dec.xyfit,wirep_struct.dec.appxyfit

; dec = {scatfit:0B,appscatfit:0B,$   ; (A) fit to the phase LC (eg. after subtr. of modes)
;        remscat:0B,$                 ; (B) borders marked by user
;        xyfit:0B,appxyfit:0B, $      ; (C) fit to x,y positions (decorrelation)
;          sfit:  fltarr(nstar,np), $   ; [A] the fitted scat. curve
;          rscat: fltarr(nstar,2), $    ; [B] borders of rem scat !!
;          xfit:  fltarr(nstar,np)}     ; [C] the x,y fit !

;  ; Struct for storing button IDs
;    field = {button_fit_onoff:0L,button_fit_weights:0L, $
;             button_scat2:0L,button_dec_xy1:0L,button_dec_xy2:0L}

;    'Plot Fit'    : info.fiton  = (info.fiton  + 1) mod 2
;    'Sub Fit': info.fitsub = (info.fitsub + 1) mod 2

;       remscat:0B,$                 ; (B) borders marked by user
;       xyfit:0B,appxyfit:0B, $      ; (C) fit to x,y positions (decorrelation)



END
