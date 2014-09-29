PRO wire_export_p98, base

; Program will export significant frequencies to P98 format

; Example: 
;  wire_export_p98, '/ai40/bruntt/wire/wire_lc/wire_lc_ZetaOph_star_0'

impfile = base + '_imp_clean.idl' ; filename of input stuctures
ampfile = base + '_clean.idl'     ; filename of input structure

restore,impfile & restore,ampfile ; import frequency arrays

wire_pername, impfile, pername    ; construct the new PERIOD98 name  
wire_guess_unit, fc, unit, conv   ; get the right unit for the frequency

wire_clean_exportp98,fc2,n_elements(fc2),pername,unit=unit ; export the freqs.


END
