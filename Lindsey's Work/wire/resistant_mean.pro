PRO RESISTANT_MEAN,Y,CUT,MEAN,SIGMA,NUM_REJ 
;+
; NAME:
;	RESISTANT_MEAN  
;
; PURPOSE:
;	An outlier-resistant determination of the mean and its standard 
;	deviation.  It trims away outliers using the median and the median 
;	absolute deviation.
;
; CALLING SEQUENCE:
;	RESISTANT_MEAN,VECTOR,SIGMA_CUT, MEAN,SIGMA,NUM_REJECTED
;
; INPUT ARGUMENT:
;	VECTOR    = Vector to average
;	SIGMA_CUT = Data more than this number of standard deviations from the
;		median is ignored. Suggested values: 2.0 and up.
;
; OUTPUT ARGUMENT:
;	MEAN  = the mean
;	SIGMA = the standard deviation of the mean
;	NUM_REJECTED = the number of points trimmed
;
; SUBROUTINE CALLS:
;	MED, which calculates a median
;
; REVISION HISTORY:
;	Written, H. Freudenreich, STX, 1989; Second iteration added 5/91.
;-

ON_ERROR,2

NPTS    = N_ELEMENTS(Y)
YMED    = MED(Y)
ABSDEV  = ABS(Y-YMED)
MEDABSDEV = MED( ABSDEV )/.6745
IF MEDABSDEV LT 1.0E-24 THEN MEDABSDEV = AVG(ABSDEV)/.8

CUTOFF    = CUT*MEDABSDEV

GOODPTS = Y( WHERE( ABSDEV LE CUTOFF ) )
MEAN    = AVG( GOODPTS )
NUM_GOOD = N_ELEMENTS( GOODPTS )
SIGMA   = SQRT( TOTAL((GOODPTS-MEAN)^2)/NUM_GOOD )
NUM_REJ = NPTS - NUM_GOOD

; Compenate SIGMA for truncation (formula by HF):
SC=CUT
IF SC LT 1.75 THEN SC=1.75
IF SIGMA LE 3.4 THEN SIGMA=SIGMA/(.18553+.505246*SC-.0784189*SC*SC)

CUTOFF = CUT*SIGMA 

GOODPTS = Y( WHERE( ABSDEV LE CUTOFF ) )
MEAN    = AVG( GOODPTS )
NUM_GOOD = N_ELEMENTS( GOODPTS )
SIGMA   = SQRT( TOTAL((GOODPTS-MEAN)^2)/NUM_GOOD )
NUM_REJ = NPTS - NUM_GOOD

SC=CUT
IF SC LT 1.75 THEN SC=1.75
IF SIGMA LE 3.4 THEN SIGMA=SIGMA/(.18553+.505246*SC-.0784189*SC*SC)

; Now the standard deviation of the mean:
SIGMA = SIGMA/SQRT(NPTS-1.)

RETURN
END
