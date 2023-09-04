module Pitch.Literals (
    c, cs, df, d, ds, ef, e, f, fs, g, gs, af, a, as, bf, b, cqs, dqf, dqs, eqf, eqs, fqs, gqf, gqs, aqf, aqs, bqf, bqs,
    _c, _cs, _df, _d, _ds, _ef, _e, _f, _fs, _g, _gs, _af, _a, _as, _bf, _b, _cqs, _dqf, _dqs, _eqf, _eqs, _fqs, _gqf, _gqs, _aqf, _aqs, _bqf, _bqs,
    c', cs', df', d', ds', ef', e', f', fs', g', gs', af', a', as', bf', b', cqs', dqf', dqs', eqf', eqs', fqs', gqf', gqs', aqf', aqs', bqf', bqs',
    c_, cs_, df_, d_, ds_, ef_, e_, f_, fs_, g_, gs_, af_, a_, as_, bf_, b_, cqs_, dqf_, dqs_, eqf_, eqs_, fqs_, gqf_, gqs_, aqf_, aqs_, bqf_, bqs_,
    c'', cs'', df'', d'', ds'', ef'', e'', f'', fs'', g'', gs'', af'', a'', as'', bf'', b'', cqs'', dqf'', dqs'', eqf'', eqs'', fqs'', gqf'', gqs'', aqf'', aqs'', bqf'', bqs'',
    c__, cs__, df__, d__, ds__, ef__, e__, f__, fs__, g__, gs__, af__, a__, as__, bf__, b__, cqs__, dqf__, dqs__, eqf__, eqs__, fqs__, gqf__, gqs__, aqf__, aqs__, bqf__, bqs__,
    c''', cs''', df''', d''', ds''', ef''', e''', f''', fs''', g''', gs''', af''', a''', as''', bf''', b''', cqs''', dqf''', dqs''', eqf''', eqs''', fqs''', gqf''', gqs''', aqf''', aqs''', bqf''', bqs''',
    c___, cs___, df___, d___, ds___, ef___, e___, f___, fs___, g___, gs___, af___, a___, as___, bf___, b___, cqs___, dqf___, dqs___, eqf___, eqs___, fqs___, gqf___, gqs___, aqf___, aqs___, bqf___, bqs___,
    c'''', cs'''', df'''', d'''', ds'''', ef'''', e'''', f'''', fs'''', g'''', gs'''', af'''', a'''', as'''', bf'''', b'''', cqs'''', dqf'''', dqs'''', eqf'''', eqs'''', fqs'''', gqf'''', gqs'''', aqf'''', aqs'''', bqf'''', bqs'''',
    _gf, gf, gf' , gf'' 
) where

import Pitch.Accidental

import Pitch.Pitch


_c, _cs, _df, _d, _ds, _ef, _e, _f, _fs, _g, _gs, _af, _a, _as, _bf, _b :: PitchClass
_c = createPitchClass C natural
_cs = createPitchClass C sharp
_df = createPitchClass D flat
_d = createPitchClass D natural
_ds = createPitchClass D sharp
_ef = createPitchClass E flat
_e = createPitchClass E natural
_f = createPitchClass F natural
_fs = createPitchClass F sharp
_gf :: PitchClass
_gf = createPitchClass G flat
_g = createPitchClass G natural
_gs = createPitchClass G sharp
_af = createPitchClass A flat
_a = createPitchClass A natural
_as = createPitchClass A sharp
_bf = createPitchClass B flat
_b = createPitchClass B natural

_cqs, _dqf, _dqs, _eqf, _eqs, _fqs, _gqf, _gqs, _aqf, _aqs, _bqf, _bqs :: PitchClass
_cqs = createPitchClass C quarterSharp
_dqf = createPitchClass D quarterFlat
_dqs = createPitchClass D quarterSharp
_eqf = createPitchClass E quarterFlat
_eqs = createPitchClass E quarterSharp
_fqs = createPitchClass F quarterSharp
_gqf = createPitchClass G quarterFlat
_gqs = createPitchClass G quarterSharp
_aqf = createPitchClass A quarterFlat
_aqs = createPitchClass A quarterSharp
_bqf = createPitchClass B quarterFlat
_bqs = createPitchClass B quarterSharp

c, cs, df, d, ds, ef, e, f, fs, g, gs, gf, af, a, as, bf, b :: Pitch
c = createPitch _c 4
cs = createPitch _cs 4
df = createPitch _df 4
d = createPitch _d 4
ds = createPitch _ds 4
ef = createPitch _ef 4
e = createPitch _e 4
f = createPitch _f 4
fs = createPitch _fs 4
gf  = createPitch _gf 4
g = createPitch _g 4
gs = createPitch _gs 4
af = createPitch _af 4
a = createPitch _a 4
as = createPitch _as 4
bf = createPitch _bf 4
b = createPitch _b 4

c', cs', df', d', ds', ef', e', f', fs', g', gf', gs', af', a', as', bf', b' :: Pitch
c' = createPitch _c 5
cs' = createPitch _cs 5
df' = createPitch _df 5
d' = createPitch _d 5
ds' = createPitch _ds 5
ef' = createPitch _ef 5
e' = createPitch _e 5
f' = createPitch _f 5
fs' = createPitch _fs 5
g' = createPitch _g 5
gf' = createPitch _gf 5
gs' = createPitch _gs 5
af' = createPitch _af 5
a' = createPitch _a 5
as' = createPitch _as 5
bf' = createPitch _bf 5
b' = createPitch _b 5

c'', cs'', df'', d'', ds'', ef'', e'', f'', fs'', g'', gs'', af'', a'', as'', bf'', b'' :: Pitch
c'' = createPitch _c 6
cs'' = createPitch _cs 6
df'' = createPitch _df 6
d'' = createPitch _d 6
ds'' = createPitch _ds 6
ef'' = createPitch _ef 6
e'' = createPitch _e 6
f'' = createPitch _f 6
fs'' = createPitch _fs 6
gf'' :: Pitch
gf''  = createPitch _gf 6
g'' = createPitch _g 6
gs'' = createPitch _gs 6
af'' = createPitch _af 6
a'' = createPitch _a 6
as'' = createPitch _as 6
bf'' = createPitch _bf 6
b'' = createPitch _b 6

c''', cs''', df''', d''', ds''', ef''', e''', f''', fs''', g''', gs''', af''', a''', as''', bf''', b''' :: Pitch
gf''' :: Pitch
c''' = createPitch _c 7
cs''' = createPitch _cs 7
df''' = createPitch _df 7
d''' = createPitch _d 7
ds''' = createPitch _ds 7
ef''' = createPitch _ef 7
e''' = createPitch _e 7
f''' = createPitch _f 7
fs''' = createPitch _fs 7
gf'''   = createPitch _gf 7
g''' = createPitch _g 7
gs''' = createPitch _gs 7
af''' = createPitch _af 7
a''' = createPitch _a 7
as''' = createPitch _as 7
bf''' = createPitch _bf 7
b''' = createPitch _b 7

c'''', cs'''', df'''', d'''', ds'''', ef'''', e'''', f'''', fs'''', g'''', gs'''', af'''', a'''', as'''', bf'''', b'''' :: Pitch
c'''' = createPitch _c 8
cs'''' = createPitch _cs 8
df'''' = createPitch _df 8
d'''' = createPitch _d 8
ds'''' = createPitch _ds 8
ef'''' = createPitch _ef 8
e'''' = createPitch _e 8
f'''' = createPitch _f 8
fs'''' = createPitch _fs 8
g'''' = createPitch _g 8
gs'''' = createPitch _gs 8
af'''' = createPitch _af 8
a'''' = createPitch _a 8
as'''' = createPitch _as 8
bf'''' = createPitch _bf 8
b'''' = createPitch _b 8

c_, cs_, df_, d_, ds_, ef_, e_, f_, fs_, g_, gs_, af_, a_, as_, bf_, b_ :: Pitch
c_ = createPitch _c 3
cs_ = createPitch _cs 3
df_ = createPitch _df 3
d_ = createPitch _d 3
ds_ = createPitch _ds 3
ef_ = createPitch _ef 3
e_ = createPitch _e 3
f_ = createPitch _f 3
fs_ = createPitch _fs 3
g_ = createPitch _g 3
gs_ = createPitch _gs 3
af_ = createPitch _af 3
a_ = createPitch _a 3
as_ = createPitch _as 3
bf_ = createPitch _bf 3
b_ = createPitch _b 3

c__, cs__, df__, d__, ds__, ef__, e__, f__, fs__, g__, gs__, af__, a__, as__, bf__, b__ :: Pitch
c__ = createPitch _c 2
cs__ = createPitch _cs 2
df__ = createPitch _df 2
d__ = createPitch _d 2
ds__ = createPitch _ds 2
ef__ = createPitch _ef 2
e__ = createPitch _e 2
f__ = createPitch _f 2
fs__ = createPitch _fs 2
g__ = createPitch _g 2
gs__ = createPitch _gs 2
af__ = createPitch _af 2
a__ = createPitch _a 2
as__ = createPitch _as 2
bf__ = createPitch _bf 2
b__ = createPitch _b 2

c___, cs___, df___, d___, ds___, ef___, e___, f___, fs___, g___, gs___, af___, a___, as___, bf___, b___ :: Pitch
c___ = createPitch _c 1
cs___ = createPitch _cs 1
df___ = createPitch _df 1
d___ = createPitch _d 1
ds___ = createPitch _ds 1
ef___ = createPitch _ef 1
e___ = createPitch _e 1
f___ = createPitch _f 1
fs___ = createPitch _fs 1
g___ = createPitch _g 1
gs___ = createPitch _gs 1
af___ = createPitch _af 1
a___ = createPitch _a 1
as___ = createPitch _as 1
bf___ = createPitch _bf 1
b___ = createPitch _b 1

cqs, dqf, dqs, eqf, eqs, fqs, gqf, gqs, aqf, aqs, bqf, bqs :: Pitch
cqs = createPitch _cqs 4
dqf = createPitch _dqf 4
dqs = createPitch _dqs 4
eqf = createPitch _eqf 4
eqs = createPitch _eqs 4
fqs = createPitch _fqs 4
gqf = createPitch _gqf 4
gqs = createPitch _gqs 4
aqf = createPitch _aqf 4
aqs = createPitch _aqs 4
bqf = createPitch _bqf 4
bqs = createPitch _bqs 4

cqs', dqf', dqs', eqf', eqs', fqs', gqf', gqs', aqf', aqs', bqf', bqs' :: Pitch
cqs' = createPitch _cqs 5
dqf' = createPitch _dqf 5
dqs' = createPitch _dqs 5
eqf' = createPitch _eqf 5
eqs' = createPitch _eqs 5
fqs' = createPitch _fqs 5
gqf' = createPitch _gqf 5
gqs' = createPitch _gqs 5
aqf' = createPitch _aqf 5
aqs' = createPitch _aqs 5
bqf' = createPitch _bqf 5
bqs' = createPitch _bqs 5

cqs'', dqf'', dqs'', eqf'', eqs'', fqs'', gqf'', gqs'', aqf'', aqs'', bqf'', bqs'' :: Pitch
cqs'' = createPitch _cqs 6
dqf'' = createPitch _dqf 6
dqs'' = createPitch _dqs 6
eqf'' = createPitch _eqf 6
eqs'' = createPitch _eqs 6
fqs'' = createPitch _fqs 6
gqf'' = createPitch _gqf 6
gqs'' = createPitch _gqs 6
aqf'' = createPitch _aqf 6
aqs'' = createPitch _aqs 6
bqf'' = createPitch _bqf 6
bqs'' = createPitch _bqs 6

cqs''', dqf''', dqs''', eqf''', eqs''', fqs''', gqf''', gqs''', aqf''', aqs''', bqf''', bqs''' :: Pitch
cqs''' = createPitch _cqs 8
dqf''' = createPitch _dqf 8
dqs''' = createPitch _dqs 8
eqf''' = createPitch _eqf 8
eqs''' = createPitch _eqs 8
fqs''' = createPitch _fqs 8
gqf''' = createPitch _gqf 8
gqs''' = createPitch _gqs 8
aqf''' = createPitch _aqf 8
aqs''' = createPitch _aqs 8
bqf''' = createPitch _bqf 8
bqs''' = createPitch _bqs 8

cqs'''', dqf'''', dqs'''', eqf'''', eqs'''', fqs'''', gqf'''', gqs'''', aqf'''', aqs'''', bqf'''', bqs'''' :: Pitch
cqs'''' = createPitch _cqs 9
dqf'''' = createPitch _dqf 9
dqs'''' = createPitch _dqs 9
eqf'''' = createPitch _eqf 9
eqs'''' = createPitch _eqs 9
fqs'''' = createPitch _fqs 9
gqf'''' = createPitch _gqf 9
gqs'''' = createPitch _gqs 9
aqf'''' = createPitch _aqf 9
aqs'''' = createPitch _aqs 9
bqf'''' = createPitch _bqf 9
bqs'''' = createPitch _bqs 9

cqs_, dqf_, dqs_, eqf_, eqs_, fqs_, gqf_, gqs_, aqf_, aqs_, bqf_, bqs_ :: Pitch
cqs_ = createPitch _cqs 3
dqf_ = createPitch _dqf 3
dqs_ = createPitch _dqs 3
eqf_ = createPitch _eqf 3
eqs_ = createPitch _eqs 3
fqs_ = createPitch _fqs 3
gqf_ = createPitch _gqf 3
gqs_ = createPitch _gqs 3
aqf_ = createPitch _aqf 3
aqs_ = createPitch _aqs 3
bqf_ = createPitch _bqf 3
bqs_ = createPitch _bqs 3

cqs__, dqf__, dqs__, eqf__, eqs__, fqs__, gqf__, gqs__, aqf__, aqs__, bqf__, bqs__ :: Pitch
cqs__ = createPitch _cqs 2
dqf__ = createPitch _dqf 2
dqs__ = createPitch _dqs 2
eqf__ = createPitch _eqf 2
eqs__ = createPitch _eqs 2
fqs__ = createPitch _fqs 2
gqf__ = createPitch _gqf 2
gqs__ = createPitch _gqs 2
aqf__ = createPitch _aqf 2
aqs__ = createPitch _aqs 2
bqf__ = createPitch _bqf 2
bqs__ = createPitch _bqs 2

cqs___, dqf___, dqs___, eqf___, eqs___, fqs___, gqf___, gqs___, aqf___, aqs___, bqf___, bqs___ :: Pitch
cqs___ = createPitch _cqs 1
dqf___ = createPitch _dqf 1
dqs___ = createPitch _dqs 1
eqf___ = createPitch _eqf 1
eqs___ = createPitch _eqs 1
fqs___ = createPitch _fqs 1
gqf___ = createPitch _gqf 1
gqs___ = createPitch _gqs 1
aqf___ = createPitch _aqf 1
aqs___ = createPitch _aqs 1
bqf___ = createPitch _bqf 1
bqs___ = createPitch _bqs 1