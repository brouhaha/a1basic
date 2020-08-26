; Disassembly of Apple 1 BASIC

; Apple 1 BASIC was written by Steve Wozniak

; This disassembly is copyright 2003, 2019 Eric Smith <spacewar@gmail.com>
; Includes some updates by Jeff Tranter <tranter@pobox.com>

; Cross-assembles on modern systems using the Macro Assembler AS
; by Alfred Arnold:
;    http://john.ccac.rwth-aachen.de:8000/as/

	cpu	6502

fcs	macro	arg
	irpc	char,arg
	fcb	'char'+$80
	endm
	endm


errstr	macro	arg
	if	strlen(arg) > 1
	fcs	substr(arg,0,strlen(arg)-1)
	endif
	fcb	charfromstr(arg,strlen(arg)-1)
	endm


synstr	macro	arg
i	set	strlen(arg)
        while	i > 0
i	set	i-1
        fcb	charfromstr(arg, i)+$60
        endm
	endm

synstr1	macro	arg
i	set	strlen(arg)
i	set	i-1
        fcb	charfromstr(arg, i)+$a0
        while	i > 0
i	set	i-1
        fcb	charfromstr(arg, i)+$60
        endm
	endm


verb		macro	num,precv,target
		org	verb_prec_tbl+num
		fcb	precv
		org	verb_addr_l+num
		fcb	target & $ff
		org	verb_addr_h+num
		fcb	target >> 8
		endm


ch_cr		equ	$0d


reset		equ	$00
Z1d		equ	$1d
ch		equ	$24		; cursor horizontal location
cv		equ	$25		; cusror vertical location
lomem		equ	$4a		; lower limit of memory used by BASIC (2 bytes)
himem		equ	$4c		; upper limit of memory used by BASIC (2 bytes)
rnd		equ	$4e		; random number (2 bytes)

; The noun stack and syntax stack appear to overlap, which is OK since
; they apparently are not used simultaneously.

; The noun stack size appears to be 32 entries, based on LDX #$20
; instruction at e67f.  However, there seems to be enough room for
; another 8 entries.  The noun stack builds down from noun_stk_<part>+$1f
; to noun_stk_<part>+$00, indexed by the X register.

; Noun stack usage appears to be:
;   integer:
;       (noun_stk_h_int,noun_stk_l) = value
;       noun_stk_h_str = 0
;   string:
;       (noun_stk_h_str,noun_stk_l) = pointer to string
;       noun_stk_h_int = any
; Since noun_stk_h_str determines whether stack entry is integer or string,
; strings can't start in zero page.

noun_stk_l	equ	$50
syn_stk_h	equ	$58		; through $77
noun_stk_h_str	equ	$78
syn_stk_l	equ	$80		; through $9f
noun_stk_h_int	equ	$a0
txtndxstk	equ	$a8		; through $c7
text_index	equ	$c8		; index into text being tokenized (in buffer at $0200)
leadbl		equ	$c9		; leading blanks
pp		equ	$ca		; pointer to end fo program (2 bytes)
pv		equ	$cc		; pointer to end of variable storage (2 bytes)
acc		equ	$ce		; (2 bytes)
srch		equ	$d0
tokndxstk	equ	$d1
srch2		equ	$d2
if_flag		equ	$d4
cr_flag		equ	$d5
current_verb	equ	$d6
precedence	equ	$d7
x_save		equ	$d8
run_flag	equ	$d9
aux		equ	$da
pline		equ	$dc		; pointer to current program line (2 bytes)
pverb		equ	$e0		; pointer to current verb (2 bytes)
p1		equ	$e2
p2		equ	$e4
p3		equ	$e6
token_index	equ	$f1		; pointer used to write tokens into buffer (2 bytes)
pcon		equ	$f2		; temp used in decimal output (2 bytes)
auto_inc	equ	$f4
auto_ln		equ	$f6
auto_flag	equ	$f8
char		equ	$f9
leadzr		equ	$fa
for_nest_count	equ	$fb		; count of active (nested) FOR loops
gosub_nest_count	equ	$fc	; count of active (nested) subroutine calls (GOSUB)
synstkdx	equ	$fd
synpag		equ	$fe


; GOSUB stack, max eight entries
; note that the Apple II version has sixteen entries
gstk_pverbl	equ	$0100
gstk_pverbh	equ	$0108
gstk_plinel	equ	$0110
gstk_plineh	equ	$0118

; FOR stack, max eight entries
; note that the Apple II version has sixteen entries
fstk_varl	equ	$0120
fstk_varh	equ	$0128
fstk_stepl	equ	$0130
fstk_steph	equ	$0138
fstk_plinel	equ	$0140
fstk_plineh	equ	$0148
fstk_pverbl	equ	$0150
fstk_pverbh	equ	$0158
fstk_tol	equ	$0160
fstk_toh	equ	$0168

buffer		equ	$0200

; hardware locations:
kbd		equ	$d010
kbdcr		equ	$d011
dsp		equ	$d0f2		; the canonical address is $d012, but due to incomplete decode, this works
	
		org	$e000

Pe000:		jmp	cold		; BASIC cold start entry point

; Get character from keybaord, return in A.
rdkey:		lda	kbdcr		; read control register
		bpl	rdkey		; loop if no key pressed
		lda	kbd		; read data
		rts

Se00c:		txa
		and	#$20
		beq	Le034

Se011:		lda	#' '+$80
		sta	p2
		jmp	cout

Se018:		lda	#' '

Se01a:		cmp	ch
		bcs	nextbyte
		lda	#ch_cr+$80
		ldy	#$07
Le022:		jsr	cout
		lda	#' '+$80
		dey
		bne	Le022

nextbyte:	ldy	#$00
		lda	(p1),y
		inc	p1
		bne	Le034
		inc	p1+1
Le034:		rts

list_comman:	jsr	get16bit
		jsr	find_line2
Le03b:		lda	p1
		cmp	p3
		lda	p1+1
		sbc	p3+1
		bcs	Le034
		jsr	list_line
		jmp	Le03b

list_all:	lda	pp
		sta	p1
		lda	pp+1
		sta	p1+1
		lda	himem
		sta	p3
		lda	himem+1
		sta	p3+1
		bne	Le03b

list_cmd:	jsr	get16bit
		jsr	find_line
		lda	p2
		sta	p1
		lda	p2+1
		sta	p1+1
		bcs	Le034

list_line:	stx	x_save
		lda	#$a0
		sta	leadzr
		jsr	nextbyte
		tya
list_int:	sta	p2
		jsr	nextbyte
		tax
		jsr	nextbyte
		jsr	prdec
Le083:		jsr	Se018
		sty	leadzr
		tax
		bpl	list_token
		asl	a
		bpl	list_int
		lda	p2
		bne	Le095
		jsr	Se011
Le095:		txa
Le096:		jsr	cout
Le099:		lda	#$25
		jsr	Se01a
		tax
		bmi	Le096
		sta	p2
list_token:	cmp	#$01
		bne	Le0ac
		ldx	x_save
		jmp	crout
Le0ac:		pha
		sty	acc
		ldx	#$ed
		stx	acc+1
		cmp	#$51
		bcc	Le0bb
		dec	acc+1
		sbc	#$50
Le0bb:		pha
		lda	(acc),y
Le0be:		tax
		dey
		lda	(acc),y
		bpl	Le0be
		cpx	#$c0
		bcs	Le0cc
		cpx	#$00
		bmi	Le0be
Le0cc:		tax
		pla
		sbc	#$01
		bne	Le0bb
		bit	p2
		bmi	Le0d9
		jsr	Seff8
Le0d9:		lda	(acc),y
		bpl	Le0ed
		tax
		and	#$3f
		sta	p2
		clc
		adc	#' '+$80
		jsr	cout
		dey
		cpx	#$c0
		bcc	Le0d9
Le0ed:		jsr	Se00c
		pla
		cmp	#$5d
		beq	Le099
		cmp	#$28
		bne	Le083
		beq	Le099

paren_substr:	jsr	Se118
		sta	noun_stk_l,x
		cmp	noun_stk_h_str,x
Le102:		bcc	Le115
string_err:	ldy	#$2b
go_errmess_1:	jmp	do_error

comma_substr:	jsr	getbyte
		cmp	noun_stk_l,x
		bcc	string_err
		jsr	Sefe4
		sta	noun_stk_h_str,x
Le115:		jmp	left_paren

Se118:		jsr	getbyte
		beq	string_err
		sec
		sbc	#$01
		rts

str_arr_dest:	jsr	Se118
		sta	noun_stk_l,x
		clc
		sbc	noun_stk_h_str,x
		jmp	Le102
Le12c:		ldy	#$14
		bne	go_errmess_1

dim_str:	jsr	Se118
		inx
Le134:		lda	noun_stk_l,x
		sta	aux
		adc	acc
		pha
		tay
		lda	noun_stk_h_str,x
		sta	aux+1
		adc	acc+1
		pha
		cpy	pp
		sbc	pp+1
		bcs	Le12c
		lda	aux
		adc	#$fe
		sta	aux
		lda	#$ff
		tay
		adc	aux+1
		sta	aux+1
Le156:		iny
		lda	(aux),y
		cmp	pv,y
		bne	Le16d
		tya
		beq	Le156
Le161:		pla
		sta	(aux),y
		sta	pv,y
		dey
		bpl	Le161
		inx
		rts

		nop	; unused

Le16d:		ldy	#$80
Le16f:		bne	go_errmess_1

input_str:	lda	#$00
		jsr	push_a_noun_stk
		ldy	#$02
		sty	noun_stk_h_str,x
		jsr	push_a_noun_stk
		lda	#$bf
		jsr	cout
		ldy	#$00
		jsr	read_line
		sty	noun_stk_h_str,x
		
		nop
		nop
		nop

string_lit:	lda	noun_stk_l+1,x
		sta	acc
		lda	noun_stk_h_str+1,x
		sta	acc+1
		inx
		inx
		jsr	Se1bc
Le199:		lda	rnd,x
		cmp	syn_stk_h+30,x
		bcs	Le1b4
		inc	rnd,x
		tay
		lda	(acc),y
		ldy	noun_stk_l,x
		cpy	p2
		bcc	Le1ae
		ldy	#$83
		bne	Le16f
Le1ae:		sta	(aux),y
		inc	noun_stk_l,x
		bcc	Le199
Le1b4:		ldy	noun_stk_l,x
		txa
		sta	(aux),y
		inx
		inx
		rts

Se1bc:		lda	noun_stk_l+1,x
		sta	aux
		sec
		sbc	#$02
		sta	p2
		lda	noun_stk_h_str+1,x
		sta	aux+1
		sbc	#$00
		sta	p2+1
		ldy	#$00
		lda	(p2),y
		clc
		sbc	aux
		sta	p2
		rts

string_eq:	lda	noun_stk_l+3,x
		sta	acc
		lda	noun_stk_h_str+3,x
		sta	acc+1
		lda	noun_stk_l+1,x
		sta	aux
		lda	noun_stk_h_str+1,x
		sta	aux+1
		inx
		inx
		inx
		ldy	#$00
		sty	noun_stk_h_str,x
		sty	noun_stk_h_int,x
		iny
		sty	noun_stk_l,x
Le1f3:		lda	himem+1,x
		cmp	syn_stk_h+29,x
		php
		pha
		lda	rnd+1,x
		cmp	syn_stk_h+31,x
		bcc	Le206
		pla
		plp
		bcs	Le205
Le203:		lsr	noun_stk_l,x
Le205:		rts

Le206:		tay
		lda	(acc),y
		sta	p2
		pla
		tay
		plp
		bcs	Le203
		lda	(aux),y
		cmp	p2
		bne	Le203
		inc	rnd+1,x
		inc	himem+1,x
		bcs	Le1f3

string_neq:	jsr	string_eq
		jmp	not_op

mult_op:	jsr	Se254
Le225:		asl	acc
		rol	acc+1
		bcc	Le238
		clc
		lda	p3
		adc	aux
		sta	p3
		lda	p3+1
		adc	aux+1
		sta	p3+1
Le238:		dey
		beq	Le244
		asl	p3
		rol	p3+1
		bpl	Le225
		jmp	Le77e
Le244:		lda	p3
		jsr	push_ya_noun_stk
		lda	p3+1
		sta	noun_stk_h_int,x
		asl	p2+1
		bcc	Le279
		jmp	negate

Se254:		lda	#$55
		sta	p2+1
		jsr	Se25b

Se25b:		lda	acc
		sta	aux
		lda	acc+1
		sta	aux+1
		jsr	get16bit
		sty	p3
		sty	p3+1
		lda	acc+1
		bpl	Le277
		dex
		asl	p2+1
		jsr	negate
		jsr	get16bit
Le277:		ldy	#$10
Le279:		rts

mod_op:		jsr	See6c
		beq	Le244	; hoepfully always taken
		fcb	$ff	; hopefully unused

Le280:		cmp	#$84
		bne	Le286
		lsr	auto_flag
Le286:		cmp	#$df
		beq	Le29b
		cmp	#$9b
		beq	Le294
		sta	buffer,y
		iny
		bpl	read_line
Le294:		ldy	#$8b
		jsr	Se3c4

Se299:		ldy	#$01
Le29b:		dey
		bmi	Le294

read_line:	jsr	rdkey

		nop
		nop

		jsr	cout
		cmp	#ch_cr+$80
		bne	Le280
		lda	#'_'+$80	; mark end of line with underscore
		sta	buffer,y
		rts

cold:		jsr	mem_init_4k
warm:		jsr	crout
Le2b6:		lsr	run_flag
		lda	#$be
		jsr	cout
		ldy	#$00
		sty	leadzr
		bit	auto_flag
		bpl	Le2d1
		ldx	auto_ln
		lda	auto_ln+1
		jsr	prdec
		lda	#' '+$80
		jsr	cout
Le2d1:		ldx	#$ff
		txs
		jsr	read_line
		sty	token_index
		txa
		sta	text_index
		ldx	#$20
		jsr	Se491
		lda	text_index
		adc	#$00
		sta	pverb
		lda	#$00
		tax
		adc	#$02
		sta	pverb+1
		lda	(pverb,x)
		and	#$f0
		cmp	#$b0
		beq	Le2f9
		jmp	Le883
Le2f9:		ldy	#$02
Le2fb:		lda	(pverb),y
		sta	pv+1,y
		dey
		bne	Le2fb
		jsr	Se38a
		lda	token_index
		sbc	text_index
		cmp	#$04
		beq	Le2b6
		sta	(pverb),y
		lda	pp
		sbc	(pverb),y
		sta	p2
		lda	pp+1
		sbc	#$00
		sta	p2+1
		lda	p2
		cmp	pv
		lda	p2+1
		sbc	pv+1
		bcc	Le36b
Le326:		lda	pp
		sbc	(pverb),y
		sta	p3
		lda	pp+1
		sbc	#$00
		sta	p3+1
		lda	(pp),y
		sta	(p3),y
		inc	pp
		bne	Le33c
		inc	pp+1
Le33c:		lda	p1
		cmp	pp
		lda	p1+1
		sbc	pp+1
		bcs	Le326
Le346:		lda	p2,x
		sta	pp,x
		dex
		bpl	Le346
		lda	(pverb),y
		tay
Le350:		dey
		lda	(pverb),y
		sta	(p3),y
		tya
		bne	Le350
		bit	auto_flag
		bpl	Le365
Le35c:		lda	auto_ln+1,x
		adc	auto_inc+1,x
		sta	auto_ln+1,x
		inx
		beq	Le35c
Le365:		bpl	Le3e5

		fcb	$00,$00,$00,$00		; BRK instructions, hopefully never executed

Le36b:		ldy	#erri_mem_full
		bne	do_error

del_comma:	jsr	get16bit
		lda	p1
		sta	p3
		lda	p1+1
		sta	p3+1
		jsr	find_line1
		lda	p1
		sta	p2
		lda	p1+1
		sta	p2+1
		bne	Le395

del_cmd:	jsr	get16bit

Se38a:		jsr	find_line
		lda	p3
		sta	p1
		lda	p3+1
		sta	p1+1
Le395:		ldy	#$00
Le397:		lda	pp
		cmp	p2
		lda	pp+1
		sbc	p2+1
		bcs	Le3b7
		lda	p2
		bne	Le3a7
		dec	p2+1
Le3a7:		dec	p2
		lda	p3
		bne	Le3af
		dec	p3+1
Le3af:		dec	p3
		lda	(p2),y
		sta	(p3),y
		bcc	Le397
Le3b7:		lda	p3
		sta	pp
		lda	p3+1
		sta	pp+1
		rts
Le3c0:		jsr	cout
		iny

Se3c4:		lda	error_msg_tbl,y
		bmi	Le3c0

cout:		cmp	#ch_cr+$80
		bne	Le3d3

crout:		lda	#$00
		sta	ch
		lda	#ch_cr+$80
Le3d3:		inc	ch

; Send character in A to display.
Le3d5:		bit	dsp
		bmi	Le3d5
		sta	dsp
		rts

too_long_err:	ldy	#erri_too_long

do_error:	jsr	print_err_msg
		bit	run_flag
Le3e5:		bmi	Le3ea
		jmp	Le2b6
Le3ea:		jmp	Leb9a

Le3ed:		rol
		adc	#$a0
		cmp	buffer,x
		bne	Le448
		lda	(synpag),y
		asl
		bmi	Le400
		dey
		lda	(synpag),y
		bmi	Le428
		iny
Le400:		stx	text_index
		tya
		pha
		ldx	#$00
		lda	(synpag,x)
		tax
Le409:		lsr
		eor	#$48
		ora	(synpag),y
		cmp	#$c0
		bcc	Le413
		inx
Le413:		iny
		bne	Le409
		pla
		tay
		txa
		jmp	Le4c0

put_token:	inc	token_index
		ldx	token_index
		beq	too_long_err
		sta	buffer,x
Le425:		rts

Le426:		ldx	text_index
Le428:		lda	#$a0
Le42a:		inx
		cmp	buffer,x
		bcs	Le42a
		lda	(synpag),y
		and	#$3f
		lsr
		bne	Le3ed
		lda	buffer,x
		bcs	Le442
		adc	#$3f
		cmp	#$1a
		bcc	Le4b1
Le442:		adc	#$4f
		cmp	#$0a
		bcc	Le4b1
Le448:		ldx	synstkdx
Le44a:		iny
		lda	(synpag),y
		and	#$e0
		cmp	#$20
		beq	Le4cd
		lda	txtndxstk,x
		sta	text_index
		lda	tokndxstk,x
		sta	token_index
Le45b:		dey
		lda	(synpag),y
		asl	a
		bpl	Le45b
		dey
		bcs	Le49c
		asl	a
		bmi	Le49c
		ldy	syn_stk_h,x
		sty	synpag+1
		ldy	syn_stk_l,x
		inx
		bpl	Le44a
Le470:		beq	Le425
		cmp	#$7e
		bcs	Le498
		dex
		bpl	Le47d
		ldy	#$06
		bpl	go_errmess_2	; always taken


Le47d:		sty	syn_stk_l,x
		ldy	synpag+1
		sty	syn_stk_h,x
		ldy	text_index
		sty	txtndxstk,x
		ldy	token_index
		sty	tokndxstk,x
		and	#$1f		; mask to get syntax category number
		tay
		lda	category_table,y

Se491:		asl	a
		tay
		lda	#$ec/2
		rol
		sta	synpag+1
Le498:		bne	Le49b
		iny
Le49b:		iny
Le49c:		stx	synstkdx
		lda	(synpag),y
		bmi	Le426
		bne	Le4a9
		ldy	#erri_syntax
go_errmess_2:	jmp	do_error

Le4a9:		cmp	#$03
		bcs	Le470
		lsr	a
		ldx	text_index
		inx
Le4b1:		lda	buffer,x
		bcc	Le4ba
		cmp	#'"'+$80
		beq	Le4c4
Le4ba:		cmp	#'_'+$80
		beq	Le4c4
		stx	text_index
Le4c0:		jsr	put_token
		iny
Le4c4:		dey
		ldx	synstkdx
Le4c7:		lda	(synpag),y
		dey
		asl
		bpl	Le49c
Le4cd:		ldy	syn_stk_h,x
		sty	synpag+1
		ldy	syn_stk_l,x
		inx
		lda	(synpag),y
		and	#$9f
		bne	Le4c7
		sta	pcon
		sta	pcon+1
		tya
		pha
		stx	synstkdx
		ldy	srch,x
		sty	leadbl
		clc
Le4e7:		lda	#$0a
		sta	char
		ldx	#$00
		iny
		lda	buffer,y
		and	#$0f
Le4f3:		adc	pcon
		pha
		txa
		adc	pcon+1
		bmi	Le517
		tax
		pla
		dec	char
		bne	Le4f3
		sta	pcon
		stx	pcon+1
		cpy	token_index
		bne	Le4e7
		ldy	leadbl
		iny
		sty	token_index
		jsr	put_token
		pla
		tay
		lda	pcon+1
		bcs	Le4c0
Le517:		ldy	#$00
		bpl	go_errmess_2

prdec:		sta	pcon+1
		stx	pcon
		ldx	#$04
		stx	leadbl
Le523:		lda	#'0'+$80
		sta	char
Le527:		lda	pcon
		cmp	dectabl,x
		lda	pcon+1
		sbc	dectabh,x
		bcc	Le540
		sta	pcon+1
		lda	pcon
		sbc	dectabl,x
		sta	pcon
		inc	char
		bne	Le527
Le540:		lda	char
		inx
		dex
		beq	Le554
		cmp	#'0'+$80
		beq	Le54c
		sta	leadbl
Le54c:		bit	leadbl
		bmi	Le554
		lda	leadzr
		beq	Le55f
Le554:		jsr	cout
		bit	auto_flag
		bpl	Le55f
		sta	buffer,y
		iny
Le55f:		dex
		bpl	Le523
		rts

dectabl:	fcb	$01,$0a,$64,$e8,$10
dectabh:	fcb	$00,$00,$00,$03,$27

 find_line:	lda	pp
	 	sta	p3
	 	lda	pp+1
	 	sta	p3+1

 find_line1:	inx

 find_line2:	lda	p3+1
	 	sta	p2+1
	 	lda	p3
	 	sta	p2
	 	cmp	himem
	 	lda	p2+1
	 	sbc	himem+1
	 	bcs	Le5ac
	 	ldy	#$01
	 	lda	(p2),y
	 	sbc	acc
	 	iny
	 	lda	(p2),y
	 	sbc	acc+1
	 	bcs	Le5ac
	 	ldy	#$00
	 	lda	p3
	 	adc	(p2),y
	 	sta	p3
	 	bcc	Le5a0
	 	inc	p3+1
	 	clc
 Le5a0:		iny
	 	lda	acc
	 	sbc	(p2),y
	 	iny
	 	lda	acc+1
	 	sbc	(p2),y
	 	bcs	find_line2
 Le5ac:		rts

 new_cmd:	lsr	auto_flag
	 	lda	himem
	 	sta	pp
	 	lda	himem+1
	 	sta	pp+1

 clr:		lda	lomem
	 	sta	pv
	 	lda	lomem+1
	 	sta	pv+1
	 	lda	#$00
	 	sta	for_nest_count
	 	sta	gosub_nest_count
	 	sta	synpag
	 	lda	#$00
	 	sta	Z1d
	 	rts

 Le5cc:		lda	srch
	 	adc	#$05
	 	sta	srch2
	 	lda	tokndxstk
	 	adc	#$00
	 	sta	srch2+1
	 	lda	srch2
	 	cmp	pp
	 	lda	srch2+1
	 	sbc	pp+1
	 	bcc	Le5e5
	 	jmp	Le36b
Le5e5:		lda	acc
	 	sta	(srch),y
	 	lda	acc+1
		iny
	 	sta	(srch),y
	 	lda	srch2
	 	iny
	 	sta	(srch),y
	 	lda	srch2+1
	 	iny
	 	sta	(srch),y
	 	lda	#$00
	 	iny
	 	sta	(srch),y
	 	iny
	 	sta	(srch),y
		lda	srch2
	 	sta	pv
	 	lda	srch2+1
	 	sta	pv+1
	 	lda	srch
	 	bcc	Le64f
execute_var:	sta	acc
	 	sty	acc+1
	 	jsr	get_next_prog_byte
	 	bmi	Le623
	 	cmp	#$40
	 	beq	Le623
	 	jmp	Le628

	 	fcb	$06,$c9,$49,$d0,$07,$a9,$49

Le623:		sta	acc+1
	 	jsr	get_next_prog_byte
Le628:		lda	lomem+1
	 	sta	tokndxstk
	 	lda	lomem
Le62e:		sta	srch
	 	cmp	pv
	 	lda	tokndxstk
	 	sbc	pv+1
	 	bcs	Le5cc
	 	lda	(srch),y
	 	iny
	 	cmp	acc
	 	bne	Le645
	 	lda	(srch),y
	 	cmp	acc+1
	 	beq	Le653
Le645:		iny
	 	lda	(srch),y
		pha
	 	iny
	 	lda	(srch),y
	 	sta	tokndxstk
	 	pla
Le64f:		ldy	#$00
	 	beq	Le62e
Le653:		lda	srch
	 	adc	#$03
	 	jsr	push_a_noun_stk
	 	lda	tokndxstk
	 	adc	#$00
	 	sta	noun_stk_h_str,x
	 	lda	acc+1
	 	cmp	#$40
	 	bne	fetch_prog_byte
	 	dey
		tya
	 	jsr	push_a_noun_stk
	 	dey
	 	sty	noun_stk_h_str,x
	 	ldy	#$03
Le670:		inc	noun_stk_h_str,x
	 	iny
	 	lda	(srch),y
	 	bmi	Le670
	 	bpl	fetch_prog_byte

execute_stmt:	lda	#$00
	 	sta	if_flag
	 	sta	cr_flag
	 	ldx	#$20
push_old_verb:	pha
fetch_prog_byte:	ldy	#$00
	 	lda	(pverb),y
Le686:		bpl	execute_token
	 	asl	a
	 	bmi	execute_var
	 	jsr	get_next_prog_byte
	 	jsr	push_ya_noun_stk
	 	jsr	get_next_prog_byte
	 	sta	noun_stk_h_int,x
Le696:		bit	if_flag
	 	bpl	Le69b
	 	dex
Le69b:		jsr	get_next_prog_byte
 		bcs	Le686
execute_token:	cmp	#$28
	 	bne	execute_verb
	 	lda	pverb
	 	jsr	push_a_noun_stk
	 	lda	pverb+1
	 	sta	noun_stk_h_str,x
	 	bit	if_flag
	 	bmi	Le6bc
	 	lda	#$01
	 	jsr	push_a_noun_stk
	 	lda	#$00
	 	sta	noun_stk_h_str,x
Le6ba:		inc	noun_stk_h_str,x
Le6bc:		jsr	get_next_prog_byte
	 	bmi	Le6ba
	 	bcs	Le696
execute_verb:	bit	if_flag
	 	bpl	Le6cd
	 	cmp	#$04
	 	bcs	Le69b
	 	lsr	if_flag
Le6cd:		tay
		sta	current_verb
	 	lda	verb_prec_tbl,y
	 	and	#$55
	 	asl
	 	sta	precedence
Le6d8:		pla
	 	tay
	 	lda	verb_prec_tbl,y
	 	and	#$aa
	 	cmp	precedence
	 	bcs	do_verb
	 	tya
	 	pha
	 	jsr	get_next_prog_byte
	 	lda	current_verb
	 	bcc	push_old_verb
do_verb:	lda	verb_addr_l,y
	 	sta	acc
	 	lda	verb_addr_h,y
	 	sta	acc+1
	 	jsr	Se6fc
	 	jmp	Le6d8

Se6fc:		jmp	(acc)

get_next_prog_byte:
		inc	pverb
	 	bne	Le705
	 	inc	pverb+1
Le705:		lda	(pverb),y
	 	rts

push_ya_noun_stk:
		sty	syn_stk_h+31,x

push_a_noun_stk:
		dex
	 	bmi	Le710
	 	sta	noun_stk_l,x
	 	rts

Le710:		ldy	#erri_stopped_at+3	; The "+3" is a bug!
go_errmess_3:	jmp	do_error

get16bit:	ldy	#$00
	 	lda	noun_stk_l,x
	 	sta	acc
	 	lda	noun_stk_h_int,x
	 	sta	acc+1
	 	lda	noun_stk_h_str,x
	 	beq	Le731
	 	sta	acc+1
	 	lda	(acc),y
	 	pha
	 	iny
	 	lda	(acc),y
	 	sta	acc+1
	 	pla
	 	sta	acc
	 	dey
Le731:		inx
	 	rts

eq_op:		jsr	neq_op

not_op:		jsr	get16bit
	 	tya
	 	jsr	push_ya_noun_stk
	 	sta	noun_stk_h_int,x
	 	cmp	acc
	 	bne	Le749
	 	cmp	acc+1
	 	bne	Le749
	 	inc	noun_stk_l,x
Le749:		rts

neq_op:		jsr	subtract
	 	jsr	sgn_fn

abs_fn:		jsr	get16bit
	 	bit	acc+1
 		bmi	Se772
Le757:		dex
Le758:		rts

sgn_fn:		jsr	get16bit
	 	lda	acc+1
	 	bne	Le764
	 	lda	acc
	 	beq	Le757
Le764:		lda	#$ff
	 	jsr	push_ya_noun_stk
	 	sta	noun_stk_h_int,x
	 	bit	acc+1
	 	bmi	Le758

negate:		jsr	get16bit

Se772:		tya
	 	sec
	 	sbc	acc
	 	jsr	push_ya_noun_stk
	 	tya
	 	sbc	acc+1
	 	bvc	Le7a1
Le77e:		ldy	#$00
	 	bpl	go_errmess_3

subtract:	jsr	negate

add:		jsr	get16bit
	 	lda	acc
	 	sta	aux
	 	lda	acc+1
	 	sta	aux+1
	 	jsr	get16bit

Se793:		clc
	 	lda	acc
	 	adc	aux
	 	jsr	push_ya_noun_stk
	 	lda	acc+1
	 	adc	aux+1
		bvs	Le77e
Le7a1:		sta	noun_stk_h_int,x

unary_pos:	rts

tab_fn:		jsr	get16bit
	 	ldy	acc
	 	beq	Le7b0
	 	dey
	 	lda	acc+1
	 	beq	Le7bc
Le7b0:		rts

tabout:		lda	ch
	 	ora	#7
	 	tay
	 	iny
Le7b7:		lda	#' '+$80
	 	jsr	cout
Le7bc:		cpy	ch
	 	bcs	Le7b7
	 	rts

print_com_num:	jsr	tabout

print_num:	jsr	get16bit
	 	lda	acc+1
	 	bpl	Le7d5
	 	lda	#'-'+$80
	 	jsr	cout
	 	jsr	Se772
	 	bvc	print_num
Le7d5:		dey
	 	sty	cr_flag
	 	stx	acc+1
	 	ldx	acc
	 	jsr	prdec
	 	ldx	acc+1
	 	rts

auto_cmd:	jsr	get16bit
	 	lda	acc
	 	sta	auto_ln
	 	lda	acc+1
	 	sta	auto_ln+1
	 	dey
	 	sty	auto_flag
	 	iny
	 	lda	#$0a
Le7f3:		sta	auto_inc
	 	sty	auto_inc+1
	 	rts

auto_com:	jsr	get16bit
	 	lda	acc
	 	ldy	acc+1
	 	bpl	Le7f3

var_assign:	jsr	get16bit
	 	lda	noun_stk_l,x
	 	sta	aux
	 	lda	noun_stk_h_str,x
	 	sta	aux+1
	 	lda	acc
	 	sta	(aux),y
	 	iny
	 	lda	acc+1
	 	sta	(aux),y
	 	inx

null_stmt:	rts		; used to execute LET keyword or colon
				; (statement separator), which need no
				; processing

begin_line:	pla
	 	pla

colon:		bit	cr_flag
	 	bpl	Le822

print_cr:	jsr	crout

print_semi:	lsr	cr_flag
Le822:		rts

left_paren:	ldy	#$ff
	 	sty	precedence

right_paren:	rts

if_stmt:	jsr	Sefcd
	 	beq	Le834
	 	lda	#$25
	 	sta	current_verb
	 	dey
	 	sty	if_flag
Le834:		inx
	 	rts

run_warm:	lda	pp
	 	ldy	pp+1
	 	bne	Le896

gosub_stmt:	ldy	#$41
	 	lda	gosub_nest_count
	 	cmp	#8
	 	bcs	go_errmess_4
	 	tay
	 	inc	gosub_nest_count
	 	lda	pverb
	 	sta	gstk_pverbl,y
	 	lda	pverb+1
	 	sta	gstk_pverbh,y
	 	lda	pline
	 	sta	gstk_plinel,y
	 	lda	pline+1
	 	sta	gstk_plineh,y

goto_stmt:	jsr	get16bit
	 	jsr	find_line
	 	bcc	Le867
	 	ldy	#$37
	 	bne	go_errmess_4
Le867:		lda	p2
	 	ldy	p2+1
run_loop:	sta	pline
	 	sty	pline+1
	 	bit	kbdcr
	 	bmi	Le8c3
	 	clc
	 	adc	#$03
	 	bcc	Le87a
	 	iny
Le87a:		ldx	#$ff
	 	stx	run_flag
	 	txs
	 	sta	pverb
	 	sty	pverb+1
Le883:		jsr	execute_stmt
	 	bit	run_flag
	 	bpl	end_stmt
	 	clc
	 	ldy	#$00
	 	lda	pline
	 	adc	(pline),y
	 	ldy	pline+1
	 	bcc	Le896
	 	iny
Le896:		cmp	himem
	 	bne	run_loop
	 	cpy	himem+1
	 	bne	run_loop
	 	ldy	#erri_end
	 	lsr	run_flag
go_errmess_4:	jmp	do_error

return_stmt:	ldy	#$4a
	 	lda	gosub_nest_count
	 	beq	go_errmess_4
	 	dec	gosub_nest_count
	 	tay
	 	lda	gstk_plinel-1,y
	 	sta	pline
	 	lda	gstk_plineh-1,y
	 	sta	pline+1

		fcb	$be			; ldx synpag+1,y, but force absolute addressing mode
		fdb	synpag+1

	 	lda	gstk_pverbh-1,y
Le8be:		tay
	 	txa
	 	jmp	Le87a

Le8c3:		ldy	#$63
	 	jsr	Se3c4
	 	ldy	#$01
	 	lda	(pline),y
	 	tax
	 	iny
	 	lda	(pline),y
	 	jsr	prdec

end_stmt:	jmp	warm
Le8d6:		dec	for_nest_count

next_stmt:	ldy	#erri_bad_next
	 	lda	for_nest_count
Le8dc:		beq	go_errmess_4
	 	tay
		lda	noun_stk_l,x
	 	cmp	fstk_varl-1,y
	 	bne	Le8d6
	 	lda	noun_stk_h_str,x
	 	cmp	fstk_varh-1,y
	 	bne	Le8d6
	 	lda	fstk_stepl-1,y
	 	sta	aux
	 	lda	fstk_steph-1,y
	 	sta	aux+1
	 	jsr	get16bit
	 	dex
	 	jsr	Se793
	 	jsr	var_assign
	 	dex
	 	ldy	for_nest_count
		lda	fstk_toh-1,y
	 	sta	syn_stk_l+31,x
	 	lda	fstk_tol-1,y
	 	ldy	#$00
	 	jsr	push_ya_noun_stk
	 	jsr	subtract
	 	jsr	sgn_fn
	 	jsr	get16bit
	 	ldy	for_nest_count
	 	lda	acc
	 	beq	Le925
	 	eor	fstk_steph-1,y
	 	bpl	Le937
Le925:		lda	fstk_plinel-1,y
	 	sta	pline
	 	lda	fstk_plineh-1,y
	 	sta	pline+1
	 	ldx	fstk_pverbl-1,y
	 	lda	fstk_pverbh-1,y
		bne	Le8be
Le937:		dec	for_nest_count
 		rts

for_stmt:	ldy	#erri_too_many_fors
	 	lda	for_nest_count
	 	cmp	#$08
	 	beq	Le8dc
	 	inc	for_nest_count
	 	tay
	 	lda	noun_stk_l,x
	 	sta	fstk_varl,y
	 	lda	noun_stk_h_str,x
	 	sta	fstk_varh,y
	 	rts

to_clause:	jsr	get16bit
	 	ldy	for_nest_count
	 	lda	acc
	 	sta	fstk_tol-1,y
	 	lda	acc+1
	 	sta	fstk_toh-1,y
	 	lda	#$01
	 	sta	fstk_stepl-1,y
		lda	#$00
Le966:		sta	fstk_steph-1,y
	 	lda	pline
	 	sta	fstk_plinel-1,y
	 	lda	pline+1
	 	sta	fstk_plineh-1,y
	 	lda	pverb
	 	sta	fstk_pverbl-1,y
	 	lda	pverb+1
	 	sta	fstk_pverbh-1,y
	 	rts

step_clause:	jsr	get16bit
	 	ldy	for_nest_count
	 	lda	acc
	 	sta	fstk_stepl-1,y
	 	lda	acc+1
	 	jmp	Le966

	 	fcb	$00,$00,$00,$00,$00,$00,$00,$00
	 	fcb	$00,$00,$00


verb_prec_tbl:	rmb	$78
verb_addr_l:	rmb	$78
verb_addr_h:	rmb	$78

		verb	$00,$00,begin_line
		verb	$01,$00,$ffff
		verb	$02,$00,$ffff
		verb	$03,$ab,colon
		verb	$04,$03,list_cmd
		verb	$05,$03,list_comman
		verb	$06,$03,list_all
		verb	$07,$03,Teff2		; RUN w/ line #
		verb	$08,$03,$efec		; RUN w/o line #
		verb	$09,$03,$e387		; DEL
		verb	$0a,$03,$e36f		; comma used with DEL
		verb	$0b,$03,$e5ad		; SCR (New)
		verb	$0c,$03,$e5b7		; CLR
		verb	$0d,$03,$e7e2		; AUTO
		verb	$0e,$03,$e7f8		; comma used with AUTO
		verb	$0f,$03,$ee54		; MAN
		verb	$10,$03,$ef80		; HIMEM:
		verb	$11,$03,$ef96		; LOMEM:
		verb	$12,$3f,$e785		; +
		verb	$13,$3f,$e782		; -
		verb	$14,$c0,$e222		; *
		verb	$15,$c0,$ef10		; /
		verb	$16,$3c,$e733		; =
		verb	$17,$3c,$e74a		; #
		verb	$18,$3c,$ec13		; >=
		verb	$19,$3c,$ec06		; >
		verb	$1a,$3c,$ec0b		; <=
		verb	$1b,$3c,$e74a		; <>
		verb	$1c,$3c,$ec01		; <
		verb	$1d,$30,$ec40		; AND
		verb	$1e,$0f,$ec47		; OR
		verb	$1f,$c0,$e27a		; MOD
		verb	$20,$cc,$0000
		verb	$21,$ff,$ffff
		verb	$22,$55,$e823		; ( used in string DIM
		verb	$23,$00,$e109		; ,
		verb	$24,$ab,$e85b		; THEN followed by a line number
		verb	$25,$ab,null_stmt	; THEN followed by a statement
		verb	$26,$03,$efb6		; , used in string INPUT
		verb	$27,$03,$ebcb		; , used in numeric input
		verb	$28,$ff,$ffff
		verb	$29,$ff,$ffff
		verb	$2a,$55,$e0fb		; ( substring
		verb	$2b,$ff,$ffff
		verb	$2c,$ff,$ffff
		verb	$2d,$55,$ef24		; ( variable array subscript
		verb	$2e,$cf,$eef6		; PEEK
		verb	$2f,$cf,$ef4e		; RND
		verb	$30,$cf,$e759		; SGN
		verb	$31,$cf,$e750		; ABS
		verb	$32,$cf,$0000		; USR
		verb	$33,$ff,$ffff
		verb	$34,$55,$e823		; ( used in variable DIM
		verb	$35,$c3,$e7a3		; unary +
		verb	$36,$c3,$e76f		; unary -
		verb	$37,$c3,$e736		; NOT
		verb	$38,$55,$e823		; ( numeric
		verb	$39,$f0,$e1d7		; = string
		verb	$3a,$f0,$e21c		; # string
		verb	$3b,$cf,len_fn		; LEN(
		verb	$3c,$56,$eec2		; ASC(   XXX doesnt' work
		verb	$3d,$56,Teeae		; SCRN(  ???
		verb	$3e,$56,$eeba		; , used in SCRN
		verb	$3f,$55,left_paren	; (
		verb	$40,$ff,$ffff
		verb	$41,$ff,$ffff
		verb	$42,$55,str_arr_dest	; ( for string index on left side of assignment
		verb	$43,$03,dim_str		; , for DIM string var
		verb	$44,$03,dim_num		; , for DIM numeric var
		verb	$45,$03,print_str	; ; string PRINT
		verb	$46,$03,print_num	; ; numeric PRINT
		verb	$47,$03,print_semi	; ; end of PRINT
		verb	$48,$03,print_str_comma	; , string PRINT
		verb	$49,$03,print_com_num	; , numeric PRINT
		verb	$4a,$ff,$ffff
		verb	$4b,$ff,$ffff
		verb	$4c,$ff,$ffff
		verb	$4d,$03,call_stmt	; CALL
		verb	$4e,$03,dim_str		; DIM string var
		verb	$4f,$03,dim_num		; DIM numeric var
		verb	$50,$03,tab_fn		; TAB
		verb	$51,$03,end_stmt	; END
		verb	$52,$03,string_input	; INPUT string with no prompt
		verb	$53,$03,input_prompt	; INPUT prompt
		verb	$54,$03,input_num_stmt	; INPUT numeric
		verb	$55,$03,for_stmt	; FOR
		verb	$56,$03,var_assign	; = in FOR
		verb	$57,$03,to_clause	; TO
		verb	$58,$03,step_clause	; STEP
		verb	$59,$03,next_stmt	; NEXT
		verb	$5a,$03,next_stmt	; ,
		verb	$5b,$03,return_stmt	; RETURN
		verb	$5c,$03,gosub_stmt	; GOSUB
		verb	$5d,$00,$ffff		; REM
		verb	$5e,$ab,null_stmt	; LET
		verb	$5f,$03,goto_stmt	; GOTO
		verb	$60,$57,if_stmt		; IF
		verb	$61,$03,print_str	; PRINT string
		verb	$62,$03,print_num	; PRINT numeric
		verb	$63,$03,print_cr	; PRINT w/o arg
		verb	$64,$03,poke_stmt	; POKE
		verb	$65,$07,poke_comma	; , in POKE
		verb	$66,$03,$ee4e		; COLOR=	???
		verb	$67,$03,$ef00		; PLOT		???
		verb	$68,$03,$ee3e		; , in PLOT	???
		verb	$69,$03,$ef00		; HLIN		???
		verb	$6a,$03,$eea6		; , in HLIN	???
		verb	$6b,$03,$eeb0		; AT in HLIN	???
		verb	$6c,$03,$ef00		; VLIN		???
		verb	$6d,$03,$eebc		; , in VLIN	???
		verb	$6e,$03,$eec6		; AT in VLIN	???
		verb	$6f,$03,$ee57		; VTAB		???
		verb	$70,$03,string_lit	; = string assginment
		verb	$71,$03,var_assign	; = numeric assignment
		verb	$72,$aa,right_paren	; )
		verb	$73,$ff,$ffff
		verb	$74,$ff,$ffff
		verb	$75,$ff,$ffff
		verb	$76,$ff,$ffff
		verb	$77,$ff,$ffff


error_msg_tbl	equ	*

erri_gr_32767	equ	*-error_msg_tbl
		errstr	">32767"

erri_too_long	equ	*-error_msg_tbl
		errstr	"TOO LONG"

erri_syntax	equ	*-error_msg_tbl
		errstr	"SYNTAX"

erri_mem_full	equ	*-error_msg_tbl
		errstr	"MEM FULL"

erri_too_many_parens	equ	*-error_msg_tbl
		errstr	"TOO MANY PARENS"

erri_string	equ	*-error_msg_tbl
		errstr	"STRING"

erri_no_end	equ	*-error_msg_tbl
		fcs	"NO "

erri_end	equ	*-error_msg_tbl
		errstr	"END"

erri_bad_branch	equ	*-error_msg_tbl
		errstr	"BAD BRANCH"

erri_too_many_gosubs	equ  *-error_msg_tbl
		errstr	">8 GOSUBS"

erri_bad_return	equ	*-error_msg_tbl
		errstr	"BAD RETURN"

erri_too_many_fors	equ  *-error_msg_tbl
		errstr	">8 FORS"

erri_bad_next	equ	*-error_msg_tbl
		errstr	"BAD NEXT"

erri_stopped_at	equ	*-error_msg_tbl
		errstr	"STOPPED AT "

erri_err	equ	*-error_msg_tbl
		errstr	"*** "
		fcs	" ERR"
		fcb	ch_cr

erri_gr_255	equ	*-error_msg_tbl
		errstr	">255"

erri_range	equ	*-error_msg_tbl
		errstr	"RANGE"

erri_dim	equ	*-error_msg_tbl
		errstr	"DIM"

erri_str_ovfl	equ	*-error_msg_tbl
		errstr	"STR OVFL"

		fcb	$dc			; backslash
		fcb	ch_cr

erri_retype_line	equ	*-error_msg_tbl
		fcs	"RETYPE LINE"
		fcb	ch_cr+$80,'?'


Leb9a:		lsr	run_flag
		bcc	Leba1
		jmp	Le8c3
Leba1:		ldx	acc+1
		txs
		ldx	acc
		ldy	#$8d
		bne	Lebac

input_num_stmt:	ldy	#$99
Lebac:		jsr	Se3c4
		stx	acc
		tsx
		stx	acc+1
		ldy	#$fe
		sty	run_flag
		iny
		sty	text_index
		jsr	Se299
		sty	token_index
		ldx	#$20
		lda	#$30
		jsr	Se491
		inc	run_flag
		ldx	acc

input_num_comma:	ldy	text_index
		asl	a
Lebce:		sta	acc
		iny
		lda	buffer,y
		cmp	#$74
		beq	input_num_stmt
		eor	#$b0
		cmp	#$0a
		bcs	Lebce
		iny
		iny
		sty	text_index
		lda	buffer,y
		pha
		lda	buffer-1,y
		ldy	#$00
		jsr	push_ya_noun_stk
		pla
		sta	noun_stk_h_int,x
		lda	acc
		cmp	#$c7
		bne	Lebfa
		jsr	negate
Lebfa:		jmp	var_assign

		fcb	$ff,$ff,$ff,$50

Tec01:		jsr	Tec13
		bne	Lec1b

Tec06:		jsr	Tec0b
		bne	Lec1b

Tec0b:		jsr	subtract
		jsr	negate
		bvc	Lec16

Tec13:		jsr	subtract
Lec16:		jsr	sgn_fn
		lsr	noun_stk_l,x
Lec1b:		jmp	not_op

		fcb	$ff,$ff

category_table:
		fcb	((syn_cat_00-1)/2)&$ff
		fcb	$ff
		fcb	((syn_cat_02-1)/2)&$ff
		fcb	$d1		; $03
		fcb	((syn_cat_04-1)/2)&$ff
		fcb	((syn_cat_05-1)/2)&$ff
		fcb	((syn_cat_06-1)/2)&$ff
		fcb	((syn_cat_07-1)/2)&$ff	; numeric expressoin
		fcb	((syn_cat_08-1)/2)&$ff
		fcb	((syn_cat_09-1)/2)&$ff
		fcb	((syn_cat_0a-1)/2)&$ff
		fcb	((syn_cat_0b-1)/2)&$ff	; statement
		fcb	((syn_cat_0c-1)/2)&$ff
		fcb	((syn_cat_0d-1)/2)&$ff
		fcb	((syn_cat_0e-1)/2)&$ff
		fcb	((syn_cat_0f-1)/2)&$ff
		fcb	((syn_cat_10-1)/2)&$ff
		fcb	((syn_cat_11-1)/2)&$ff
		fcb	((syn_cat_12-1)/2)&$ff
		fcb	$2b		; $13
		fcb	((syn_cat_14-1)/2)&$ff	; function
		fcb	((syn_cat_15-1)/2)&$ff
		fcb	((syn_cat_16-1)/2)&$ff
		fcb	((syn_cat_17-1)/2)&$ff
		fcb	$35		; $18
		fcb	$8e		; $19: numeric variable name
		fcb	((syn_cat_1a-1)/2)&$ff
		fcb	$ff
		fcb	$ff
		fcb	$ff
		fcb	((syn_cat_1e-1)/2)&$ff	; binary operator
		fcb	((syn_cat_1f-1)/2)&$ff

Tec40:		jsr	Sefc9
		ora	rnd+1,x
		bpl	Lec4c

Tec47:		jsr	Sefc9
		and	rnd+1,x
Lec4c:		sta	noun_stk_l,x
		bpl	Lec1b
		jmp	Sefc9

; syntax tables
		fcb	$40		; cat 00, end of category

		fcb	$60		; unsigned integer, end of rule
		synstr	"-"
syn_cat_13:

		fcb	$60		; cat 00, end of rule
		synstr	"+"

		fcb	$00

		fcb	$7e		; branch "backward" two bytes
		synstr	","
		fcb	$33
syn_cat_12:

		fcb	$00
		fcb	$00

		fcb	$60		; cat 00, end of rule
		fcb	$03		; string literal
		synstr	"_"
		fcb	$12

		fcb	$00		; return with syntax error
		fcb	$40
		synstr	")"
syn_cat_11:

		synstr1	")"

		fcb	$47
		synstr	"="
		fcb	$17
syn_cat_18:

		fcb	$68
		synstr	"="
		fcb	$0a

		fcb	$00		; return with syntax error
		fcb	$40

		fcb	$60
		synstr	"-"

		fcb	$60
		synstr	"+"

		fcb	$00		; return with syntax error
		fcb	$7e		; branch "backward" two bytes
		synstr	","
		fcb	$3c

		fcb	$00

		fcb	$00		; return with syntax error

		fcb	$60
		fcb	$03		; string literal
		synstr	"_"
		fcb	$1b
		fcb	$4b

		fcb	$67		; numeric expression, end of rule
		synstr	"AT"
		fcb	$07		; numeric expression
		synstr	","
		fcb	$07		; numeric expression
		synstr	"HLIN"

		fcb	$67		; numeric expression, end of rule
		synstr	","
		fcb	$07		; numeric expression
		synstr	"PLOT"

		fcb	$67		; numeric expression, end of rule
		synstr	"COLOR="

		fcb	$67		; numeric expression, end of rule
		synstr	","
		fcb	$07		; numeric expression
		synstr	"POKE"

		synstr1	"PRINT"

		fcb	$7f		; branch "backward" one byte
		fcb	$0e
		fcb	$27
		synstr	"PRINT"

		fcb	$7f		; branch "backward" one byte
		fcb	$0e
		fcb	$28
		synstr	"PRINT"

		fcb	$64
		fcb	$07		; numeric expressoin
		synstr	"IF"

		fcb	$67
		synstr	"GOTO"

		fcb	$78
		synstr	"LET"

		fcb	$78		; end of rule
		fcb	$7f		; branch "backward" one byte
		fcb	$02		; comment characte
		synstr	"REM"
syn_cat_1a:

		fcb	$67		; numeric expression, end of rule
		synstr	"GOSUB"

		synstr1	"RETURN"

		fcb	$7e		; branch "backward" two bytes
		synstr	","
		fcb	$39
		synstr	"NEXT"

		fcb	$67		; numeric expression, end of rule
		synstr	"STEP"
		fcb	$27		; numeric expression, rest optional
		synstr	"TO"
		fcb	$07		; numeric expression
		synstr	"="
		fcb	$19
		synstr	"FOR"

		fcb	$7f		; branch "backward" one byte
		fcb	$05
		fcb	$37
		synstr	"INPUT"

		fcb	$7f		; branch "backward" one byte
		fcb	$05
		fcb	$28
		synstr	"INPUT"

		fcb	$7f		; branch "backward" one byte
		fcb	$05
		fcb	$2a
		synstr	"INPUT"

		synstr1	"END"
syn_cat_02:

		fcb	$00
		fcb	$ff
		fcb	$ff

		fcb	$47		; numeric expression, end of category
		synstr	"TAB"

		fcb	$7f		; branch "backward" one byte
		fcb	$0d
		fcb	$30
		synstr	"DIM"

		fcb	$7f		; branch "backward" one byte
		fcb	$0d		; category 0d, required
		fcb	$23		; category 03, optional
		synstr	"DIM"

		fcb	$67		; numeric expression, end of rule
		synstr	"CALL"
syn_cat_0b:

		fcb	$00

		fcb	$40		; unsigned integer, end of category
		fcb	$80		; letter A-Z
syn_cat_19:

		fcb	$c0
		fcb	$c1
		fcb	$80
		fcb	$00

		fcb	$47		; numeric expression, end of category
		synstr	","
		fcb	$68
		synstr	","
		fcb	$db

		fcb	$67		; numeric expression, end of rule
		synstr	";"
		fcb	$68
		synstr	";"
syn_cat_0e:

		fcb	$50
		synstr	","
		fcb	$63
		synstr	","
syn_cat_0d:

		fcb	$7f		; branch "backward" one byte
		fcb	$01		; return with no error
syn_cat_0c:

		fcb	$51
		fcb	$07		; numeric expression
		fcb	$88
syn_cat_0a:

		fcb	$29
		fcb	$84
		fcb	$80
		fcb	$c4
syn_cat_09:

		fcb	$80
		fcb	$57
		fcb	$71
		fcb	$07		; numeric expression
		synstr	"("
		fcb	$14

		synstr1	"LOMEM"

		synstr1	"HIMEM"

		synstr1	"COLOR"

		fcb	$71
		fcb	$08
		synstr	"LEN("

		fcb	$68
		fcb	$83
		fcb	$08

		fcb	$68
		synstr	"="
		fcb	$08

		fcb	$71		; cat 11, end of rule
		fcb	$07		; numeric expression
		synstr	"("
syn_cat_16:

		fcb	$60		; cat 00, end of rule

		fcb	$76		; cat 16, end of rule
		synstr	"NOT"

		fcb	$76		; cat 16, end of rule
		synstr	"-"

		fcb	$76		; cat 16, end of rule
		synstr	"+"
syn_cat_15:

		fcb	$51
		fcb	$07
		synstr	"("
		fcb	$19
syn_cat_10:

		synstr	"RNDX"		; can never be matched since it
					; comes "after" RND

		synstr1	"USR"

		synstr1	"ABS"

		synstr1	"SGN"

		synstr1	"RND"

		synstr1	"PEEK"
syn_cat_14:

		fcb	$51
		fcb	$07		; numeric expression
		synstr	"("
syn_cat_17:

		fcb	$39
		synstr	"!"
syn_cat_0f:

		fcb	$c1		; parse 0-9, end of rule

		fcb	$4f		; cat 15, end of category
		fcb	$7f		; branch "backward" one byte
		fcb	$0f		; cat 15
syn_cat_00:

		fcb	$2f		; cat 15
		fcb	$00
		fcb	$51
		fcb	$06
		synstr	"("
		fcb	$29
		fcb	$c2
		fcb	$0c
syn_cat_08:

		synstr	"\""
		fcb	$57
		synstr	","
		fcb	$6a
syn_cat_05:

		synstr	","
		fcb	$42
		synstr	"THEN"

		fcb	$60
		synstr	"THEN"
syn_cat_04:

		fcb	$4f
		fcb	$7e		; branch "backward" two bytes
		fcb	$1e
syn_cat_07:
		fcb	$35
		synstr	","
syn_cat_06:

		fcb	$27		; optional reset, numeric expr

		fcb	$51
		fcb	$07		; numeric expr
		synstr	"("

		fcb	$09
		synstr	"+"
syn_cat_03:

		synstr1	"^"
		synstr1	"MOD"
		synstr1	"OR"
		synstr1	"AND"
		synstr1	"<"
		synstr1	"<>"
		synstr1	"<="
		synstr1	">"
		synstr1	">="
		synstr1	"#"
		synstr1	"="
		synstr1	"/"
		synstr1	"*"
		synstr1	"-"
		synstr1	"+"
syn_cat_1e:

		fcb	$00

		fcb	$47
		synstr	"LOMEM="

		fcb	$76
		synstr	"HIMEM="

		synstr1	"OFF"

		fcb	$60		; unsigned integer, end of rule
		synstr	","
		fcb	$20		; (optional) unsigned integer
		synstr	"AUTO"

		synstr1	"CLR"

		synstr1	"SCR"

		fcb	$60		; unsgined integer, end of rule
		synstr	","
		fcb	$20		; (optional) unsigned integer
		synstr	"DEL"

		synstr1	"RUN"		; RUN with no line number

		fcb	$60		; unsigned integer, end of rule
		synstr	"RUN"

		synstr1	"LIST"

		fcb	$60		; unsigned integer, end of rule
		synstr	","
		fcb	$20		; (optional) unsigned integer
		synstr	"LIST"

		fcb	$7a
		fcb	$7e		; branch "backward" two bytes
		fcb	$9a
		fcb	$22
		fcb	$20		; (optional) unsigned integer
syn_cat_1f:

		fcb	$00
		fcb	$60
		fcb	$03		; string literal
		fcb	$bf
		fcb	$60
		fcb	$03
		fcb	$bf
		fcb	$1f

print_str_comma:	jsr	tabout

print_str:	inx
		inx
		lda	rnd+1,x
		sta	aux
		lda	syn_stk_h+31,x
		sta	aux+1
		ldy	rnd,x
Lee0f:		tya
		cmp	syn_stk_h+30,x
		bcs	Lee1d
		lda	(aux),y
		jsr	cout
		iny
		jmp	Lee0f
Lee1d:		lda	#$ff
		sta	cr_flag
		rts

len_fn:		inx
		lda	#$00
		sta	noun_stk_h_str,x
		sta	noun_stk_h_int,x
		lda	syn_stk_h+31,x
		sec
		sbc	rnd+1,x
		sta	noun_stk_l,x
		jmp	left_paren

		fcb	$ff

getbyte:	jsr	get16bit
		lda	acc+1
		bne	gr_255_err
		lda	acc
		rts

plot_comma:	jsr	getbyte
		ldy	text_index
		cmp	#$30
		bcs	range_err
		cpy	#$28
		bcs	range_err
		rts

		nop
		nop

Tee4e:		jsr	getbyte
		rts

		nop
		nop

man_cmd:	lsr	auto_flag
		rts

vtab_stmt:	jsr	getbyte
		cmp	#$18
		bcs	range_err
		sta	cv
		rts

		nop
		nop

gr_255_err:	ldy	#erri_gr_255
go_errmess_5:	jmp	do_error

range_err:	ldy	#erri_range
		bne	go_errmess_5

See6c:		jsr	Se254
		lda	aux
		bne	Lee7a
		lda	aux+1
		bne	Lee7a
		jmp	Le77e
Lee7a:		asl	acc
		rol	acc+1
		rol	p3
		rol	p3+1
		lda	p3
		cmp	aux
		lda	p3+1
		sbc	aux+1
		bcc	Lee96
		sta	p3+1
		lda	p3
		sbc	aux
		sta	p3
		inc	acc
Lee96:		dey
		bne	Lee7a
		rts

		fcb	$ff,$ff,$ff,$ff,$ff,$ff

call_stmt:	jsr	get16bit
		jmp	(acc)

		fcb	$20,$34,$ee,$c5,$c8,$90,$bb,$85

Teeae:		lda	himem+1

Teeb0:		pha
		lda	himem
		jsr	push_ya_noun_stk
		pla
		sta	noun_stk_h_int,x
		rts

Teeba:		lda	lomem+1

Teebc:		pha
		lda	lomem
		jmp	Lefb3

		fcb	$a5,$85,$2d,$60

Teec6:		jsr	getbyte
		cmp	#$28
Leecb:		bcs	range_err
		tay
		lda	text_index
		rts

		nop
		nop

print_err_msg:	tya
		tax
		ldy	#$6e
		jsr	Se3c4
		txa
		tay
		jsr	Se3c4
		ldy	#$72
		jmp	Se3c4

Seee4:		jsr	get16bit
Leee7:		asl	acc
		rol	acc+1
		bmi	Leee7
		bcs	Leecb
		bne	Leef5
		cmp	acc
		bcs	Leecb
Leef5:		rts

peek_fn:	jsr	get16bit
		lda	(acc),y
		sty	syn_stk_l+31,x
		jmp	push_ya_noun_stk

poke_stmt:	jsr	getbyte
		lda	acc
		pha
		jsr	get16bit
		pla
		sta	(acc),y

poke_comma:	rts

		fcb	$ff,$ff,$ff

divide:		jsr	See6c
		lda	acc
		sta	p3
		lda	acc+1
		sta	p3+1
		jmp	Le244

dim_num:	jsr	Seee4
		jmp	Le134

num_array_subs:	jsr	Seee4
		ldy	noun_stk_h_str,x
		lda	noun_stk_l,x
		adc	#$fe
		bcs	Lef30
		dey
Lef30:		sta	aux
		sty	aux+1
		clc
		adc	acc
		sta	noun_stk_l,x
		tya
		adc	acc+1
		sta	noun_stk_h_str,x
		ldy	#$00
		lda	noun_stk_l,x
		cmp	(aux),y
		iny
		lda	noun_stk_h_str,x
		sbc	(aux),y
		bcs	Leecb
		jmp	left_paren

rnd_fn:		jsr	get16bit
		lda	rnd
		jsr	push_ya_noun_stk
		lda	rnd+1
		bne	Lef5e
		cmp	rnd
		adc	#$00
Lef5e:		and	#$7f
		sta	rnd+1
		sta	noun_stk_h_int,x
		ldy	#$11
Lef66:		lda	rnd+1
		asl
		clc
		adc	#$40
		asl
		rol	rnd
		rol	rnd+1
		dey
		bne	Lef66
		lda	acc
		jsr	push_ya_noun_stk
		lda	acc+1
		sta	noun_stk_h_int,x
		jmp	mod_op

Tef80:		jsr	get16bit
		ldy	acc
		cpy	lomem
		lda	acc+1
		sbc	lomem+1
		bcc	Lefab
		sty	himem
		lda	acc+1
		sta	himem+1
Lef93:		jmp	new_cmd

Tef96:		jsr	get16bit
		ldy	acc
		cpy	himem
		lda	acc+1
		sbc	himem+1
		bcs	Lefab
		sty	lomem
		lda	acc+1
		sta	lomem+1
		bcc	Lef93
Lefab:		jmp	Leecb

		fcb	$a5,$4d,$48,$a5,$4c

Lefb3:		jsr	Sefc9

string_input:	jsr	input_str
		jmp	Lefbf

input_prompt:	jsr	print_str
Lefbf:		lda	#$ff
		sta	text_index
		lda	#$74
		sta	buffer
		rts

Sefc9:		jsr	not_op
		inx

Sefcd:		jsr	not_op
		lda	noun_stk_l,x
		rts

mem_init_4k:	lda	#$00
		sta	lomem
		sta	himem
		lda	#$08
		sta	lomem+1
		lda	#$10
		sta	himem+1
		jmp	new_cmd

Sefe4:		cmp	noun_stk_h_str,x
		bne	Lefe9
		clc
Lefe9:		jmp	Le102

Tefec:		jsr	clr
		jmp	run_warm

Teff2:		jsr	clr
		jmp	goto_stmt

Seff8:		cpx	#$80
		bne	Leffd
		dey
Leffd:		jmp	Se00c
