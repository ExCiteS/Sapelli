/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text;

/**
 * @author mstevens
 * 
 * @see TextSMSTransmission
 * @see TextMessage
 * @see Decoding
 */
public final class Encoding
{

	private Encoding(){ /*do not instantiate*/ }
	
	/**
	 * Integer value of the space (<code>SP</code>) character.
	 */
	static public final int ESCAPE_SP	= 0x20;
	
	/**
	 * Integer value of the <code>ESC</code> character.
	 */
	static public final int ESCAPE_ESC	= 0x1B;
	
	/**
	 * Basic Character Set of the GSM 7 bit default alphabet (standardised in GSM 03.38 / 3GPP TS 23.038),
	 * containing 128 characters including the ESC character which we must avoid in both header and body.
	 * 
	 * @see <a href="http://en.wikipedia.org/wiki/GSM_03.38">GSM 03.38 / 3GPP TS 23.038</a>
	 */
	static public final char[] GSM_0338_CHAR_TABLE =
	{
		/*	0	0x00	@	*/	'\u0040',
		/*	1	0x01	£	*/	'\u00A3',
		/*	2	0x02	$	*/	'\u0024',
		/*	3	0x03	¥	*/	'\u00A5',
		/*	4	0x04	è	*/	'\u00E8',
		/*	5	0x05	é	*/	'\u00E9',
		/*	6	0x06	ù	*/	'\u00F9',
		/*	7	0x07	ì	*/	'\u00EC',
		/*	8	0x08	ò	*/	'\u00F2',
		/*	9	0x09	Ç	*/	'\u00C7',
		/*	10	0x0A	LF	*/		'\n',
		/*	11	0x0B	Ø	*/	'\u00D8',
		/*	12	0x0C	ø	*/	'\u00F8',
		/*	13	0x0D	CR	*/		'\r',
		/*	14	0x0E	Å	*/	'\u00C5',
		/*	15	0x0F	å	*/	'\u00E5',
		/*	16	0x10	Δ	*/	'\u0394',
		/*	17	0x11	_	*/	'\u005F',
		/*	18	0x12	Φ	*/	'\u03A6',
		/*	19	0x13	Γ	*/	'\u0393',
		/*	20	0x14	Λ	*/	'\u039B',
		/*	21	0x15	Ω	*/	'\u03A9',
		/*	22	0x16	Π	*/	'\u03A0',
		/*	23	0x17	Ψ	*/	'\u03A8',
		/*	24	0x18	Σ	*/	'\u03A3',
		/*	25	0x19	Θ	*/	'\u0398',
		/*	26	0x1A	Ξ	*/	'\u039E',
		/*	27	0x1B	ESC	*/	'\u0000', // Not used in header; We map ESC to NULL here but this value should never be used!
		/*	28	0x1C	Æ	*/	'\u00C6',
		/*	29	0x1D	æ	*/	'\u00E6',
		/*	30	0x1E	ß	*/	'\u00DF',
		/*	31	0x1F	É	*/	'\u00C9',
		/*	32	0x20	SP	*/	'\u0020', // Not used in header
		/*	33	0x21	!	*/	'\u0021', // Not used in header
		/*	34	0x22	"	*/	'\u0022',
		/*	35	0x23	#	*/	'\u0023', // Not used in header
		/*	36	0x24	¤	*/	'\u00A4',
		/*	37	0x25	%	*/	'\u0025',
		/*	38	0x26	&	*/	'\u0026',
		/*	39	0x27	'	*/		'\'',
		/*	40	0x28	(	*/	'\u0028',
		/*	41	0x29	)	*/	'\u0029',
		/*	42	0x2A	*	*/	'\u002A',
		/*	43	0x2B	+	*/	'\u002B',
		/*	44	0x2C	,	*/	'\u002C',
		/*	45	0x2D	-	*/	'\u002D',
		/*	46	0x2E	.	*/	'\u002E', // Not used in header
		/*	47	0x2F	/	*/	'\u002F',
		/*	48	0x30	0	*/	'\u0030', // Not used in header
		/*	49	0x31	1	*/	'\u0031', // Not used in header
		/*	50	0x32	2	*/	'\u0032', // Not used in header
		/*	51	0x33	3	*/	'\u0033', // Not used in header
		/*	52	0x34	4	*/	'\u0034', // Not used in header
		/*	53	0x35	5	*/	'\u0035', // Not used in header
		/*	54	0x36	6	*/	'\u0036', // Not used in header
		/*	55	0x37	7	*/	'\u0037', // Not used in header
		/*	56	0x38	8	*/	'\u0038', // Not used in header
		/*	57	0x39	9	*/	'\u0039', // Not used in header
		/*	58	0x3A	:	*/	'\u003A',
		/*	59	0x3B	;	*/	'\u003B',
		/*	60	0x3C	<	*/	'\u003C',
		/*	61	0x3D	=	*/	'\u003D',
		/*	62	0x3E	>	*/	'\u003E',
		/*	63	0x3F	?	*/	'\u003F', // Not used in header
		/*	64	0x40	¡	*/	'\u00A1',
		/*	65	0x41	A	*/	'\u0041', // Not used in header
		/*	66	0x42	B	*/	'\u0042', // Not used in header
		/*	67	0x43	C	*/	'\u0043', // Not used in header
		/*	68	0x44	D	*/	'\u0044', // Not used in header
		/*	69	0x45	E	*/	'\u0045', // Not used in header
		/*	70	0x46	F	*/	'\u0046', // Not used in header
		/*	71	0x47	G	*/	'\u0047', // Not used in header
		/*	72	0x48	H	*/	'\u0048', // Not used in header
		/*	73	0x49	I	*/	'\u0049', // Not used in header
		/*	74	0x4A	J	*/	'\u004A', // Not used in header
		/*	75	0x4B	K	*/	'\u004B', // Not used in header
		/*	76	0x4C	L	*/	'\u004C', // Not used in header
		/*	77	0x4D	M	*/	'\u004D', // Not used in header
		/*	78	0x4E	N	*/	'\u004E', // Not used in header
		/*	79	0x4F	O	*/	'\u004F', // Not used in header
		/*	80	0x50	P	*/	'\u0050', // Not used in header
		/*	81	0x51	Q	*/	'\u0051',
		/*	82	0x52	R	*/	'\u0052', // Not used in header
		/*	83	0x53	S	*/	'\u0053', // Not used in header
		/*	84	0x54	T	*/	'\u0054', // Not used in header
		/*	85	0x55	U	*/	'\u0055', // Not used in header
		/*	86	0x56	V	*/	'\u0056', // Not used in header
		/*	87	0x57	W	*/	'\u0057', // Not used in header
		/*	88	0x58	X	*/	'\u0058', // Not used in header
		/*	89	0x59	Y	*/	'\u0059',
		/*	90	0x5A	Z	*/	'\u005A', // Not used in header
		/*	91	0x5B	Ä	*/	'\u00C4',
		/*	92	0x5C	Ö	*/	'\u00D6',
		/*	93	0x5D	Ñ	*/	'\u00D1',
		/*	94	0x5E	Ü	*/	'\u00DC',
		/*	95	0x5F	§	*/	'\u00A7',
		/*	96	0x60	¿	*/	'\u00BF',
		/*	97	0x61	a	*/	'\u0061', // Not used in header
		/*	98	0x62	b	*/	'\u0062', // Not used in header
		/*	99	0x63	c	*/	'\u0063', // Not used in header
		/*	100	0x64	d	*/	'\u0064', // Not used in header
		/*	101	0x65	e	*/	'\u0065', // Not used in header
		/*	102	0x66	f	*/	'\u0066', // Not used in header
		/*	103	0x67	g	*/	'\u0067', // Not used in header
		/*	104	0x68	h	*/	'\u0068', // Not used in header
		/*	105	0x69	i	*/	'\u0069', // Not used in header
		/*	106	0x6A	j	*/	'\u006A', // Not used in header
		/*	107	0x6B	k	*/	'\u006B', // Not used in header
		/*	108	0x6C	l	*/	'\u006C', // Not used in header
		/*	109	0x6D	m	*/	'\u006D', // Not used in header
		/*	110	0x6E	n	*/	'\u006E', // Not used in header
		/*	111	0x6F	o	*/	'\u006F', // Not used in header
		/*	112	0x70	p	*/	'\u0070', // Not used in header
		/*	113	0x71	q	*/	'\u0071',
		/*	114	0x72	r	*/	'\u0072', // Not used in header
		/*	115	0x73	s	*/	'\u0073', // Not used in header
		/*	116	0x74	t	*/	'\u0074', // Not used in header
		/*	117	0x75	u	*/	'\u0075', // Not used in header
		/*	118	0x76	v	*/	'\u0076', // Not used in header
		/*	119	0x77	w	*/	'\u0077', // Not used in header
		/*	120	0x78	x	*/	'\u0078', // Not used in header
		/*	121	0x79	y	*/	'\u0079',
		/*	122	0x7A	z	*/	'\u007A', // Not used in header
		/*	123	0x7B	ä	*/	'\u00E4',
		/*	124	0x7C	ö	*/	'\u00F6',
		/*	125	0x7D	ñ	*/	'\u00F1',
		/*	126	0x7E	ü	*/	'\u00FC',
		/*	127	0x7F	à	*/	'\u00E0'
	};
	
	/**
	 * This array serves to map 6 bit chunks of header information (values 0-63), to indexes of GSM 03.38
	 * characters (as stored in {@link #GSM_0338_CHAR_TABLE}) which can be used in the message header.
	 * <br/><br/>
	 * For the header we use a subset of the GSM alphabet, containing 64 characters out of the total of 128.<br/>
	 * The subset does not contain the <code>ESC</code> character and has been designed to reduce the risk of
	 * human-written SMS messages being mistaken for Sapelli messages. To achieve this we have excluded most Latin
	 * alpha-numeric characters (0-9, a-p, A-P, R-X, r-x, z & Z) as well as spaces and common punctuation/special
	 * characters (., !, ? & #).<br/>
	 * Because we need to select them out of an alphabet of 64 (rather than 128) possibilities, each header character
	 * can only represent 6 (rather than 7) bits of meaning information.<br/>
	 * 
	 * @see TextMessage
	 */
	static public final int[] HEADER_CHAR_INDEX_MAPPING = 
	{
		/*  0 --> */  0,
		/*  1 --> */  1,
		/*  2 --> */  2,
		/*  3 --> */  3,
		/*  4 --> */  4,
		/*  5 --> */  5,
		/*  6 --> */  6,
		/*  7 --> */  7,
		/*  8 --> */  8,
		/*  9 --> */  9,
		/* 10 --> */ 10,
		/* 11 --> */ 11,
		/* 12 --> */ 12,
		/* 13 --> */ 13,
		/* 14 --> */ 14,
		/* 15 --> */ 15,
		/* 16 --> */ 16,
		/* 17 --> */ 17,
		/* 18 --> */ 18,
		/* 19 --> */ 19,
		/* 20 --> */ 20,
		/* 21 --> */ 21,
		/* 22 --> */ 22,
		/* 23 --> */ 23,
		/* 24 --> */ 24,
		/* 25 --> */ 25,
		/* 26 --> */ 26,
		/* 27 --> */ 28, // Skip ESC	(27)
		/* 28 --> */ 29,
		/* 29 --> */ 30,
		/* 30 --> */ 31,
		/* 31 --> */ 34, // Skip SP-!	(32-33)
		/* 32 --> */ 36, // Skip #		(35)
		/* 33 --> */ 37,
		/* 34 --> */ 38,
		/* 35 --> */ 39,
		/* 36 --> */ 40,
		/* 37 --> */ 41,
		/* 38 --> */ 42,
		/* 39 --> */ 43,
		/* 40 --> */ 44,
		/* 41 --> */ 45,
		/* 42 --> */ 47, // Skip .		(46)
		/* 43 --> */ 58, // Skip 0-9	(48-57)
		/* 44 --> */ 59,
		/* 45 --> */ 60,
		/* 46 --> */ 61,
		/* 47 --> */ 62,
		/* 48 --> */ 64, // Skip ?		(63)
		/* 49 --> */ 81, // Skip A-P	(65-80)
		/* 50 --> */ 89, // Skip R-X	(82-88)
		/* 51 --> */ 91, // Skip Z		(90)
		/* 52 --> */ 92,
		/* 53 --> */ 93,
		/* 54 --> */ 94,
		/* 55 --> */ 95,
		/* 56 --> */ 96,
		/* 57 --> */ 113, // Skip a-p	(97-112)
		/* 58 --> */ 121, // Skip r-x	(114-120)
		/* 59 --> */ 123, // Skip z		(122)
		/* 60 --> */ 124,
		/* 61 --> */ 125,
		/* 62 --> */ 126,
		/* 63 --> */ 127
	};
	
}
