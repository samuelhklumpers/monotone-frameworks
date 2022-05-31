{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import AttributeGrammar
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,401) ([0,0,0,8,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,37376,130,17488,31,0,0,0,0,18432,522,2368,125,0,0,0,0,0,0,0,0,0,768,128,0,32768,8451,1280,0,0,16903,2560,0,0,0,512,0,0,12,10241,0,0,2632,16386,32001,0,5264,32772,64002,0,32,0,0,0,64,0,0,0,0,0,4,0,0,0,8,0,0,0,2,0,0,0,4,0,2048,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,32768,8193,1280,0,0,1,0,0,0,0,4096,0,0,0,8192,0,0,2632,16386,32003,0,5264,32772,64003,0,32768,0,0,0,0,32768,0,0,0,0,0,0,768,64,10,0,1536,128,20,0,3072,256,40,0,0,0,0,0,0,36608,15,0,0,0,0,0,0,49184,1,0,0,0,0,0,0,0,512,0,0,0,0,0,0,2076,10241,0,0,4152,20482,0,0,256,112,0,0,96,16392,1,0,33216,32784,2,0,384,32,5,0,0,0,0,0,0,0,0,0,0,960,16,0,0,14336,8,0,0,3840,16,0,8192,2080,1280,500,49152,4225,640,0,32768,8451,1280,0,0,16903,2560,0,0,57344,8689,0,0,0,16412,0,0,0,0,0,0,48,40964,0,0,8224,8,62469,1,192,32784,2,0,384,32,5,0,768,64,10,0,1536,128,20,0,3072,256,40,0,6144,512,80,0,12288,1024,160,0,24576,2048,320,0,49152,4096,640,0,0,30720,2048,0,0,61440,1024,0,0,0,0,0,0,12,10241,0,0,0,0,0,0,0,0,0,0,0,0,256,0,33216,32784,2,0,0,0,8192,0,0,0,16,0,1536,128,20,0,0,960,16,0,0,0,8,0,12288,1024,160,0,0,0,0,8,0,0,0,0,0,30720,124,0,0,0,7,0,0,0,0,32,0,4,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,60,0,0,0,120,0,0,0,240,0,0,0,480,0,0,0,960,0,0,0,0,0,0,0,0,0,0,0,6144,0,0,0,12288,0,0,0,0,0,0,0,61440,1024,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,1536,128,20,0,1024,260,32928,62,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,4096,0,0,2,0,0,0,2076,10241,0,0,32768,32775,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,36,0,0,0,0,0,0,0,8192,0,4096,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,33426,20480,8000,0,0,1024,0,0,0,0,0,0,0,0,0,0,10528,8,62501,1,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happy","Start","Program","Procs","Proc","ValArgs","ResArg","Args","Arg","Stats","Stat","Stat0","CallArgs","EitherExpr","AExpr","AExpr0","BExpr","BExpr0","ident","int","bool","if","then","else","while","do","skip","not","\":=\"","\"b=\"","\"+\"","\"-\"","\"*\"","\"/\"","and","or","\"<=>\"","\"==\"","\"<\"","\">\"","\"<=\"","\">=\"","\";\"","\"[\"","\"]\"","\"(\"","\")\"","\"{\"","\"}\"","begin","end","proc","is","val","res","call","\",\"","malloc","free","continue","break","tyint","%eof"]
        bit_start = st Prelude.* 65
        bit_end = (st Prelude.+ 1) Prelude.* 65
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..64]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (52) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (52) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyGoto action_5
action_3 _ = happyReduce_4

action_4 (65) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (21) = happyShift action_10
action_5 (24) = happyShift action_11
action_5 (27) = happyShift action_12
action_5 (29) = happyShift action_13
action_5 (35) = happyShift action_14
action_5 (48) = happyShift action_15
action_5 (50) = happyShift action_16
action_5 (54) = happyShift action_17
action_5 (58) = happyShift action_18
action_5 (60) = happyShift action_19
action_5 (61) = happyShift action_20
action_5 (62) = happyShift action_21
action_5 (63) = happyShift action_22
action_5 (64) = happyShift action_23
action_5 (7) = happyGoto action_6
action_5 (12) = happyGoto action_7
action_5 (13) = happyGoto action_8
action_5 (14) = happyGoto action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_3

action_7 (21) = happyShift action_10
action_7 (24) = happyShift action_11
action_7 (27) = happyShift action_12
action_7 (29) = happyShift action_13
action_7 (35) = happyShift action_14
action_7 (48) = happyShift action_15
action_7 (50) = happyShift action_16
action_7 (53) = happyShift action_53
action_7 (58) = happyShift action_18
action_7 (60) = happyShift action_19
action_7 (61) = happyShift action_20
action_7 (62) = happyShift action_21
action_7 (63) = happyShift action_22
action_7 (64) = happyShift action_23
action_7 (13) = happyGoto action_52
action_7 (14) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_13

action_9 _ = happyReduce_17

action_10 (31) = happyShift action_49
action_10 (32) = happyShift action_50
action_10 (46) = happyShift action_51
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (21) = happyShift action_44
action_11 (22) = happyShift action_35
action_11 (23) = happyShift action_45
action_11 (30) = happyShift action_46
action_11 (35) = happyShift action_36
action_11 (46) = happyShift action_37
action_11 (48) = happyShift action_47
action_11 (17) = happyGoto action_40
action_11 (18) = happyGoto action_41
action_11 (19) = happyGoto action_48
action_11 (20) = happyGoto action_43
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (21) = happyShift action_44
action_12 (22) = happyShift action_35
action_12 (23) = happyShift action_45
action_12 (30) = happyShift action_46
action_12 (35) = happyShift action_36
action_12 (46) = happyShift action_37
action_12 (48) = happyShift action_47
action_12 (17) = happyGoto action_40
action_12 (18) = happyGoto action_41
action_12 (19) = happyGoto action_42
action_12 (20) = happyGoto action_43
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (45) = happyShift action_39
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (21) = happyShift action_34
action_14 (22) = happyShift action_35
action_14 (35) = happyShift action_36
action_14 (46) = happyShift action_37
action_14 (48) = happyShift action_38
action_14 (18) = happyGoto action_33
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (21) = happyShift action_10
action_15 (24) = happyShift action_11
action_15 (27) = happyShift action_12
action_15 (29) = happyShift action_13
action_15 (35) = happyShift action_14
action_15 (48) = happyShift action_15
action_15 (50) = happyShift action_16
action_15 (58) = happyShift action_18
action_15 (60) = happyShift action_19
action_15 (61) = happyShift action_20
action_15 (62) = happyShift action_21
action_15 (63) = happyShift action_22
action_15 (64) = happyShift action_23
action_15 (12) = happyGoto action_32
action_15 (13) = happyGoto action_8
action_15 (14) = happyGoto action_9
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (21) = happyShift action_10
action_16 (24) = happyShift action_11
action_16 (27) = happyShift action_12
action_16 (29) = happyShift action_13
action_16 (35) = happyShift action_14
action_16 (48) = happyShift action_15
action_16 (50) = happyShift action_16
action_16 (58) = happyShift action_18
action_16 (60) = happyShift action_19
action_16 (61) = happyShift action_20
action_16 (62) = happyShift action_21
action_16 (63) = happyShift action_22
action_16 (64) = happyShift action_23
action_16 (12) = happyGoto action_31
action_16 (13) = happyGoto action_8
action_16 (14) = happyGoto action_9
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_30
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (21) = happyShift action_29
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (48) = happyShift action_28
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (48) = happyShift action_27
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (45) = happyShift action_26
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (45) = happyShift action_25
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (21) = happyShift action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (46) = happyShift action_85
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_28

action_26 _ = happyReduce_27

action_27 (21) = happyShift action_34
action_27 (22) = happyShift action_35
action_27 (35) = happyShift action_36
action_27 (46) = happyShift action_37
action_27 (48) = happyShift action_38
action_27 (18) = happyGoto action_84
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (21) = happyShift action_83
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (48) = happyShift action_82
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (48) = happyShift action_81
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (21) = happyShift action_10
action_31 (24) = happyShift action_11
action_31 (27) = happyShift action_12
action_31 (29) = happyShift action_13
action_31 (35) = happyShift action_14
action_31 (48) = happyShift action_15
action_31 (50) = happyShift action_16
action_31 (51) = happyShift action_80
action_31 (58) = happyShift action_18
action_31 (60) = happyShift action_19
action_31 (61) = happyShift action_20
action_31 (62) = happyShift action_21
action_31 (63) = happyShift action_22
action_31 (64) = happyShift action_23
action_31 (13) = happyGoto action_52
action_31 (14) = happyGoto action_9
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (21) = happyShift action_10
action_32 (24) = happyShift action_11
action_32 (27) = happyShift action_12
action_32 (29) = happyShift action_13
action_32 (35) = happyShift action_14
action_32 (48) = happyShift action_15
action_32 (49) = happyShift action_79
action_32 (50) = happyShift action_16
action_32 (58) = happyShift action_18
action_32 (60) = happyShift action_19
action_32 (61) = happyShift action_20
action_32 (62) = happyShift action_21
action_32 (63) = happyShift action_22
action_32 (64) = happyShift action_23
action_32 (13) = happyGoto action_52
action_32 (14) = happyGoto action_9
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (31) = happyShift action_78
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (46) = happyShift action_64
action_34 _ = happyReduce_42

action_35 _ = happyReduce_41

action_36 (21) = happyShift action_34
action_36 (22) = happyShift action_35
action_36 (35) = happyShift action_36
action_36 (46) = happyShift action_37
action_36 (48) = happyShift action_38
action_36 (18) = happyGoto action_77
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (21) = happyShift action_34
action_37 (22) = happyShift action_35
action_37 (35) = happyShift action_36
action_37 (46) = happyShift action_37
action_37 (48) = happyShift action_38
action_37 (17) = happyGoto action_76
action_37 (18) = happyGoto action_41
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (21) = happyShift action_34
action_38 (22) = happyShift action_35
action_38 (35) = happyShift action_36
action_38 (46) = happyShift action_37
action_38 (48) = happyShift action_38
action_38 (17) = happyGoto action_75
action_38 (18) = happyGoto action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_18

action_40 (33) = happyShift action_66
action_40 (34) = happyShift action_67
action_40 (35) = happyShift action_68
action_40 (36) = happyShift action_69
action_40 (40) = happyShift action_70
action_40 (41) = happyShift action_71
action_40 (42) = happyShift action_72
action_40 (43) = happyShift action_73
action_40 (44) = happyShift action_74
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_40

action_42 (28) = happyShift action_65
action_42 (37) = happyShift action_58
action_42 (38) = happyShift action_59
action_42 (39) = happyShift action_60
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_56

action_44 (33) = happyReduce_42
action_44 (34) = happyReduce_42
action_44 (35) = happyReduce_42
action_44 (36) = happyReduce_42
action_44 (40) = happyReduce_42
action_44 (41) = happyReduce_42
action_44 (42) = happyReduce_42
action_44 (43) = happyReduce_42
action_44 (44) = happyReduce_42
action_44 (46) = happyShift action_64
action_44 (49) = happyReduce_58
action_44 (59) = happyReduce_58
action_44 _ = happyReduce_58

action_45 _ = happyReduce_57

action_46 (21) = happyShift action_44
action_46 (22) = happyShift action_35
action_46 (23) = happyShift action_45
action_46 (30) = happyShift action_46
action_46 (35) = happyShift action_36
action_46 (46) = happyShift action_37
action_46 (48) = happyShift action_47
action_46 (17) = happyGoto action_40
action_46 (18) = happyGoto action_41
action_46 (19) = happyGoto action_63
action_46 (20) = happyGoto action_43
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (21) = happyShift action_44
action_47 (22) = happyShift action_35
action_47 (23) = happyShift action_45
action_47 (30) = happyShift action_46
action_47 (35) = happyShift action_36
action_47 (46) = happyShift action_37
action_47 (48) = happyShift action_47
action_47 (17) = happyGoto action_61
action_47 (18) = happyGoto action_41
action_47 (19) = happyGoto action_62
action_47 (20) = happyGoto action_43
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (25) = happyShift action_57
action_48 (37) = happyShift action_58
action_48 (38) = happyShift action_59
action_48 (39) = happyShift action_60
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (21) = happyShift action_34
action_49 (22) = happyShift action_35
action_49 (35) = happyShift action_36
action_49 (46) = happyShift action_37
action_49 (48) = happyShift action_38
action_49 (17) = happyGoto action_56
action_49 (18) = happyGoto action_41
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (21) = happyShift action_44
action_50 (22) = happyShift action_35
action_50 (23) = happyShift action_45
action_50 (30) = happyShift action_46
action_50 (35) = happyShift action_36
action_50 (46) = happyShift action_37
action_50 (48) = happyShift action_47
action_50 (17) = happyGoto action_40
action_50 (18) = happyGoto action_41
action_50 (19) = happyGoto action_55
action_50 (20) = happyGoto action_43
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (21) = happyShift action_34
action_51 (22) = happyShift action_35
action_51 (35) = happyShift action_36
action_51 (46) = happyShift action_37
action_51 (48) = happyShift action_38
action_51 (17) = happyGoto action_54
action_51 (18) = happyGoto action_41
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_12

action_53 _ = happyReduce_2

action_54 (33) = happyShift action_66
action_54 (34) = happyShift action_67
action_54 (35) = happyShift action_68
action_54 (36) = happyShift action_69
action_54 (47) = happyShift action_116
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (37) = happyShift action_58
action_55 (38) = happyShift action_59
action_55 (39) = happyShift action_60
action_55 (45) = happyShift action_115
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (33) = happyShift action_66
action_56 (34) = happyShift action_67
action_56 (35) = happyShift action_68
action_56 (36) = happyShift action_69
action_56 (45) = happyShift action_114
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (21) = happyShift action_10
action_57 (29) = happyShift action_13
action_57 (35) = happyShift action_14
action_57 (48) = happyShift action_15
action_57 (50) = happyShift action_16
action_57 (58) = happyShift action_18
action_57 (60) = happyShift action_19
action_57 (61) = happyShift action_20
action_57 (62) = happyShift action_21
action_57 (63) = happyShift action_22
action_57 (64) = happyShift action_23
action_57 (14) = happyGoto action_113
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (21) = happyShift action_44
action_58 (22) = happyShift action_35
action_58 (23) = happyShift action_45
action_58 (30) = happyShift action_46
action_58 (35) = happyShift action_36
action_58 (46) = happyShift action_37
action_58 (48) = happyShift action_47
action_58 (17) = happyGoto action_40
action_58 (18) = happyGoto action_41
action_58 (19) = happyGoto action_112
action_58 (20) = happyGoto action_43
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (21) = happyShift action_44
action_59 (22) = happyShift action_35
action_59 (23) = happyShift action_45
action_59 (30) = happyShift action_46
action_59 (35) = happyShift action_36
action_59 (46) = happyShift action_37
action_59 (48) = happyShift action_47
action_59 (17) = happyGoto action_40
action_59 (18) = happyGoto action_41
action_59 (19) = happyGoto action_111
action_59 (20) = happyGoto action_43
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (21) = happyShift action_44
action_60 (22) = happyShift action_35
action_60 (23) = happyShift action_45
action_60 (30) = happyShift action_46
action_60 (35) = happyShift action_36
action_60 (46) = happyShift action_37
action_60 (48) = happyShift action_47
action_60 (17) = happyGoto action_40
action_60 (18) = happyGoto action_41
action_60 (19) = happyGoto action_110
action_60 (20) = happyGoto action_43
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (33) = happyShift action_66
action_61 (34) = happyShift action_67
action_61 (35) = happyShift action_68
action_61 (36) = happyShift action_69
action_61 (40) = happyShift action_70
action_61 (41) = happyShift action_71
action_61 (42) = happyShift action_72
action_61 (43) = happyShift action_73
action_61 (44) = happyShift action_74
action_61 (49) = happyShift action_97
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (37) = happyShift action_58
action_62 (38) = happyShift action_59
action_62 (39) = happyShift action_60
action_62 (49) = happyShift action_109
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (39) = happyShift action_60
action_63 _ = happyReduce_47

action_64 (21) = happyShift action_34
action_64 (22) = happyShift action_35
action_64 (35) = happyShift action_36
action_64 (46) = happyShift action_37
action_64 (48) = happyShift action_38
action_64 (17) = happyGoto action_108
action_64 (18) = happyGoto action_41
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (21) = happyShift action_10
action_65 (29) = happyShift action_13
action_65 (35) = happyShift action_14
action_65 (48) = happyShift action_15
action_65 (50) = happyShift action_16
action_65 (58) = happyShift action_18
action_65 (60) = happyShift action_19
action_65 (61) = happyShift action_20
action_65 (62) = happyShift action_21
action_65 (63) = happyShift action_22
action_65 (64) = happyShift action_23
action_65 (14) = happyGoto action_107
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (21) = happyShift action_34
action_66 (22) = happyShift action_35
action_66 (35) = happyShift action_36
action_66 (46) = happyShift action_37
action_66 (48) = happyShift action_38
action_66 (17) = happyGoto action_106
action_66 (18) = happyGoto action_41
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (21) = happyShift action_34
action_67 (22) = happyShift action_35
action_67 (35) = happyShift action_36
action_67 (46) = happyShift action_37
action_67 (48) = happyShift action_38
action_67 (17) = happyGoto action_105
action_67 (18) = happyGoto action_41
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (21) = happyShift action_34
action_68 (22) = happyShift action_35
action_68 (35) = happyShift action_36
action_68 (46) = happyShift action_37
action_68 (48) = happyShift action_38
action_68 (17) = happyGoto action_104
action_68 (18) = happyGoto action_41
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (21) = happyShift action_34
action_69 (22) = happyShift action_35
action_69 (35) = happyShift action_36
action_69 (46) = happyShift action_37
action_69 (48) = happyShift action_38
action_69 (17) = happyGoto action_103
action_69 (18) = happyGoto action_41
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (21) = happyShift action_34
action_70 (22) = happyShift action_35
action_70 (35) = happyShift action_36
action_70 (46) = happyShift action_37
action_70 (48) = happyShift action_38
action_70 (17) = happyGoto action_102
action_70 (18) = happyGoto action_41
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (21) = happyShift action_34
action_71 (22) = happyShift action_35
action_71 (35) = happyShift action_36
action_71 (46) = happyShift action_37
action_71 (48) = happyShift action_38
action_71 (17) = happyGoto action_101
action_71 (18) = happyGoto action_41
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (21) = happyShift action_34
action_72 (22) = happyShift action_35
action_72 (35) = happyShift action_36
action_72 (46) = happyShift action_37
action_72 (48) = happyShift action_38
action_72 (17) = happyGoto action_100
action_72 (18) = happyGoto action_41
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (21) = happyShift action_34
action_73 (22) = happyShift action_35
action_73 (35) = happyShift action_36
action_73 (46) = happyShift action_37
action_73 (48) = happyShift action_38
action_73 (17) = happyGoto action_99
action_73 (18) = happyGoto action_41
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (21) = happyShift action_34
action_74 (22) = happyShift action_35
action_74 (35) = happyShift action_36
action_74 (46) = happyShift action_37
action_74 (48) = happyShift action_38
action_74 (17) = happyGoto action_98
action_74 (18) = happyGoto action_41
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (33) = happyShift action_66
action_75 (34) = happyShift action_67
action_75 (35) = happyShift action_68
action_75 (36) = happyShift action_69
action_75 (49) = happyShift action_97
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (33) = happyShift action_66
action_76 (34) = happyShift action_67
action_76 (35) = happyShift action_68
action_76 (36) = happyShift action_69
action_76 (47) = happyShift action_96
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_45

action_78 (21) = happyShift action_34
action_78 (22) = happyShift action_35
action_78 (35) = happyShift action_36
action_78 (46) = happyShift action_37
action_78 (48) = happyShift action_38
action_78 (18) = happyGoto action_95
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_25

action_80 _ = happyReduce_26

action_81 (56) = happyShift action_94
action_81 (8) = happyGoto action_93
action_81 _ = happyReduce_7

action_82 (21) = happyShift action_44
action_82 (22) = happyShift action_35
action_82 (23) = happyShift action_45
action_82 (30) = happyShift action_46
action_82 (35) = happyShift action_36
action_82 (46) = happyShift action_37
action_82 (48) = happyShift action_47
action_82 (15) = happyGoto action_89
action_82 (16) = happyGoto action_90
action_82 (17) = happyGoto action_91
action_82 (18) = happyGoto action_41
action_82 (19) = happyGoto action_92
action_82 (20) = happyGoto action_43
action_82 _ = happyReduce_33

action_83 (59) = happyShift action_88
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (49) = happyShift action_87
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (21) = happyShift action_34
action_85 (22) = happyShift action_35
action_85 (35) = happyShift action_36
action_85 (46) = happyShift action_37
action_85 (48) = happyShift action_38
action_85 (17) = happyGoto action_86
action_85 (18) = happyGoto action_41
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (33) = happyShift action_66
action_86 (34) = happyShift action_67
action_86 (35) = happyShift action_68
action_86 (36) = happyShift action_69
action_86 (47) = happyShift action_129
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (45) = happyShift action_128
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (21) = happyShift action_34
action_88 (22) = happyShift action_35
action_88 (35) = happyShift action_36
action_88 (46) = happyShift action_37
action_88 (48) = happyShift action_38
action_88 (17) = happyGoto action_127
action_88 (18) = happyGoto action_41
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (59) = happyShift action_126
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_32

action_91 (33) = happyShift action_66
action_91 (34) = happyShift action_67
action_91 (35) = happyShift action_68
action_91 (36) = happyShift action_69
action_91 (40) = happyShift action_70
action_91 (41) = happyShift action_71
action_91 (42) = happyShift action_72
action_91 (43) = happyShift action_73
action_91 (44) = happyShift action_74
action_91 _ = happyReduce_34

action_92 (37) = happyShift action_58
action_92 (38) = happyShift action_59
action_92 (39) = happyShift action_60
action_92 _ = happyReduce_35

action_93 (57) = happyShift action_125
action_93 (9) = happyGoto action_124
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (21) = happyShift action_123
action_94 (10) = happyGoto action_121
action_94 (11) = happyGoto action_122
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (45) = happyShift action_120
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_44

action_97 _ = happyReduce_43

action_98 (33) = happyShift action_66
action_98 (34) = happyShift action_67
action_98 (35) = happyShift action_68
action_98 (36) = happyShift action_69
action_98 _ = happyReduce_54

action_99 (33) = happyShift action_66
action_99 (34) = happyShift action_67
action_99 (35) = happyShift action_68
action_99 (36) = happyShift action_69
action_99 _ = happyReduce_55

action_100 (33) = happyShift action_66
action_100 (34) = happyShift action_67
action_100 (35) = happyShift action_68
action_100 (36) = happyShift action_69
action_100 _ = happyReduce_53

action_101 (33) = happyShift action_66
action_101 (34) = happyShift action_67
action_101 (35) = happyShift action_68
action_101 (36) = happyShift action_69
action_101 _ = happyReduce_52

action_102 (33) = happyShift action_66
action_102 (34) = happyShift action_67
action_102 (35) = happyShift action_68
action_102 (36) = happyShift action_69
action_102 _ = happyReduce_51

action_103 _ = happyReduce_39

action_104 _ = happyReduce_38

action_105 (35) = happyShift action_68
action_105 (36) = happyShift action_69
action_105 _ = happyReduce_37

action_106 (35) = happyShift action_68
action_106 (36) = happyShift action_69
action_106 _ = happyReduce_36

action_107 _ = happyReduce_16

action_108 (33) = happyShift action_66
action_108 (34) = happyShift action_67
action_108 (35) = happyShift action_68
action_108 (36) = happyShift action_69
action_108 (47) = happyShift action_119
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_59

action_110 (37) = happyShift action_58
action_110 (38) = happyShift action_59
action_110 (39) = happyShift action_60
action_110 _ = happyReduce_50

action_111 (37) = happyShift action_58
action_111 (39) = happyShift action_60
action_111 _ = happyReduce_49

action_112 (39) = happyShift action_60
action_112 _ = happyReduce_48

action_113 (26) = happyShift action_118
action_113 _ = happyReduce_15

action_114 _ = happyReduce_19

action_115 _ = happyReduce_20

action_116 (31) = happyShift action_117
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (21) = happyShift action_34
action_117 (22) = happyShift action_35
action_117 (35) = happyShift action_36
action_117 (46) = happyShift action_37
action_117 (48) = happyShift action_38
action_117 (18) = happyGoto action_138
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (21) = happyShift action_10
action_118 (29) = happyShift action_13
action_118 (35) = happyShift action_14
action_118 (48) = happyShift action_15
action_118 (50) = happyShift action_16
action_118 (58) = happyShift action_18
action_118 (60) = happyShift action_19
action_118 (61) = happyShift action_20
action_118 (62) = happyShift action_21
action_118 (63) = happyShift action_22
action_118 (64) = happyShift action_23
action_118 (14) = happyGoto action_137
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_46

action_120 _ = happyReduce_21

action_121 (59) = happyShift action_136
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_10

action_123 _ = happyReduce_11

action_124 (49) = happyShift action_135
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (21) = happyShift action_123
action_125 (11) = happyGoto action_134
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (21) = happyShift action_133
action_126 (22) = happyShift action_35
action_126 (23) = happyShift action_45
action_126 (30) = happyShift action_46
action_126 (35) = happyShift action_36
action_126 (46) = happyShift action_37
action_126 (48) = happyShift action_47
action_126 (16) = happyGoto action_132
action_126 (17) = happyGoto action_91
action_126 (18) = happyGoto action_41
action_126 (19) = happyGoto action_92
action_126 (20) = happyGoto action_43
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (33) = happyShift action_66
action_127 (34) = happyShift action_67
action_127 (35) = happyShift action_68
action_127 (36) = happyShift action_69
action_127 (49) = happyShift action_131
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_24

action_129 (45) = happyShift action_130
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_29

action_131 (45) = happyShift action_143
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_31

action_133 (37) = happyReduce_58
action_133 (38) = happyReduce_58
action_133 (39) = happyReduce_58
action_133 (46) = happyShift action_64
action_133 (49) = happyShift action_142
action_133 (59) = happyReduce_58
action_133 _ = happyReduce_42

action_134 _ = happyReduce_8

action_135 (55) = happyShift action_141
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (21) = happyShift action_123
action_136 (11) = happyGoto action_140
action_136 _ = happyReduce_6

action_137 _ = happyReduce_14

action_138 (45) = happyShift action_139
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_30

action_140 _ = happyReduce_9

action_141 (21) = happyShift action_10
action_141 (24) = happyShift action_11
action_141 (27) = happyShift action_12
action_141 (29) = happyShift action_13
action_141 (35) = happyShift action_14
action_141 (48) = happyShift action_15
action_141 (50) = happyShift action_16
action_141 (58) = happyShift action_18
action_141 (60) = happyShift action_19
action_141 (61) = happyShift action_20
action_141 (62) = happyShift action_21
action_141 (63) = happyShift action_22
action_141 (64) = happyShift action_23
action_141 (12) = happyGoto action_145
action_141 (13) = happyGoto action_8
action_141 (14) = happyGoto action_9
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (45) = happyShift action_144
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_23

action_144 _ = happyReduce_22

action_145 (21) = happyShift action_10
action_145 (24) = happyShift action_11
action_145 (27) = happyShift action_12
action_145 (29) = happyShift action_13
action_145 (35) = happyShift action_14
action_145 (48) = happyShift action_15
action_145 (50) = happyShift action_16
action_145 (53) = happyShift action_146
action_145 (58) = happyShift action_18
action_145 (60) = happyShift action_19
action_145 (61) = happyShift action_20
action_145 (62) = happyShift action_21
action_145 (63) = happyShift action_22
action_145 (64) = happyShift action_23
action_145 (13) = happyGoto action_52
action_145 (14) = happyGoto action_9
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_5

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Program happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [ happy_var_2 ]
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([ ]
	)

happyReduce_5 = happyReduce 9 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Proc happy_var_2 happy_var_4 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  8 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [ happy_var_3 ]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([ happy_var_1 ]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  12 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Seq happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 6 13 happyReduction_14
happyReduction_14 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 13 happyReduction_15
happyReduction_15 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (IfThenElse happy_var_2 happy_var_4 Skip
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 _
	_
	 =  HappyAbsSyn14
		 (Skip
	)

happyReduce_19 = happyReduce 4 14 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (IAssign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 14 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (BAssign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 5 14 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (RefAssign happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 8 14 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Call happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 14 happyReduction_23
happyReduction_23 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Malloc happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 14 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Free happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 _
	_
	 =  HappyAbsSyn14
		 (Continue
	)

happyReduce_28 = happySpecReduce_2  14 happyReduction_28
happyReduction_28 _
	_
	 =  HappyAbsSyn14
		 (Break
	)

happyReduce_29 = happyReduce 6 14 happyReduction_29
happyReduction_29 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Malloc happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 7 14 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (RefAssign (Plus (Var happy_var_1) happy_var_3) happy_var_6
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [ happy_var_3 ]
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([ happy_var_1 ]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  15 happyReduction_33
happyReduction_33  =  HappyAbsSyn15
		 ([]
	)

happyReduce_34 = happySpecReduce_1  16 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (I happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn16
		 (B happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  17 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  17 happyReduction_38
happyReduction_38 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Times happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  17 happyReduction_39
happyReduction_39 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Divide happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  17 happyReduction_40
happyReduction_40 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  18 happyReduction_41
happyReduction_41 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn18
		 (IConst happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  18 happyReduction_42
happyReduction_42 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn18
		 (Var happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  18 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  18 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Deref happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happyReduce 4 18 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Deref (Plus (Var happy_var_1) happy_var_3)
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_2  19 happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Not happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  19 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (And happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  19 happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Or happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  19 happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (BEqual happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  19 happyReduction_51
happyReduction_51 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (IEqual happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  19 happyReduction_52
happyReduction_52 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  19 happyReduction_53
happyReduction_53 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterThan happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  19 happyReduction_54
happyReduction_54 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  19 happyReduction_55
happyReduction_55 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  19 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  20 happyReduction_57
happyReduction_57 (HappyTerminal (TBool happy_var_1))
	 =  HappyAbsSyn20
		 (BConst happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  20 happyReduction_58
happyReduction_58 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn20
		 (BVar happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  20 happyReduction_59
happyReduction_59 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 65 65 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TIdent happy_dollar_dollar -> cont 21;
	TInt happy_dollar_dollar -> cont 22;
	TBool happy_dollar_dollar -> cont 23;
	TIf -> cont 24;
	TThen -> cont 25;
	TElse -> cont 26;
	TWhile -> cont 27;
	TDo -> cont 28;
	TSkip -> cont 29;
	TNot -> cont 30;
	TIAssign -> cont 31;
	TBAssign -> cont 32;
	TArithmeticOp "+" -> cont 33;
	TArithmeticOp "-" -> cont 34;
	TStar -> cont 35;
	TArithmeticOp "/" -> cont 36;
	TBoolOp "and" -> cont 37;
	TBoolOp "or" -> cont 38;
	TBoolOp "<=>" -> cont 39;
	TRelOp "==" -> cont 40;
	TRelOp "<" -> cont 41;
	TRelOp ">" -> cont 42;
	TRelOp "<=" -> cont 43;
	TRelOp ">=" -> cont 44;
	TSemicolon -> cont 45;
	TBlockOpen -> cont 46;
	TBlockClose -> cont 47;
	TParenOpen -> cont 48;
	TParenClose -> cont 49;
	TBraceOpen -> cont 50;
	TBraceClose -> cont 51;
	TBegin -> cont 52;
	TEnd -> cont 53;
	TProc -> cont 54;
	TIs -> cont 55;
	TVal -> cont 56;
	TRes -> cont 57;
	TCall -> cont 58;
	TComma -> cont 59;
	TMalloc -> cont 60;
	TFree -> cont 61;
	TContinue -> cont 62;
	TBreak -> cont 63;
	TTyInt -> cont 64;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 65 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
happy tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error: " ++ show xs)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
