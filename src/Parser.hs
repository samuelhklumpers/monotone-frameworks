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
happyExpList = Happy_Data_Array.listArray (0,417) ([0,0,0,16,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,8402,5120,2001,0,0,0,0,0,3364,16386,32009,0,0,0,0,0,0,0,0,0,0,24,4,0,7168,528,80,0,28672,2112,320,0,0,0,1024,0,0,0,512,0,0,12,20482,0,0,13456,8,62469,1,53824,32,53268,7,256,0,0,0,1024,0,0,0,0,0,256,0,0,0,1024,0,0,0,512,0,0,0,2048,0,0,16,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,12288,2048,320,0,16384,0,0,0,0,0,4096,0,0,0,16384,0,0,13456,8,62477,1,53824,32,53276,7,0,8,0,0,0,0,16,0,0,0,0,0,49152,8192,1280,0,0,32771,5120,0,0,12,20482,0,0,0,0,0,0,448,33,5,0,0,61920,1,0,0,0,0,0,0,57352,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,16496,16392,1,0,448,33,5,0,4096,3584,0,0,3072,512,80,0,28672,2112,320,0,49152,8192,1280,0,0,0,0,0,0,0,0,0,0,0,32798,0,0,0,33664,0,0,0,480,2,0,1024,524,320,125,28672,2112,320,0,49152,8449,1280,0,0,33799,5120,0,0,32768,34759,0,0,0,224,2,0,0,0,0,0,768,128,20,0,1024,524,320,125,12288,2048,320,0,49152,8192,1280,0,0,32771,5120,0,0,12,20482,0,0,48,16392,1,0,192,32,5,0,768,128,20,0,3072,512,80,0,12288,2048,320,0,0,0,2048,0,0,57344,497,0,0,0,56,0,0,0,30,2,0,0,120,2,0,0,0,0,0,3072,512,80,0,0,0,0,0,0,0,0,0,0,0,0,16,0,4124,20482,0,0,0,0,2048,0,0,0,8,0,768,128,20,0,0,1920,32,0,0,0,32,0,49152,8192,1280,0,0,0,0,128,0,0,0,0,0,0,0,512,0,64,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,128,0,0,57344,1,0,0,32768,7,0,0,0,30,0,0,0,120,0,0,0,480,0,0,0,0,0,0,0,0,0,0,0,24576,0,0,0,32768,1,0,0,0,0,0,0,0,32798,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,192,32,5,0,256,131,16464,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,8,0,256,0,0,0,7168,528,80,0,0,7680,512,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,36,0,0,0,0,0,0,0,32768,0,16384,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,18688,131,16464,31,0,0,8,0,0,0,0,0,0,0,0,0,0,33609,20480,8002,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happy","Start","Program","Procs","Proc","ValArgs","ResArg","Args","Arg","Stats","Stat","Stat0","CallArgs","EitherExpr","AExpr","AExpr0","BExpr","BExpr0","ident","int","bool","if","then","else","while","do","print","skip","not","\":=\"","\"b=\"","\"+\"","\"-\"","\"*\"","\"/\"","and","or","\"<=>\"","\"==\"","\"<\"","\">\"","\"<=\"","\">=\"","\";\"","\"[\"","\"]\"","\"(\"","\")\"","\"{\"","\"}\"","begin","end","proc","is","val","res","call","\",\"","malloc","free","continue","break","tyint","%eof"]
        bit_start = st Prelude.* 66
        bit_end = (st Prelude.+ 1) Prelude.* 66
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..65]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (53) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (53) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyGoto action_5
action_3 _ = happyReduce_4

action_4 (66) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (21) = happyShift action_10
action_5 (24) = happyShift action_11
action_5 (27) = happyShift action_12
action_5 (29) = happyShift action_13
action_5 (30) = happyShift action_14
action_5 (36) = happyShift action_15
action_5 (49) = happyShift action_16
action_5 (51) = happyShift action_17
action_5 (55) = happyShift action_18
action_5 (59) = happyShift action_19
action_5 (61) = happyShift action_20
action_5 (62) = happyShift action_21
action_5 (63) = happyShift action_22
action_5 (64) = happyShift action_23
action_5 (65) = happyShift action_24
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
action_7 (30) = happyShift action_14
action_7 (36) = happyShift action_15
action_7 (49) = happyShift action_16
action_7 (51) = happyShift action_17
action_7 (54) = happyShift action_55
action_7 (59) = happyShift action_19
action_7 (61) = happyShift action_20
action_7 (62) = happyShift action_21
action_7 (63) = happyShift action_22
action_7 (64) = happyShift action_23
action_7 (65) = happyShift action_24
action_7 (13) = happyGoto action_54
action_7 (14) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_13

action_9 _ = happyReduce_17

action_10 (32) = happyShift action_51
action_10 (33) = happyShift action_52
action_10 (47) = happyShift action_53
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (21) = happyShift action_46
action_11 (22) = happyShift action_36
action_11 (23) = happyShift action_47
action_11 (31) = happyShift action_48
action_11 (36) = happyShift action_37
action_11 (47) = happyShift action_38
action_11 (49) = happyShift action_49
action_11 (17) = happyGoto action_42
action_11 (18) = happyGoto action_43
action_11 (19) = happyGoto action_50
action_11 (20) = happyGoto action_45
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (21) = happyShift action_46
action_12 (22) = happyShift action_36
action_12 (23) = happyShift action_47
action_12 (31) = happyShift action_48
action_12 (36) = happyShift action_37
action_12 (47) = happyShift action_38
action_12 (49) = happyShift action_49
action_12 (17) = happyGoto action_42
action_12 (18) = happyGoto action_43
action_12 (19) = happyGoto action_44
action_12 (20) = happyGoto action_45
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (49) = happyShift action_41
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (46) = happyShift action_40
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (21) = happyShift action_35
action_15 (22) = happyShift action_36
action_15 (36) = happyShift action_37
action_15 (47) = happyShift action_38
action_15 (49) = happyShift action_39
action_15 (18) = happyGoto action_34
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (21) = happyShift action_10
action_16 (24) = happyShift action_11
action_16 (27) = happyShift action_12
action_16 (29) = happyShift action_13
action_16 (30) = happyShift action_14
action_16 (36) = happyShift action_15
action_16 (49) = happyShift action_16
action_16 (51) = happyShift action_17
action_16 (59) = happyShift action_19
action_16 (61) = happyShift action_20
action_16 (62) = happyShift action_21
action_16 (63) = happyShift action_22
action_16 (64) = happyShift action_23
action_16 (65) = happyShift action_24
action_16 (12) = happyGoto action_33
action_16 (13) = happyGoto action_8
action_16 (14) = happyGoto action_9
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_10
action_17 (24) = happyShift action_11
action_17 (27) = happyShift action_12
action_17 (29) = happyShift action_13
action_17 (30) = happyShift action_14
action_17 (36) = happyShift action_15
action_17 (49) = happyShift action_16
action_17 (51) = happyShift action_17
action_17 (59) = happyShift action_19
action_17 (61) = happyShift action_20
action_17 (62) = happyShift action_21
action_17 (63) = happyShift action_22
action_17 (64) = happyShift action_23
action_17 (65) = happyShift action_24
action_17 (12) = happyGoto action_32
action_17 (13) = happyGoto action_8
action_17 (14) = happyGoto action_9
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (21) = happyShift action_31
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (21) = happyShift action_30
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (49) = happyShift action_29
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (49) = happyShift action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (46) = happyShift action_27
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (46) = happyShift action_26
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (21) = happyShift action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (47) = happyShift action_90
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_29

action_27 _ = happyReduce_28

action_28 (21) = happyShift action_35
action_28 (22) = happyShift action_36
action_28 (36) = happyShift action_37
action_28 (47) = happyShift action_38
action_28 (49) = happyShift action_39
action_28 (18) = happyGoto action_89
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (21) = happyShift action_88
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (49) = happyShift action_87
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (49) = happyShift action_86
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (21) = happyShift action_10
action_32 (24) = happyShift action_11
action_32 (27) = happyShift action_12
action_32 (29) = happyShift action_13
action_32 (30) = happyShift action_14
action_32 (36) = happyShift action_15
action_32 (49) = happyShift action_16
action_32 (51) = happyShift action_17
action_32 (52) = happyShift action_85
action_32 (59) = happyShift action_19
action_32 (61) = happyShift action_20
action_32 (62) = happyShift action_21
action_32 (63) = happyShift action_22
action_32 (64) = happyShift action_23
action_32 (65) = happyShift action_24
action_32 (13) = happyGoto action_54
action_32 (14) = happyGoto action_9
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (21) = happyShift action_10
action_33 (24) = happyShift action_11
action_33 (27) = happyShift action_12
action_33 (29) = happyShift action_13
action_33 (30) = happyShift action_14
action_33 (36) = happyShift action_15
action_33 (49) = happyShift action_16
action_33 (50) = happyShift action_84
action_33 (51) = happyShift action_17
action_33 (59) = happyShift action_19
action_33 (61) = happyShift action_20
action_33 (62) = happyShift action_21
action_33 (63) = happyShift action_22
action_33 (64) = happyShift action_23
action_33 (65) = happyShift action_24
action_33 (13) = happyGoto action_54
action_33 (14) = happyGoto action_9
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (32) = happyShift action_83
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (47) = happyShift action_66
action_35 _ = happyReduce_43

action_36 _ = happyReduce_42

action_37 (21) = happyShift action_35
action_37 (22) = happyShift action_36
action_37 (36) = happyShift action_37
action_37 (47) = happyShift action_38
action_37 (49) = happyShift action_39
action_37 (18) = happyGoto action_82
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (21) = happyShift action_35
action_38 (22) = happyShift action_36
action_38 (36) = happyShift action_37
action_38 (47) = happyShift action_38
action_38 (49) = happyShift action_39
action_38 (17) = happyGoto action_81
action_38 (18) = happyGoto action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (21) = happyShift action_35
action_39 (22) = happyShift action_36
action_39 (36) = happyShift action_37
action_39 (47) = happyShift action_38
action_39 (49) = happyShift action_39
action_39 (17) = happyGoto action_80
action_39 (18) = happyGoto action_43
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_18

action_41 (21) = happyShift action_46
action_41 (22) = happyShift action_36
action_41 (23) = happyShift action_47
action_41 (31) = happyShift action_48
action_41 (36) = happyShift action_37
action_41 (47) = happyShift action_38
action_41 (49) = happyShift action_49
action_41 (16) = happyGoto action_77
action_41 (17) = happyGoto action_78
action_41 (18) = happyGoto action_43
action_41 (19) = happyGoto action_79
action_41 (20) = happyGoto action_45
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (34) = happyShift action_68
action_42 (35) = happyShift action_69
action_42 (36) = happyShift action_70
action_42 (37) = happyShift action_71
action_42 (41) = happyShift action_72
action_42 (42) = happyShift action_73
action_42 (43) = happyShift action_74
action_42 (44) = happyShift action_75
action_42 (45) = happyShift action_76
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_41

action_44 (28) = happyShift action_67
action_44 (38) = happyShift action_60
action_44 (39) = happyShift action_61
action_44 (40) = happyShift action_62
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_57

action_46 (34) = happyReduce_43
action_46 (35) = happyReduce_43
action_46 (36) = happyReduce_43
action_46 (37) = happyReduce_43
action_46 (41) = happyReduce_43
action_46 (42) = happyReduce_43
action_46 (43) = happyReduce_43
action_46 (44) = happyReduce_43
action_46 (45) = happyReduce_43
action_46 (47) = happyShift action_66
action_46 (50) = happyReduce_59
action_46 (60) = happyReduce_59
action_46 _ = happyReduce_59

action_47 _ = happyReduce_58

action_48 (21) = happyShift action_46
action_48 (22) = happyShift action_36
action_48 (23) = happyShift action_47
action_48 (31) = happyShift action_48
action_48 (36) = happyShift action_37
action_48 (47) = happyShift action_38
action_48 (49) = happyShift action_49
action_48 (17) = happyGoto action_42
action_48 (18) = happyGoto action_43
action_48 (19) = happyGoto action_65
action_48 (20) = happyGoto action_45
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (21) = happyShift action_46
action_49 (22) = happyShift action_36
action_49 (23) = happyShift action_47
action_49 (31) = happyShift action_48
action_49 (36) = happyShift action_37
action_49 (47) = happyShift action_38
action_49 (49) = happyShift action_49
action_49 (17) = happyGoto action_63
action_49 (18) = happyGoto action_43
action_49 (19) = happyGoto action_64
action_49 (20) = happyGoto action_45
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (25) = happyShift action_59
action_50 (38) = happyShift action_60
action_50 (39) = happyShift action_61
action_50 (40) = happyShift action_62
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (21) = happyShift action_35
action_51 (22) = happyShift action_36
action_51 (36) = happyShift action_37
action_51 (47) = happyShift action_38
action_51 (49) = happyShift action_39
action_51 (17) = happyGoto action_58
action_51 (18) = happyGoto action_43
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (21) = happyShift action_46
action_52 (22) = happyShift action_36
action_52 (23) = happyShift action_47
action_52 (31) = happyShift action_48
action_52 (36) = happyShift action_37
action_52 (47) = happyShift action_38
action_52 (49) = happyShift action_49
action_52 (17) = happyGoto action_42
action_52 (18) = happyGoto action_43
action_52 (19) = happyGoto action_57
action_52 (20) = happyGoto action_45
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (21) = happyShift action_35
action_53 (22) = happyShift action_36
action_53 (36) = happyShift action_37
action_53 (47) = happyShift action_38
action_53 (49) = happyShift action_39
action_53 (17) = happyGoto action_56
action_53 (18) = happyGoto action_43
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_12

action_55 _ = happyReduce_2

action_56 (34) = happyShift action_68
action_56 (35) = happyShift action_69
action_56 (36) = happyShift action_70
action_56 (37) = happyShift action_71
action_56 (48) = happyShift action_120
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (38) = happyShift action_60
action_57 (39) = happyShift action_61
action_57 (40) = happyShift action_62
action_57 (46) = happyShift action_119
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (34) = happyShift action_68
action_58 (35) = happyShift action_69
action_58 (36) = happyShift action_70
action_58 (37) = happyShift action_71
action_58 (46) = happyShift action_118
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (21) = happyShift action_10
action_59 (29) = happyShift action_13
action_59 (30) = happyShift action_14
action_59 (36) = happyShift action_15
action_59 (49) = happyShift action_16
action_59 (51) = happyShift action_17
action_59 (59) = happyShift action_19
action_59 (61) = happyShift action_20
action_59 (62) = happyShift action_21
action_59 (63) = happyShift action_22
action_59 (64) = happyShift action_23
action_59 (65) = happyShift action_24
action_59 (14) = happyGoto action_117
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (21) = happyShift action_46
action_60 (22) = happyShift action_36
action_60 (23) = happyShift action_47
action_60 (31) = happyShift action_48
action_60 (36) = happyShift action_37
action_60 (47) = happyShift action_38
action_60 (49) = happyShift action_49
action_60 (17) = happyGoto action_42
action_60 (18) = happyGoto action_43
action_60 (19) = happyGoto action_116
action_60 (20) = happyGoto action_45
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (21) = happyShift action_46
action_61 (22) = happyShift action_36
action_61 (23) = happyShift action_47
action_61 (31) = happyShift action_48
action_61 (36) = happyShift action_37
action_61 (47) = happyShift action_38
action_61 (49) = happyShift action_49
action_61 (17) = happyGoto action_42
action_61 (18) = happyGoto action_43
action_61 (19) = happyGoto action_115
action_61 (20) = happyGoto action_45
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (21) = happyShift action_46
action_62 (22) = happyShift action_36
action_62 (23) = happyShift action_47
action_62 (31) = happyShift action_48
action_62 (36) = happyShift action_37
action_62 (47) = happyShift action_38
action_62 (49) = happyShift action_49
action_62 (17) = happyGoto action_42
action_62 (18) = happyGoto action_43
action_62 (19) = happyGoto action_114
action_62 (20) = happyGoto action_45
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (34) = happyShift action_68
action_63 (35) = happyShift action_69
action_63 (36) = happyShift action_70
action_63 (37) = happyShift action_71
action_63 (41) = happyShift action_72
action_63 (42) = happyShift action_73
action_63 (43) = happyShift action_74
action_63 (44) = happyShift action_75
action_63 (45) = happyShift action_76
action_63 (50) = happyShift action_100
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (38) = happyShift action_60
action_64 (39) = happyShift action_61
action_64 (40) = happyShift action_62
action_64 (50) = happyShift action_113
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (40) = happyShift action_62
action_65 _ = happyReduce_48

action_66 (21) = happyShift action_35
action_66 (22) = happyShift action_36
action_66 (36) = happyShift action_37
action_66 (47) = happyShift action_38
action_66 (49) = happyShift action_39
action_66 (17) = happyGoto action_112
action_66 (18) = happyGoto action_43
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (21) = happyShift action_10
action_67 (29) = happyShift action_13
action_67 (30) = happyShift action_14
action_67 (36) = happyShift action_15
action_67 (49) = happyShift action_16
action_67 (51) = happyShift action_17
action_67 (59) = happyShift action_19
action_67 (61) = happyShift action_20
action_67 (62) = happyShift action_21
action_67 (63) = happyShift action_22
action_67 (64) = happyShift action_23
action_67 (65) = happyShift action_24
action_67 (14) = happyGoto action_111
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (21) = happyShift action_35
action_68 (22) = happyShift action_36
action_68 (36) = happyShift action_37
action_68 (47) = happyShift action_38
action_68 (49) = happyShift action_39
action_68 (17) = happyGoto action_110
action_68 (18) = happyGoto action_43
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (21) = happyShift action_35
action_69 (22) = happyShift action_36
action_69 (36) = happyShift action_37
action_69 (47) = happyShift action_38
action_69 (49) = happyShift action_39
action_69 (17) = happyGoto action_109
action_69 (18) = happyGoto action_43
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (21) = happyShift action_35
action_70 (22) = happyShift action_36
action_70 (36) = happyShift action_37
action_70 (47) = happyShift action_38
action_70 (49) = happyShift action_39
action_70 (17) = happyGoto action_108
action_70 (18) = happyGoto action_43
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (21) = happyShift action_35
action_71 (22) = happyShift action_36
action_71 (36) = happyShift action_37
action_71 (47) = happyShift action_38
action_71 (49) = happyShift action_39
action_71 (17) = happyGoto action_107
action_71 (18) = happyGoto action_43
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (21) = happyShift action_35
action_72 (22) = happyShift action_36
action_72 (36) = happyShift action_37
action_72 (47) = happyShift action_38
action_72 (49) = happyShift action_39
action_72 (17) = happyGoto action_106
action_72 (18) = happyGoto action_43
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (21) = happyShift action_35
action_73 (22) = happyShift action_36
action_73 (36) = happyShift action_37
action_73 (47) = happyShift action_38
action_73 (49) = happyShift action_39
action_73 (17) = happyGoto action_105
action_73 (18) = happyGoto action_43
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (21) = happyShift action_35
action_74 (22) = happyShift action_36
action_74 (36) = happyShift action_37
action_74 (47) = happyShift action_38
action_74 (49) = happyShift action_39
action_74 (17) = happyGoto action_104
action_74 (18) = happyGoto action_43
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (21) = happyShift action_35
action_75 (22) = happyShift action_36
action_75 (36) = happyShift action_37
action_75 (47) = happyShift action_38
action_75 (49) = happyShift action_39
action_75 (17) = happyGoto action_103
action_75 (18) = happyGoto action_43
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (21) = happyShift action_35
action_76 (22) = happyShift action_36
action_76 (36) = happyShift action_37
action_76 (47) = happyShift action_38
action_76 (49) = happyShift action_39
action_76 (17) = happyGoto action_102
action_76 (18) = happyGoto action_43
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (50) = happyShift action_101
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (34) = happyShift action_68
action_78 (35) = happyShift action_69
action_78 (36) = happyShift action_70
action_78 (37) = happyShift action_71
action_78 (41) = happyShift action_72
action_78 (42) = happyShift action_73
action_78 (43) = happyShift action_74
action_78 (44) = happyShift action_75
action_78 (45) = happyShift action_76
action_78 _ = happyReduce_35

action_79 (38) = happyShift action_60
action_79 (39) = happyShift action_61
action_79 (40) = happyShift action_62
action_79 _ = happyReduce_36

action_80 (34) = happyShift action_68
action_80 (35) = happyShift action_69
action_80 (36) = happyShift action_70
action_80 (37) = happyShift action_71
action_80 (50) = happyShift action_100
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (34) = happyShift action_68
action_81 (35) = happyShift action_69
action_81 (36) = happyShift action_70
action_81 (37) = happyShift action_71
action_81 (48) = happyShift action_99
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_46

action_83 (21) = happyShift action_35
action_83 (22) = happyShift action_36
action_83 (36) = happyShift action_37
action_83 (47) = happyShift action_38
action_83 (49) = happyShift action_39
action_83 (18) = happyGoto action_98
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_26

action_85 _ = happyReduce_27

action_86 (57) = happyShift action_97
action_86 (8) = happyGoto action_96
action_86 _ = happyReduce_7

action_87 (21) = happyShift action_46
action_87 (22) = happyShift action_36
action_87 (23) = happyShift action_47
action_87 (31) = happyShift action_48
action_87 (36) = happyShift action_37
action_87 (47) = happyShift action_38
action_87 (49) = happyShift action_49
action_87 (15) = happyGoto action_94
action_87 (16) = happyGoto action_95
action_87 (17) = happyGoto action_78
action_87 (18) = happyGoto action_43
action_87 (19) = happyGoto action_79
action_87 (20) = happyGoto action_45
action_87 _ = happyReduce_34

action_88 (60) = happyShift action_93
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (50) = happyShift action_92
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (21) = happyShift action_35
action_90 (22) = happyShift action_36
action_90 (36) = happyShift action_37
action_90 (47) = happyShift action_38
action_90 (49) = happyShift action_39
action_90 (17) = happyGoto action_91
action_90 (18) = happyGoto action_43
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (34) = happyShift action_68
action_91 (35) = happyShift action_69
action_91 (36) = happyShift action_70
action_91 (37) = happyShift action_71
action_91 (48) = happyShift action_134
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (46) = happyShift action_133
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (21) = happyShift action_35
action_93 (22) = happyShift action_36
action_93 (36) = happyShift action_37
action_93 (47) = happyShift action_38
action_93 (49) = happyShift action_39
action_93 (17) = happyGoto action_132
action_93 (18) = happyGoto action_43
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (60) = happyShift action_131
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_33

action_96 (58) = happyShift action_130
action_96 (9) = happyGoto action_129
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (21) = happyShift action_128
action_97 (10) = happyGoto action_126
action_97 (11) = happyGoto action_127
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (46) = happyShift action_125
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_45

action_100 _ = happyReduce_44

action_101 (46) = happyShift action_124
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (34) = happyShift action_68
action_102 (35) = happyShift action_69
action_102 (36) = happyShift action_70
action_102 (37) = happyShift action_71
action_102 _ = happyReduce_55

action_103 (34) = happyShift action_68
action_103 (35) = happyShift action_69
action_103 (36) = happyShift action_70
action_103 (37) = happyShift action_71
action_103 _ = happyReduce_56

action_104 (34) = happyShift action_68
action_104 (35) = happyShift action_69
action_104 (36) = happyShift action_70
action_104 (37) = happyShift action_71
action_104 _ = happyReduce_54

action_105 (34) = happyShift action_68
action_105 (35) = happyShift action_69
action_105 (36) = happyShift action_70
action_105 (37) = happyShift action_71
action_105 _ = happyReduce_53

action_106 (34) = happyShift action_68
action_106 (35) = happyShift action_69
action_106 (36) = happyShift action_70
action_106 (37) = happyShift action_71
action_106 _ = happyReduce_52

action_107 _ = happyReduce_40

action_108 _ = happyReduce_39

action_109 (36) = happyShift action_70
action_109 (37) = happyShift action_71
action_109 _ = happyReduce_38

action_110 (36) = happyShift action_70
action_110 (37) = happyShift action_71
action_110 _ = happyReduce_37

action_111 _ = happyReduce_16

action_112 (34) = happyShift action_68
action_112 (35) = happyShift action_69
action_112 (36) = happyShift action_70
action_112 (37) = happyShift action_71
action_112 (48) = happyShift action_123
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_60

action_114 (38) = happyShift action_60
action_114 (39) = happyShift action_61
action_114 (40) = happyShift action_62
action_114 _ = happyReduce_51

action_115 (38) = happyShift action_60
action_115 (40) = happyShift action_62
action_115 _ = happyReduce_50

action_116 (40) = happyShift action_62
action_116 _ = happyReduce_49

action_117 (26) = happyShift action_122
action_117 _ = happyReduce_15

action_118 _ = happyReduce_19

action_119 _ = happyReduce_20

action_120 (32) = happyShift action_121
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (21) = happyShift action_35
action_121 (22) = happyShift action_36
action_121 (36) = happyShift action_37
action_121 (47) = happyShift action_38
action_121 (49) = happyShift action_39
action_121 (18) = happyGoto action_143
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (21) = happyShift action_10
action_122 (29) = happyShift action_13
action_122 (30) = happyShift action_14
action_122 (36) = happyShift action_15
action_122 (49) = happyShift action_16
action_122 (51) = happyShift action_17
action_122 (59) = happyShift action_19
action_122 (61) = happyShift action_20
action_122 (62) = happyShift action_21
action_122 (63) = happyShift action_22
action_122 (64) = happyShift action_23
action_122 (65) = happyShift action_24
action_122 (14) = happyGoto action_142
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_47

action_124 _ = happyReduce_21

action_125 _ = happyReduce_22

action_126 (60) = happyShift action_141
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_10

action_128 _ = happyReduce_11

action_129 (50) = happyShift action_140
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (21) = happyShift action_128
action_130 (11) = happyGoto action_139
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (21) = happyShift action_138
action_131 (22) = happyShift action_36
action_131 (23) = happyShift action_47
action_131 (31) = happyShift action_48
action_131 (36) = happyShift action_37
action_131 (47) = happyShift action_38
action_131 (49) = happyShift action_49
action_131 (16) = happyGoto action_137
action_131 (17) = happyGoto action_78
action_131 (18) = happyGoto action_43
action_131 (19) = happyGoto action_79
action_131 (20) = happyGoto action_45
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (34) = happyShift action_68
action_132 (35) = happyShift action_69
action_132 (36) = happyShift action_70
action_132 (37) = happyShift action_71
action_132 (50) = happyShift action_136
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_25

action_134 (46) = happyShift action_135
action_134 _ = happyFail (happyExpListPerState 134)

action_135 _ = happyReduce_30

action_136 (46) = happyShift action_148
action_136 _ = happyFail (happyExpListPerState 136)

action_137 _ = happyReduce_32

action_138 (38) = happyReduce_59
action_138 (39) = happyReduce_59
action_138 (40) = happyReduce_59
action_138 (47) = happyShift action_66
action_138 (50) = happyShift action_147
action_138 (60) = happyReduce_59
action_138 _ = happyReduce_43

action_139 _ = happyReduce_8

action_140 (56) = happyShift action_146
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (21) = happyShift action_128
action_141 (11) = happyGoto action_145
action_141 _ = happyReduce_6

action_142 _ = happyReduce_14

action_143 (46) = happyShift action_144
action_143 _ = happyFail (happyExpListPerState 143)

action_144 _ = happyReduce_31

action_145 _ = happyReduce_9

action_146 (21) = happyShift action_10
action_146 (24) = happyShift action_11
action_146 (27) = happyShift action_12
action_146 (29) = happyShift action_13
action_146 (30) = happyShift action_14
action_146 (36) = happyShift action_15
action_146 (49) = happyShift action_16
action_146 (51) = happyShift action_17
action_146 (59) = happyShift action_19
action_146 (61) = happyShift action_20
action_146 (62) = happyShift action_21
action_146 (63) = happyShift action_22
action_146 (64) = happyShift action_23
action_146 (65) = happyShift action_24
action_146 (12) = happyGoto action_150
action_146 (13) = happyGoto action_8
action_146 (14) = happyGoto action_9
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (46) = happyShift action_149
action_147 _ = happyFail (happyExpListPerState 147)

action_148 _ = happyReduce_24

action_149 _ = happyReduce_23

action_150 (21) = happyShift action_10
action_150 (24) = happyShift action_11
action_150 (27) = happyShift action_12
action_150 (29) = happyShift action_13
action_150 (30) = happyShift action_14
action_150 (36) = happyShift action_15
action_150 (49) = happyShift action_16
action_150 (51) = happyShift action_17
action_150 (54) = happyShift action_151
action_150 (59) = happyShift action_19
action_150 (61) = happyShift action_20
action_150 (62) = happyShift action_21
action_150 (63) = happyShift action_22
action_150 (64) = happyShift action_23
action_150 (65) = happyShift action_24
action_150 (13) = happyGoto action_54
action_150 (14) = happyGoto action_9
action_150 _ = happyFail (happyExpListPerState 150)

action_151 _ = happyReduce_5

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
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Print happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 14 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (RefAssign happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 8 14 happyReduction_23
happyReduction_23 (_ `HappyStk`
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

happyReduce_24 = happyReduce 7 14 happyReduction_24
happyReduction_24 (_ `HappyStk`
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

happyReduce_25 = happyReduce 5 14 happyReduction_25
happyReduction_25 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Free happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  14 happyReduction_28
happyReduction_28 _
	_
	 =  HappyAbsSyn14
		 (Continue
	)

happyReduce_29 = happySpecReduce_2  14 happyReduction_29
happyReduction_29 _
	_
	 =  HappyAbsSyn14
		 (Break
	)

happyReduce_30 = happyReduce 6 14 happyReduction_30
happyReduction_30 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Malloc happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 7 14 happyReduction_31
happyReduction_31 (_ `HappyStk`
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

happyReduce_32 = happySpecReduce_3  15 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [ happy_var_3 ]
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([ happy_var_1 ]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  15 happyReduction_34
happyReduction_34  =  HappyAbsSyn15
		 ([]
	)

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (I happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  16 happyReduction_36
happyReduction_36 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn16
		 (B happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  17 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  17 happyReduction_38
happyReduction_38 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  17 happyReduction_39
happyReduction_39 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Times happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  17 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Divide happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  17 happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  18 happyReduction_42
happyReduction_42 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn18
		 (IConst happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  18 happyReduction_43
happyReduction_43 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn18
		 (Var happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  18 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  18 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Deref happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 4 18 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Deref (Plus (Var happy_var_1) happy_var_3)
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_2  19 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Not happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  19 happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (And happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  19 happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Or happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  19 happyReduction_51
happyReduction_51 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (BEqual happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  19 happyReduction_52
happyReduction_52 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (IEqual happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  19 happyReduction_53
happyReduction_53 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  19 happyReduction_54
happyReduction_54 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterThan happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  19 happyReduction_55
happyReduction_55 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  19 happyReduction_56
happyReduction_56 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  19 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  20 happyReduction_58
happyReduction_58 (HappyTerminal (TBool happy_var_1))
	 =  HappyAbsSyn20
		 (BConst happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  20 happyReduction_59
happyReduction_59 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn20
		 (BVar happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  20 happyReduction_60
happyReduction_60 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 66 66 notHappyAtAll (HappyState action) sts stk []

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
	TPrint -> cont 29;
	TSkip -> cont 30;
	TNot -> cont 31;
	TIAssign -> cont 32;
	TBAssign -> cont 33;
	TArithmeticOp "+" -> cont 34;
	TArithmeticOp "-" -> cont 35;
	TStar -> cont 36;
	TArithmeticOp "/" -> cont 37;
	TBoolOp "and" -> cont 38;
	TBoolOp "or" -> cont 39;
	TBoolOp "<=>" -> cont 40;
	TRelOp "==" -> cont 41;
	TRelOp "<" -> cont 42;
	TRelOp ">" -> cont 43;
	TRelOp "<=" -> cont 44;
	TRelOp ">=" -> cont 45;
	TSemicolon -> cont 46;
	TBlockOpen -> cont 47;
	TBlockClose -> cont 48;
	TParenOpen -> cont 49;
	TParenClose -> cont 50;
	TBraceOpen -> cont 51;
	TBraceClose -> cont 52;
	TBegin -> cont 53;
	TEnd -> cont 54;
	TProc -> cont 55;
	TIs -> cont 56;
	TVal -> cont 57;
	TRes -> cont 58;
	TCall -> cont 59;
	TComma -> cont 60;
	TMalloc -> cont 61;
	TFree -> cont 62;
	TContinue -> cont 63;
	TBreak -> cont 64;
	TTyInt -> cont 65;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 66 tk tks = happyError' (tks, explist)
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
