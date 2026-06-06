# Generated from src/mdl/grammar/MDL.g4 by ANTLR 4.13.2
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO

def serializedATN():
    return [
        4,1,71,756,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,
        2,14,7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,
        7,20,2,21,7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,26,
        2,27,7,27,2,28,7,28,2,29,7,29,2,30,7,30,2,31,7,31,2,32,7,32,2,33,
        7,33,2,34,7,34,2,35,7,35,2,36,7,36,2,37,7,37,2,38,7,38,2,39,7,39,
        2,40,7,40,2,41,7,41,2,42,7,42,2,43,7,43,2,44,7,44,2,45,7,45,2,46,
        7,46,2,47,7,47,2,48,7,48,2,49,7,49,2,50,7,50,2,51,7,51,2,52,7,52,
        2,53,7,53,2,54,7,54,2,55,7,55,2,56,7,56,2,57,7,57,2,58,7,58,2,59,
        7,59,2,60,7,60,2,61,7,61,2,62,7,62,2,63,7,63,2,64,7,64,2,65,7,65,
        2,66,7,66,2,67,7,67,1,0,3,0,138,8,0,1,0,1,0,1,0,3,0,143,8,0,1,0,
        5,0,146,8,0,10,0,12,0,149,9,0,1,0,1,0,1,1,3,1,154,8,1,1,1,1,1,3,
        1,158,8,1,1,1,1,1,1,2,3,2,163,8,2,1,2,1,2,3,2,167,8,2,1,2,1,2,1,
        3,1,3,1,3,1,3,3,3,175,8,3,1,3,3,3,178,8,3,1,4,1,4,3,4,182,8,4,5,
        4,184,8,4,10,4,12,4,187,9,4,1,5,1,5,1,5,1,6,1,6,1,6,1,7,1,7,1,7,
        1,8,1,8,1,8,1,8,1,8,1,8,1,8,3,8,205,8,8,1,9,1,9,1,9,3,9,210,8,9,
        1,9,1,9,1,9,1,10,1,10,1,10,1,10,5,10,219,8,10,10,10,12,10,222,9,
        10,3,10,224,8,10,1,11,1,11,1,11,1,11,1,12,1,12,1,12,5,12,233,8,12,
        10,12,12,12,236,9,12,1,12,3,12,239,8,12,1,13,1,13,1,13,1,13,1,13,
        1,14,1,14,1,14,5,14,249,8,14,10,14,12,14,252,9,14,1,14,3,14,255,
        8,14,1,15,1,15,1,15,1,15,1,15,3,15,262,8,15,1,16,1,16,1,16,3,16,
        267,8,16,1,17,1,17,3,17,271,8,17,1,17,1,17,1,18,1,18,1,18,5,18,278,
        8,18,10,18,12,18,281,9,18,1,18,3,18,284,8,18,1,19,1,19,1,19,1,19,
        1,20,1,20,1,20,1,20,1,20,1,20,1,20,1,20,1,20,1,20,5,20,300,8,20,
        10,20,12,20,303,9,20,1,20,1,20,3,20,307,8,20,1,21,1,21,3,21,311,
        8,21,1,22,1,22,1,22,1,22,1,23,1,23,1,23,5,23,320,8,23,10,23,12,23,
        323,9,23,1,24,1,24,1,24,3,24,328,8,24,1,24,1,24,1,24,1,25,1,25,1,
        25,3,25,336,8,25,1,25,1,25,3,25,340,8,25,1,25,1,25,1,25,1,25,1,25,
        1,25,1,26,1,26,1,26,5,26,351,8,26,10,26,12,26,354,9,26,1,26,3,26,
        357,8,26,1,27,1,27,1,27,1,27,1,28,1,28,1,28,1,28,1,28,1,29,3,29,
        369,8,29,1,29,1,29,1,29,1,29,3,29,375,8,29,1,30,1,30,1,31,1,31,1,
        31,1,31,1,31,3,31,384,8,31,1,31,1,31,1,31,3,31,389,8,31,1,31,1,31,
        1,31,3,31,394,8,31,1,32,1,32,1,33,1,33,1,33,1,33,5,33,402,8,33,10,
        33,12,33,405,9,33,1,34,1,34,1,34,1,34,3,34,411,8,34,1,34,1,34,1,
        35,1,35,1,35,5,35,418,8,35,10,35,12,35,421,9,35,1,35,3,35,424,8,
        35,1,35,3,35,427,8,35,1,35,1,35,3,35,431,8,35,1,36,1,36,1,36,3,36,
        436,8,36,1,36,1,36,1,36,1,36,1,37,1,37,1,37,1,38,1,38,1,39,1,39,
        5,39,449,8,39,10,39,12,39,452,9,39,1,40,1,40,1,41,1,41,1,41,5,41,
        459,8,41,10,41,12,41,462,9,41,1,42,1,42,1,42,5,42,467,8,42,10,42,
        12,42,470,9,42,1,43,1,43,1,43,5,43,475,8,43,10,43,12,43,478,9,43,
        1,44,1,44,1,44,5,44,483,8,44,10,44,12,44,486,9,44,1,45,1,45,1,45,
        5,45,491,8,45,10,45,12,45,494,9,45,1,46,1,46,1,46,5,46,499,8,46,
        10,46,12,46,502,9,46,1,47,1,47,1,47,1,47,1,47,1,47,3,47,510,8,47,
        1,47,1,47,3,47,514,8,47,1,48,1,48,1,48,3,48,519,8,48,1,48,1,48,1,
        48,3,48,524,8,48,1,48,1,48,1,48,1,49,1,49,1,49,3,49,532,8,49,1,49,
        1,49,1,49,3,49,537,8,49,1,49,1,49,1,49,1,50,1,50,1,50,1,50,1,50,
        1,51,1,51,1,51,3,51,550,8,51,1,51,1,51,3,51,554,8,51,1,51,5,51,557,
        8,51,10,51,12,51,560,9,51,1,51,3,51,563,8,51,1,51,1,51,1,51,1,51,
        3,51,569,8,51,1,51,1,51,3,51,573,8,51,1,51,5,51,576,8,51,10,51,12,
        51,579,9,51,1,51,3,51,582,8,51,3,51,584,8,51,1,52,1,52,1,52,1,52,
        3,52,590,8,52,1,52,1,52,1,52,1,53,1,53,5,53,597,8,53,10,53,12,53,
        600,9,53,1,54,1,54,1,54,3,54,605,8,54,1,54,1,54,1,54,3,54,610,8,
        54,1,55,1,55,3,55,614,8,55,1,55,1,55,1,56,1,56,1,56,5,56,621,8,56,
        10,56,12,56,624,9,56,1,56,3,56,627,8,56,1,57,1,57,1,57,1,57,1,58,
        1,58,1,58,1,58,1,58,1,58,1,58,1,58,1,58,1,58,1,58,1,58,1,58,1,58,
        1,58,1,58,1,58,1,58,1,58,1,58,5,58,653,8,58,10,58,12,58,656,9,58,
        1,58,1,58,3,58,660,8,58,1,59,1,59,1,59,5,59,665,8,59,10,59,12,59,
        668,9,59,1,59,3,59,671,8,59,1,60,1,60,1,60,1,60,1,60,1,60,1,60,1,
        60,1,60,1,60,1,60,1,60,1,60,1,60,1,60,1,60,1,60,5,60,690,8,60,10,
        60,12,60,693,9,60,1,60,1,60,1,60,1,60,3,60,699,8,60,1,60,1,60,1,
        60,1,60,3,60,705,8,60,1,60,3,60,708,8,60,3,60,710,8,60,1,61,1,61,
        1,61,5,61,715,8,61,10,61,12,61,718,9,61,1,61,3,61,721,8,61,1,62,
        1,62,1,62,5,62,726,8,62,10,62,12,62,729,9,62,1,62,3,62,732,8,62,
        1,63,1,63,1,63,3,63,737,8,63,1,64,1,64,1,64,5,64,742,8,64,10,64,
        12,64,745,9,64,1,65,1,65,1,66,1,66,1,67,4,67,752,8,67,11,67,12,67,
        753,1,67,0,0,68,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,
        36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,
        80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,
        118,120,122,124,126,128,130,132,134,0,9,1,0,14,16,1,0,41,43,1,0,
        17,18,1,0,37,39,2,0,2,5,64,66,1,0,59,60,1,0,61,63,3,0,26,28,41,43,
        50,50,1,0,32,36,806,0,137,1,0,0,0,2,153,1,0,0,0,4,162,1,0,0,0,6,
        170,1,0,0,0,8,185,1,0,0,0,10,188,1,0,0,0,12,191,1,0,0,0,14,194,1,
        0,0,0,16,204,1,0,0,0,18,206,1,0,0,0,20,223,1,0,0,0,22,225,1,0,0,
        0,24,229,1,0,0,0,26,240,1,0,0,0,28,245,1,0,0,0,30,261,1,0,0,0,32,
        266,1,0,0,0,34,268,1,0,0,0,36,274,1,0,0,0,38,285,1,0,0,0,40,306,
        1,0,0,0,42,308,1,0,0,0,44,312,1,0,0,0,46,316,1,0,0,0,48,324,1,0,
        0,0,50,332,1,0,0,0,52,347,1,0,0,0,54,358,1,0,0,0,56,362,1,0,0,0,
        58,368,1,0,0,0,60,376,1,0,0,0,62,393,1,0,0,0,64,395,1,0,0,0,66,397,
        1,0,0,0,68,406,1,0,0,0,70,430,1,0,0,0,72,432,1,0,0,0,74,441,1,0,
        0,0,76,444,1,0,0,0,78,446,1,0,0,0,80,453,1,0,0,0,82,455,1,0,0,0,
        84,463,1,0,0,0,86,471,1,0,0,0,88,479,1,0,0,0,90,487,1,0,0,0,92,495,
        1,0,0,0,94,513,1,0,0,0,96,515,1,0,0,0,98,528,1,0,0,0,100,541,1,0,
        0,0,102,583,1,0,0,0,104,585,1,0,0,0,106,594,1,0,0,0,108,609,1,0,
        0,0,110,611,1,0,0,0,112,617,1,0,0,0,114,628,1,0,0,0,116,659,1,0,
        0,0,118,661,1,0,0,0,120,709,1,0,0,0,122,711,1,0,0,0,124,722,1,0,
        0,0,126,733,1,0,0,0,128,738,1,0,0,0,130,746,1,0,0,0,132,748,1,0,
        0,0,134,751,1,0,0,0,136,138,3,134,67,0,137,136,1,0,0,0,137,138,1,
        0,0,0,138,139,1,0,0,0,139,140,3,8,4,0,140,142,3,10,5,0,141,143,3,
        134,67,0,142,141,1,0,0,0,142,143,1,0,0,0,143,147,1,0,0,0,144,146,
        3,6,3,0,145,144,1,0,0,0,146,149,1,0,0,0,147,145,1,0,0,0,147,148,
        1,0,0,0,148,150,1,0,0,0,149,147,1,0,0,0,150,151,5,0,0,1,151,1,1,
        0,0,0,152,154,3,134,67,0,153,152,1,0,0,0,153,154,1,0,0,0,154,155,
        1,0,0,0,155,157,3,76,38,0,156,158,3,134,67,0,157,156,1,0,0,0,157,
        158,1,0,0,0,158,159,1,0,0,0,159,160,5,0,0,1,160,3,1,0,0,0,161,163,
        3,134,67,0,162,161,1,0,0,0,162,163,1,0,0,0,163,164,1,0,0,0,164,166,
        3,32,16,0,165,167,3,134,67,0,166,165,1,0,0,0,166,167,1,0,0,0,167,
        168,1,0,0,0,168,169,5,0,0,1,169,5,1,0,0,0,170,174,3,8,4,0,171,175,
        3,12,6,0,172,175,3,14,7,0,173,175,3,16,8,0,174,171,1,0,0,0,174,172,
        1,0,0,0,174,173,1,0,0,0,175,177,1,0,0,0,176,178,3,134,67,0,177,176,
        1,0,0,0,177,178,1,0,0,0,178,7,1,0,0,0,179,181,5,44,0,0,180,182,3,
        134,67,0,181,180,1,0,0,0,181,182,1,0,0,0,182,184,1,0,0,0,183,179,
        1,0,0,0,184,187,1,0,0,0,185,183,1,0,0,0,185,186,1,0,0,0,186,9,1,
        0,0,0,187,185,1,0,0,0,188,189,5,6,0,0,189,190,3,128,64,0,190,11,
        1,0,0,0,191,192,5,7,0,0,192,193,5,45,0,0,193,13,1,0,0,0,194,195,
        5,8,0,0,195,196,3,128,64,0,196,15,1,0,0,0,197,205,3,18,9,0,198,205,
        3,48,24,0,199,205,3,50,25,0,200,205,3,56,28,0,201,205,3,58,29,0,
        202,205,3,66,33,0,203,205,3,68,34,0,204,197,1,0,0,0,204,198,1,0,
        0,0,204,199,1,0,0,0,204,200,1,0,0,0,204,201,1,0,0,0,204,202,1,0,
        0,0,204,203,1,0,0,0,205,17,1,0,0,0,206,207,5,9,0,0,207,209,3,130,
        65,0,208,210,3,22,11,0,209,208,1,0,0,0,209,210,1,0,0,0,210,211,1,
        0,0,0,211,212,5,64,0,0,212,213,3,20,10,0,213,19,1,0,0,0,214,224,
        3,34,17,0,215,220,3,26,13,0,216,217,5,58,0,0,217,219,3,26,13,0,218,
        216,1,0,0,0,219,222,1,0,0,0,220,218,1,0,0,0,220,221,1,0,0,0,221,
        224,1,0,0,0,222,220,1,0,0,0,223,214,1,0,0,0,223,215,1,0,0,0,224,
        21,1,0,0,0,225,226,5,65,0,0,226,227,3,24,12,0,227,228,5,66,0,0,228,
        23,1,0,0,0,229,234,3,130,65,0,230,231,5,55,0,0,231,233,3,130,65,
        0,232,230,1,0,0,0,233,236,1,0,0,0,234,232,1,0,0,0,234,235,1,0,0,
        0,235,238,1,0,0,0,236,234,1,0,0,0,237,239,5,55,0,0,238,237,1,0,0,
        0,238,239,1,0,0,0,239,25,1,0,0,0,240,241,3,130,65,0,241,242,5,51,
        0,0,242,243,3,28,14,0,243,244,5,52,0,0,244,27,1,0,0,0,245,250,3,
        30,15,0,246,247,5,55,0,0,247,249,3,30,15,0,248,246,1,0,0,0,249,252,
        1,0,0,0,250,248,1,0,0,0,250,251,1,0,0,0,251,254,1,0,0,0,252,250,
        1,0,0,0,253,255,5,55,0,0,254,253,1,0,0,0,254,255,1,0,0,0,255,29,
        1,0,0,0,256,257,3,130,65,0,257,258,5,56,0,0,258,259,3,32,16,0,259,
        262,1,0,0,0,260,262,3,32,16,0,261,256,1,0,0,0,261,260,1,0,0,0,262,
        31,1,0,0,0,263,267,3,34,17,0,264,267,3,40,20,0,265,267,3,42,21,0,
        266,263,1,0,0,0,266,264,1,0,0,0,266,265,1,0,0,0,267,33,1,0,0,0,268,
        270,5,53,0,0,269,271,3,36,18,0,270,269,1,0,0,0,270,271,1,0,0,0,271,
        272,1,0,0,0,272,273,5,54,0,0,273,35,1,0,0,0,274,279,3,38,19,0,275,
        276,5,55,0,0,276,278,3,38,19,0,277,275,1,0,0,0,278,281,1,0,0,0,279,
        277,1,0,0,0,279,280,1,0,0,0,280,283,1,0,0,0,281,279,1,0,0,0,282,
        284,5,55,0,0,283,282,1,0,0,0,283,284,1,0,0,0,284,37,1,0,0,0,285,
        286,3,130,65,0,286,287,5,56,0,0,287,288,3,32,16,0,288,39,1,0,0,0,
        289,290,5,51,0,0,290,291,3,32,16,0,291,292,5,52,0,0,292,307,1,0,
        0,0,293,294,5,51,0,0,294,295,3,32,16,0,295,296,5,55,0,0,296,301,
        3,32,16,0,297,298,5,55,0,0,298,300,3,32,16,0,299,297,1,0,0,0,300,
        303,1,0,0,0,301,299,1,0,0,0,301,302,1,0,0,0,302,304,1,0,0,0,303,
        301,1,0,0,0,304,305,5,52,0,0,305,307,1,0,0,0,306,289,1,0,0,0,306,
        293,1,0,0,0,307,41,1,0,0,0,308,310,3,128,64,0,309,311,3,44,22,0,
        310,309,1,0,0,0,310,311,1,0,0,0,311,43,1,0,0,0,312,313,5,65,0,0,
        313,314,3,46,23,0,314,315,5,66,0,0,315,45,1,0,0,0,316,321,3,32,16,
        0,317,318,5,55,0,0,318,320,3,32,16,0,319,317,1,0,0,0,320,323,1,0,
        0,0,321,319,1,0,0,0,321,322,1,0,0,0,322,47,1,0,0,0,323,321,1,0,0,
        0,324,325,5,10,0,0,325,327,3,130,65,0,326,328,3,74,37,0,327,326,
        1,0,0,0,327,328,1,0,0,0,328,329,1,0,0,0,329,330,5,64,0,0,330,331,
        3,76,38,0,331,49,1,0,0,0,332,333,5,11,0,0,333,335,3,130,65,0,334,
        336,3,22,11,0,335,334,1,0,0,0,335,336,1,0,0,0,336,337,1,0,0,0,337,
        339,5,51,0,0,338,340,3,52,26,0,339,338,1,0,0,0,339,340,1,0,0,0,340,
        341,1,0,0,0,341,342,5,52,0,0,342,343,5,1,0,0,343,344,3,32,16,0,344,
        345,5,56,0,0,345,346,3,70,35,0,346,51,1,0,0,0,347,352,3,54,27,0,
        348,349,5,55,0,0,349,351,3,54,27,0,350,348,1,0,0,0,351,354,1,0,0,
        0,352,350,1,0,0,0,352,353,1,0,0,0,353,356,1,0,0,0,354,352,1,0,0,
        0,355,357,5,55,0,0,356,355,1,0,0,0,356,357,1,0,0,0,357,53,1,0,0,
        0,358,359,3,120,60,0,359,360,5,56,0,0,360,361,3,32,16,0,361,55,1,
        0,0,0,362,363,5,12,0,0,363,364,3,130,65,0,364,365,5,56,0,0,365,366,
        3,32,16,0,366,57,1,0,0,0,367,369,3,60,30,0,368,367,1,0,0,0,368,369,
        1,0,0,0,369,370,1,0,0,0,370,371,5,13,0,0,371,374,3,62,31,0,372,373,
        5,40,0,0,373,375,3,76,38,0,374,372,1,0,0,0,374,375,1,0,0,0,375,59,
        1,0,0,0,376,377,7,0,0,0,377,61,1,0,0,0,378,379,3,64,32,0,379,380,
        5,56,0,0,380,381,3,76,38,0,381,394,1,0,0,0,382,384,3,64,32,0,383,
        382,1,0,0,0,383,384,1,0,0,0,384,385,1,0,0,0,385,388,3,128,64,0,386,
        387,5,24,0,0,387,389,3,76,38,0,388,386,1,0,0,0,388,389,1,0,0,0,389,
        390,1,0,0,0,390,391,5,56,0,0,391,392,3,76,38,0,392,394,1,0,0,0,393,
        378,1,0,0,0,393,383,1,0,0,0,394,63,1,0,0,0,395,396,7,1,0,0,396,65,
        1,0,0,0,397,398,7,2,0,0,398,403,3,128,64,0,399,400,5,66,0,0,400,
        402,3,128,64,0,401,399,1,0,0,0,402,405,1,0,0,0,403,401,1,0,0,0,403,
        404,1,0,0,0,404,67,1,0,0,0,405,403,1,0,0,0,406,410,5,19,0,0,407,
        408,3,130,65,0,408,409,5,64,0,0,409,411,1,0,0,0,410,407,1,0,0,0,
        410,411,1,0,0,0,411,412,1,0,0,0,412,413,3,76,38,0,413,69,1,0,0,0,
        414,415,5,67,0,0,415,419,5,70,0,0,416,418,3,72,36,0,417,416,1,0,
        0,0,418,421,1,0,0,0,419,417,1,0,0,0,419,420,1,0,0,0,420,423,1,0,
        0,0,421,419,1,0,0,0,422,424,3,76,38,0,423,422,1,0,0,0,423,424,1,
        0,0,0,424,426,1,0,0,0,425,427,3,134,67,0,426,425,1,0,0,0,426,427,
        1,0,0,0,427,428,1,0,0,0,428,431,5,71,0,0,429,431,3,76,38,0,430,414,
        1,0,0,0,430,429,1,0,0,0,431,71,1,0,0,0,432,433,5,10,0,0,433,435,
        3,120,60,0,434,436,3,74,37,0,435,434,1,0,0,0,435,436,1,0,0,0,436,
        437,1,0,0,0,437,438,5,64,0,0,438,439,3,76,38,0,439,440,3,134,67,
        0,440,73,1,0,0,0,441,442,5,56,0,0,442,443,3,32,16,0,443,75,1,0,0,
        0,444,445,3,78,39,0,445,77,1,0,0,0,446,450,3,80,40,0,447,449,3,132,
        66,0,448,447,1,0,0,0,449,452,1,0,0,0,450,448,1,0,0,0,450,451,1,0,
        0,0,451,79,1,0,0,0,452,450,1,0,0,0,453,454,3,82,41,0,454,81,1,0,
        0,0,455,460,3,84,42,0,456,457,5,30,0,0,457,459,3,84,42,0,458,456,
        1,0,0,0,459,462,1,0,0,0,460,458,1,0,0,0,460,461,1,0,0,0,461,83,1,
        0,0,0,462,460,1,0,0,0,463,468,3,86,43,0,464,465,5,29,0,0,465,467,
        3,86,43,0,466,464,1,0,0,0,467,470,1,0,0,0,468,466,1,0,0,0,468,469,
        1,0,0,0,469,85,1,0,0,0,470,468,1,0,0,0,471,476,3,88,44,0,472,473,
        7,3,0,0,473,475,3,88,44,0,474,472,1,0,0,0,475,478,1,0,0,0,476,474,
        1,0,0,0,476,477,1,0,0,0,477,87,1,0,0,0,478,476,1,0,0,0,479,484,3,
        90,45,0,480,481,7,4,0,0,481,483,3,90,45,0,482,480,1,0,0,0,483,486,
        1,0,0,0,484,482,1,0,0,0,484,485,1,0,0,0,485,89,1,0,0,0,486,484,1,
        0,0,0,487,492,3,92,46,0,488,489,7,5,0,0,489,491,3,92,46,0,490,488,
        1,0,0,0,491,494,1,0,0,0,492,490,1,0,0,0,492,493,1,0,0,0,493,91,1,
        0,0,0,494,492,1,0,0,0,495,500,3,94,47,0,496,497,7,6,0,0,497,499,
        3,94,47,0,498,496,1,0,0,0,499,502,1,0,0,0,500,498,1,0,0,0,500,501,
        1,0,0,0,501,93,1,0,0,0,502,500,1,0,0,0,503,514,3,96,48,0,504,514,
        3,98,49,0,505,514,3,100,50,0,506,510,5,31,0,0,507,510,5,60,0,0,508,
        510,3,132,66,0,509,506,1,0,0,0,509,507,1,0,0,0,509,508,1,0,0,0,510,
        511,1,0,0,0,511,514,3,94,47,0,512,514,3,106,53,0,513,503,1,0,0,0,
        513,504,1,0,0,0,513,505,1,0,0,0,513,509,1,0,0,0,513,512,1,0,0,0,
        514,95,1,0,0,0,515,516,5,20,0,0,516,518,3,76,38,0,517,519,3,134,
        67,0,518,517,1,0,0,0,518,519,1,0,0,0,519,520,1,0,0,0,520,521,5,21,
        0,0,521,523,3,76,38,0,522,524,3,134,67,0,523,522,1,0,0,0,523,524,
        1,0,0,0,524,525,1,0,0,0,525,526,5,22,0,0,526,527,3,76,38,0,527,97,
        1,0,0,0,528,529,5,10,0,0,529,531,3,120,60,0,530,532,3,74,37,0,531,
        530,1,0,0,0,531,532,1,0,0,0,532,533,1,0,0,0,533,534,5,64,0,0,534,
        536,3,76,38,0,535,537,3,134,67,0,536,535,1,0,0,0,536,537,1,0,0,0,
        537,538,1,0,0,0,538,539,5,25,0,0,539,540,3,76,38,0,540,99,1,0,0,
        0,541,542,5,23,0,0,542,543,3,76,38,0,543,544,5,56,0,0,544,545,3,
        102,51,0,545,101,1,0,0,0,546,547,5,67,0,0,547,549,5,70,0,0,548,550,
        3,134,67,0,549,548,1,0,0,0,549,550,1,0,0,0,550,551,1,0,0,0,551,558,
        3,104,52,0,552,554,3,134,67,0,553,552,1,0,0,0,553,554,1,0,0,0,554,
        555,1,0,0,0,555,557,3,104,52,0,556,553,1,0,0,0,557,560,1,0,0,0,558,
        556,1,0,0,0,558,559,1,0,0,0,559,562,1,0,0,0,560,558,1,0,0,0,561,
        563,3,134,67,0,562,561,1,0,0,0,562,563,1,0,0,0,563,564,1,0,0,0,564,
        565,5,71,0,0,565,584,1,0,0,0,566,568,5,67,0,0,567,569,3,134,67,0,
        568,567,1,0,0,0,568,569,1,0,0,0,569,570,1,0,0,0,570,577,3,104,52,
        0,571,573,3,134,67,0,572,571,1,0,0,0,572,573,1,0,0,0,573,574,1,0,
        0,0,574,576,3,104,52,0,575,572,1,0,0,0,576,579,1,0,0,0,577,575,1,
        0,0,0,577,578,1,0,0,0,578,581,1,0,0,0,579,577,1,0,0,0,580,582,3,
        134,67,0,581,580,1,0,0,0,581,582,1,0,0,0,582,584,1,0,0,0,583,546,
        1,0,0,0,583,566,1,0,0,0,584,103,1,0,0,0,585,586,5,58,0,0,586,589,
        3,120,60,0,587,588,5,24,0,0,588,590,3,76,38,0,589,587,1,0,0,0,589,
        590,1,0,0,0,590,591,1,0,0,0,591,592,5,56,0,0,592,593,3,70,35,0,593,
        105,1,0,0,0,594,598,3,116,58,0,595,597,3,108,54,0,596,595,1,0,0,
        0,597,600,1,0,0,0,598,596,1,0,0,0,598,599,1,0,0,0,599,107,1,0,0,
        0,600,598,1,0,0,0,601,610,3,110,55,0,602,604,5,51,0,0,603,605,3,
        118,59,0,604,603,1,0,0,0,604,605,1,0,0,0,605,606,1,0,0,0,606,610,
        5,52,0,0,607,608,5,57,0,0,608,610,3,130,65,0,609,601,1,0,0,0,609,
        602,1,0,0,0,609,607,1,0,0,0,610,109,1,0,0,0,611,613,5,53,0,0,612,
        614,3,112,56,0,613,612,1,0,0,0,613,614,1,0,0,0,614,615,1,0,0,0,615,
        616,5,54,0,0,616,111,1,0,0,0,617,622,3,114,57,0,618,619,5,55,0,0,
        619,621,3,114,57,0,620,618,1,0,0,0,621,624,1,0,0,0,622,620,1,0,0,
        0,622,623,1,0,0,0,623,626,1,0,0,0,624,622,1,0,0,0,625,627,5,55,0,
        0,626,625,1,0,0,0,626,627,1,0,0,0,627,113,1,0,0,0,628,629,3,130,
        65,0,629,630,5,64,0,0,630,631,3,76,38,0,631,115,1,0,0,0,632,660,
        5,45,0,0,633,660,5,48,0,0,634,660,5,47,0,0,635,660,5,46,0,0,636,
        660,5,26,0,0,637,660,5,27,0,0,638,660,5,28,0,0,639,660,3,128,64,
        0,640,641,5,51,0,0,641,660,5,52,0,0,642,643,5,51,0,0,643,644,3,76,
        38,0,644,645,5,52,0,0,645,660,1,0,0,0,646,647,5,51,0,0,647,648,3,
        76,38,0,648,649,5,55,0,0,649,654,3,76,38,0,650,651,5,55,0,0,651,
        653,3,76,38,0,652,650,1,0,0,0,653,656,1,0,0,0,654,652,1,0,0,0,654,
        655,1,0,0,0,655,657,1,0,0,0,656,654,1,0,0,0,657,658,5,52,0,0,658,
        660,1,0,0,0,659,632,1,0,0,0,659,633,1,0,0,0,659,634,1,0,0,0,659,
        635,1,0,0,0,659,636,1,0,0,0,659,637,1,0,0,0,659,638,1,0,0,0,659,
        639,1,0,0,0,659,640,1,0,0,0,659,642,1,0,0,0,659,646,1,0,0,0,660,
        117,1,0,0,0,661,666,3,76,38,0,662,663,5,55,0,0,663,665,3,76,38,0,
        664,662,1,0,0,0,665,668,1,0,0,0,666,664,1,0,0,0,666,667,1,0,0,0,
        667,670,1,0,0,0,668,666,1,0,0,0,669,671,5,55,0,0,670,669,1,0,0,0,
        670,671,1,0,0,0,671,119,1,0,0,0,672,710,5,49,0,0,673,710,5,45,0,
        0,674,710,5,48,0,0,675,710,5,47,0,0,676,710,5,46,0,0,677,678,5,51,
        0,0,678,710,5,52,0,0,679,680,5,51,0,0,680,681,3,120,60,0,681,682,
        5,52,0,0,682,710,1,0,0,0,683,684,5,51,0,0,684,685,3,120,60,0,685,
        686,5,55,0,0,686,691,3,120,60,0,687,688,5,55,0,0,688,690,3,120,60,
        0,689,687,1,0,0,0,690,693,1,0,0,0,691,689,1,0,0,0,691,692,1,0,0,
        0,692,694,1,0,0,0,693,691,1,0,0,0,694,695,5,52,0,0,695,710,1,0,0,
        0,696,698,5,53,0,0,697,699,3,124,62,0,698,697,1,0,0,0,698,699,1,
        0,0,0,699,700,1,0,0,0,700,710,5,54,0,0,701,707,3,128,64,0,702,704,
        5,51,0,0,703,705,3,122,61,0,704,703,1,0,0,0,704,705,1,0,0,0,705,
        706,1,0,0,0,706,708,5,52,0,0,707,702,1,0,0,0,707,708,1,0,0,0,708,
        710,1,0,0,0,709,672,1,0,0,0,709,673,1,0,0,0,709,674,1,0,0,0,709,
        675,1,0,0,0,709,676,1,0,0,0,709,677,1,0,0,0,709,679,1,0,0,0,709,
        683,1,0,0,0,709,696,1,0,0,0,709,701,1,0,0,0,710,121,1,0,0,0,711,
        716,3,120,60,0,712,713,5,55,0,0,713,715,3,120,60,0,714,712,1,0,0,
        0,715,718,1,0,0,0,716,714,1,0,0,0,716,717,1,0,0,0,717,720,1,0,0,
        0,718,716,1,0,0,0,719,721,5,55,0,0,720,719,1,0,0,0,720,721,1,0,0,
        0,721,123,1,0,0,0,722,727,3,126,63,0,723,724,5,55,0,0,724,726,3,
        126,63,0,725,723,1,0,0,0,726,729,1,0,0,0,727,725,1,0,0,0,727,728,
        1,0,0,0,728,731,1,0,0,0,729,727,1,0,0,0,730,732,5,55,0,0,731,730,
        1,0,0,0,731,732,1,0,0,0,732,125,1,0,0,0,733,736,3,130,65,0,734,735,
        5,64,0,0,735,737,3,120,60,0,736,734,1,0,0,0,736,737,1,0,0,0,737,
        127,1,0,0,0,738,743,3,130,65,0,739,740,5,57,0,0,740,742,3,130,65,
        0,741,739,1,0,0,0,742,745,1,0,0,0,743,741,1,0,0,0,743,744,1,0,0,
        0,744,129,1,0,0,0,745,743,1,0,0,0,746,747,7,7,0,0,747,131,1,0,0,
        0,748,749,7,8,0,0,749,133,1,0,0,0,750,752,5,67,0,0,751,750,1,0,0,
        0,752,753,1,0,0,0,753,751,1,0,0,0,753,754,1,0,0,0,754,135,1,0,0,
        0,90,137,142,147,153,157,162,166,174,177,181,185,204,209,220,223,
        234,238,250,254,261,266,270,279,283,301,306,310,321,327,335,339,
        352,356,368,374,383,388,393,403,410,419,423,426,430,435,450,460,
        468,476,484,492,500,509,513,518,523,531,536,549,553,558,562,568,
        572,577,581,583,589,598,604,609,613,622,626,654,659,666,670,691,
        698,704,707,709,716,720,727,731,736,743,753
    ]

class MDLParser ( Parser ):

    grammarFileName = "MDL.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'->'", "'<='", "'>='", "'!='", "'=='", 
                     "'module'", "'import'", "'open'", "'type'", "'let'", 
                     "'func'", "'entity'", "'rule'", "'strict'", "'defeasible'", 
                     "'defeater'", "'priority'", "'override'", "'fact'", 
                     "'if'", "'then'", "'else'", "'case'", "'when'", "'in'", 
                     "'true'", "'false'", "'last'", "'and'", "'or'", "'not'", 
                     "'always'", "'eventually'", "'next'", "'weak_next'", 
                     "'never'", "'until'", "'release'", "'weak_until'", 
                     "'otherwise'", "'O'", "'P'", "'F'", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "<INVALID>", "<INVALID>", "'_'", "<INVALID>", 
                     "'('", "')'", "'{'", "'}'", "','", "':'", "'.'", "'|'", 
                     "'+'", "'-'", "'*'", "'/'", "'%'", "'='", "'<'", "'>'" ]

    symbolicNames = [ "<INVALID>", "ARROW", "LE", "GE", "NE", "EQEQ", "MODULE", 
                      "IMPORT", "OPEN", "TYPE", "LET", "FUNC", "ENTITY", 
                      "RULE", "STRICT", "DEFEASIBLE", "DEFEATER", "PRIORITY", 
                      "OVERRIDE", "FACT", "IF", "THEN", "ELSE", "CASE", 
                      "WHEN", "IN", "TRUE", "FALSE", "LAST", "AND", "OR", 
                      "NOT", "ALWAYS", "EVENTUALLY", "NEXT", "WEAK_NEXT", 
                      "NEVER", "UNTIL", "RELEASE", "WEAK_UNTIL", "OTHERWISE", 
                      "O", "P", "F", "ANNOT", "STRING", "RAT", "DECIMAL", 
                      "INT", "UNDERSCORE", "IDENT", "LPAREN", "RPAREN", 
                      "LBRACE", "RBRACE", "COMMA", "COLON", "DOT", "BAR", 
                      "PLUS", "MINUS", "STAR", "SLASH", "PERCENT", "EQ", 
                      "LT", "GT", "NEWLINE", "COMMENT", "WS", "INDENT", 
                      "DEDENT" ]

    RULE_program = 0
    RULE_exprOnly = 1
    RULE_typeExprOnly = 2
    RULE_topItem = 3
    RULE_annotations = 4
    RULE_moduleDecl = 5
    RULE_importDecl = 6
    RULE_openDecl = 7
    RULE_declaration = 8
    RULE_typeDecl = 9
    RULE_typeDefinition = 10
    RULE_typeParams = 11
    RULE_nameList = 12
    RULE_variant = 13
    RULE_variantFieldList = 14
    RULE_variantField = 15
    RULE_typeExpr = 16
    RULE_recordType = 17
    RULE_typeFieldList = 18
    RULE_typeField = 19
    RULE_tupleOrParenType = 20
    RULE_typeRef = 21
    RULE_typeArgs = 22
    RULE_typeExprList = 23
    RULE_valueDecl = 24
    RULE_funcDecl = 25
    RULE_paramList = 26
    RULE_param = 27
    RULE_entityDecl = 28
    RULE_ruleDecl = 29
    RULE_ruleStrength = 30
    RULE_ruleBody = 31
    RULE_deonticMod = 32
    RULE_priorityDecl = 33
    RULE_factDecl = 34
    RULE_block = 35
    RULE_blockLetStmt = 36
    RULE_typeAnnotation = 37
    RULE_expr = 38
    RULE_temporalPostfix = 39
    RULE_implication = 40
    RULE_orExpr = 41
    RULE_andExpr = 42
    RULE_temporalBinary = 43
    RULE_comparison = 44
    RULE_additive = 45
    RULE_multiplicative = 46
    RULE_unary = 47
    RULE_ifExpr = 48
    RULE_letExpr = 49
    RULE_matchExpr = 50
    RULE_caseBody = 51
    RULE_caseArm = 52
    RULE_postfix = 53
    RULE_postfixSuffix = 54
    RULE_recordConstructorFields = 55
    RULE_recordConstructorFieldList = 56
    RULE_recordConstructorField = 57
    RULE_primary = 58
    RULE_exprList = 59
    RULE_pattern = 60
    RULE_patternList = 61
    RULE_recordPatternFieldList = 62
    RULE_recordPatternField = 63
    RULE_qualifiedName = 64
    RULE_nameToken = 65
    RULE_temporalUnaryOp = 66
    RULE_newlines = 67

    ruleNames =  [ "program", "exprOnly", "typeExprOnly", "topItem", "annotations", 
                   "moduleDecl", "importDecl", "openDecl", "declaration", 
                   "typeDecl", "typeDefinition", "typeParams", "nameList", 
                   "variant", "variantFieldList", "variantField", "typeExpr", 
                   "recordType", "typeFieldList", "typeField", "tupleOrParenType", 
                   "typeRef", "typeArgs", "typeExprList", "valueDecl", "funcDecl", 
                   "paramList", "param", "entityDecl", "ruleDecl", "ruleStrength", 
                   "ruleBody", "deonticMod", "priorityDecl", "factDecl", 
                   "block", "blockLetStmt", "typeAnnotation", "expr", "temporalPostfix", 
                   "implication", "orExpr", "andExpr", "temporalBinary", 
                   "comparison", "additive", "multiplicative", "unary", 
                   "ifExpr", "letExpr", "matchExpr", "caseBody", "caseArm", 
                   "postfix", "postfixSuffix", "recordConstructorFields", 
                   "recordConstructorFieldList", "recordConstructorField", 
                   "primary", "exprList", "pattern", "patternList", "recordPatternFieldList", 
                   "recordPatternField", "qualifiedName", "nameToken", "temporalUnaryOp", 
                   "newlines" ]

    EOF = Token.EOF
    ARROW=1
    LE=2
    GE=3
    NE=4
    EQEQ=5
    MODULE=6
    IMPORT=7
    OPEN=8
    TYPE=9
    LET=10
    FUNC=11
    ENTITY=12
    RULE=13
    STRICT=14
    DEFEASIBLE=15
    DEFEATER=16
    PRIORITY=17
    OVERRIDE=18
    FACT=19
    IF=20
    THEN=21
    ELSE=22
    CASE=23
    WHEN=24
    IN=25
    TRUE=26
    FALSE=27
    LAST=28
    AND=29
    OR=30
    NOT=31
    ALWAYS=32
    EVENTUALLY=33
    NEXT=34
    WEAK_NEXT=35
    NEVER=36
    UNTIL=37
    RELEASE=38
    WEAK_UNTIL=39
    OTHERWISE=40
    O=41
    P=42
    F=43
    ANNOT=44
    STRING=45
    RAT=46
    DECIMAL=47
    INT=48
    UNDERSCORE=49
    IDENT=50
    LPAREN=51
    RPAREN=52
    LBRACE=53
    RBRACE=54
    COMMA=55
    COLON=56
    DOT=57
    BAR=58
    PLUS=59
    MINUS=60
    STAR=61
    SLASH=62
    PERCENT=63
    EQ=64
    LT=65
    GT=66
    NEWLINE=67
    COMMENT=68
    WS=69
    INDENT=70
    DEDENT=71

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class ProgramContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def annotations(self):
            return self.getTypedRuleContext(MDLParser.AnnotationsContext,0)


        def moduleDecl(self):
            return self.getTypedRuleContext(MDLParser.ModuleDeclContext,0)


        def EOF(self):
            return self.getToken(MDLParser.EOF, 0)

        def newlines(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NewlinesContext)
            else:
                return self.getTypedRuleContext(MDLParser.NewlinesContext,i)


        def topItem(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.TopItemContext)
            else:
                return self.getTypedRuleContext(MDLParser.TopItemContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_program

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitProgram" ):
                return visitor.visitProgram(self)
            else:
                return visitor.visitChildren(self)




    def program(self):

        localctx = MDLParser.ProgramContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_program)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 137
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 136
                self.newlines()


            self.state = 139
            self.annotations()
            self.state = 140
            self.moduleDecl()
            self.state = 142
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 141
                self.newlines()


            self.state = 147
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 17592187092864) != 0):
                self.state = 144
                self.topItem()
                self.state = 149
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 150
            self.match(MDLParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprOnlyContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def EOF(self):
            return self.getToken(MDLParser.EOF, 0)

        def newlines(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NewlinesContext)
            else:
                return self.getTypedRuleContext(MDLParser.NewlinesContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_exprOnly

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExprOnly" ):
                return visitor.visitExprOnly(self)
            else:
                return visitor.visitChildren(self)




    def exprOnly(self):

        localctx = MDLParser.ExprOnlyContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_exprOnly)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 153
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 152
                self.newlines()


            self.state = 155
            self.expr()
            self.state = 157
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 156
                self.newlines()


            self.state = 159
            self.match(MDLParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeExprOnlyContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def typeExpr(self):
            return self.getTypedRuleContext(MDLParser.TypeExprContext,0)


        def EOF(self):
            return self.getToken(MDLParser.EOF, 0)

        def newlines(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NewlinesContext)
            else:
                return self.getTypedRuleContext(MDLParser.NewlinesContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_typeExprOnly

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeExprOnly" ):
                return visitor.visitTypeExprOnly(self)
            else:
                return visitor.visitChildren(self)




    def typeExprOnly(self):

        localctx = MDLParser.TypeExprOnlyContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_typeExprOnly)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 162
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 161
                self.newlines()


            self.state = 164
            self.typeExpr()
            self.state = 166
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 165
                self.newlines()


            self.state = 168
            self.match(MDLParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TopItemContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def annotations(self):
            return self.getTypedRuleContext(MDLParser.AnnotationsContext,0)


        def importDecl(self):
            return self.getTypedRuleContext(MDLParser.ImportDeclContext,0)


        def openDecl(self):
            return self.getTypedRuleContext(MDLParser.OpenDeclContext,0)


        def declaration(self):
            return self.getTypedRuleContext(MDLParser.DeclarationContext,0)


        def newlines(self):
            return self.getTypedRuleContext(MDLParser.NewlinesContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_topItem

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTopItem" ):
                return visitor.visitTopItem(self)
            else:
                return visitor.visitChildren(self)




    def topItem(self):

        localctx = MDLParser.TopItemContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_topItem)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 170
            self.annotations()
            self.state = 174
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [7]:
                self.state = 171
                self.importDecl()
                pass
            elif token in [8]:
                self.state = 172
                self.openDecl()
                pass
            elif token in [9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]:
                self.state = 173
                self.declaration()
                pass
            else:
                raise NoViableAltException(self)

            self.state = 177
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 176
                self.newlines()


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AnnotationsContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def ANNOT(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.ANNOT)
            else:
                return self.getToken(MDLParser.ANNOT, i)

        def newlines(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NewlinesContext)
            else:
                return self.getTypedRuleContext(MDLParser.NewlinesContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_annotations

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAnnotations" ):
                return visitor.visitAnnotations(self)
            else:
                return visitor.visitChildren(self)




    def annotations(self):

        localctx = MDLParser.AnnotationsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_annotations)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 185
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==44:
                self.state = 179
                self.match(MDLParser.ANNOT)
                self.state = 181
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==67:
                    self.state = 180
                    self.newlines()


                self.state = 187
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ModuleDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def MODULE(self):
            return self.getToken(MDLParser.MODULE, 0)

        def qualifiedName(self):
            return self.getTypedRuleContext(MDLParser.QualifiedNameContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_moduleDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitModuleDecl" ):
                return visitor.visitModuleDecl(self)
            else:
                return visitor.visitChildren(self)




    def moduleDecl(self):

        localctx = MDLParser.ModuleDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_moduleDecl)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 188
            self.match(MDLParser.MODULE)
            self.state = 189
            self.qualifiedName()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ImportDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IMPORT(self):
            return self.getToken(MDLParser.IMPORT, 0)

        def STRING(self):
            return self.getToken(MDLParser.STRING, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_importDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitImportDecl" ):
                return visitor.visitImportDecl(self)
            else:
                return visitor.visitChildren(self)




    def importDecl(self):

        localctx = MDLParser.ImportDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_importDecl)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 191
            self.match(MDLParser.IMPORT)
            self.state = 192
            self.match(MDLParser.STRING)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class OpenDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def OPEN(self):
            return self.getToken(MDLParser.OPEN, 0)

        def qualifiedName(self):
            return self.getTypedRuleContext(MDLParser.QualifiedNameContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_openDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitOpenDecl" ):
                return visitor.visitOpenDecl(self)
            else:
                return visitor.visitChildren(self)




    def openDecl(self):

        localctx = MDLParser.OpenDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_openDecl)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 194
            self.match(MDLParser.OPEN)
            self.state = 195
            self.qualifiedName()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class DeclarationContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def typeDecl(self):
            return self.getTypedRuleContext(MDLParser.TypeDeclContext,0)


        def valueDecl(self):
            return self.getTypedRuleContext(MDLParser.ValueDeclContext,0)


        def funcDecl(self):
            return self.getTypedRuleContext(MDLParser.FuncDeclContext,0)


        def entityDecl(self):
            return self.getTypedRuleContext(MDLParser.EntityDeclContext,0)


        def ruleDecl(self):
            return self.getTypedRuleContext(MDLParser.RuleDeclContext,0)


        def priorityDecl(self):
            return self.getTypedRuleContext(MDLParser.PriorityDeclContext,0)


        def factDecl(self):
            return self.getTypedRuleContext(MDLParser.FactDeclContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_declaration

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitDeclaration" ):
                return visitor.visitDeclaration(self)
            else:
                return visitor.visitChildren(self)




    def declaration(self):

        localctx = MDLParser.DeclarationContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_declaration)
        try:
            self.state = 204
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [9]:
                self.enterOuterAlt(localctx, 1)
                self.state = 197
                self.typeDecl()
                pass
            elif token in [10]:
                self.enterOuterAlt(localctx, 2)
                self.state = 198
                self.valueDecl()
                pass
            elif token in [11]:
                self.enterOuterAlt(localctx, 3)
                self.state = 199
                self.funcDecl()
                pass
            elif token in [12]:
                self.enterOuterAlt(localctx, 4)
                self.state = 200
                self.entityDecl()
                pass
            elif token in [13, 14, 15, 16]:
                self.enterOuterAlt(localctx, 5)
                self.state = 201
                self.ruleDecl()
                pass
            elif token in [17, 18]:
                self.enterOuterAlt(localctx, 6)
                self.state = 202
                self.priorityDecl()
                pass
            elif token in [19]:
                self.enterOuterAlt(localctx, 7)
                self.state = 203
                self.factDecl()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def TYPE(self):
            return self.getToken(MDLParser.TYPE, 0)

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def EQ(self):
            return self.getToken(MDLParser.EQ, 0)

        def typeDefinition(self):
            return self.getTypedRuleContext(MDLParser.TypeDefinitionContext,0)


        def typeParams(self):
            return self.getTypedRuleContext(MDLParser.TypeParamsContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_typeDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeDecl" ):
                return visitor.visitTypeDecl(self)
            else:
                return visitor.visitChildren(self)




    def typeDecl(self):

        localctx = MDLParser.TypeDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 18, self.RULE_typeDecl)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 206
            self.match(MDLParser.TYPE)
            self.state = 207
            self.nameToken()
            self.state = 209
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==65:
                self.state = 208
                self.typeParams()


            self.state = 211
            self.match(MDLParser.EQ)
            self.state = 212
            self.typeDefinition()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeDefinitionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def recordType(self):
            return self.getTypedRuleContext(MDLParser.RecordTypeContext,0)


        def variant(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.VariantContext)
            else:
                return self.getTypedRuleContext(MDLParser.VariantContext,i)


        def BAR(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.BAR)
            else:
                return self.getToken(MDLParser.BAR, i)

        def getRuleIndex(self):
            return MDLParser.RULE_typeDefinition

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeDefinition" ):
                return visitor.visitTypeDefinition(self)
            else:
                return visitor.visitChildren(self)




    def typeDefinition(self):

        localctx = MDLParser.TypeDefinitionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 20, self.RULE_typeDefinition)
        self._la = 0 # Token type
        try:
            self.state = 223
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [53]:
                self.enterOuterAlt(localctx, 1)
                self.state = 214
                self.recordType()
                pass
            elif token in [26, 27, 28, 41, 42, 43, 50]:
                self.enterOuterAlt(localctx, 2)
                self.state = 215
                self.variant()
                self.state = 220
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while _la==58:
                    self.state = 216
                    self.match(MDLParser.BAR)
                    self.state = 217
                    self.variant()
                    self.state = 222
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)

                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeParamsContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LT(self):
            return self.getToken(MDLParser.LT, 0)

        def nameList(self):
            return self.getTypedRuleContext(MDLParser.NameListContext,0)


        def GT(self):
            return self.getToken(MDLParser.GT, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_typeParams

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeParams" ):
                return visitor.visitTypeParams(self)
            else:
                return visitor.visitChildren(self)




    def typeParams(self):

        localctx = MDLParser.TypeParamsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 22, self.RULE_typeParams)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 225
            self.match(MDLParser.LT)
            self.state = 226
            self.nameList()
            self.state = 227
            self.match(MDLParser.GT)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class NameListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def nameToken(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NameTokenContext)
            else:
                return self.getTypedRuleContext(MDLParser.NameTokenContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_nameList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitNameList" ):
                return visitor.visitNameList(self)
            else:
                return visitor.visitChildren(self)




    def nameList(self):

        localctx = MDLParser.NameListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 24, self.RULE_nameList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 229
            self.nameToken()
            self.state = 234
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,15,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 230
                    self.match(MDLParser.COMMA)
                    self.state = 231
                    self.nameToken() 
                self.state = 236
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,15,self._ctx)

            self.state = 238
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 237
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class VariantContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def LPAREN(self):
            return self.getToken(MDLParser.LPAREN, 0)

        def variantFieldList(self):
            return self.getTypedRuleContext(MDLParser.VariantFieldListContext,0)


        def RPAREN(self):
            return self.getToken(MDLParser.RPAREN, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_variant

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitVariant" ):
                return visitor.visitVariant(self)
            else:
                return visitor.visitChildren(self)




    def variant(self):

        localctx = MDLParser.VariantContext(self, self._ctx, self.state)
        self.enterRule(localctx, 26, self.RULE_variant)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 240
            self.nameToken()
            self.state = 241
            self.match(MDLParser.LPAREN)
            self.state = 242
            self.variantFieldList()
            self.state = 243
            self.match(MDLParser.RPAREN)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class VariantFieldListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def variantField(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.VariantFieldContext)
            else:
                return self.getTypedRuleContext(MDLParser.VariantFieldContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_variantFieldList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitVariantFieldList" ):
                return visitor.visitVariantFieldList(self)
            else:
                return visitor.visitChildren(self)




    def variantFieldList(self):

        localctx = MDLParser.VariantFieldListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 28, self.RULE_variantFieldList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 245
            self.variantField()
            self.state = 250
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,17,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 246
                    self.match(MDLParser.COMMA)
                    self.state = 247
                    self.variantField() 
                self.state = 252
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,17,self._ctx)

            self.state = 254
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 253
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class VariantFieldContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def typeExpr(self):
            return self.getTypedRuleContext(MDLParser.TypeExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_variantField

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitVariantField" ):
                return visitor.visitVariantField(self)
            else:
                return visitor.visitChildren(self)




    def variantField(self):

        localctx = MDLParser.VariantFieldContext(self, self._ctx, self.state)
        self.enterRule(localctx, 30, self.RULE_variantField)
        try:
            self.state = 261
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,19,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 256
                self.nameToken()
                self.state = 257
                self.match(MDLParser.COLON)
                self.state = 258
                self.typeExpr()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 260
                self.typeExpr()
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def recordType(self):
            return self.getTypedRuleContext(MDLParser.RecordTypeContext,0)


        def tupleOrParenType(self):
            return self.getTypedRuleContext(MDLParser.TupleOrParenTypeContext,0)


        def typeRef(self):
            return self.getTypedRuleContext(MDLParser.TypeRefContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_typeExpr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeExpr" ):
                return visitor.visitTypeExpr(self)
            else:
                return visitor.visitChildren(self)




    def typeExpr(self):

        localctx = MDLParser.TypeExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 32, self.RULE_typeExpr)
        try:
            self.state = 266
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [53]:
                self.enterOuterAlt(localctx, 1)
                self.state = 263
                self.recordType()
                pass
            elif token in [51]:
                self.enterOuterAlt(localctx, 2)
                self.state = 264
                self.tupleOrParenType()
                pass
            elif token in [26, 27, 28, 41, 42, 43, 50]:
                self.enterOuterAlt(localctx, 3)
                self.state = 265
                self.typeRef()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RecordTypeContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LBRACE(self):
            return self.getToken(MDLParser.LBRACE, 0)

        def RBRACE(self):
            return self.getToken(MDLParser.RBRACE, 0)

        def typeFieldList(self):
            return self.getTypedRuleContext(MDLParser.TypeFieldListContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_recordType

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRecordType" ):
                return visitor.visitRecordType(self)
            else:
                return visitor.visitChildren(self)




    def recordType(self):

        localctx = MDLParser.RecordTypeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 34, self.RULE_recordType)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 268
            self.match(MDLParser.LBRACE)
            self.state = 270
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 1141293539393536) != 0):
                self.state = 269
                self.typeFieldList()


            self.state = 272
            self.match(MDLParser.RBRACE)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeFieldListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def typeField(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.TypeFieldContext)
            else:
                return self.getTypedRuleContext(MDLParser.TypeFieldContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_typeFieldList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeFieldList" ):
                return visitor.visitTypeFieldList(self)
            else:
                return visitor.visitChildren(self)




    def typeFieldList(self):

        localctx = MDLParser.TypeFieldListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 36, self.RULE_typeFieldList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 274
            self.typeField()
            self.state = 279
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,22,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 275
                    self.match(MDLParser.COMMA)
                    self.state = 276
                    self.typeField() 
                self.state = 281
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,22,self._ctx)

            self.state = 283
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 282
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeFieldContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def typeExpr(self):
            return self.getTypedRuleContext(MDLParser.TypeExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_typeField

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeField" ):
                return visitor.visitTypeField(self)
            else:
                return visitor.visitChildren(self)




    def typeField(self):

        localctx = MDLParser.TypeFieldContext(self, self._ctx, self.state)
        self.enterRule(localctx, 38, self.RULE_typeField)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 285
            self.nameToken()
            self.state = 286
            self.match(MDLParser.COLON)
            self.state = 287
            self.typeExpr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TupleOrParenTypeContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LPAREN(self):
            return self.getToken(MDLParser.LPAREN, 0)

        def typeExpr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.TypeExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.TypeExprContext,i)


        def RPAREN(self):
            return self.getToken(MDLParser.RPAREN, 0)

        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_tupleOrParenType

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTupleOrParenType" ):
                return visitor.visitTupleOrParenType(self)
            else:
                return visitor.visitChildren(self)




    def tupleOrParenType(self):

        localctx = MDLParser.TupleOrParenTypeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 40, self.RULE_tupleOrParenType)
        self._la = 0 # Token type
        try:
            self.state = 306
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,25,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 289
                self.match(MDLParser.LPAREN)
                self.state = 290
                self.typeExpr()
                self.state = 291
                self.match(MDLParser.RPAREN)
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 293
                self.match(MDLParser.LPAREN)
                self.state = 294
                self.typeExpr()
                self.state = 295
                self.match(MDLParser.COMMA)
                self.state = 296
                self.typeExpr()
                self.state = 301
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while _la==55:
                    self.state = 297
                    self.match(MDLParser.COMMA)
                    self.state = 298
                    self.typeExpr()
                    self.state = 303
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)

                self.state = 304
                self.match(MDLParser.RPAREN)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeRefContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def qualifiedName(self):
            return self.getTypedRuleContext(MDLParser.QualifiedNameContext,0)


        def typeArgs(self):
            return self.getTypedRuleContext(MDLParser.TypeArgsContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_typeRef

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeRef" ):
                return visitor.visitTypeRef(self)
            else:
                return visitor.visitChildren(self)




    def typeRef(self):

        localctx = MDLParser.TypeRefContext(self, self._ctx, self.state)
        self.enterRule(localctx, 42, self.RULE_typeRef)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 308
            self.qualifiedName()
            self.state = 310
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==65:
                self.state = 309
                self.typeArgs()


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeArgsContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LT(self):
            return self.getToken(MDLParser.LT, 0)

        def typeExprList(self):
            return self.getTypedRuleContext(MDLParser.TypeExprListContext,0)


        def GT(self):
            return self.getToken(MDLParser.GT, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_typeArgs

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeArgs" ):
                return visitor.visitTypeArgs(self)
            else:
                return visitor.visitChildren(self)




    def typeArgs(self):

        localctx = MDLParser.TypeArgsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 44, self.RULE_typeArgs)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 312
            self.match(MDLParser.LT)
            self.state = 313
            self.typeExprList()
            self.state = 314
            self.match(MDLParser.GT)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeExprListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def typeExpr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.TypeExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.TypeExprContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_typeExprList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeExprList" ):
                return visitor.visitTypeExprList(self)
            else:
                return visitor.visitChildren(self)




    def typeExprList(self):

        localctx = MDLParser.TypeExprListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 46, self.RULE_typeExprList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 316
            self.typeExpr()
            self.state = 321
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==55:
                self.state = 317
                self.match(MDLParser.COMMA)
                self.state = 318
                self.typeExpr()
                self.state = 323
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ValueDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LET(self):
            return self.getToken(MDLParser.LET, 0)

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def EQ(self):
            return self.getToken(MDLParser.EQ, 0)

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def typeAnnotation(self):
            return self.getTypedRuleContext(MDLParser.TypeAnnotationContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_valueDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitValueDecl" ):
                return visitor.visitValueDecl(self)
            else:
                return visitor.visitChildren(self)




    def valueDecl(self):

        localctx = MDLParser.ValueDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 48, self.RULE_valueDecl)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 324
            self.match(MDLParser.LET)
            self.state = 325
            self.nameToken()
            self.state = 327
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==56:
                self.state = 326
                self.typeAnnotation()


            self.state = 329
            self.match(MDLParser.EQ)
            self.state = 330
            self.expr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FuncDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def FUNC(self):
            return self.getToken(MDLParser.FUNC, 0)

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def LPAREN(self):
            return self.getToken(MDLParser.LPAREN, 0)

        def RPAREN(self):
            return self.getToken(MDLParser.RPAREN, 0)

        def ARROW(self):
            return self.getToken(MDLParser.ARROW, 0)

        def typeExpr(self):
            return self.getTypedRuleContext(MDLParser.TypeExprContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def block(self):
            return self.getTypedRuleContext(MDLParser.BlockContext,0)


        def typeParams(self):
            return self.getTypedRuleContext(MDLParser.TypeParamsContext,0)


        def paramList(self):
            return self.getTypedRuleContext(MDLParser.ParamListContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_funcDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitFuncDecl" ):
                return visitor.visitFuncDecl(self)
            else:
                return visitor.visitChildren(self)




    def funcDecl(self):

        localctx = MDLParser.FuncDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 50, self.RULE_funcDecl)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 332
            self.match(MDLParser.FUNC)
            self.state = 333
            self.nameToken()
            self.state = 335
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==65:
                self.state = 334
                self.typeParams()


            self.state = 337
            self.match(MDLParser.LPAREN)
            self.state = 339
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 13491008142573568) != 0):
                self.state = 338
                self.paramList()


            self.state = 341
            self.match(MDLParser.RPAREN)
            self.state = 342
            self.match(MDLParser.ARROW)
            self.state = 343
            self.typeExpr()
            self.state = 344
            self.match(MDLParser.COLON)
            self.state = 345
            self.block()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ParamListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def param(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.ParamContext)
            else:
                return self.getTypedRuleContext(MDLParser.ParamContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_paramList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitParamList" ):
                return visitor.visitParamList(self)
            else:
                return visitor.visitChildren(self)




    def paramList(self):

        localctx = MDLParser.ParamListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 52, self.RULE_paramList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 347
            self.param()
            self.state = 352
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,31,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 348
                    self.match(MDLParser.COMMA)
                    self.state = 349
                    self.param() 
                self.state = 354
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,31,self._ctx)

            self.state = 356
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 355
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ParamContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def pattern(self):
            return self.getTypedRuleContext(MDLParser.PatternContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def typeExpr(self):
            return self.getTypedRuleContext(MDLParser.TypeExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_param

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitParam" ):
                return visitor.visitParam(self)
            else:
                return visitor.visitChildren(self)




    def param(self):

        localctx = MDLParser.ParamContext(self, self._ctx, self.state)
        self.enterRule(localctx, 54, self.RULE_param)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 358
            self.pattern()
            self.state = 359
            self.match(MDLParser.COLON)
            self.state = 360
            self.typeExpr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class EntityDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def ENTITY(self):
            return self.getToken(MDLParser.ENTITY, 0)

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def typeExpr(self):
            return self.getTypedRuleContext(MDLParser.TypeExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_entityDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitEntityDecl" ):
                return visitor.visitEntityDecl(self)
            else:
                return visitor.visitChildren(self)




    def entityDecl(self):

        localctx = MDLParser.EntityDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 56, self.RULE_entityDecl)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 362
            self.match(MDLParser.ENTITY)
            self.state = 363
            self.nameToken()
            self.state = 364
            self.match(MDLParser.COLON)
            self.state = 365
            self.typeExpr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RuleDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def RULE(self):
            return self.getToken(MDLParser.RULE, 0)

        def ruleBody(self):
            return self.getTypedRuleContext(MDLParser.RuleBodyContext,0)


        def ruleStrength(self):
            return self.getTypedRuleContext(MDLParser.RuleStrengthContext,0)


        def OTHERWISE(self):
            return self.getToken(MDLParser.OTHERWISE, 0)

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_ruleDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRuleDecl" ):
                return visitor.visitRuleDecl(self)
            else:
                return visitor.visitChildren(self)




    def ruleDecl(self):

        localctx = MDLParser.RuleDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 58, self.RULE_ruleDecl)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 368
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 114688) != 0):
                self.state = 367
                self.ruleStrength()


            self.state = 370
            self.match(MDLParser.RULE)
            self.state = 371
            self.ruleBody()
            self.state = 374
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==40:
                self.state = 372
                self.match(MDLParser.OTHERWISE)
                self.state = 373
                self.expr()


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RuleStrengthContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRICT(self):
            return self.getToken(MDLParser.STRICT, 0)

        def DEFEASIBLE(self):
            return self.getToken(MDLParser.DEFEASIBLE, 0)

        def DEFEATER(self):
            return self.getToken(MDLParser.DEFEATER, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_ruleStrength

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRuleStrength" ):
                return visitor.visitRuleStrength(self)
            else:
                return visitor.visitChildren(self)




    def ruleStrength(self):

        localctx = MDLParser.RuleStrengthContext(self, self._ctx, self.state)
        self.enterRule(localctx, 60, self.RULE_ruleStrength)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 376
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 114688) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RuleBodyContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def deonticMod(self):
            return self.getTypedRuleContext(MDLParser.DeonticModContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.ExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.ExprContext,i)


        def qualifiedName(self):
            return self.getTypedRuleContext(MDLParser.QualifiedNameContext,0)


        def WHEN(self):
            return self.getToken(MDLParser.WHEN, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_ruleBody

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRuleBody" ):
                return visitor.visitRuleBody(self)
            else:
                return visitor.visitChildren(self)




    def ruleBody(self):

        localctx = MDLParser.RuleBodyContext(self, self._ctx, self.state)
        self.enterRule(localctx, 62, self.RULE_ruleBody)
        self._la = 0 # Token type
        try:
            self.state = 393
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,37,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 378
                self.deonticMod()
                self.state = 379
                self.match(MDLParser.COLON)
                self.state = 380
                self.expr()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 383
                self._errHandler.sync(self)
                la_ = self._interp.adaptivePredict(self._input,35,self._ctx)
                if la_ == 1:
                    self.state = 382
                    self.deonticMod()


                self.state = 385
                self.qualifiedName()
                self.state = 388
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==24:
                    self.state = 386
                    self.match(MDLParser.WHEN)
                    self.state = 387
                    self.expr()


                self.state = 390
                self.match(MDLParser.COLON)
                self.state = 391
                self.expr()
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class DeonticModContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def O(self):
            return self.getToken(MDLParser.O, 0)

        def P(self):
            return self.getToken(MDLParser.P, 0)

        def F(self):
            return self.getToken(MDLParser.F, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_deonticMod

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitDeonticMod" ):
                return visitor.visitDeonticMod(self)
            else:
                return visitor.visitChildren(self)




    def deonticMod(self):

        localctx = MDLParser.DeonticModContext(self, self._ctx, self.state)
        self.enterRule(localctx, 64, self.RULE_deonticMod)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 395
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 15393162788864) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PriorityDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def qualifiedName(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.QualifiedNameContext)
            else:
                return self.getTypedRuleContext(MDLParser.QualifiedNameContext,i)


        def PRIORITY(self):
            return self.getToken(MDLParser.PRIORITY, 0)

        def OVERRIDE(self):
            return self.getToken(MDLParser.OVERRIDE, 0)

        def GT(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.GT)
            else:
                return self.getToken(MDLParser.GT, i)

        def getRuleIndex(self):
            return MDLParser.RULE_priorityDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitPriorityDecl" ):
                return visitor.visitPriorityDecl(self)
            else:
                return visitor.visitChildren(self)




    def priorityDecl(self):

        localctx = MDLParser.PriorityDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 66, self.RULE_priorityDecl)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 397
            _la = self._input.LA(1)
            if not(_la==17 or _la==18):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
            self.state = 398
            self.qualifiedName()
            self.state = 403
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==66:
                self.state = 399
                self.match(MDLParser.GT)
                self.state = 400
                self.qualifiedName()
                self.state = 405
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FactDeclContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def FACT(self):
            return self.getToken(MDLParser.FACT, 0)

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def EQ(self):
            return self.getToken(MDLParser.EQ, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_factDecl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitFactDecl" ):
                return visitor.visitFactDecl(self)
            else:
                return visitor.visitChildren(self)




    def factDecl(self):

        localctx = MDLParser.FactDeclContext(self, self._ctx, self.state)
        self.enterRule(localctx, 68, self.RULE_factDecl)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 406
            self.match(MDLParser.FACT)
            self.state = 410
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,39,self._ctx)
            if la_ == 1:
                self.state = 407
                self.nameToken()
                self.state = 408
                self.match(MDLParser.EQ)


            self.state = 412
            self.expr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class BlockContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def NEWLINE(self):
            return self.getToken(MDLParser.NEWLINE, 0)

        def INDENT(self):
            return self.getToken(MDLParser.INDENT, 0)

        def DEDENT(self):
            return self.getToken(MDLParser.DEDENT, 0)

        def blockLetStmt(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.BlockLetStmtContext)
            else:
                return self.getTypedRuleContext(MDLParser.BlockLetStmtContext,i)


        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def newlines(self):
            return self.getTypedRuleContext(MDLParser.NewlinesContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_block

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitBlock" ):
                return visitor.visitBlock(self)
            else:
                return visitor.visitChildren(self)




    def block(self):

        localctx = MDLParser.BlockContext(self, self._ctx, self.state)
        self.enterRule(localctx, 70, self.RULE_block)
        self._la = 0 # Token type
        try:
            self.state = 430
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [67]:
                self.enterOuterAlt(localctx, 1)
                self.state = 414
                self.match(MDLParser.NEWLINE)
                self.state = 415
                self.match(MDLParser.INDENT)
                self.state = 419
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,40,self._ctx)
                while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                    if _alt==1:
                        self.state = 416
                        self.blockLetStmt() 
                    self.state = 421
                    self._errHandler.sync(self)
                    _alt = self._interp.adaptivePredict(self._input,40,self._ctx)

                self.state = 423
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if (((_la) & ~0x3f) == 0 and ((1 << _la) & 1156842498842166272) != 0):
                    self.state = 422
                    self.expr()


                self.state = 426
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==67:
                    self.state = 425
                    self.newlines()


                self.state = 428
                self.match(MDLParser.DEDENT)
                pass
            elif token in [10, 20, 23, 26, 27, 28, 31, 32, 33, 34, 35, 36, 41, 42, 43, 45, 46, 47, 48, 50, 51, 60]:
                self.enterOuterAlt(localctx, 2)
                self.state = 429
                self.expr()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class BlockLetStmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LET(self):
            return self.getToken(MDLParser.LET, 0)

        def pattern(self):
            return self.getTypedRuleContext(MDLParser.PatternContext,0)


        def EQ(self):
            return self.getToken(MDLParser.EQ, 0)

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def newlines(self):
            return self.getTypedRuleContext(MDLParser.NewlinesContext,0)


        def typeAnnotation(self):
            return self.getTypedRuleContext(MDLParser.TypeAnnotationContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_blockLetStmt

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitBlockLetStmt" ):
                return visitor.visitBlockLetStmt(self)
            else:
                return visitor.visitChildren(self)




    def blockLetStmt(self):

        localctx = MDLParser.BlockLetStmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 72, self.RULE_blockLetStmt)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 432
            self.match(MDLParser.LET)
            self.state = 433
            self.pattern()
            self.state = 435
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==56:
                self.state = 434
                self.typeAnnotation()


            self.state = 437
            self.match(MDLParser.EQ)
            self.state = 438
            self.expr()
            self.state = 439
            self.newlines()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TypeAnnotationContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def typeExpr(self):
            return self.getTypedRuleContext(MDLParser.TypeExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_typeAnnotation

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTypeAnnotation" ):
                return visitor.visitTypeAnnotation(self)
            else:
                return visitor.visitChildren(self)




    def typeAnnotation(self):

        localctx = MDLParser.TypeAnnotationContext(self, self._ctx, self.state)
        self.enterRule(localctx, 74, self.RULE_typeAnnotation)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 441
            self.match(MDLParser.COLON)
            self.state = 442
            self.typeExpr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def temporalPostfix(self):
            return self.getTypedRuleContext(MDLParser.TemporalPostfixContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_expr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExpr" ):
                return visitor.visitExpr(self)
            else:
                return visitor.visitChildren(self)




    def expr(self):

        localctx = MDLParser.ExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 76, self.RULE_expr)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 444
            self.temporalPostfix()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TemporalPostfixContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def implication(self):
            return self.getTypedRuleContext(MDLParser.ImplicationContext,0)


        def temporalUnaryOp(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.TemporalUnaryOpContext)
            else:
                return self.getTypedRuleContext(MDLParser.TemporalUnaryOpContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_temporalPostfix

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTemporalPostfix" ):
                return visitor.visitTemporalPostfix(self)
            else:
                return visitor.visitChildren(self)




    def temporalPostfix(self):

        localctx = MDLParser.TemporalPostfixContext(self, self._ctx, self.state)
        self.enterRule(localctx, 78, self.RULE_temporalPostfix)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 446
            self.implication()
            self.state = 450
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,45,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 447
                    self.temporalUnaryOp() 
                self.state = 452
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,45,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ImplicationContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def orExpr(self):
            return self.getTypedRuleContext(MDLParser.OrExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_implication

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitImplication" ):
                return visitor.visitImplication(self)
            else:
                return visitor.visitChildren(self)




    def implication(self):

        localctx = MDLParser.ImplicationContext(self, self._ctx, self.state)
        self.enterRule(localctx, 80, self.RULE_implication)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 453
            self.orExpr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class OrExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def andExpr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.AndExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.AndExprContext,i)


        def OR(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.OR)
            else:
                return self.getToken(MDLParser.OR, i)

        def getRuleIndex(self):
            return MDLParser.RULE_orExpr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitOrExpr" ):
                return visitor.visitOrExpr(self)
            else:
                return visitor.visitChildren(self)




    def orExpr(self):

        localctx = MDLParser.OrExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 82, self.RULE_orExpr)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 455
            self.andExpr()
            self.state = 460
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,46,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 456
                    self.match(MDLParser.OR)
                    self.state = 457
                    self.andExpr() 
                self.state = 462
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,46,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AndExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def temporalBinary(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.TemporalBinaryContext)
            else:
                return self.getTypedRuleContext(MDLParser.TemporalBinaryContext,i)


        def AND(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.AND)
            else:
                return self.getToken(MDLParser.AND, i)

        def getRuleIndex(self):
            return MDLParser.RULE_andExpr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAndExpr" ):
                return visitor.visitAndExpr(self)
            else:
                return visitor.visitChildren(self)




    def andExpr(self):

        localctx = MDLParser.AndExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 84, self.RULE_andExpr)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 463
            self.temporalBinary()
            self.state = 468
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,47,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 464
                    self.match(MDLParser.AND)
                    self.state = 465
                    self.temporalBinary() 
                self.state = 470
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,47,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TemporalBinaryContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def comparison(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.ComparisonContext)
            else:
                return self.getTypedRuleContext(MDLParser.ComparisonContext,i)


        def UNTIL(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.UNTIL)
            else:
                return self.getToken(MDLParser.UNTIL, i)

        def RELEASE(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.RELEASE)
            else:
                return self.getToken(MDLParser.RELEASE, i)

        def WEAK_UNTIL(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.WEAK_UNTIL)
            else:
                return self.getToken(MDLParser.WEAK_UNTIL, i)

        def getRuleIndex(self):
            return MDLParser.RULE_temporalBinary

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTemporalBinary" ):
                return visitor.visitTemporalBinary(self)
            else:
                return visitor.visitChildren(self)




    def temporalBinary(self):

        localctx = MDLParser.TemporalBinaryContext(self, self._ctx, self.state)
        self.enterRule(localctx, 86, self.RULE_temporalBinary)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 471
            self.comparison()
            self.state = 476
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,48,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 472
                    _la = self._input.LA(1)
                    if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 962072674304) != 0)):
                        self._errHandler.recoverInline(self)
                    else:
                        self._errHandler.reportMatch(self)
                        self.consume()
                    self.state = 473
                    self.comparison() 
                self.state = 478
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,48,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ComparisonContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def additive(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.AdditiveContext)
            else:
                return self.getTypedRuleContext(MDLParser.AdditiveContext,i)


        def EQ(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.EQ)
            else:
                return self.getToken(MDLParser.EQ, i)

        def EQEQ(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.EQEQ)
            else:
                return self.getToken(MDLParser.EQEQ, i)

        def NE(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.NE)
            else:
                return self.getToken(MDLParser.NE, i)

        def LT(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.LT)
            else:
                return self.getToken(MDLParser.LT, i)

        def LE(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.LE)
            else:
                return self.getToken(MDLParser.LE, i)

        def GT(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.GT)
            else:
                return self.getToken(MDLParser.GT, i)

        def GE(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.GE)
            else:
                return self.getToken(MDLParser.GE, i)

        def getRuleIndex(self):
            return MDLParser.RULE_comparison

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitComparison" ):
                return visitor.visitComparison(self)
            else:
                return visitor.visitChildren(self)




    def comparison(self):

        localctx = MDLParser.ComparisonContext(self, self._ctx, self.state)
        self.enterRule(localctx, 88, self.RULE_comparison)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 479
            self.additive()
            self.state = 484
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,49,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 480
                    _la = self._input.LA(1)
                    if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 60) != 0) or ((((_la - 64)) & ~0x3f) == 0 and ((1 << (_la - 64)) & 7) != 0)):
                        self._errHandler.recoverInline(self)
                    else:
                        self._errHandler.reportMatch(self)
                        self.consume()
                    self.state = 481
                    self.additive() 
                self.state = 486
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,49,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AdditiveContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def multiplicative(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.MultiplicativeContext)
            else:
                return self.getTypedRuleContext(MDLParser.MultiplicativeContext,i)


        def PLUS(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.PLUS)
            else:
                return self.getToken(MDLParser.PLUS, i)

        def MINUS(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.MINUS)
            else:
                return self.getToken(MDLParser.MINUS, i)

        def getRuleIndex(self):
            return MDLParser.RULE_additive

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAdditive" ):
                return visitor.visitAdditive(self)
            else:
                return visitor.visitChildren(self)




    def additive(self):

        localctx = MDLParser.AdditiveContext(self, self._ctx, self.state)
        self.enterRule(localctx, 90, self.RULE_additive)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 487
            self.multiplicative()
            self.state = 492
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,50,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 488
                    _la = self._input.LA(1)
                    if not(_la==59 or _la==60):
                        self._errHandler.recoverInline(self)
                    else:
                        self._errHandler.reportMatch(self)
                        self.consume()
                    self.state = 489
                    self.multiplicative() 
                self.state = 494
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,50,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class MultiplicativeContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def unary(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.UnaryContext)
            else:
                return self.getTypedRuleContext(MDLParser.UnaryContext,i)


        def STAR(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.STAR)
            else:
                return self.getToken(MDLParser.STAR, i)

        def SLASH(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.SLASH)
            else:
                return self.getToken(MDLParser.SLASH, i)

        def PERCENT(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.PERCENT)
            else:
                return self.getToken(MDLParser.PERCENT, i)

        def getRuleIndex(self):
            return MDLParser.RULE_multiplicative

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitMultiplicative" ):
                return visitor.visitMultiplicative(self)
            else:
                return visitor.visitChildren(self)




    def multiplicative(self):

        localctx = MDLParser.MultiplicativeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 92, self.RULE_multiplicative)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 495
            self.unary()
            self.state = 500
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,51,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 496
                    _la = self._input.LA(1)
                    if not((((_la) & ~0x3f) == 0 and ((1 << _la) & -2305843009213693952) != 0)):
                        self._errHandler.recoverInline(self)
                    else:
                        self._errHandler.reportMatch(self)
                        self.consume()
                    self.state = 497
                    self.unary() 
                self.state = 502
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,51,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class UnaryContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def ifExpr(self):
            return self.getTypedRuleContext(MDLParser.IfExprContext,0)


        def letExpr(self):
            return self.getTypedRuleContext(MDLParser.LetExprContext,0)


        def matchExpr(self):
            return self.getTypedRuleContext(MDLParser.MatchExprContext,0)


        def unary(self):
            return self.getTypedRuleContext(MDLParser.UnaryContext,0)


        def NOT(self):
            return self.getToken(MDLParser.NOT, 0)

        def MINUS(self):
            return self.getToken(MDLParser.MINUS, 0)

        def temporalUnaryOp(self):
            return self.getTypedRuleContext(MDLParser.TemporalUnaryOpContext,0)


        def postfix(self):
            return self.getTypedRuleContext(MDLParser.PostfixContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_unary

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitUnary" ):
                return visitor.visitUnary(self)
            else:
                return visitor.visitChildren(self)




    def unary(self):

        localctx = MDLParser.UnaryContext(self, self._ctx, self.state)
        self.enterRule(localctx, 94, self.RULE_unary)
        try:
            self.state = 513
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [20]:
                self.enterOuterAlt(localctx, 1)
                self.state = 503
                self.ifExpr()
                pass
            elif token in [10]:
                self.enterOuterAlt(localctx, 2)
                self.state = 504
                self.letExpr()
                pass
            elif token in [23]:
                self.enterOuterAlt(localctx, 3)
                self.state = 505
                self.matchExpr()
                pass
            elif token in [31, 32, 33, 34, 35, 36, 60]:
                self.enterOuterAlt(localctx, 4)
                self.state = 509
                self._errHandler.sync(self)
                token = self._input.LA(1)
                if token in [31]:
                    self.state = 506
                    self.match(MDLParser.NOT)
                    pass
                elif token in [60]:
                    self.state = 507
                    self.match(MDLParser.MINUS)
                    pass
                elif token in [32, 33, 34, 35, 36]:
                    self.state = 508
                    self.temporalUnaryOp()
                    pass
                else:
                    raise NoViableAltException(self)

                self.state = 511
                self.unary()
                pass
            elif token in [26, 27, 28, 41, 42, 43, 45, 46, 47, 48, 50, 51]:
                self.enterOuterAlt(localctx, 5)
                self.state = 512
                self.postfix()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class IfExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IF(self):
            return self.getToken(MDLParser.IF, 0)

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.ExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.ExprContext,i)


        def THEN(self):
            return self.getToken(MDLParser.THEN, 0)

        def ELSE(self):
            return self.getToken(MDLParser.ELSE, 0)

        def newlines(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NewlinesContext)
            else:
                return self.getTypedRuleContext(MDLParser.NewlinesContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_ifExpr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitIfExpr" ):
                return visitor.visitIfExpr(self)
            else:
                return visitor.visitChildren(self)




    def ifExpr(self):

        localctx = MDLParser.IfExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 96, self.RULE_ifExpr)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 515
            self.match(MDLParser.IF)
            self.state = 516
            self.expr()
            self.state = 518
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 517
                self.newlines()


            self.state = 520
            self.match(MDLParser.THEN)
            self.state = 521
            self.expr()
            self.state = 523
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 522
                self.newlines()


            self.state = 525
            self.match(MDLParser.ELSE)
            self.state = 526
            self.expr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LetExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LET(self):
            return self.getToken(MDLParser.LET, 0)

        def pattern(self):
            return self.getTypedRuleContext(MDLParser.PatternContext,0)


        def EQ(self):
            return self.getToken(MDLParser.EQ, 0)

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.ExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.ExprContext,i)


        def IN(self):
            return self.getToken(MDLParser.IN, 0)

        def typeAnnotation(self):
            return self.getTypedRuleContext(MDLParser.TypeAnnotationContext,0)


        def newlines(self):
            return self.getTypedRuleContext(MDLParser.NewlinesContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_letExpr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitLetExpr" ):
                return visitor.visitLetExpr(self)
            else:
                return visitor.visitChildren(self)




    def letExpr(self):

        localctx = MDLParser.LetExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 98, self.RULE_letExpr)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 528
            self.match(MDLParser.LET)
            self.state = 529
            self.pattern()
            self.state = 531
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==56:
                self.state = 530
                self.typeAnnotation()


            self.state = 533
            self.match(MDLParser.EQ)
            self.state = 534
            self.expr()
            self.state = 536
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==67:
                self.state = 535
                self.newlines()


            self.state = 538
            self.match(MDLParser.IN)
            self.state = 539
            self.expr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class MatchExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def CASE(self):
            return self.getToken(MDLParser.CASE, 0)

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def caseBody(self):
            return self.getTypedRuleContext(MDLParser.CaseBodyContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_matchExpr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitMatchExpr" ):
                return visitor.visitMatchExpr(self)
            else:
                return visitor.visitChildren(self)




    def matchExpr(self):

        localctx = MDLParser.MatchExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 100, self.RULE_matchExpr)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 541
            self.match(MDLParser.CASE)
            self.state = 542
            self.expr()
            self.state = 543
            self.match(MDLParser.COLON)
            self.state = 544
            self.caseBody()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class CaseBodyContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def NEWLINE(self):
            return self.getToken(MDLParser.NEWLINE, 0)

        def INDENT(self):
            return self.getToken(MDLParser.INDENT, 0)

        def caseArm(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.CaseArmContext)
            else:
                return self.getTypedRuleContext(MDLParser.CaseArmContext,i)


        def DEDENT(self):
            return self.getToken(MDLParser.DEDENT, 0)

        def newlines(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NewlinesContext)
            else:
                return self.getTypedRuleContext(MDLParser.NewlinesContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_caseBody

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitCaseBody" ):
                return visitor.visitCaseBody(self)
            else:
                return visitor.visitChildren(self)




    def caseBody(self):

        localctx = MDLParser.CaseBodyContext(self, self._ctx, self.state)
        self.enterRule(localctx, 102, self.RULE_caseBody)
        self._la = 0 # Token type
        try:
            self.state = 583
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,66,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 546
                self.match(MDLParser.NEWLINE)
                self.state = 547
                self.match(MDLParser.INDENT)
                self.state = 549
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==67:
                    self.state = 548
                    self.newlines()


                self.state = 551
                self.caseArm()
                self.state = 558
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,60,self._ctx)
                while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                    if _alt==1:
                        self.state = 553
                        self._errHandler.sync(self)
                        _la = self._input.LA(1)
                        if _la==67:
                            self.state = 552
                            self.newlines()


                        self.state = 555
                        self.caseArm() 
                    self.state = 560
                    self._errHandler.sync(self)
                    _alt = self._interp.adaptivePredict(self._input,60,self._ctx)

                self.state = 562
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==67:
                    self.state = 561
                    self.newlines()


                self.state = 564
                self.match(MDLParser.DEDENT)
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 566
                self.match(MDLParser.NEWLINE)
                self.state = 568
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==67:
                    self.state = 567
                    self.newlines()


                self.state = 570
                self.caseArm()
                self.state = 577
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,64,self._ctx)
                while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                    if _alt==1:
                        self.state = 572
                        self._errHandler.sync(self)
                        _la = self._input.LA(1)
                        if _la==67:
                            self.state = 571
                            self.newlines()


                        self.state = 574
                        self.caseArm() 
                    self.state = 579
                    self._errHandler.sync(self)
                    _alt = self._interp.adaptivePredict(self._input,64,self._ctx)

                self.state = 581
                self._errHandler.sync(self)
                la_ = self._interp.adaptivePredict(self._input,65,self._ctx)
                if la_ == 1:
                    self.state = 580
                    self.newlines()


                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class CaseArmContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def BAR(self):
            return self.getToken(MDLParser.BAR, 0)

        def pattern(self):
            return self.getTypedRuleContext(MDLParser.PatternContext,0)


        def COLON(self):
            return self.getToken(MDLParser.COLON, 0)

        def block(self):
            return self.getTypedRuleContext(MDLParser.BlockContext,0)


        def WHEN(self):
            return self.getToken(MDLParser.WHEN, 0)

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_caseArm

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitCaseArm" ):
                return visitor.visitCaseArm(self)
            else:
                return visitor.visitChildren(self)




    def caseArm(self):

        localctx = MDLParser.CaseArmContext(self, self._ctx, self.state)
        self.enterRule(localctx, 104, self.RULE_caseArm)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 585
            self.match(MDLParser.BAR)
            self.state = 586
            self.pattern()
            self.state = 589
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==24:
                self.state = 587
                self.match(MDLParser.WHEN)
                self.state = 588
                self.expr()


            self.state = 591
            self.match(MDLParser.COLON)
            self.state = 592
            self.block()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PostfixContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def primary(self):
            return self.getTypedRuleContext(MDLParser.PrimaryContext,0)


        def postfixSuffix(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.PostfixSuffixContext)
            else:
                return self.getTypedRuleContext(MDLParser.PostfixSuffixContext,i)


        def getRuleIndex(self):
            return MDLParser.RULE_postfix

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitPostfix" ):
                return visitor.visitPostfix(self)
            else:
                return visitor.visitChildren(self)




    def postfix(self):

        localctx = MDLParser.PostfixContext(self, self._ctx, self.state)
        self.enterRule(localctx, 106, self.RULE_postfix)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 594
            self.primary()
            self.state = 598
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 155374187144282112) != 0):
                self.state = 595
                self.postfixSuffix()
                self.state = 600
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PostfixSuffixContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def recordConstructorFields(self):
            return self.getTypedRuleContext(MDLParser.RecordConstructorFieldsContext,0)


        def LPAREN(self):
            return self.getToken(MDLParser.LPAREN, 0)

        def RPAREN(self):
            return self.getToken(MDLParser.RPAREN, 0)

        def exprList(self):
            return self.getTypedRuleContext(MDLParser.ExprListContext,0)


        def DOT(self):
            return self.getToken(MDLParser.DOT, 0)

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_postfixSuffix

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitPostfixSuffix" ):
                return visitor.visitPostfixSuffix(self)
            else:
                return visitor.visitChildren(self)




    def postfixSuffix(self):

        localctx = MDLParser.PostfixSuffixContext(self, self._ctx, self.state)
        self.enterRule(localctx, 108, self.RULE_postfixSuffix)
        self._la = 0 # Token type
        try:
            self.state = 609
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [53]:
                self.enterOuterAlt(localctx, 1)
                self.state = 601
                self.recordConstructorFields()
                pass
            elif token in [51]:
                self.enterOuterAlt(localctx, 2)
                self.state = 602
                self.match(MDLParser.LPAREN)
                self.state = 604
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if (((_la) & ~0x3f) == 0 and ((1 << _la) & 1156842498842166272) != 0):
                    self.state = 603
                    self.exprList()


                self.state = 606
                self.match(MDLParser.RPAREN)
                pass
            elif token in [57]:
                self.enterOuterAlt(localctx, 3)
                self.state = 607
                self.match(MDLParser.DOT)
                self.state = 608
                self.nameToken()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RecordConstructorFieldsContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LBRACE(self):
            return self.getToken(MDLParser.LBRACE, 0)

        def RBRACE(self):
            return self.getToken(MDLParser.RBRACE, 0)

        def recordConstructorFieldList(self):
            return self.getTypedRuleContext(MDLParser.RecordConstructorFieldListContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_recordConstructorFields

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRecordConstructorFields" ):
                return visitor.visitRecordConstructorFields(self)
            else:
                return visitor.visitChildren(self)




    def recordConstructorFields(self):

        localctx = MDLParser.RecordConstructorFieldsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 110, self.RULE_recordConstructorFields)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 611
            self.match(MDLParser.LBRACE)
            self.state = 613
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 1141293539393536) != 0):
                self.state = 612
                self.recordConstructorFieldList()


            self.state = 615
            self.match(MDLParser.RBRACE)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RecordConstructorFieldListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def recordConstructorField(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.RecordConstructorFieldContext)
            else:
                return self.getTypedRuleContext(MDLParser.RecordConstructorFieldContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_recordConstructorFieldList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRecordConstructorFieldList" ):
                return visitor.visitRecordConstructorFieldList(self)
            else:
                return visitor.visitChildren(self)




    def recordConstructorFieldList(self):

        localctx = MDLParser.RecordConstructorFieldListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 112, self.RULE_recordConstructorFieldList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 617
            self.recordConstructorField()
            self.state = 622
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,72,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 618
                    self.match(MDLParser.COMMA)
                    self.state = 619
                    self.recordConstructorField() 
                self.state = 624
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,72,self._ctx)

            self.state = 626
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 625
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RecordConstructorFieldContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def EQ(self):
            return self.getToken(MDLParser.EQ, 0)

        def expr(self):
            return self.getTypedRuleContext(MDLParser.ExprContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_recordConstructorField

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRecordConstructorField" ):
                return visitor.visitRecordConstructorField(self)
            else:
                return visitor.visitChildren(self)




    def recordConstructorField(self):

        localctx = MDLParser.RecordConstructorFieldContext(self, self._ctx, self.state)
        self.enterRule(localctx, 114, self.RULE_recordConstructorField)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 628
            self.nameToken()
            self.state = 629
            self.match(MDLParser.EQ)
            self.state = 630
            self.expr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PrimaryContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRING(self):
            return self.getToken(MDLParser.STRING, 0)

        def INT(self):
            return self.getToken(MDLParser.INT, 0)

        def DECIMAL(self):
            return self.getToken(MDLParser.DECIMAL, 0)

        def RAT(self):
            return self.getToken(MDLParser.RAT, 0)

        def TRUE(self):
            return self.getToken(MDLParser.TRUE, 0)

        def FALSE(self):
            return self.getToken(MDLParser.FALSE, 0)

        def LAST(self):
            return self.getToken(MDLParser.LAST, 0)

        def qualifiedName(self):
            return self.getTypedRuleContext(MDLParser.QualifiedNameContext,0)


        def LPAREN(self):
            return self.getToken(MDLParser.LPAREN, 0)

        def RPAREN(self):
            return self.getToken(MDLParser.RPAREN, 0)

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.ExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.ExprContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_primary

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitPrimary" ):
                return visitor.visitPrimary(self)
            else:
                return visitor.visitChildren(self)




    def primary(self):

        localctx = MDLParser.PrimaryContext(self, self._ctx, self.state)
        self.enterRule(localctx, 116, self.RULE_primary)
        self._la = 0 # Token type
        try:
            self.state = 659
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,75,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 632
                self.match(MDLParser.STRING)
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 633
                self.match(MDLParser.INT)
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 634
                self.match(MDLParser.DECIMAL)
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 635
                self.match(MDLParser.RAT)
                pass

            elif la_ == 5:
                self.enterOuterAlt(localctx, 5)
                self.state = 636
                self.match(MDLParser.TRUE)
                pass

            elif la_ == 6:
                self.enterOuterAlt(localctx, 6)
                self.state = 637
                self.match(MDLParser.FALSE)
                pass

            elif la_ == 7:
                self.enterOuterAlt(localctx, 7)
                self.state = 638
                self.match(MDLParser.LAST)
                pass

            elif la_ == 8:
                self.enterOuterAlt(localctx, 8)
                self.state = 639
                self.qualifiedName()
                pass

            elif la_ == 9:
                self.enterOuterAlt(localctx, 9)
                self.state = 640
                self.match(MDLParser.LPAREN)
                self.state = 641
                self.match(MDLParser.RPAREN)
                pass

            elif la_ == 10:
                self.enterOuterAlt(localctx, 10)
                self.state = 642
                self.match(MDLParser.LPAREN)
                self.state = 643
                self.expr()
                self.state = 644
                self.match(MDLParser.RPAREN)
                pass

            elif la_ == 11:
                self.enterOuterAlt(localctx, 11)
                self.state = 646
                self.match(MDLParser.LPAREN)
                self.state = 647
                self.expr()
                self.state = 648
                self.match(MDLParser.COMMA)
                self.state = 649
                self.expr()
                self.state = 654
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while _la==55:
                    self.state = 650
                    self.match(MDLParser.COMMA)
                    self.state = 651
                    self.expr()
                    self.state = 656
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)

                self.state = 657
                self.match(MDLParser.RPAREN)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.ExprContext)
            else:
                return self.getTypedRuleContext(MDLParser.ExprContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_exprList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExprList" ):
                return visitor.visitExprList(self)
            else:
                return visitor.visitChildren(self)




    def exprList(self):

        localctx = MDLParser.ExprListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 118, self.RULE_exprList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 661
            self.expr()
            self.state = 666
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,76,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 662
                    self.match(MDLParser.COMMA)
                    self.state = 663
                    self.expr() 
                self.state = 668
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,76,self._ctx)

            self.state = 670
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 669
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PatternContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def UNDERSCORE(self):
            return self.getToken(MDLParser.UNDERSCORE, 0)

        def STRING(self):
            return self.getToken(MDLParser.STRING, 0)

        def INT(self):
            return self.getToken(MDLParser.INT, 0)

        def DECIMAL(self):
            return self.getToken(MDLParser.DECIMAL, 0)

        def RAT(self):
            return self.getToken(MDLParser.RAT, 0)

        def LPAREN(self):
            return self.getToken(MDLParser.LPAREN, 0)

        def RPAREN(self):
            return self.getToken(MDLParser.RPAREN, 0)

        def pattern(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.PatternContext)
            else:
                return self.getTypedRuleContext(MDLParser.PatternContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def LBRACE(self):
            return self.getToken(MDLParser.LBRACE, 0)

        def RBRACE(self):
            return self.getToken(MDLParser.RBRACE, 0)

        def recordPatternFieldList(self):
            return self.getTypedRuleContext(MDLParser.RecordPatternFieldListContext,0)


        def qualifiedName(self):
            return self.getTypedRuleContext(MDLParser.QualifiedNameContext,0)


        def patternList(self):
            return self.getTypedRuleContext(MDLParser.PatternListContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_pattern

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitPattern" ):
                return visitor.visitPattern(self)
            else:
                return visitor.visitChildren(self)




    def pattern(self):

        localctx = MDLParser.PatternContext(self, self._ctx, self.state)
        self.enterRule(localctx, 120, self.RULE_pattern)
        self._la = 0 # Token type
        try:
            self.state = 709
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,82,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 672
                self.match(MDLParser.UNDERSCORE)
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 673
                self.match(MDLParser.STRING)
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 674
                self.match(MDLParser.INT)
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 675
                self.match(MDLParser.DECIMAL)
                pass

            elif la_ == 5:
                self.enterOuterAlt(localctx, 5)
                self.state = 676
                self.match(MDLParser.RAT)
                pass

            elif la_ == 6:
                self.enterOuterAlt(localctx, 6)
                self.state = 677
                self.match(MDLParser.LPAREN)
                self.state = 678
                self.match(MDLParser.RPAREN)
                pass

            elif la_ == 7:
                self.enterOuterAlt(localctx, 7)
                self.state = 679
                self.match(MDLParser.LPAREN)
                self.state = 680
                self.pattern()
                self.state = 681
                self.match(MDLParser.RPAREN)
                pass

            elif la_ == 8:
                self.enterOuterAlt(localctx, 8)
                self.state = 683
                self.match(MDLParser.LPAREN)
                self.state = 684
                self.pattern()
                self.state = 685
                self.match(MDLParser.COMMA)
                self.state = 686
                self.pattern()
                self.state = 691
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while _la==55:
                    self.state = 687
                    self.match(MDLParser.COMMA)
                    self.state = 688
                    self.pattern()
                    self.state = 693
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)

                self.state = 694
                self.match(MDLParser.RPAREN)
                pass

            elif la_ == 9:
                self.enterOuterAlt(localctx, 9)
                self.state = 696
                self.match(MDLParser.LBRACE)
                self.state = 698
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if (((_la) & ~0x3f) == 0 and ((1 << _la) & 1141293539393536) != 0):
                    self.state = 697
                    self.recordPatternFieldList()


                self.state = 700
                self.match(MDLParser.RBRACE)
                pass

            elif la_ == 10:
                self.enterOuterAlt(localctx, 10)
                self.state = 701
                self.qualifiedName()
                self.state = 707
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==51:
                    self.state = 702
                    self.match(MDLParser.LPAREN)
                    self.state = 704
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if (((_la) & ~0x3f) == 0 and ((1 << _la) & 13491008142573568) != 0):
                        self.state = 703
                        self.patternList()


                    self.state = 706
                    self.match(MDLParser.RPAREN)


                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PatternListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def pattern(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.PatternContext)
            else:
                return self.getTypedRuleContext(MDLParser.PatternContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_patternList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitPatternList" ):
                return visitor.visitPatternList(self)
            else:
                return visitor.visitChildren(self)




    def patternList(self):

        localctx = MDLParser.PatternListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 122, self.RULE_patternList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 711
            self.pattern()
            self.state = 716
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,83,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 712
                    self.match(MDLParser.COMMA)
                    self.state = 713
                    self.pattern() 
                self.state = 718
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,83,self._ctx)

            self.state = 720
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 719
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RecordPatternFieldListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def recordPatternField(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.RecordPatternFieldContext)
            else:
                return self.getTypedRuleContext(MDLParser.RecordPatternFieldContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.COMMA)
            else:
                return self.getToken(MDLParser.COMMA, i)

        def getRuleIndex(self):
            return MDLParser.RULE_recordPatternFieldList

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRecordPatternFieldList" ):
                return visitor.visitRecordPatternFieldList(self)
            else:
                return visitor.visitChildren(self)




    def recordPatternFieldList(self):

        localctx = MDLParser.RecordPatternFieldListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 124, self.RULE_recordPatternFieldList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 722
            self.recordPatternField()
            self.state = 727
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,85,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 723
                    self.match(MDLParser.COMMA)
                    self.state = 724
                    self.recordPatternField() 
                self.state = 729
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,85,self._ctx)

            self.state = 731
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==55:
                self.state = 730
                self.match(MDLParser.COMMA)


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RecordPatternFieldContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def nameToken(self):
            return self.getTypedRuleContext(MDLParser.NameTokenContext,0)


        def EQ(self):
            return self.getToken(MDLParser.EQ, 0)

        def pattern(self):
            return self.getTypedRuleContext(MDLParser.PatternContext,0)


        def getRuleIndex(self):
            return MDLParser.RULE_recordPatternField

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRecordPatternField" ):
                return visitor.visitRecordPatternField(self)
            else:
                return visitor.visitChildren(self)




    def recordPatternField(self):

        localctx = MDLParser.RecordPatternFieldContext(self, self._ctx, self.state)
        self.enterRule(localctx, 126, self.RULE_recordPatternField)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 733
            self.nameToken()
            self.state = 736
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==64:
                self.state = 734
                self.match(MDLParser.EQ)
                self.state = 735
                self.pattern()


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class QualifiedNameContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def nameToken(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MDLParser.NameTokenContext)
            else:
                return self.getTypedRuleContext(MDLParser.NameTokenContext,i)


        def DOT(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.DOT)
            else:
                return self.getToken(MDLParser.DOT, i)

        def getRuleIndex(self):
            return MDLParser.RULE_qualifiedName

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitQualifiedName" ):
                return visitor.visitQualifiedName(self)
            else:
                return visitor.visitChildren(self)




    def qualifiedName(self):

        localctx = MDLParser.QualifiedNameContext(self, self._ctx, self.state)
        self.enterRule(localctx, 128, self.RULE_qualifiedName)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 738
            self.nameToken()
            self.state = 743
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,88,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 739
                    self.match(MDLParser.DOT)
                    self.state = 740
                    self.nameToken() 
                self.state = 745
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,88,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class NameTokenContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IDENT(self):
            return self.getToken(MDLParser.IDENT, 0)

        def TRUE(self):
            return self.getToken(MDLParser.TRUE, 0)

        def FALSE(self):
            return self.getToken(MDLParser.FALSE, 0)

        def LAST(self):
            return self.getToken(MDLParser.LAST, 0)

        def O(self):
            return self.getToken(MDLParser.O, 0)

        def P(self):
            return self.getToken(MDLParser.P, 0)

        def F(self):
            return self.getToken(MDLParser.F, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_nameToken

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitNameToken" ):
                return visitor.visitNameToken(self)
            else:
                return visitor.visitChildren(self)




    def nameToken(self):

        localctx = MDLParser.NameTokenContext(self, self._ctx, self.state)
        self.enterRule(localctx, 130, self.RULE_nameToken)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 746
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 1141293539393536) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TemporalUnaryOpContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def ALWAYS(self):
            return self.getToken(MDLParser.ALWAYS, 0)

        def EVENTUALLY(self):
            return self.getToken(MDLParser.EVENTUALLY, 0)

        def NEXT(self):
            return self.getToken(MDLParser.NEXT, 0)

        def WEAK_NEXT(self):
            return self.getToken(MDLParser.WEAK_NEXT, 0)

        def NEVER(self):
            return self.getToken(MDLParser.NEVER, 0)

        def getRuleIndex(self):
            return MDLParser.RULE_temporalUnaryOp

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTemporalUnaryOp" ):
                return visitor.visitTemporalUnaryOp(self)
            else:
                return visitor.visitChildren(self)




    def temporalUnaryOp(self):

        localctx = MDLParser.TemporalUnaryOpContext(self, self._ctx, self.state)
        self.enterRule(localctx, 132, self.RULE_temporalUnaryOp)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 748
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 133143986176) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class NewlinesContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def NEWLINE(self, i:int=None):
            if i is None:
                return self.getTokens(MDLParser.NEWLINE)
            else:
                return self.getToken(MDLParser.NEWLINE, i)

        def getRuleIndex(self):
            return MDLParser.RULE_newlines

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitNewlines" ):
                return visitor.visitNewlines(self)
            else:
                return visitor.visitChildren(self)




    def newlines(self):

        localctx = MDLParser.NewlinesContext(self, self._ctx, self.state)
        self.enterRule(localctx, 134, self.RULE_newlines)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 751 
            self._errHandler.sync(self)
            _alt = 1
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt == 1:
                    self.state = 750
                    self.match(MDLParser.NEWLINE)

                else:
                    raise NoViableAltException(self)
                self.state = 753 
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,89,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





