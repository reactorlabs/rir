#ifndef RIR_BUILTIN_IDS_H
#define RIR_BUILTIN_IDS_H
// This file is generated using rir.printBuiltinIds()
#include "utils/String.h"
#include <cassert>
namespace rir {
static inline void errorWrongBuiltin() { assert(false && "wrong builtin id"); }
constexpr static inline int blt(const char* name) {
    if (staticStringEqual(name, "if"))
        return 0;
    else if (staticStringEqual(name, "while"))
        return 1;
    else if (staticStringEqual(name, "for"))
        return 2;
    else if (staticStringEqual(name, "repeat"))
        return 3;
    else if (staticStringEqual(name, "break"))
        return 4;
    else if (staticStringEqual(name, "next"))
        return 5;
    else if (staticStringEqual(name, "return"))
        return 6;
    else if (staticStringEqual(name, "function"))
        return 7;
    else if (staticStringEqual(name, "<-"))
        return 8;
    else if (staticStringEqual(name, "="))
        return 9;
    else if (staticStringEqual(name, "<<-"))
        return 10;
    else if (staticStringEqual(name, "{"))
        return 11;
    else if (staticStringEqual(name, "("))
        return 12;
    else if (staticStringEqual(name, ".subset"))
        return 13;
    else if (staticStringEqual(name, ".subset2"))
        return 14;
    else if (staticStringEqual(name, "["))
        return 15;
    else if (staticStringEqual(name, "[["))
        return 16;
    else if (staticStringEqual(name, "$"))
        return 17;
    else if (staticStringEqual(name, "@"))
        return 18;
    else if (staticStringEqual(name, "[<-"))
        return 19;
    else if (staticStringEqual(name, "[[<-"))
        return 20;
    else if (staticStringEqual(name, "$<-"))
        return 21;
    else if (staticStringEqual(name, "switch"))
        return 22;
    else if (staticStringEqual(name, "browser"))
        return 23;
    else if (staticStringEqual(name, ".primTrace"))
        return 24;
    else if (staticStringEqual(name, ".primUntrace"))
        return 25;
    else if (staticStringEqual(name, ".Internal"))
        return 26;
    else if (staticStringEqual(name, ".Primitive"))
        return 27;
    else if (staticStringEqual(name, "call"))
        return 28;
    else if (staticStringEqual(name, "quote"))
        return 29;
    else if (staticStringEqual(name, "substitute"))
        return 30;
    else if (staticStringEqual(name, "missing"))
        return 31;
    else if (staticStringEqual(name, "nargs"))
        return 32;
    else if (staticStringEqual(name, "on.exit"))
        return 33;
    else if (staticStringEqual(name, "forceAndCall"))
        return 34;
    else if (staticStringEqual(name, "stop"))
        return 35;
    else if (staticStringEqual(name, "warning"))
        return 36;
    else if (staticStringEqual(name, "gettext"))
        return 37;
    else if (staticStringEqual(name, "ngettext"))
        return 38;
    else if (staticStringEqual(name, "bindtextdomain"))
        return 39;
    else if (staticStringEqual(name, ".addCondHands"))
        return 40;
    else if (staticStringEqual(name, ".addGlobHands"))
        return 41;
    else if (staticStringEqual(name, ".resetCondHands"))
        return 42;
    else if (staticStringEqual(name, ".signalCondition"))
        return 43;
    else if (staticStringEqual(name, ".dfltStop"))
        return 44;
    else if (staticStringEqual(name, ".dfltWarn"))
        return 45;
    else if (staticStringEqual(name, ".addRestart"))
        return 46;
    else if (staticStringEqual(name, ".getRestart"))
        return 47;
    else if (staticStringEqual(name, ".invokeRestart"))
        return 48;
    else if (staticStringEqual(name, ".addTryHandlers"))
        return 49;
    else if (staticStringEqual(name, "geterrmessage"))
        return 50;
    else if (staticStringEqual(name, "seterrmessage"))
        return 51;
    else if (staticStringEqual(name, "printDeferredWarnings"))
        return 52;
    else if (staticStringEqual(name, "interruptsSuspended"))
        return 53;
    else if (staticStringEqual(name, "as.function.default"))
        return 54;
    else if (staticStringEqual(name, "debug"))
        return 55;
    else if (staticStringEqual(name, "undebug"))
        return 56;
    else if (staticStringEqual(name, "isdebugged"))
        return 57;
    else if (staticStringEqual(name, "debugonce"))
        return 58;
    else if (staticStringEqual(name, "Recall"))
        return 59;
    else if (staticStringEqual(name, "delayedAssign"))
        return 60;
    else if (staticStringEqual(name, "makeLazy"))
        return 61;
    else if (staticStringEqual(name, "identical"))
        return 62;
    else if (staticStringEqual(name, "C_tryCatchHelper"))
        return 63;
    else if (staticStringEqual(name, "getNamespaceValue"))
        return 64;
    else if (staticStringEqual(name, "+"))
        return 65;
    else if (staticStringEqual(name, "-"))
        return 66;
    else if (staticStringEqual(name, "*"))
        return 67;
    else if (staticStringEqual(name, "/"))
        return 68;
    else if (staticStringEqual(name, "^"))
        return 69;
    else if (staticStringEqual(name, "%%"))
        return 70;
    else if (staticStringEqual(name, "%/%"))
        return 71;
    else if (staticStringEqual(name, "%*%"))
        return 72;
    else if (staticStringEqual(name, "=="))
        return 73;
    else if (staticStringEqual(name, "!="))
        return 74;
    else if (staticStringEqual(name, "<"))
        return 75;
    else if (staticStringEqual(name, "<="))
        return 76;
    else if (staticStringEqual(name, ">="))
        return 77;
    else if (staticStringEqual(name, ">"))
        return 78;
    else if (staticStringEqual(name, "&"))
        return 79;
    else if (staticStringEqual(name, "|"))
        return 80;
    else if (staticStringEqual(name, "!"))
        return 81;
    else if (staticStringEqual(name, "&&"))
        return 82;
    else if (staticStringEqual(name, "||"))
        return 83;
    else if (staticStringEqual(name, ":"))
        return 84;
    else if (staticStringEqual(name, "~"))
        return 85;
    else if (staticStringEqual(name, "::"))
        return 86;
    else if (staticStringEqual(name, ":::"))
        return 87;
    else if (staticStringEqual(name, "all"))
        return 88;
    else if (staticStringEqual(name, "any"))
        return 89;
    else if (staticStringEqual(name, "...elt"))
        return 90;
    else if (staticStringEqual(name, "...length"))
        return 91;
    else if (staticStringEqual(name, "...names"))
        return 92;
    else if (staticStringEqual(name, "length"))
        return 93;
    else if (staticStringEqual(name, "length<-"))
        return 94;
    else if (staticStringEqual(name, "c"))
        return 95;
    else if (staticStringEqual(name, "oldClass"))
        return 96;
    else if (staticStringEqual(name, "oldClass<-"))
        return 97;
    else if (staticStringEqual(name, "class"))
        return 98;
    else if (staticStringEqual(name, ".cache_class"))
        return 99;
    else if (staticStringEqual(name, ".class2"))
        return 100;
    else if (staticStringEqual(name, "class<-"))
        return 101;
    else if (staticStringEqual(name, "unclass"))
        return 102;
    else if (staticStringEqual(name, "names"))
        return 103;
    else if (staticStringEqual(name, "names<-"))
        return 104;
    else if (staticStringEqual(name, "dimnames"))
        return 105;
    else if (staticStringEqual(name, "dimnames<-"))
        return 106;
    else if (staticStringEqual(name, "dim"))
        return 107;
    else if (staticStringEqual(name, "dim<-"))
        return 108;
    else if (staticStringEqual(name, "attributes"))
        return 109;
    else if (staticStringEqual(name, "attributes<-"))
        return 110;
    else if (staticStringEqual(name, "attr"))
        return 111;
    else if (staticStringEqual(name, "attr<-"))
        return 112;
    else if (staticStringEqual(name, "@<-"))
        return 113;
    else if (staticStringEqual(name, "levels<-"))
        return 114;
    else if (staticStringEqual(name, "vector"))
        return 115;
    else if (staticStringEqual(name, "complex"))
        return 116;
    else if (staticStringEqual(name, "matrix"))
        return 117;
    else if (staticStringEqual(name, "array"))
        return 118;
    else if (staticStringEqual(name, "diag"))
        return 119;
    else if (staticStringEqual(name, "backsolve"))
        return 120;
    else if (staticStringEqual(name, "max.col"))
        return 121;
    else if (staticStringEqual(name, "row"))
        return 122;
    else if (staticStringEqual(name, "col"))
        return 123;
    else if (staticStringEqual(name, "unlist"))
        return 124;
    else if (staticStringEqual(name, "cbind"))
        return 125;
    else if (staticStringEqual(name, "rbind"))
        return 126;
    else if (staticStringEqual(name, "drop"))
        return 127;
    else if (staticStringEqual(name, "all.names"))
        return 128;
    else if (staticStringEqual(name, "comment"))
        return 129;
    else if (staticStringEqual(name, "comment<-"))
        return 130;
    else if (staticStringEqual(name, "get"))
        return 131;
    else if (staticStringEqual(name, "get0"))
        return 132;
    else if (staticStringEqual(name, "mget"))
        return 133;
    else if (staticStringEqual(name, "exists"))
        return 134;
    else if (staticStringEqual(name, "assign"))
        return 135;
    else if (staticStringEqual(name, "list2env"))
        return 136;
    else if (staticStringEqual(name, "remove"))
        return 137;
    else if (staticStringEqual(name, "duplicated"))
        return 138;
    else if (staticStringEqual(name, "unique"))
        return 139;
    else if (staticStringEqual(name, "anyDuplicated"))
        return 140;
    else if (staticStringEqual(name, "anyNA"))
        return 141;
    else if (staticStringEqual(name, "which"))
        return 142;
    else if (staticStringEqual(name, "which.min"))
        return 143;
    else if (staticStringEqual(name, "pmin"))
        return 144;
    else if (staticStringEqual(name, "pmax"))
        return 145;
    else if (staticStringEqual(name, "which.max"))
        return 146;
    else if (staticStringEqual(name, "match"))
        return 147;
    else if (staticStringEqual(name, "pmatch"))
        return 148;
    else if (staticStringEqual(name, "charmatch"))
        return 149;
    else if (staticStringEqual(name, "match.call"))
        return 150;
    else if (staticStringEqual(name, "crossprod"))
        return 151;
    else if (staticStringEqual(name, "tcrossprod"))
        return 152;
    else if (staticStringEqual(name, "lengths"))
        return 153;
    else if (staticStringEqual(name, "sequence"))
        return 154;
    else if (staticStringEqual(name, "attach"))
        return 155;
    else if (staticStringEqual(name, "detach"))
        return 156;
    else if (staticStringEqual(name, "search"))
        return 157;
    else if (staticStringEqual(name, "setFileTime"))
        return 158;
    else if (staticStringEqual(name, "round"))
        return 159;
    else if (staticStringEqual(name, "signif"))
        return 160;
    else if (staticStringEqual(name, "log"))
        return 161;
    else if (staticStringEqual(name, "log10"))
        return 162;
    else if (staticStringEqual(name, "log2"))
        return 163;
    else if (staticStringEqual(name, "abs"))
        return 164;
    else if (staticStringEqual(name, "floor"))
        return 165;
    else if (staticStringEqual(name, "ceiling"))
        return 166;
    else if (staticStringEqual(name, "sqrt"))
        return 167;
    else if (staticStringEqual(name, "sign"))
        return 168;
    else if (staticStringEqual(name, "trunc"))
        return 169;
    else if (staticStringEqual(name, "exp"))
        return 170;
    else if (staticStringEqual(name, "expm1"))
        return 171;
    else if (staticStringEqual(name, "log1p"))
        return 172;
    else if (staticStringEqual(name, "cos"))
        return 173;
    else if (staticStringEqual(name, "sin"))
        return 174;
    else if (staticStringEqual(name, "tan"))
        return 175;
    else if (staticStringEqual(name, "acos"))
        return 176;
    else if (staticStringEqual(name, "asin"))
        return 177;
    else if (staticStringEqual(name, "atan"))
        return 178;
    else if (staticStringEqual(name, "cosh"))
        return 179;
    else if (staticStringEqual(name, "sinh"))
        return 180;
    else if (staticStringEqual(name, "tanh"))
        return 181;
    else if (staticStringEqual(name, "acosh"))
        return 182;
    else if (staticStringEqual(name, "asinh"))
        return 183;
    else if (staticStringEqual(name, "atanh"))
        return 184;
    else if (staticStringEqual(name, "lgamma"))
        return 185;
    else if (staticStringEqual(name, "gamma"))
        return 186;
    else if (staticStringEqual(name, "digamma"))
        return 187;
    else if (staticStringEqual(name, "trigamma"))
        return 188;
    else if (staticStringEqual(name, "cospi"))
        return 189;
    else if (staticStringEqual(name, "sinpi"))
        return 190;
    else if (staticStringEqual(name, "tanpi"))
        return 191;
    else if (staticStringEqual(name, "atan2"))
        return 192;
    else if (staticStringEqual(name, "lbeta"))
        return 193;
    else if (staticStringEqual(name, "beta"))
        return 194;
    else if (staticStringEqual(name, "lchoose"))
        return 195;
    else if (staticStringEqual(name, "choose"))
        return 196;
    else if (staticStringEqual(name, "dchisq"))
        return 197;
    else if (staticStringEqual(name, "pchisq"))
        return 198;
    else if (staticStringEqual(name, "qchisq"))
        return 199;
    else if (staticStringEqual(name, "dexp"))
        return 200;
    else if (staticStringEqual(name, "pexp"))
        return 201;
    else if (staticStringEqual(name, "qexp"))
        return 202;
    else if (staticStringEqual(name, "dgeom"))
        return 203;
    else if (staticStringEqual(name, "pgeom"))
        return 204;
    else if (staticStringEqual(name, "qgeom"))
        return 205;
    else if (staticStringEqual(name, "dpois"))
        return 206;
    else if (staticStringEqual(name, "ppois"))
        return 207;
    else if (staticStringEqual(name, "qpois"))
        return 208;
    else if (staticStringEqual(name, "dt"))
        return 209;
    else if (staticStringEqual(name, "pt"))
        return 210;
    else if (staticStringEqual(name, "qt"))
        return 211;
    else if (staticStringEqual(name, "dsignrank"))
        return 212;
    else if (staticStringEqual(name, "psignrank"))
        return 213;
    else if (staticStringEqual(name, "qsignrank"))
        return 214;
    else if (staticStringEqual(name, "besselJ"))
        return 215;
    else if (staticStringEqual(name, "besselY"))
        return 216;
    else if (staticStringEqual(name, "psigamma"))
        return 217;
    else if (staticStringEqual(name, "Re"))
        return 218;
    else if (staticStringEqual(name, "Im"))
        return 219;
    else if (staticStringEqual(name, "Mod"))
        return 220;
    else if (staticStringEqual(name, "Arg"))
        return 221;
    else if (staticStringEqual(name, "Conj"))
        return 222;
    else if (staticStringEqual(name, "dbeta"))
        return 223;
    else if (staticStringEqual(name, "pbeta"))
        return 224;
    else if (staticStringEqual(name, "qbeta"))
        return 225;
    else if (staticStringEqual(name, "dbinom"))
        return 226;
    else if (staticStringEqual(name, "pbinom"))
        return 227;
    else if (staticStringEqual(name, "qbinom"))
        return 228;
    else if (staticStringEqual(name, "dcauchy"))
        return 229;
    else if (staticStringEqual(name, "pcauchy"))
        return 230;
    else if (staticStringEqual(name, "qcauchy"))
        return 231;
    else if (staticStringEqual(name, "df"))
        return 232;
    else if (staticStringEqual(name, "pf"))
        return 233;
    else if (staticStringEqual(name, "qf"))
        return 234;
    else if (staticStringEqual(name, "dgamma"))
        return 235;
    else if (staticStringEqual(name, "pgamma"))
        return 236;
    else if (staticStringEqual(name, "qgamma"))
        return 237;
    else if (staticStringEqual(name, "dlnorm"))
        return 238;
    else if (staticStringEqual(name, "plnorm"))
        return 239;
    else if (staticStringEqual(name, "qlnorm"))
        return 240;
    else if (staticStringEqual(name, "dlogis"))
        return 241;
    else if (staticStringEqual(name, "plogis"))
        return 242;
    else if (staticStringEqual(name, "qlogis"))
        return 243;
    else if (staticStringEqual(name, "dnbinom"))
        return 244;
    else if (staticStringEqual(name, "pnbinom"))
        return 245;
    else if (staticStringEqual(name, "qnbinom"))
        return 246;
    else if (staticStringEqual(name, "dnorm"))
        return 247;
    else if (staticStringEqual(name, "pnorm"))
        return 248;
    else if (staticStringEqual(name, "qnorm"))
        return 249;
    else if (staticStringEqual(name, "dunif"))
        return 250;
    else if (staticStringEqual(name, "punif"))
        return 251;
    else if (staticStringEqual(name, "qunif"))
        return 252;
    else if (staticStringEqual(name, "dweibull"))
        return 253;
    else if (staticStringEqual(name, "pweibull"))
        return 254;
    else if (staticStringEqual(name, "qweibull"))
        return 255;
    else if (staticStringEqual(name, "dnchisq"))
        return 256;
    else if (staticStringEqual(name, "pnchisq"))
        return 257;
    else if (staticStringEqual(name, "qnchisq"))
        return 258;
    else if (staticStringEqual(name, "dnt"))
        return 259;
    else if (staticStringEqual(name, "pnt"))
        return 260;
    else if (staticStringEqual(name, "qnt"))
        return 261;
    else if (staticStringEqual(name, "dwilcox"))
        return 262;
    else if (staticStringEqual(name, "pwilcox"))
        return 263;
    else if (staticStringEqual(name, "qwilcox"))
        return 264;
    else if (staticStringEqual(name, "besselI"))
        return 265;
    else if (staticStringEqual(name, "besselK"))
        return 266;
    else if (staticStringEqual(name, "dnbinom_mu"))
        return 267;
    else if (staticStringEqual(name, "pnbinom_mu"))
        return 268;
    else if (staticStringEqual(name, "qnbinom_mu"))
        return 269;
    else if (staticStringEqual(name, "dhyper"))
        return 270;
    else if (staticStringEqual(name, "phyper"))
        return 271;
    else if (staticStringEqual(name, "qhyper"))
        return 272;
    else if (staticStringEqual(name, "dnbeta"))
        return 273;
    else if (staticStringEqual(name, "pnbeta"))
        return 274;
    else if (staticStringEqual(name, "qnbeta"))
        return 275;
    else if (staticStringEqual(name, "dnf"))
        return 276;
    else if (staticStringEqual(name, "pnf"))
        return 277;
    else if (staticStringEqual(name, "qnf"))
        return 278;
    else if (staticStringEqual(name, "dtukey"))
        return 279;
    else if (staticStringEqual(name, "ptukey"))
        return 280;
    else if (staticStringEqual(name, "qtukey"))
        return 281;
    else if (staticStringEqual(name, "rchisq"))
        return 282;
    else if (staticStringEqual(name, "rexp"))
        return 283;
    else if (staticStringEqual(name, "rgeom"))
        return 284;
    else if (staticStringEqual(name, "rpois"))
        return 285;
    else if (staticStringEqual(name, "rt"))
        return 286;
    else if (staticStringEqual(name, "rsignrank"))
        return 287;
    else if (staticStringEqual(name, "rbeta"))
        return 288;
    else if (staticStringEqual(name, "rbinom"))
        return 289;
    else if (staticStringEqual(name, "rcauchy"))
        return 290;
    else if (staticStringEqual(name, "rf"))
        return 291;
    else if (staticStringEqual(name, "rgamma"))
        return 292;
    else if (staticStringEqual(name, "rlnorm"))
        return 293;
    else if (staticStringEqual(name, "rlogis"))
        return 294;
    else if (staticStringEqual(name, "rnbinom"))
        return 295;
    else if (staticStringEqual(name, "rnbinom_mu"))
        return 296;
    else if (staticStringEqual(name, "rnchisq"))
        return 297;
    else if (staticStringEqual(name, "rnorm"))
        return 298;
    else if (staticStringEqual(name, "runif"))
        return 299;
    else if (staticStringEqual(name, "rweibull"))
        return 300;
    else if (staticStringEqual(name, "rwilcox"))
        return 301;
    else if (staticStringEqual(name, "rhyper"))
        return 302;
    else if (staticStringEqual(name, "sample"))
        return 303;
    else if (staticStringEqual(name, "sample2"))
        return 304;
    else if (staticStringEqual(name, "RNGkind"))
        return 305;
    else if (staticStringEqual(name, "set.seed"))
        return 306;
    else if (staticStringEqual(name, "sum"))
        return 307;
    else if (staticStringEqual(name, "min"))
        return 308;
    else if (staticStringEqual(name, "max"))
        return 309;
    else if (staticStringEqual(name, "prod"))
        return 310;
    else if (staticStringEqual(name, "mean"))
        return 311;
    else if (staticStringEqual(name, "range"))
        return 312;
    else if (staticStringEqual(name, "cumsum"))
        return 313;
    else if (staticStringEqual(name, "cumprod"))
        return 314;
    else if (staticStringEqual(name, "cummax"))
        return 315;
    else if (staticStringEqual(name, "cummin"))
        return 316;
    else if (staticStringEqual(name, "as.character"))
        return 317;
    else if (staticStringEqual(name, "as.integer"))
        return 318;
    else if (staticStringEqual(name, "as.double"))
        return 319;
    else if (staticStringEqual(name, "as.numeric"))
        return 320;
    else if (staticStringEqual(name, "as.complex"))
        return 321;
    else if (staticStringEqual(name, "as.logical"))
        return 322;
    else if (staticStringEqual(name, "as.raw"))
        return 323;
    else if (staticStringEqual(name, "as.call"))
        return 324;
    else if (staticStringEqual(name, "as.environment"))
        return 325;
    else if (staticStringEqual(name, "storage.mode<-"))
        return 326;
    else if (staticStringEqual(name, "asCharacterFactor"))
        return 327;
    else if (staticStringEqual(name, "as.vector"))
        return 328;
    else if (staticStringEqual(name, "paste"))
        return 329;
    else if (staticStringEqual(name, "paste0"))
        return 330;
    else if (staticStringEqual(name, "file.path"))
        return 331;
    else if (staticStringEqual(name, "format"))
        return 332;
    else if (staticStringEqual(name, "format.info"))
        return 333;
    else if (staticStringEqual(name, "cat"))
        return 334;
    else if (staticStringEqual(name, "do.call"))
        return 335;
    else if (staticStringEqual(name, "str2lang"))
        return 336;
    else if (staticStringEqual(name, "str2expression"))
        return 337;
    else if (staticStringEqual(name, "nchar"))
        return 338;
    else if (staticStringEqual(name, "nzchar"))
        return 339;
    else if (staticStringEqual(name, "substr"))
        return 340;
    else if (staticStringEqual(name, "startsWith"))
        return 341;
    else if (staticStringEqual(name, "endsWith"))
        return 342;
    else if (staticStringEqual(name, "substr<-"))
        return 343;
    else if (staticStringEqual(name, "strsplit"))
        return 344;
    else if (staticStringEqual(name, "abbreviate"))
        return 345;
    else if (staticStringEqual(name, "make.names"))
        return 346;
    else if (staticStringEqual(name, "pcre_config"))
        return 347;
    else if (staticStringEqual(name, "grep"))
        return 348;
    else if (staticStringEqual(name, "grepl"))
        return 349;
    else if (staticStringEqual(name, "grepRaw"))
        return 350;
    else if (staticStringEqual(name, "sub"))
        return 351;
    else if (staticStringEqual(name, "gsub"))
        return 352;
    else if (staticStringEqual(name, "regexpr"))
        return 353;
    else if (staticStringEqual(name, "gregexpr"))
        return 354;
    else if (staticStringEqual(name, "regexec"))
        return 355;
    else if (staticStringEqual(name, "agrep"))
        return 356;
    else if (staticStringEqual(name, "agrepl"))
        return 357;
    else if (staticStringEqual(name, "adist"))
        return 358;
    else if (staticStringEqual(name, "aregexec"))
        return 359;
    else if (staticStringEqual(name, "tolower"))
        return 360;
    else if (staticStringEqual(name, "toupper"))
        return 361;
    else if (staticStringEqual(name, "chartr"))
        return 362;
    else if (staticStringEqual(name, "sprintf"))
        return 363;
    else if (staticStringEqual(name, "make.unique"))
        return 364;
    else if (staticStringEqual(name, "charToRaw"))
        return 365;
    else if (staticStringEqual(name, "rawToChar"))
        return 366;
    else if (staticStringEqual(name, "rawShift"))
        return 367;
    else if (staticStringEqual(name, "intToBits"))
        return 368;
    else if (staticStringEqual(name, "numToBits"))
        return 369;
    else if (staticStringEqual(name, "numToInts"))
        return 370;
    else if (staticStringEqual(name, "rawToBits"))
        return 371;
    else if (staticStringEqual(name, "packBits"))
        return 372;
    else if (staticStringEqual(name, "utf8ToInt"))
        return 373;
    else if (staticStringEqual(name, "intToUtf8"))
        return 374;
    else if (staticStringEqual(name, "validUTF8"))
        return 375;
    else if (staticStringEqual(name, "validEnc"))
        return 376;
    else if (staticStringEqual(name, "encodeString"))
        return 377;
    else if (staticStringEqual(name, "iconv"))
        return 378;
    else if (staticStringEqual(name, "strtrim"))
        return 379;
    else if (staticStringEqual(name, "strtoi"))
        return 380;
    else if (staticStringEqual(name, "strrep"))
        return 381;
    else if (staticStringEqual(name, "is.null"))
        return 382;
    else if (staticStringEqual(name, "is.logical"))
        return 383;
    else if (staticStringEqual(name, "is.integer"))
        return 384;
    else if (staticStringEqual(name, "is.double"))
        return 385;
    else if (staticStringEqual(name, "is.complex"))
        return 386;
    else if (staticStringEqual(name, "is.character"))
        return 387;
    else if (staticStringEqual(name, "is.symbol"))
        return 388;
    else if (staticStringEqual(name, "is.name"))
        return 389;
    else if (staticStringEqual(name, "is.environment"))
        return 390;
    else if (staticStringEqual(name, "is.list"))
        return 391;
    else if (staticStringEqual(name, "is.pairlist"))
        return 392;
    else if (staticStringEqual(name, "is.expression"))
        return 393;
    else if (staticStringEqual(name, "is.raw"))
        return 394;
    else if (staticStringEqual(name, "is.object"))
        return 395;
    else if (staticStringEqual(name, "isS4"))
        return 396;
    else if (staticStringEqual(name, "is.numeric"))
        return 397;
    else if (staticStringEqual(name, "is.matrix"))
        return 398;
    else if (staticStringEqual(name, "is.array"))
        return 399;
    else if (staticStringEqual(name, "is.atomic"))
        return 400;
    else if (staticStringEqual(name, "is.recursive"))
        return 401;
    else if (staticStringEqual(name, "is.call"))
        return 402;
    else if (staticStringEqual(name, "is.language"))
        return 403;
    else if (staticStringEqual(name, "is.function"))
        return 404;
    else if (staticStringEqual(name, "is.single"))
        return 405;
    else if (staticStringEqual(name, "is.na"))
        return 406;
    else if (staticStringEqual(name, "is.nan"))
        return 407;
    else if (staticStringEqual(name, "is.finite"))
        return 408;
    else if (staticStringEqual(name, "is.infinite"))
        return 409;
    else if (staticStringEqual(name, "is.vector"))
        return 410;
    else if (staticStringEqual(name, "proc.time"))
        return 411;
    else if (staticStringEqual(name, "gc.time"))
        return 412;
    else if (staticStringEqual(name, "withVisible"))
        return 413;
    else if (staticStringEqual(name, "expression"))
        return 414;
    else if (staticStringEqual(name, "interactive"))
        return 415;
    else if (staticStringEqual(name, "invisible"))
        return 416;
    else if (staticStringEqual(name, "rep"))
        return 417;
    else if (staticStringEqual(name, "rep.int"))
        return 418;
    else if (staticStringEqual(name, "rep_len"))
        return 419;
    else if (staticStringEqual(name, "seq.int"))
        return 420;
    else if (staticStringEqual(name, "seq_len"))
        return 421;
    else if (staticStringEqual(name, "seq_along"))
        return 422;
    else if (staticStringEqual(name, "list"))
        return 423;
    else if (staticStringEqual(name, "xtfrm"))
        return 424;
    else if (staticStringEqual(name, "enc2native"))
        return 425;
    else if (staticStringEqual(name, "enc2utf8"))
        return 426;
    else if (staticStringEqual(name, "emptyenv"))
        return 427;
    else if (staticStringEqual(name, "baseenv"))
        return 428;
    else if (staticStringEqual(name, "globalenv"))
        return 429;
    else if (staticStringEqual(name, "environment<-"))
        return 430;
    else if (staticStringEqual(name, "pos.to.env"))
        return 431;
    else if (staticStringEqual(name, ".C"))
        return 432;
    else if (staticStringEqual(name, ".Fortran"))
        return 433;
    else if (staticStringEqual(name, ".External"))
        return 434;
    else if (staticStringEqual(name, ".External2"))
        return 435;
    else if (staticStringEqual(name, ".Call"))
        return 436;
    else if (staticStringEqual(name, ".External.graphics"))
        return 437;
    else if (staticStringEqual(name, ".Call.graphics"))
        return 438;
    else if (staticStringEqual(name, "eapply"))
        return 439;
    else if (staticStringEqual(name, "lapply"))
        return 440;
    else if (staticStringEqual(name, "vapply"))
        return 441;
    else if (staticStringEqual(name, "mapply"))
        return 442;
    else if (staticStringEqual(name, "Version"))
        return 443;
    else if (staticStringEqual(name, "machine"))
        return 444;
    else if (staticStringEqual(name, "commandArgs"))
        return 445;
    else if (staticStringEqual(name, "internalsID"))
        return 446;
    else if (staticStringEqual(name, "system"))
        return 447;
    else if (staticStringEqual(name, "parse"))
        return 448;
    else if (staticStringEqual(name, "save"))
        return 449;
    else if (staticStringEqual(name, "saveToConn"))
        return 450;
    else if (staticStringEqual(name, "load"))
        return 451;
    else if (staticStringEqual(name, "loadFromConn2"))
        return 452;
    else if (staticStringEqual(name, "loadInfoFromConn2"))
        return 453;
    else if (staticStringEqual(name, "serializeToConn"))
        return 454;
    else if (staticStringEqual(name, "unserializeFromConn"))
        return 455;
    else if (staticStringEqual(name, "serializeInfoFromConn"))
        return 456;
    else if (staticStringEqual(name, "deparse"))
        return 457;
    else if (staticStringEqual(name, "dput"))
        return 458;
    else if (staticStringEqual(name, "dump"))
        return 459;
    else if (staticStringEqual(name, "quit"))
        return 460;
    else if (staticStringEqual(name, "readline"))
        return 461;
    else if (staticStringEqual(name, "print.default"))
        return 462;
    else if (staticStringEqual(name, "prmatrix"))
        return 463;
    else if (staticStringEqual(name, "gc"))
        return 464;
    else if (staticStringEqual(name, "gcinfo"))
        return 465;
    else if (staticStringEqual(name, "gctorture"))
        return 466;
    else if (staticStringEqual(name, "gctorture2"))
        return 467;
    else if (staticStringEqual(name, "memory.profile"))
        return 468;
    else if (staticStringEqual(name, "mem.maxVSize"))
        return 469;
    else if (staticStringEqual(name, "mem.maxNSize"))
        return 470;
    else if (staticStringEqual(name, "split"))
        return 471;
    else if (staticStringEqual(name, "is.loaded"))
        return 472;
    else if (staticStringEqual(name, "recordGraphics"))
        return 473;
    else if (staticStringEqual(name, "dyn.load"))
        return 474;
    else if (staticStringEqual(name, "dyn.unload"))
        return 475;
    else if (staticStringEqual(name, "ls"))
        return 476;
    else if (staticStringEqual(name, "typeof"))
        return 477;
    else if (staticStringEqual(name, "eval"))
        return 478;
    else if (staticStringEqual(name, "returnValue"))
        return 479;
    else if (staticStringEqual(name, "sys.parent"))
        return 480;
    else if (staticStringEqual(name, "sys.call"))
        return 481;
    else if (staticStringEqual(name, "sys.frame"))
        return 482;
    else if (staticStringEqual(name, "sys.nframe"))
        return 483;
    else if (staticStringEqual(name, "sys.calls"))
        return 484;
    else if (staticStringEqual(name, "sys.frames"))
        return 485;
    else if (staticStringEqual(name, "sys.on.exit"))
        return 486;
    else if (staticStringEqual(name, "sys.parents"))
        return 487;
    else if (staticStringEqual(name, "sys.function"))
        return 488;
    else if (staticStringEqual(name, "traceback"))
        return 489;
    else if (staticStringEqual(name, "browserText"))
        return 490;
    else if (staticStringEqual(name, "browserCondition"))
        return 491;
    else if (staticStringEqual(name, "browserSetDebug"))
        return 492;
    else if (staticStringEqual(name, "parent.frame"))
        return 493;
    else if (staticStringEqual(name, "sort"))
        return 494;
    else if (staticStringEqual(name, "is.unsorted"))
        return 495;
    else if (staticStringEqual(name, "sorted_fpass"))
        return 496;
    else if (staticStringEqual(name, "psort"))
        return 497;
    else if (staticStringEqual(name, "qsort"))
        return 498;
    else if (staticStringEqual(name, "radixsort"))
        return 499;
    else if (staticStringEqual(name, "order"))
        return 500;
    else if (staticStringEqual(name, "rank"))
        return 501;
    else if (staticStringEqual(name, "scan"))
        return 502;
    else if (staticStringEqual(name, "t.default"))
        return 503;
    else if (staticStringEqual(name, "aperm"))
        return 504;
    else if (staticStringEqual(name, "builtins"))
        return 505;
    else if (staticStringEqual(name, "args"))
        return 506;
    else if (staticStringEqual(name, "formals"))
        return 507;
    else if (staticStringEqual(name, "body"))
        return 508;
    else if (staticStringEqual(name, "bodyCode"))
        return 509;
    else if (staticStringEqual(name, "environment"))
        return 510;
    else if (staticStringEqual(name, "environmentName"))
        return 511;
    else if (staticStringEqual(name, "env2list"))
        return 512;
    else if (staticStringEqual(name, "reg.finalizer"))
        return 513;
    else if (staticStringEqual(name, "options"))
        return 514;
    else if (staticStringEqual(name, "getOption"))
        return 515;
    else if (staticStringEqual(name, "sink"))
        return 516;
    else if (staticStringEqual(name, "sink.number"))
        return 517;
    else if (staticStringEqual(name, "rapply"))
        return 518;
    else if (staticStringEqual(name, "islistfactor"))
        return 519;
    else if (staticStringEqual(name, "colSums"))
        return 520;
    else if (staticStringEqual(name, "colMeans"))
        return 521;
    else if (staticStringEqual(name, "rowSums"))
        return 522;
    else if (staticStringEqual(name, "rowMeans"))
        return 523;
    else if (staticStringEqual(name, "tracemem"))
        return 524;
    else if (staticStringEqual(name, "retracemem"))
        return 525;
    else if (staticStringEqual(name, "untracemem"))
        return 526;
    else if (staticStringEqual(name, "inspect"))
        return 527;
    else if (staticStringEqual(name, "address"))
        return 528;
    else if (staticStringEqual(name, "named"))
        return 529;
    else if (staticStringEqual(name, "refcnt"))
        return 530;
    else if (staticStringEqual(name, "merge"))
        return 531;
    else if (staticStringEqual(name, "capabilities"))
        return 532;
    else if (staticStringEqual(name, "capabilitiesX11"))
        return 533;
    else if (staticStringEqual(name, "new.env"))
        return 534;
    else if (staticStringEqual(name, "parent.env"))
        return 535;
    else if (staticStringEqual(name, "parent.env<-"))
        return 536;
    else if (staticStringEqual(name, "topenv"))
        return 537;
    else if (staticStringEqual(name, "l10n_info"))
        return 538;
    else if (staticStringEqual(name, "Cstack_info"))
        return 539;
    else if (staticStringEqual(name, "mmap_file"))
        return 540;
    else if (staticStringEqual(name, "munmap_file"))
        return 541;
    else if (staticStringEqual(name, "wrap_meta"))
        return 542;
    else if (staticStringEqual(name, "tryWrap"))
        return 543;
    else if (staticStringEqual(name, "altrep_class"))
        return 544;
    else if (staticStringEqual(name, "file.show"))
        return 545;
    else if (staticStringEqual(name, "file.create"))
        return 546;
    else if (staticStringEqual(name, "file.remove"))
        return 547;
    else if (staticStringEqual(name, "file.rename"))
        return 548;
    else if (staticStringEqual(name, "file.append"))
        return 549;
    else if (staticStringEqual(name, "file.symlink"))
        return 550;
    else if (staticStringEqual(name, "file.link"))
        return 551;
    else if (staticStringEqual(name, "file.copy"))
        return 552;
    else if (staticStringEqual(name, "list.files"))
        return 553;
    else if (staticStringEqual(name, "list.dirs"))
        return 554;
    else if (staticStringEqual(name, "file.exists"))
        return 555;
    else if (staticStringEqual(name, "file.choose"))
        return 556;
    else if (staticStringEqual(name, "file.info"))
        return 557;
    else if (staticStringEqual(name, "file.access"))
        return 558;
    else if (staticStringEqual(name, "dir.exists"))
        return 559;
    else if (staticStringEqual(name, "dir.create"))
        return 560;
    else if (staticStringEqual(name, "tempfile"))
        return 561;
    else if (staticStringEqual(name, "tempdir"))
        return 562;
    else if (staticStringEqual(name, "R.home"))
        return 563;
    else if (staticStringEqual(name, "date"))
        return 564;
    else if (staticStringEqual(name, "Sys.getenv"))
        return 565;
    else if (staticStringEqual(name, "Sys.setenv"))
        return 566;
    else if (staticStringEqual(name, "Sys.unsetenv"))
        return 567;
    else if (staticStringEqual(name, "getwd"))
        return 568;
    else if (staticStringEqual(name, "setwd"))
        return 569;
    else if (staticStringEqual(name, "basename"))
        return 570;
    else if (staticStringEqual(name, "dirname"))
        return 571;
    else if (staticStringEqual(name, "Sys.chmod"))
        return 572;
    else if (staticStringEqual(name, "Sys.umask"))
        return 573;
    else if (staticStringEqual(name, "Sys.readlink"))
        return 574;
    else if (staticStringEqual(name, "Sys.info"))
        return 575;
    else if (staticStringEqual(name, "Sys.sleep"))
        return 576;
    else if (staticStringEqual(name, "Sys.getlocale"))
        return 577;
    else if (staticStringEqual(name, "Sys.setlocale"))
        return 578;
    else if (staticStringEqual(name, "Sys.localeconv"))
        return 579;
    else if (staticStringEqual(name, "path.expand"))
        return 580;
    else if (staticStringEqual(name, "Sys.getpid"))
        return 581;
    else if (staticStringEqual(name, "normalizePath"))
        return 582;
    else if (staticStringEqual(name, "Sys.glob"))
        return 583;
    else if (staticStringEqual(name, "unlink"))
        return 584;
    else if (staticStringEqual(name, "polyroot"))
        return 585;
    else if (staticStringEqual(name, "inherits"))
        return 586;
    else if (staticStringEqual(name, "UseMethod"))
        return 587;
    else if (staticStringEqual(name, "NextMethod"))
        return 588;
    else if (staticStringEqual(name, "standardGeneric"))
        return 589;
    else if (staticStringEqual(name, "Sys.time"))
        return 590;
    else if (staticStringEqual(name, "as.POSIXct"))
        return 591;
    else if (staticStringEqual(name, "as.POSIXlt"))
        return 592;
    else if (staticStringEqual(name, "format.POSIXlt"))
        return 593;
    else if (staticStringEqual(name, "strptime"))
        return 594;
    else if (staticStringEqual(name, "Date2POSIXlt"))
        return 595;
    else if (staticStringEqual(name, "POSIXlt2Date"))
        return 596;
    else if (staticStringEqual(name, "mkCode"))
        return 597;
    else if (staticStringEqual(name, "bcClose"))
        return 598;
    else if (staticStringEqual(name, "is.builtin.internal"))
        return 599;
    else if (staticStringEqual(name, "disassemble"))
        return 600;
    else if (staticStringEqual(name, "bcVersion"))
        return 601;
    else if (staticStringEqual(name, "load.from.file"))
        return 602;
    else if (staticStringEqual(name, "save.to.file"))
        return 603;
    else if (staticStringEqual(name, "growconst"))
        return 604;
    else if (staticStringEqual(name, "putconst"))
        return 605;
    else if (staticStringEqual(name, "getconst"))
        return 606;
    else if (staticStringEqual(name, "enableJIT"))
        return 607;
    else if (staticStringEqual(name, "compilePKGS"))
        return 608;
    else if (staticStringEqual(name, "setNumMathThreads"))
        return 609;
    else if (staticStringEqual(name, "setMaxNumMathThreads"))
        return 610;
    else if (staticStringEqual(name, "stdin"))
        return 611;
    else if (staticStringEqual(name, "stdout"))
        return 612;
    else if (staticStringEqual(name, "stderr"))
        return 613;
    else if (staticStringEqual(name, "isatty"))
        return 614;
    else if (staticStringEqual(name, "readLines"))
        return 615;
    else if (staticStringEqual(name, "writeLines"))
        return 616;
    else if (staticStringEqual(name, "readBin"))
        return 617;
    else if (staticStringEqual(name, "writeBin"))
        return 618;
    else if (staticStringEqual(name, "readChar"))
        return 619;
    else if (staticStringEqual(name, "writeChar"))
        return 620;
    else if (staticStringEqual(name, "open"))
        return 621;
    else if (staticStringEqual(name, "isOpen"))
        return 622;
    else if (staticStringEqual(name, "isIncomplete"))
        return 623;
    else if (staticStringEqual(name, "isSeekable"))
        return 624;
    else if (staticStringEqual(name, "close"))
        return 625;
    else if (staticStringEqual(name, "flush"))
        return 626;
    else if (staticStringEqual(name, "file"))
        return 627;
    else if (staticStringEqual(name, "url"))
        return 628;
    else if (staticStringEqual(name, "pipe"))
        return 629;
    else if (staticStringEqual(name, "fifo"))
        return 630;
    else if (staticStringEqual(name, "gzfile"))
        return 631;
    else if (staticStringEqual(name, "bzfile"))
        return 632;
    else if (staticStringEqual(name, "xzfile"))
        return 633;
    else if (staticStringEqual(name, "unz"))
        return 634;
    else if (staticStringEqual(name, "seek"))
        return 635;
    else if (staticStringEqual(name, "truncate"))
        return 636;
    else if (staticStringEqual(name, "pushBack"))
        return 637;
    else if (staticStringEqual(name, "clearPushBack"))
        return 638;
    else if (staticStringEqual(name, "pushBackLength"))
        return 639;
    else if (staticStringEqual(name, "rawConnection"))
        return 640;
    else if (staticStringEqual(name, "rawConnectionValue"))
        return 641;
    else if (staticStringEqual(name, "textConnection"))
        return 642;
    else if (staticStringEqual(name, "textConnectionValue"))
        return 643;
    else if (staticStringEqual(name, "socketConnection"))
        return 644;
    else if (staticStringEqual(name, "socketAccept"))
        return 645;
    else if (staticStringEqual(name, "sockSelect"))
        return 646;
    else if (staticStringEqual(name, "serverSocket"))
        return 647;
    else if (staticStringEqual(name, "socketTimeout"))
        return 648;
    else if (staticStringEqual(name, "getConnection"))
        return 649;
    else if (staticStringEqual(name, "getAllConnections"))
        return 650;
    else if (staticStringEqual(name, "summary.connection"))
        return 651;
    else if (staticStringEqual(name, "gzcon"))
        return 652;
    else if (staticStringEqual(name, "memCompress"))
        return 653;
    else if (staticStringEqual(name, "memDecompress"))
        return 654;
    else if (staticStringEqual(name, "readDCF"))
        return 655;
    else if (staticStringEqual(name, "lockEnvironment"))
        return 656;
    else if (staticStringEqual(name, "environmentIsLocked"))
        return 657;
    else if (staticStringEqual(name, "lockBinding"))
        return 658;
    else if (staticStringEqual(name, "unlockBinding"))
        return 659;
    else if (staticStringEqual(name, "bindingIsLocked"))
        return 660;
    else if (staticStringEqual(name, "makeActiveBinding"))
        return 661;
    else if (staticStringEqual(name, "bindingIsActive"))
        return 662;
    else if (staticStringEqual(name, "activeBindingFunction"))
        return 663;
    else if (staticStringEqual(name, "mkUnbound"))
        return 664;
    else if (staticStringEqual(name, "isNamespaceEnv"))
        return 665;
    else if (staticStringEqual(name, "registerNamespace"))
        return 666;
    else if (staticStringEqual(name, "unregisterNamespace"))
        return 667;
    else if (staticStringEqual(name, "getRegisteredNamespace"))
        return 668;
    else if (staticStringEqual(name, "isRegisteredNamespace"))
        return 669;
    else if (staticStringEqual(name, "getNamespaceRegistry"))
        return 670;
    else if (staticStringEqual(name, "importIntoEnv"))
        return 671;
    else if (staticStringEqual(name, "env.profile"))
        return 672;
    else if (staticStringEqual(name, "Encoding"))
        return 673;
    else if (staticStringEqual(name, "setEncoding"))
        return 674;
    else if (staticStringEqual(name, "setTimeLimit"))
        return 675;
    else if (staticStringEqual(name, "setSessionTimeLimit"))
        return 676;
    else if (staticStringEqual(name, "icuSetCollate"))
        return 677;
    else if (staticStringEqual(name, "icuGetCollate"))
        return 678;
    else if (staticStringEqual(name, "readRenviron"))
        return 679;
    else if (staticStringEqual(name, "shortRowNames"))
        return 680;
    else if (staticStringEqual(name, "copyDFattr"))
        return 681;
    else if (staticStringEqual(name, "getRegisteredRoutines"))
        return 682;
    else if (staticStringEqual(name, "getLoadedDLLs"))
        return 683;
    else if (staticStringEqual(name, "getSymbolInfo"))
        return 684;
    else if (staticStringEqual(name, ".isMethodsDispatchOn"))
        return 685;
    else if (staticStringEqual(name, "lazyLoadDBfetch"))
        return 686;
    else if (staticStringEqual(name, "lazyLoadDBflush"))
        return 687;
    else if (staticStringEqual(name, "getVarsFromFrame"))
        return 688;
    else if (staticStringEqual(name, "lazyLoadDBinsertValue"))
        return 689;
    else if (staticStringEqual(name, "bincode"))
        return 690;
    else if (staticStringEqual(name, "tabulate"))
        return 691;
    else if (staticStringEqual(name, "findInterval"))
        return 692;
    else if (staticStringEqual(name, "pretty"))
        return 693;
    else if (staticStringEqual(name, "formatC"))
        return 694;
    else if (staticStringEqual(name, "crc64"))
        return 695;
    else if (staticStringEqual(name, "bitwiseAnd"))
        return 696;
    else if (staticStringEqual(name, "bitwiseNot"))
        return 697;
    else if (staticStringEqual(name, "bitwiseOr"))
        return 698;
    else if (staticStringEqual(name, "bitwiseXor"))
        return 699;
    else if (staticStringEqual(name, "bitwiseShiftL"))
        return 700;
    else if (staticStringEqual(name, "bitwiseShiftR"))
        return 701;
    else if (staticStringEqual(name, "serialize"))
        return 702;
    else if (staticStringEqual(name, "serializeb"))
        return 703;
    else if (staticStringEqual(name, "unserialize"))
        return 704;
    else if (staticStringEqual(name, "rowsum_matrix"))
        return 705;
    else if (staticStringEqual(name, "rowsum_df"))
        return 706;
    else if (staticStringEqual(name, "setS4Object"))
        return 707;
    else if (staticStringEqual(name, "traceOnOff"))
        return 708;
    else if (staticStringEqual(name, "debugOnOff"))
        return 709;
    else if (staticStringEqual(name, "La_qr_cmplx"))
        return 710;
    else if (staticStringEqual(name, "La_rs"))
        return 711;
    else if (staticStringEqual(name, "La_rs_cmplx"))
        return 712;
    else if (staticStringEqual(name, "La_rg"))
        return 713;
    else if (staticStringEqual(name, "La_rg_cmplx"))
        return 714;
    else if (staticStringEqual(name, "La_rs"))
        return 715;
    else if (staticStringEqual(name, "La_rs_cmplx"))
        return 716;
    else if (staticStringEqual(name, "La_dlange"))
        return 717;
    else if (staticStringEqual(name, "La_dgecon"))
        return 718;
    else if (staticStringEqual(name, "La_dtrcon"))
        return 719;
    else if (staticStringEqual(name, "La_zgecon"))
        return 720;
    else if (staticStringEqual(name, "La_ztrcon"))
        return 721;
    else if (staticStringEqual(name, "La_solve_cmplx"))
        return 722;
    else if (staticStringEqual(name, "La_solve"))
        return 723;
    else if (staticStringEqual(name, "La_qr"))
        return 724;
    else if (staticStringEqual(name, "La_chol"))
        return 725;
    else if (staticStringEqual(name, "La_chol2inv"))
        return 726;
    else if (staticStringEqual(name, "qr_coef_real"))
        return 727;
    else if (staticStringEqual(name, "qr_qy_real"))
        return 728;
    else if (staticStringEqual(name, "det_ge_real"))
        return 729;
    else if (staticStringEqual(name, "qr_coef_cmplx"))
        return 730;
    else if (staticStringEqual(name, "qr_qy_cmplx"))
        return 731;
    else if (staticStringEqual(name, "La_svd"))
        return 732;
    else if (staticStringEqual(name, "La_svd_cmplx"))
        return 733;
    else if (staticStringEqual(name, "La_version"))
        return 734;
    else if (staticStringEqual(name, "La_library"))
        return 735;
    else if (staticStringEqual(name, "bcprofcounts"))
        return 736;
    else if (staticStringEqual(name, "bcprofstart"))
        return 737;
    else if (staticStringEqual(name, "bcprofstop"))
        return 738;
    else if (staticStringEqual(name, "eSoftVersion"))
        return 739;
    else if (staticStringEqual(name, "curlVersion"))
        return 740;
    else if (staticStringEqual(name, "curlGetHeaders"))
        return 741;
    else if (staticStringEqual(name, "curlDownload"))
        return 742;
    else
        errorWrongBuiltin();
    return -1;
}
} // namespace rir
#endif
