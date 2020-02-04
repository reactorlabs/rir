#ifndef RIR_BUILTIN_IDS_H
#define RIR_BUILTIN_IDS_H
// This file is generated using rir.printBuiltinIds()
#include "utils/String.h"
namespace rir {
constexpr static inline int blt(const char* name) {
    if (staticStringEqual(name, "if"))
        return 0;
    if (staticStringEqual(name, "while"))
        return 1;
    if (staticStringEqual(name, "for"))
        return 2;
    if (staticStringEqual(name, "repeat"))
        return 3;
    if (staticStringEqual(name, "break"))
        return 4;
    if (staticStringEqual(name, "next"))
        return 5;
    if (staticStringEqual(name, "return"))
        return 6;
    if (staticStringEqual(name, "function"))
        return 7;
    if (staticStringEqual(name, "<-"))
        return 8;
    if (staticStringEqual(name, "="))
        return 9;
    if (staticStringEqual(name, "<<-"))
        return 10;
    if (staticStringEqual(name, "{"))
        return 11;
    if (staticStringEqual(name, "("))
        return 12;
    if (staticStringEqual(name, ".subset"))
        return 13;
    if (staticStringEqual(name, ".subset2"))
        return 14;
    if (staticStringEqual(name, "["))
        return 15;
    if (staticStringEqual(name, "[["))
        return 16;
    if (staticStringEqual(name, "$"))
        return 17;
    if (staticStringEqual(name, "@"))
        return 18;
    if (staticStringEqual(name, "[<-"))
        return 19;
    if (staticStringEqual(name, "[[<-"))
        return 20;
    if (staticStringEqual(name, "$<-"))
        return 21;
    if (staticStringEqual(name, "switch"))
        return 22;
    if (staticStringEqual(name, "browser"))
        return 23;
    if (staticStringEqual(name, ".primTrace"))
        return 24;
    if (staticStringEqual(name, ".primUntrace"))
        return 25;
    if (staticStringEqual(name, ".Internal"))
        return 26;
    if (staticStringEqual(name, ".Primitive"))
        return 27;
    if (staticStringEqual(name, "call"))
        return 28;
    if (staticStringEqual(name, "quote"))
        return 29;
    if (staticStringEqual(name, "substitute"))
        return 30;
    if (staticStringEqual(name, "missing"))
        return 31;
    if (staticStringEqual(name, "nargs"))
        return 32;
    if (staticStringEqual(name, "on.exit"))
        return 33;
    if (staticStringEqual(name, "forceAndCall"))
        return 34;
    if (staticStringEqual(name, "stop"))
        return 35;
    if (staticStringEqual(name, "warning"))
        return 36;
    if (staticStringEqual(name, "gettext"))
        return 37;
    if (staticStringEqual(name, "ngettext"))
        return 38;
    if (staticStringEqual(name, "bindtextdomain"))
        return 39;
    if (staticStringEqual(name, ".addCondHands"))
        return 40;
    if (staticStringEqual(name, ".resetCondHands"))
        return 41;
    if (staticStringEqual(name, ".signalCondition"))
        return 42;
    if (staticStringEqual(name, ".dfltStop"))
        return 43;
    if (staticStringEqual(name, ".dfltWarn"))
        return 44;
    if (staticStringEqual(name, ".addRestart"))
        return 45;
    if (staticStringEqual(name, ".getRestart"))
        return 46;
    if (staticStringEqual(name, ".invokeRestart"))
        return 47;
    if (staticStringEqual(name, ".addTryHandlers"))
        return 48;
    if (staticStringEqual(name, "geterrmessage"))
        return 49;
    if (staticStringEqual(name, "seterrmessage"))
        return 50;
    if (staticStringEqual(name, "printDeferredWarnings"))
        return 51;
    if (staticStringEqual(name, "interruptsSuspended"))
        return 52;
    if (staticStringEqual(name, "as.function.default"))
        return 53;
    if (staticStringEqual(name, "debug"))
        return 54;
    if (staticStringEqual(name, "undebug"))
        return 55;
    if (staticStringEqual(name, "isdebugged"))
        return 56;
    if (staticStringEqual(name, "debugonce"))
        return 57;
    if (staticStringEqual(name, "Recall"))
        return 58;
    if (staticStringEqual(name, "delayedAssign"))
        return 59;
    if (staticStringEqual(name, "makeLazy"))
        return 60;
    if (staticStringEqual(name, "identical"))
        return 61;
    if (staticStringEqual(name, "C_tryCatchHelper"))
        return 62;
    if (staticStringEqual(name, "+"))
        return 63;
    if (staticStringEqual(name, "-"))
        return 64;
    if (staticStringEqual(name, "*"))
        return 65;
    if (staticStringEqual(name, "/"))
        return 66;
    if (staticStringEqual(name, "^"))
        return 67;
    if (staticStringEqual(name, "%%"))
        return 68;
    if (staticStringEqual(name, "%/%"))
        return 69;
    if (staticStringEqual(name, "%*%"))
        return 70;
    if (staticStringEqual(name, "=="))
        return 71;
    if (staticStringEqual(name, "!="))
        return 72;
    if (staticStringEqual(name, "<"))
        return 73;
    if (staticStringEqual(name, "<="))
        return 74;
    if (staticStringEqual(name, ">="))
        return 75;
    if (staticStringEqual(name, ">"))
        return 76;
    if (staticStringEqual(name, "&"))
        return 77;
    if (staticStringEqual(name, "|"))
        return 78;
    if (staticStringEqual(name, "!"))
        return 79;
    if (staticStringEqual(name, "&&"))
        return 80;
    if (staticStringEqual(name, "||"))
        return 81;
    if (staticStringEqual(name, ":"))
        return 82;
    if (staticStringEqual(name, "~"))
        return 83;
    if (staticStringEqual(name, "all"))
        return 84;
    if (staticStringEqual(name, "any"))
        return 85;
    if (staticStringEqual(name, "...elt"))
        return 86;
    if (staticStringEqual(name, "...length"))
        return 87;
    if (staticStringEqual(name, "length"))
        return 88;
    if (staticStringEqual(name, "length<-"))
        return 89;
    if (staticStringEqual(name, "c"))
        return 90;
    if (staticStringEqual(name, "oldClass"))
        return 91;
    if (staticStringEqual(name, "oldClass<-"))
        return 92;
    if (staticStringEqual(name, "class"))
        return 93;
    if (staticStringEqual(name, ".cache_class"))
        return 94;
    if (staticStringEqual(name, "class<-"))
        return 95;
    if (staticStringEqual(name, "unclass"))
        return 96;
    if (staticStringEqual(name, "names"))
        return 97;
    if (staticStringEqual(name, "names<-"))
        return 98;
    if (staticStringEqual(name, "dimnames"))
        return 99;
    if (staticStringEqual(name, "dimnames<-"))
        return 100;
    if (staticStringEqual(name, "dim"))
        return 101;
    if (staticStringEqual(name, "dim<-"))
        return 102;
    if (staticStringEqual(name, "attributes"))
        return 103;
    if (staticStringEqual(name, "attributes<-"))
        return 104;
    if (staticStringEqual(name, "attr"))
        return 105;
    if (staticStringEqual(name, "attr<-"))
        return 106;
    if (staticStringEqual(name, "@<-"))
        return 107;
    if (staticStringEqual(name, "levels<-"))
        return 108;
    if (staticStringEqual(name, "vector"))
        return 109;
    if (staticStringEqual(name, "complex"))
        return 110;
    if (staticStringEqual(name, "matrix"))
        return 111;
    if (staticStringEqual(name, "array"))
        return 112;
    if (staticStringEqual(name, "diag"))
        return 113;
    if (staticStringEqual(name, "backsolve"))
        return 114;
    if (staticStringEqual(name, "max.col"))
        return 115;
    if (staticStringEqual(name, "row"))
        return 116;
    if (staticStringEqual(name, "col"))
        return 117;
    if (staticStringEqual(name, "unlist"))
        return 118;
    if (staticStringEqual(name, "cbind"))
        return 119;
    if (staticStringEqual(name, "rbind"))
        return 120;
    if (staticStringEqual(name, "drop"))
        return 121;
    if (staticStringEqual(name, "all.names"))
        return 122;
    if (staticStringEqual(name, "comment"))
        return 123;
    if (staticStringEqual(name, "comment<-"))
        return 124;
    if (staticStringEqual(name, "get"))
        return 125;
    if (staticStringEqual(name, "get0"))
        return 126;
    if (staticStringEqual(name, "mget"))
        return 127;
    if (staticStringEqual(name, "exists"))
        return 128;
    if (staticStringEqual(name, "assign"))
        return 129;
    if (staticStringEqual(name, "list2env"))
        return 130;
    if (staticStringEqual(name, "remove"))
        return 131;
    if (staticStringEqual(name, "duplicated"))
        return 132;
    if (staticStringEqual(name, "unique"))
        return 133;
    if (staticStringEqual(name, "anyDuplicated"))
        return 134;
    if (staticStringEqual(name, "anyNA"))
        return 135;
    if (staticStringEqual(name, "which"))
        return 136;
    if (staticStringEqual(name, "which.min"))
        return 137;
    if (staticStringEqual(name, "pmin"))
        return 138;
    if (staticStringEqual(name, "pmax"))
        return 139;
    if (staticStringEqual(name, "which.max"))
        return 140;
    if (staticStringEqual(name, "match"))
        return 141;
    if (staticStringEqual(name, "pmatch"))
        return 142;
    if (staticStringEqual(name, "charmatch"))
        return 143;
    if (staticStringEqual(name, "match.call"))
        return 144;
    if (staticStringEqual(name, "crossprod"))
        return 145;
    if (staticStringEqual(name, "tcrossprod"))
        return 146;
    if (staticStringEqual(name, "lengths"))
        return 147;
    if (staticStringEqual(name, "attach"))
        return 148;
    if (staticStringEqual(name, "detach"))
        return 149;
    if (staticStringEqual(name, "search"))
        return 150;
    if (staticStringEqual(name, "setFileTime"))
        return 151;
    if (staticStringEqual(name, "round"))
        return 152;
    if (staticStringEqual(name, "signif"))
        return 153;
    if (staticStringEqual(name, "log"))
        return 154;
    if (staticStringEqual(name, "log10"))
        return 155;
    if (staticStringEqual(name, "log2"))
        return 156;
    if (staticStringEqual(name, "abs"))
        return 157;
    if (staticStringEqual(name, "floor"))
        return 158;
    if (staticStringEqual(name, "ceiling"))
        return 159;
    if (staticStringEqual(name, "sqrt"))
        return 160;
    if (staticStringEqual(name, "sign"))
        return 161;
    if (staticStringEqual(name, "trunc"))
        return 162;
    if (staticStringEqual(name, "exp"))
        return 163;
    if (staticStringEqual(name, "expm1"))
        return 164;
    if (staticStringEqual(name, "log1p"))
        return 165;
    if (staticStringEqual(name, "cos"))
        return 166;
    if (staticStringEqual(name, "sin"))
        return 167;
    if (staticStringEqual(name, "tan"))
        return 168;
    if (staticStringEqual(name, "acos"))
        return 169;
    if (staticStringEqual(name, "asin"))
        return 170;
    if (staticStringEqual(name, "atan"))
        return 171;
    if (staticStringEqual(name, "cosh"))
        return 172;
    if (staticStringEqual(name, "sinh"))
        return 173;
    if (staticStringEqual(name, "tanh"))
        return 174;
    if (staticStringEqual(name, "acosh"))
        return 175;
    if (staticStringEqual(name, "asinh"))
        return 176;
    if (staticStringEqual(name, "atanh"))
        return 177;
    if (staticStringEqual(name, "lgamma"))
        return 178;
    if (staticStringEqual(name, "gamma"))
        return 179;
    if (staticStringEqual(name, "digamma"))
        return 180;
    if (staticStringEqual(name, "trigamma"))
        return 181;
    if (staticStringEqual(name, "cospi"))
        return 182;
    if (staticStringEqual(name, "sinpi"))
        return 183;
    if (staticStringEqual(name, "tanpi"))
        return 184;
    if (staticStringEqual(name, "atan2"))
        return 185;
    if (staticStringEqual(name, "lbeta"))
        return 186;
    if (staticStringEqual(name, "beta"))
        return 187;
    if (staticStringEqual(name, "lchoose"))
        return 188;
    if (staticStringEqual(name, "choose"))
        return 189;
    if (staticStringEqual(name, "dchisq"))
        return 190;
    if (staticStringEqual(name, "pchisq"))
        return 191;
    if (staticStringEqual(name, "qchisq"))
        return 192;
    if (staticStringEqual(name, "dexp"))
        return 193;
    if (staticStringEqual(name, "pexp"))
        return 194;
    if (staticStringEqual(name, "qexp"))
        return 195;
    if (staticStringEqual(name, "dgeom"))
        return 196;
    if (staticStringEqual(name, "pgeom"))
        return 197;
    if (staticStringEqual(name, "qgeom"))
        return 198;
    if (staticStringEqual(name, "dpois"))
        return 199;
    if (staticStringEqual(name, "ppois"))
        return 200;
    if (staticStringEqual(name, "qpois"))
        return 201;
    if (staticStringEqual(name, "dt"))
        return 202;
    if (staticStringEqual(name, "pt"))
        return 203;
    if (staticStringEqual(name, "qt"))
        return 204;
    if (staticStringEqual(name, "dsignrank"))
        return 205;
    if (staticStringEqual(name, "psignrank"))
        return 206;
    if (staticStringEqual(name, "qsignrank"))
        return 207;
    if (staticStringEqual(name, "besselJ"))
        return 208;
    if (staticStringEqual(name, "besselY"))
        return 209;
    if (staticStringEqual(name, "psigamma"))
        return 210;
    if (staticStringEqual(name, "Re"))
        return 211;
    if (staticStringEqual(name, "Im"))
        return 212;
    if (staticStringEqual(name, "Mod"))
        return 213;
    if (staticStringEqual(name, "Arg"))
        return 214;
    if (staticStringEqual(name, "Conj"))
        return 215;
    if (staticStringEqual(name, "dbeta"))
        return 216;
    if (staticStringEqual(name, "pbeta"))
        return 217;
    if (staticStringEqual(name, "qbeta"))
        return 218;
    if (staticStringEqual(name, "dbinom"))
        return 219;
    if (staticStringEqual(name, "pbinom"))
        return 220;
    if (staticStringEqual(name, "qbinom"))
        return 221;
    if (staticStringEqual(name, "dcauchy"))
        return 222;
    if (staticStringEqual(name, "pcauchy"))
        return 223;
    if (staticStringEqual(name, "qcauchy"))
        return 224;
    if (staticStringEqual(name, "df"))
        return 225;
    if (staticStringEqual(name, "pf"))
        return 226;
    if (staticStringEqual(name, "qf"))
        return 227;
    if (staticStringEqual(name, "dgamma"))
        return 228;
    if (staticStringEqual(name, "pgamma"))
        return 229;
    if (staticStringEqual(name, "qgamma"))
        return 230;
    if (staticStringEqual(name, "dlnorm"))
        return 231;
    if (staticStringEqual(name, "plnorm"))
        return 232;
    if (staticStringEqual(name, "qlnorm"))
        return 233;
    if (staticStringEqual(name, "dlogis"))
        return 234;
    if (staticStringEqual(name, "plogis"))
        return 235;
    if (staticStringEqual(name, "qlogis"))
        return 236;
    if (staticStringEqual(name, "dnbinom"))
        return 237;
    if (staticStringEqual(name, "pnbinom"))
        return 238;
    if (staticStringEqual(name, "qnbinom"))
        return 239;
    if (staticStringEqual(name, "dnorm"))
        return 240;
    if (staticStringEqual(name, "pnorm"))
        return 241;
    if (staticStringEqual(name, "qnorm"))
        return 242;
    if (staticStringEqual(name, "dunif"))
        return 243;
    if (staticStringEqual(name, "punif"))
        return 244;
    if (staticStringEqual(name, "qunif"))
        return 245;
    if (staticStringEqual(name, "dweibull"))
        return 246;
    if (staticStringEqual(name, "pweibull"))
        return 247;
    if (staticStringEqual(name, "qweibull"))
        return 248;
    if (staticStringEqual(name, "dnchisq"))
        return 249;
    if (staticStringEqual(name, "pnchisq"))
        return 250;
    if (staticStringEqual(name, "qnchisq"))
        return 251;
    if (staticStringEqual(name, "dnt"))
        return 252;
    if (staticStringEqual(name, "pnt"))
        return 253;
    if (staticStringEqual(name, "qnt"))
        return 254;
    if (staticStringEqual(name, "dwilcox"))
        return 255;
    if (staticStringEqual(name, "pwilcox"))
        return 256;
    if (staticStringEqual(name, "qwilcox"))
        return 257;
    if (staticStringEqual(name, "besselI"))
        return 258;
    if (staticStringEqual(name, "besselK"))
        return 259;
    if (staticStringEqual(name, "dnbinom_mu"))
        return 260;
    if (staticStringEqual(name, "pnbinom_mu"))
        return 261;
    if (staticStringEqual(name, "qnbinom_mu"))
        return 262;
    if (staticStringEqual(name, "dhyper"))
        return 263;
    if (staticStringEqual(name, "phyper"))
        return 264;
    if (staticStringEqual(name, "qhyper"))
        return 265;
    if (staticStringEqual(name, "dnbeta"))
        return 266;
    if (staticStringEqual(name, "pnbeta"))
        return 267;
    if (staticStringEqual(name, "qnbeta"))
        return 268;
    if (staticStringEqual(name, "dnf"))
        return 269;
    if (staticStringEqual(name, "pnf"))
        return 270;
    if (staticStringEqual(name, "qnf"))
        return 271;
    if (staticStringEqual(name, "dtukey"))
        return 272;
    if (staticStringEqual(name, "ptukey"))
        return 273;
    if (staticStringEqual(name, "qtukey"))
        return 274;
    if (staticStringEqual(name, "rchisq"))
        return 275;
    if (staticStringEqual(name, "rexp"))
        return 276;
    if (staticStringEqual(name, "rgeom"))
        return 277;
    if (staticStringEqual(name, "rpois"))
        return 278;
    if (staticStringEqual(name, "rt"))
        return 279;
    if (staticStringEqual(name, "rsignrank"))
        return 280;
    if (staticStringEqual(name, "rbeta"))
        return 281;
    if (staticStringEqual(name, "rbinom"))
        return 282;
    if (staticStringEqual(name, "rcauchy"))
        return 283;
    if (staticStringEqual(name, "rf"))
        return 284;
    if (staticStringEqual(name, "rgamma"))
        return 285;
    if (staticStringEqual(name, "rlnorm"))
        return 286;
    if (staticStringEqual(name, "rlogis"))
        return 287;
    if (staticStringEqual(name, "rnbinom"))
        return 288;
    if (staticStringEqual(name, "rnbinom_mu"))
        return 289;
    if (staticStringEqual(name, "rnchisq"))
        return 290;
    if (staticStringEqual(name, "rnorm"))
        return 291;
    if (staticStringEqual(name, "runif"))
        return 292;
    if (staticStringEqual(name, "rweibull"))
        return 293;
    if (staticStringEqual(name, "rwilcox"))
        return 294;
    if (staticStringEqual(name, "rhyper"))
        return 295;
    if (staticStringEqual(name, "sample"))
        return 296;
    if (staticStringEqual(name, "sample2"))
        return 297;
    if (staticStringEqual(name, "RNGkind"))
        return 298;
    if (staticStringEqual(name, "set.seed"))
        return 299;
    if (staticStringEqual(name, "sum"))
        return 300;
    if (staticStringEqual(name, "min"))
        return 301;
    if (staticStringEqual(name, "max"))
        return 302;
    if (staticStringEqual(name, "prod"))
        return 303;
    if (staticStringEqual(name, "mean"))
        return 304;
    if (staticStringEqual(name, "range"))
        return 305;
    if (staticStringEqual(name, "cumsum"))
        return 306;
    if (staticStringEqual(name, "cumprod"))
        return 307;
    if (staticStringEqual(name, "cummax"))
        return 308;
    if (staticStringEqual(name, "cummin"))
        return 309;
    if (staticStringEqual(name, "as.character"))
        return 310;
    if (staticStringEqual(name, "as.integer"))
        return 311;
    if (staticStringEqual(name, "as.double"))
        return 312;
    if (staticStringEqual(name, "as.numeric"))
        return 313;
    if (staticStringEqual(name, "as.complex"))
        return 314;
    if (staticStringEqual(name, "as.logical"))
        return 315;
    if (staticStringEqual(name, "as.raw"))
        return 316;
    if (staticStringEqual(name, "as.call"))
        return 317;
    if (staticStringEqual(name, "as.environment"))
        return 318;
    if (staticStringEqual(name, "storage.mode<-"))
        return 319;
    if (staticStringEqual(name, "asCharacterFactor"))
        return 320;
    if (staticStringEqual(name, "as.vector"))
        return 321;
    if (staticStringEqual(name, "paste"))
        return 322;
    if (staticStringEqual(name, "paste0"))
        return 323;
    if (staticStringEqual(name, "file.path"))
        return 324;
    if (staticStringEqual(name, "format"))
        return 325;
    if (staticStringEqual(name, "format.info"))
        return 326;
    if (staticStringEqual(name, "cat"))
        return 327;
    if (staticStringEqual(name, "do.call"))
        return 328;
    if (staticStringEqual(name, "str2lang"))
        return 329;
    if (staticStringEqual(name, "str2expression"))
        return 330;
    if (staticStringEqual(name, "nchar"))
        return 331;
    if (staticStringEqual(name, "nzchar"))
        return 332;
    if (staticStringEqual(name, "substr"))
        return 333;
    if (staticStringEqual(name, "startsWith"))
        return 334;
    if (staticStringEqual(name, "endsWith"))
        return 335;
    if (staticStringEqual(name, "substr<-"))
        return 336;
    if (staticStringEqual(name, "strsplit"))
        return 337;
    if (staticStringEqual(name, "abbreviate"))
        return 338;
    if (staticStringEqual(name, "make.names"))
        return 339;
    if (staticStringEqual(name, "pcre_config"))
        return 340;
    if (staticStringEqual(name, "grep"))
        return 341;
    if (staticStringEqual(name, "grepl"))
        return 342;
    if (staticStringEqual(name, "grepRaw"))
        return 343;
    if (staticStringEqual(name, "sub"))
        return 344;
    if (staticStringEqual(name, "gsub"))
        return 345;
    if (staticStringEqual(name, "regexpr"))
        return 346;
    if (staticStringEqual(name, "gregexpr"))
        return 347;
    if (staticStringEqual(name, "regexec"))
        return 348;
    if (staticStringEqual(name, "agrep"))
        return 349;
    if (staticStringEqual(name, "agrepl"))
        return 350;
    if (staticStringEqual(name, "adist"))
        return 351;
    if (staticStringEqual(name, "aregexec"))
        return 352;
    if (staticStringEqual(name, "tolower"))
        return 353;
    if (staticStringEqual(name, "toupper"))
        return 354;
    if (staticStringEqual(name, "chartr"))
        return 355;
    if (staticStringEqual(name, "sprintf"))
        return 356;
    if (staticStringEqual(name, "make.unique"))
        return 357;
    if (staticStringEqual(name, "charToRaw"))
        return 358;
    if (staticStringEqual(name, "rawToChar"))
        return 359;
    if (staticStringEqual(name, "rawShift"))
        return 360;
    if (staticStringEqual(name, "intToBits"))
        return 361;
    if (staticStringEqual(name, "rawToBits"))
        return 362;
    if (staticStringEqual(name, "packBits"))
        return 363;
    if (staticStringEqual(name, "utf8ToInt"))
        return 364;
    if (staticStringEqual(name, "intToUtf8"))
        return 365;
    if (staticStringEqual(name, "validUTF8"))
        return 366;
    if (staticStringEqual(name, "validEnc"))
        return 367;
    if (staticStringEqual(name, "encodeString"))
        return 368;
    if (staticStringEqual(name, "iconv"))
        return 369;
    if (staticStringEqual(name, "strtrim"))
        return 370;
    if (staticStringEqual(name, "strtoi"))
        return 371;
    if (staticStringEqual(name, "strrep"))
        return 372;
    if (staticStringEqual(name, "is.null"))
        return 373;
    if (staticStringEqual(name, "is.logical"))
        return 374;
    if (staticStringEqual(name, "is.integer"))
        return 375;
    if (staticStringEqual(name, "is.double"))
        return 376;
    if (staticStringEqual(name, "is.complex"))
        return 377;
    if (staticStringEqual(name, "is.character"))
        return 378;
    if (staticStringEqual(name, "is.symbol"))
        return 379;
    if (staticStringEqual(name, "is.name"))
        return 380;
    if (staticStringEqual(name, "is.environment"))
        return 381;
    if (staticStringEqual(name, "is.list"))
        return 382;
    if (staticStringEqual(name, "is.pairlist"))
        return 383;
    if (staticStringEqual(name, "is.expression"))
        return 384;
    if (staticStringEqual(name, "is.raw"))
        return 385;
    if (staticStringEqual(name, "is.object"))
        return 386;
    if (staticStringEqual(name, "isS4"))
        return 387;
    if (staticStringEqual(name, "is.numeric"))
        return 388;
    if (staticStringEqual(name, "is.matrix"))
        return 389;
    if (staticStringEqual(name, "is.array"))
        return 390;
    if (staticStringEqual(name, "is.atomic"))
        return 391;
    if (staticStringEqual(name, "is.recursive"))
        return 392;
    if (staticStringEqual(name, "is.call"))
        return 393;
    if (staticStringEqual(name, "is.language"))
        return 394;
    if (staticStringEqual(name, "is.function"))
        return 395;
    if (staticStringEqual(name, "is.single"))
        return 396;
    if (staticStringEqual(name, "is.na"))
        return 397;
    if (staticStringEqual(name, "is.nan"))
        return 398;
    if (staticStringEqual(name, "is.finite"))
        return 399;
    if (staticStringEqual(name, "is.infinite"))
        return 400;
    if (staticStringEqual(name, "is.vector"))
        return 401;
    if (staticStringEqual(name, "proc.time"))
        return 402;
    if (staticStringEqual(name, "gc.time"))
        return 403;
    if (staticStringEqual(name, "withVisible"))
        return 404;
    if (staticStringEqual(name, "expression"))
        return 405;
    if (staticStringEqual(name, "interactive"))
        return 406;
    if (staticStringEqual(name, "invisible"))
        return 407;
    if (staticStringEqual(name, "rep"))
        return 408;
    if (staticStringEqual(name, "rep.int"))
        return 409;
    if (staticStringEqual(name, "rep_len"))
        return 410;
    if (staticStringEqual(name, "seq.int"))
        return 411;
    if (staticStringEqual(name, "seq_len"))
        return 412;
    if (staticStringEqual(name, "seq_along"))
        return 413;
    if (staticStringEqual(name, "list"))
        return 414;
    if (staticStringEqual(name, "xtfrm"))
        return 415;
    if (staticStringEqual(name, "enc2native"))
        return 416;
    if (staticStringEqual(name, "enc2utf8"))
        return 417;
    if (staticStringEqual(name, "emptyenv"))
        return 418;
    if (staticStringEqual(name, "baseenv"))
        return 419;
    if (staticStringEqual(name, "globalenv"))
        return 420;
    if (staticStringEqual(name, "environment<-"))
        return 421;
    if (staticStringEqual(name, "pos.to.env"))
        return 422;
    if (staticStringEqual(name, "eapply"))
        return 423;
    if (staticStringEqual(name, "lapply"))
        return 424;
    if (staticStringEqual(name, "vapply"))
        return 425;
    if (staticStringEqual(name, "mapply"))
        return 426;
    if (staticStringEqual(name, ".C"))
        return 427;
    if (staticStringEqual(name, ".Fortran"))
        return 428;
    if (staticStringEqual(name, ".External"))
        return 429;
    if (staticStringEqual(name, ".External2"))
        return 430;
    if (staticStringEqual(name, ".Call"))
        return 431;
    if (staticStringEqual(name, ".External.graphics"))
        return 432;
    if (staticStringEqual(name, ".Call.graphics"))
        return 433;
    if (staticStringEqual(name, "Version"))
        return 434;
    if (staticStringEqual(name, "machine"))
        return 435;
    if (staticStringEqual(name, "commandArgs"))
        return 436;
    if (staticStringEqual(name, "internalsID"))
        return 437;
    if (staticStringEqual(name, "system"))
        return 438;
    if (staticStringEqual(name, "parse"))
        return 439;
    if (staticStringEqual(name, "save"))
        return 440;
    if (staticStringEqual(name, "saveToConn"))
        return 441;
    if (staticStringEqual(name, "load"))
        return 442;
    if (staticStringEqual(name, "loadFromConn2"))
        return 443;
    if (staticStringEqual(name, "loadInfoFromConn2"))
        return 444;
    if (staticStringEqual(name, "serializeToConn"))
        return 445;
    if (staticStringEqual(name, "unserializeFromConn"))
        return 446;
    if (staticStringEqual(name, "serializeInfoFromConn"))
        return 447;
    if (staticStringEqual(name, "deparse"))
        return 448;
    if (staticStringEqual(name, "dput"))
        return 449;
    if (staticStringEqual(name, "dump"))
        return 450;
    if (staticStringEqual(name, "quit"))
        return 451;
    if (staticStringEqual(name, "readline"))
        return 452;
    if (staticStringEqual(name, "print.default"))
        return 453;
    if (staticStringEqual(name, "prmatrix"))
        return 454;
    if (staticStringEqual(name, "gc"))
        return 455;
    if (staticStringEqual(name, "gcinfo"))
        return 456;
    if (staticStringEqual(name, "gctorture"))
        return 457;
    if (staticStringEqual(name, "gctorture2"))
        return 458;
    if (staticStringEqual(name, "memory.profile"))
        return 459;
    if (staticStringEqual(name, "mem.maxVSize"))
        return 460;
    if (staticStringEqual(name, "mem.maxNSize"))
        return 461;
    if (staticStringEqual(name, "split"))
        return 462;
    if (staticStringEqual(name, "is.loaded"))
        return 463;
    if (staticStringEqual(name, "recordGraphics"))
        return 464;
    if (staticStringEqual(name, "dyn.load"))
        return 465;
    if (staticStringEqual(name, "dyn.unload"))
        return 466;
    if (staticStringEqual(name, "ls"))
        return 467;
    if (staticStringEqual(name, "typeof"))
        return 468;
    if (staticStringEqual(name, "eval"))
        return 469;
    if (staticStringEqual(name, "returnValue"))
        return 470;
    if (staticStringEqual(name, "sys.parent"))
        return 471;
    if (staticStringEqual(name, "sys.call"))
        return 472;
    if (staticStringEqual(name, "sys.frame"))
        return 473;
    if (staticStringEqual(name, "sys.nframe"))
        return 474;
    if (staticStringEqual(name, "sys.calls"))
        return 475;
    if (staticStringEqual(name, "sys.frames"))
        return 476;
    if (staticStringEqual(name, "sys.on.exit"))
        return 477;
    if (staticStringEqual(name, "sys.parents"))
        return 478;
    if (staticStringEqual(name, "sys.function"))
        return 479;
    if (staticStringEqual(name, "traceback"))
        return 480;
    if (staticStringEqual(name, "browserText"))
        return 481;
    if (staticStringEqual(name, "browserCondition"))
        return 482;
    if (staticStringEqual(name, "browserSetDebug"))
        return 483;
    if (staticStringEqual(name, "parent.frame"))
        return 484;
    if (staticStringEqual(name, "sort"))
        return 485;
    if (staticStringEqual(name, "is.unsorted"))
        return 486;
    if (staticStringEqual(name, "sorted_fpass"))
        return 487;
    if (staticStringEqual(name, "psort"))
        return 488;
    if (staticStringEqual(name, "qsort"))
        return 489;
    if (staticStringEqual(name, "radixsort"))
        return 490;
    if (staticStringEqual(name, "order"))
        return 491;
    if (staticStringEqual(name, "rank"))
        return 492;
    if (staticStringEqual(name, "scan"))
        return 493;
    if (staticStringEqual(name, "t.default"))
        return 494;
    if (staticStringEqual(name, "aperm"))
        return 495;
    if (staticStringEqual(name, "builtins"))
        return 496;
    if (staticStringEqual(name, "args"))
        return 497;
    if (staticStringEqual(name, "formals"))
        return 498;
    if (staticStringEqual(name, "body"))
        return 499;
    if (staticStringEqual(name, "bodyCode"))
        return 500;
    if (staticStringEqual(name, "environment"))
        return 501;
    if (staticStringEqual(name, "environmentName"))
        return 502;
    if (staticStringEqual(name, "env2list"))
        return 503;
    if (staticStringEqual(name, "reg.finalizer"))
        return 504;
    if (staticStringEqual(name, "options"))
        return 505;
    if (staticStringEqual(name, "getOption"))
        return 506;
    if (staticStringEqual(name, "sink"))
        return 507;
    if (staticStringEqual(name, "sink.number"))
        return 508;
    if (staticStringEqual(name, "rapply"))
        return 509;
    if (staticStringEqual(name, "islistfactor"))
        return 510;
    if (staticStringEqual(name, "colSums"))
        return 511;
    if (staticStringEqual(name, "colMeans"))
        return 512;
    if (staticStringEqual(name, "rowSums"))
        return 513;
    if (staticStringEqual(name, "rowMeans"))
        return 514;
    if (staticStringEqual(name, "tracemem"))
        return 515;
    if (staticStringEqual(name, "retracemem"))
        return 516;
    if (staticStringEqual(name, "untracemem"))
        return 517;
    if (staticStringEqual(name, "inspect"))
        return 518;
    if (staticStringEqual(name, "address"))
        return 519;
    if (staticStringEqual(name, "named"))
        return 520;
    if (staticStringEqual(name, "refcnt"))
        return 521;
    if (staticStringEqual(name, "merge"))
        return 522;
    if (staticStringEqual(name, "capabilities"))
        return 523;
    if (staticStringEqual(name, "capabilitiesX11"))
        return 524;
    if (staticStringEqual(name, "new.env"))
        return 525;
    if (staticStringEqual(name, "parent.env"))
        return 526;
    if (staticStringEqual(name, "parent.env<-"))
        return 527;
    if (staticStringEqual(name, "topenv"))
        return 528;
    if (staticStringEqual(name, "l10n_info"))
        return 529;
    if (staticStringEqual(name, "Cstack_info"))
        return 530;
    if (staticStringEqual(name, "mmap_file"))
        return 531;
    if (staticStringEqual(name, "munmap_file"))
        return 532;
    if (staticStringEqual(name, "wrap_meta"))
        return 533;
    if (staticStringEqual(name, "tryWrap"))
        return 534;
    if (staticStringEqual(name, "altrep_class"))
        return 535;
    if (staticStringEqual(name, "file.show"))
        return 536;
    if (staticStringEqual(name, "file.create"))
        return 537;
    if (staticStringEqual(name, "file.remove"))
        return 538;
    if (staticStringEqual(name, "file.rename"))
        return 539;
    if (staticStringEqual(name, "file.append"))
        return 540;
    if (staticStringEqual(name, "file.symlink"))
        return 541;
    if (staticStringEqual(name, "file.link"))
        return 542;
    if (staticStringEqual(name, "file.copy"))
        return 543;
    if (staticStringEqual(name, "list.files"))
        return 544;
    if (staticStringEqual(name, "list.dirs"))
        return 545;
    if (staticStringEqual(name, "file.exists"))
        return 546;
    if (staticStringEqual(name, "file.choose"))
        return 547;
    if (staticStringEqual(name, "file.info"))
        return 548;
    if (staticStringEqual(name, "file.access"))
        return 549;
    if (staticStringEqual(name, "dir.exists"))
        return 550;
    if (staticStringEqual(name, "dir.create"))
        return 551;
    if (staticStringEqual(name, "tempfile"))
        return 552;
    if (staticStringEqual(name, "tempdir"))
        return 553;
    if (staticStringEqual(name, "R.home"))
        return 554;
    if (staticStringEqual(name, "date"))
        return 555;
    if (staticStringEqual(name, "Sys.getenv"))
        return 556;
    if (staticStringEqual(name, "Sys.setenv"))
        return 557;
    if (staticStringEqual(name, "Sys.unsetenv"))
        return 558;
    if (staticStringEqual(name, "getwd"))
        return 559;
    if (staticStringEqual(name, "setwd"))
        return 560;
    if (staticStringEqual(name, "basename"))
        return 561;
    if (staticStringEqual(name, "dirname"))
        return 562;
    if (staticStringEqual(name, "Sys.chmod"))
        return 563;
    if (staticStringEqual(name, "Sys.umask"))
        return 564;
    if (staticStringEqual(name, "Sys.readlink"))
        return 565;
    if (staticStringEqual(name, "Sys.info"))
        return 566;
    if (staticStringEqual(name, "Sys.sleep"))
        return 567;
    if (staticStringEqual(name, "Sys.getlocale"))
        return 568;
    if (staticStringEqual(name, "Sys.setlocale"))
        return 569;
    if (staticStringEqual(name, "Sys.localeconv"))
        return 570;
    if (staticStringEqual(name, "path.expand"))
        return 571;
    if (staticStringEqual(name, "Sys.getpid"))
        return 572;
    if (staticStringEqual(name, "normalizePath"))
        return 573;
    if (staticStringEqual(name, "Sys.glob"))
        return 574;
    if (staticStringEqual(name, "unlink"))
        return 575;
    if (staticStringEqual(name, "polyroot"))
        return 576;
    if (staticStringEqual(name, "inherits"))
        return 577;
    if (staticStringEqual(name, "UseMethod"))
        return 578;
    if (staticStringEqual(name, "NextMethod"))
        return 579;
    if (staticStringEqual(name, "standardGeneric"))
        return 580;
    if (staticStringEqual(name, "Sys.time"))
        return 581;
    if (staticStringEqual(name, "as.POSIXct"))
        return 582;
    if (staticStringEqual(name, "as.POSIXlt"))
        return 583;
    if (staticStringEqual(name, "format.POSIXlt"))
        return 584;
    if (staticStringEqual(name, "strptime"))
        return 585;
    if (staticStringEqual(name, "Date2POSIXlt"))
        return 586;
    if (staticStringEqual(name, "POSIXlt2Date"))
        return 587;
    if (staticStringEqual(name, "mkCode"))
        return 588;
    if (staticStringEqual(name, "bcClose"))
        return 589;
    if (staticStringEqual(name, "is.builtin.internal"))
        return 590;
    if (staticStringEqual(name, "disassemble"))
        return 591;
    if (staticStringEqual(name, "bcVersion"))
        return 592;
    if (staticStringEqual(name, "load.from.file"))
        return 593;
    if (staticStringEqual(name, "save.to.file"))
        return 594;
    if (staticStringEqual(name, "growconst"))
        return 595;
    if (staticStringEqual(name, "putconst"))
        return 596;
    if (staticStringEqual(name, "getconst"))
        return 597;
    if (staticStringEqual(name, "enableJIT"))
        return 598;
    if (staticStringEqual(name, "compilePKGS"))
        return 599;
    if (staticStringEqual(name, "setNumMathThreads"))
        return 600;
    if (staticStringEqual(name, "setMaxNumMathThreads"))
        return 601;
    if (staticStringEqual(name, "stdin"))
        return 602;
    if (staticStringEqual(name, "stdout"))
        return 603;
    if (staticStringEqual(name, "stderr"))
        return 604;
    if (staticStringEqual(name, "isatty"))
        return 605;
    if (staticStringEqual(name, "readLines"))
        return 606;
    if (staticStringEqual(name, "writeLines"))
        return 607;
    if (staticStringEqual(name, "readBin"))
        return 608;
    if (staticStringEqual(name, "writeBin"))
        return 609;
    if (staticStringEqual(name, "readChar"))
        return 610;
    if (staticStringEqual(name, "writeChar"))
        return 611;
    if (staticStringEqual(name, "open"))
        return 612;
    if (staticStringEqual(name, "isOpen"))
        return 613;
    if (staticStringEqual(name, "isIncomplete"))
        return 614;
    if (staticStringEqual(name, "isSeekable"))
        return 615;
    if (staticStringEqual(name, "close"))
        return 616;
    if (staticStringEqual(name, "flush"))
        return 617;
    if (staticStringEqual(name, "file"))
        return 618;
    if (staticStringEqual(name, "url"))
        return 619;
    if (staticStringEqual(name, "pipe"))
        return 620;
    if (staticStringEqual(name, "fifo"))
        return 621;
    if (staticStringEqual(name, "gzfile"))
        return 622;
    if (staticStringEqual(name, "bzfile"))
        return 623;
    if (staticStringEqual(name, "xzfile"))
        return 624;
    if (staticStringEqual(name, "unz"))
        return 625;
    if (staticStringEqual(name, "seek"))
        return 626;
    if (staticStringEqual(name, "truncate"))
        return 627;
    if (staticStringEqual(name, "pushBack"))
        return 628;
    if (staticStringEqual(name, "clearPushBack"))
        return 629;
    if (staticStringEqual(name, "pushBackLength"))
        return 630;
    if (staticStringEqual(name, "rawConnection"))
        return 631;
    if (staticStringEqual(name, "rawConnectionValue"))
        return 632;
    if (staticStringEqual(name, "textConnection"))
        return 633;
    if (staticStringEqual(name, "textConnectionValue"))
        return 634;
    if (staticStringEqual(name, "socketConnection"))
        return 635;
    if (staticStringEqual(name, "sockSelect"))
        return 636;
    if (staticStringEqual(name, "getConnection"))
        return 637;
    if (staticStringEqual(name, "getAllConnections"))
        return 638;
    if (staticStringEqual(name, "summary.connection"))
        return 639;
    if (staticStringEqual(name, "gzcon"))
        return 640;
    if (staticStringEqual(name, "memCompress"))
        return 641;
    if (staticStringEqual(name, "memDecompress"))
        return 642;
    if (staticStringEqual(name, "readDCF"))
        return 643;
    if (staticStringEqual(name, "lockEnvironment"))
        return 644;
    if (staticStringEqual(name, "environmentIsLocked"))
        return 645;
    if (staticStringEqual(name, "lockBinding"))
        return 646;
    if (staticStringEqual(name, "unlockBinding"))
        return 647;
    if (staticStringEqual(name, "bindingIsLocked"))
        return 648;
    if (staticStringEqual(name, "makeActiveBinding"))
        return 649;
    if (staticStringEqual(name, "bindingIsActive"))
        return 650;
    if (staticStringEqual(name, "mkUnbound"))
        return 651;
    if (staticStringEqual(name, "isNamespaceEnv"))
        return 652;
    if (staticStringEqual(name, "registerNamespace"))
        return 653;
    if (staticStringEqual(name, "unregisterNamespace"))
        return 654;
    if (staticStringEqual(name, "getRegisteredNamespace"))
        return 655;
    if (staticStringEqual(name, "isRegisteredNamespace"))
        return 656;
    if (staticStringEqual(name, "getNamespaceRegistry"))
        return 657;
    if (staticStringEqual(name, "importIntoEnv"))
        return 658;
    if (staticStringEqual(name, "env.profile"))
        return 659;
    if (staticStringEqual(name, "Encoding"))
        return 660;
    if (staticStringEqual(name, "setEncoding"))
        return 661;
    if (staticStringEqual(name, "setTimeLimit"))
        return 662;
    if (staticStringEqual(name, "setSessionTimeLimit"))
        return 663;
    if (staticStringEqual(name, "icuSetCollate"))
        return 664;
    if (staticStringEqual(name, "icuGetCollate"))
        return 665;
    if (staticStringEqual(name, "readRenviron"))
        return 666;
    if (staticStringEqual(name, "shortRowNames"))
        return 667;
    if (staticStringEqual(name, "copyDFattr"))
        return 668;
    if (staticStringEqual(name, "getRegisteredRoutines"))
        return 669;
    if (staticStringEqual(name, "getLoadedDLLs"))
        return 670;
    if (staticStringEqual(name, "getSymbolInfo"))
        return 671;
    if (staticStringEqual(name, ".isMethodsDispatchOn"))
        return 672;
    if (staticStringEqual(name, "lazyLoadDBfetch"))
        return 673;
    if (staticStringEqual(name, "lazyLoadDBflush"))
        return 674;
    if (staticStringEqual(name, "getVarsFromFrame"))
        return 675;
    if (staticStringEqual(name, "lazyLoadDBinsertValue"))
        return 676;
    if (staticStringEqual(name, "bincode"))
        return 677;
    if (staticStringEqual(name, "tabulate"))
        return 678;
    if (staticStringEqual(name, "findInterval"))
        return 679;
    if (staticStringEqual(name, "pretty"))
        return 680;
    if (staticStringEqual(name, "formatC"))
        return 681;
    if (staticStringEqual(name, "crc64"))
        return 682;
    if (staticStringEqual(name, "bitwiseAnd"))
        return 683;
    if (staticStringEqual(name, "bitwiseNot"))
        return 684;
    if (staticStringEqual(name, "bitwiseOr"))
        return 685;
    if (staticStringEqual(name, "bitwiseXor"))
        return 686;
    if (staticStringEqual(name, "bitwiseShiftL"))
        return 687;
    if (staticStringEqual(name, "bitwiseShiftR"))
        return 688;
    if (staticStringEqual(name, "serialize"))
        return 689;
    if (staticStringEqual(name, "serializeb"))
        return 690;
    if (staticStringEqual(name, "unserialize"))
        return 691;
    if (staticStringEqual(name, "rowsum_matrix"))
        return 692;
    if (staticStringEqual(name, "rowsum_df"))
        return 693;
    if (staticStringEqual(name, "setS4Object"))
        return 694;
    if (staticStringEqual(name, "traceOnOff"))
        return 695;
    if (staticStringEqual(name, "debugOnOff"))
        return 696;
    if (staticStringEqual(name, "La_qr_cmplx"))
        return 697;
    if (staticStringEqual(name, "La_rs"))
        return 698;
    if (staticStringEqual(name, "La_rs_cmplx"))
        return 699;
    if (staticStringEqual(name, "La_rg"))
        return 700;
    if (staticStringEqual(name, "La_rg_cmplx"))
        return 701;
    if (staticStringEqual(name, "La_rs"))
        return 702;
    if (staticStringEqual(name, "La_rs_cmplx"))
        return 703;
    if (staticStringEqual(name, "La_dlange"))
        return 704;
    if (staticStringEqual(name, "La_dgecon"))
        return 705;
    if (staticStringEqual(name, "La_dtrcon"))
        return 706;
    if (staticStringEqual(name, "La_zgecon"))
        return 707;
    if (staticStringEqual(name, "La_ztrcon"))
        return 708;
    if (staticStringEqual(name, "La_solve_cmplx"))
        return 709;
    if (staticStringEqual(name, "La_solve"))
        return 710;
    if (staticStringEqual(name, "La_qr"))
        return 711;
    if (staticStringEqual(name, "La_chol"))
        return 712;
    if (staticStringEqual(name, "La_chol2inv"))
        return 713;
    if (staticStringEqual(name, "qr_coef_real"))
        return 714;
    if (staticStringEqual(name, "qr_qy_real"))
        return 715;
    if (staticStringEqual(name, "det_ge_real"))
        return 716;
    if (staticStringEqual(name, "qr_coef_cmplx"))
        return 717;
    if (staticStringEqual(name, "qr_qy_cmplx"))
        return 718;
    if (staticStringEqual(name, "La_svd"))
        return 719;
    if (staticStringEqual(name, "La_svd_cmplx"))
        return 720;
    if (staticStringEqual(name, "La_version"))
        return 721;
    if (staticStringEqual(name, "La_library"))
        return 722;
    if (staticStringEqual(name, "bcprofcounts"))
        return 723;
    if (staticStringEqual(name, "bcprofstart"))
        return 724;
    if (staticStringEqual(name, "bcprofstop"))
        return 725;
    if (staticStringEqual(name, "eSoftVersion"))
        return 726;
    if (staticStringEqual(name, "curlVersion"))
        return 727;
    if (staticStringEqual(name, "curlGetHeaders"))
        return 728;
    if (staticStringEqual(name, "curlDownload"))
        return 729;
    return -1;
}
} // namespace rir
#endif
