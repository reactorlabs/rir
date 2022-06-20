define berr
    b Rf_errorcall
end

define pv
    if $arg0==R_MissingArg
        printf "R_MissingArg"
    end
    if $arg0!=R_MissingArg
        call Rf_PrintValue($arg0)
    end
end

define pe
    set $x=R_lsInternal($arg0, 1)
    pv $x
end

define pve
    set $x=Rf_findVar(Rf_install($arg0), $arg1)
    pv $x
end

define pp
    if TYPEOF($arg0)!=PROMSXP
        printf "Not a promise\n"
    end
    if TYPEOF($arg0)==PROMSXP
        printf "Value: "
        if PRVALUE($arg0)==R_UnboundValue
            printf "R_UnboundValue\n"
        end
        if PRVALUE($arg0)!=R_UnboundValue
            pv PRVALUE($arg0)
        end
        printf "Expr: "
        if PRCODE($arg0)==R_MissingArg
            printf "R_MissingArg\n"
        end
        if PRCODE($arg0)!=R_MissingArg
            pv PRCODE($arg0)
        end
    end
end

set history save
set print pretty on
set print array off
set print array-indexes on

define dumpsxp
    printf "\n\n>> SEXP %p\n", $arg0

    if $arg0==0
        printf "NULL\n"
        return
    end

    if $arg1==1
      set $insp=do_inspect(0, 0, Rf_cons($arg0, R_NilValue), 0)
      printf "%s\n", $insp
    end

    set $sexptype=TYPEOF($arg0)

    # typename
    printf "Type: %s (%d)\n", Rf_type2char($sexptype), $sexptype

    # SYMSXP
    if $sexptype==1
        # CHAR(PRINTNAME(x))
        print_char PRINTNAME($arg0)
    end

    # LISTSXP
    if $sexptype==2
        printf "(%s,%s)\n", Rf_type2char(TYPEOF(CDR($arg0))), Rf_type2char(TYPEOF(CAR($arg0)))
#        printf "(  ,%s)\n", type2char(TYPEOF(CDR($arg0)))
    end

    # CLOSXP
    if $sexptype==3
        dumpsxp BODY($arg0) 0
    end

    # PROMSXP
    # Promises contain pointers to value, expr and env
    # tmp = eval(tmp, rho);
    if $sexptype==5
        printf "Promise under evaluation: %d\n", PRSEEN($arg0)
        printf "Expression: "
        dumpsxp ($arg0)->u.promsxp.expr 0
        # Expression: (CAR(chain))->u.promsxp.expr
    end

    # LANGSXP
    if $sexptype==6
        printf "Function:"
        dumpsxp CAR($arg0) 0
        printf "Args:"
        dumpsxp CDR($arg0) 0
    end

    # SPECIALSXP
    if $sexptype==7
        printf "Special function: %s\n", R_FunTab[($arg0)->u.primsxp.offset].name
    end

    # BUILTINSXP
    if $sexptype==8
        printf "Function: %s\n", R_FunTab[($arg0)->u.primsxp.offset].name
    end

    # CHARSXP
    if $sexptype==9
        printf "length=%d\n", ((VECTOR_SEXPREC)(*$arg0))->vecsxp.length
        #print_veclen $arg0
        print_char $arg0
    end

    # LGLSXP
    if $sexptype==10
        set $lgl=*LOGICAL($arg0)
        if $lgl > 0
            printf "TRUE\n"
        end
        if $lgl == 0
            printf "FALSE\n"
        end
    end

    # INTSXP
    if $sexptype==13
        printf "%d\n", *(INTEGER($arg0))
    end

    # REALSXP
    if $sexptype==14
        print_veclen $arg0
        #print_double $arg0
    end

    # STRSXP || VECSXP
    if $sexptype==16 || $sexptype==19
        print_veclen $arg0
        set $i=LENGTH($arg0)
        set $count=0
        while ($count < $i)
            printf "Element #%d:\n", $count
            dumpsxp STRING_ELT($arg0,$count) 0
            set $count = $count + 1
        end
    end

    # RAWSXP
    if $sexptype==24
        print_veclen $arg0
    end

    printf "\n"

end

define print_veclen
    printf "Vector length=%d\n", LENGTH($arg0)
end

define print_char
        # this may be a bit dodgy, as I am not using the aligned union
        printf "\"%s\"\n", (const char*)((VECTOR_SEXPREC *) ($arg0)+1)
end

define print_double
        printf "%g\n", (double*)((VECTOR_SEXPREC *) ($arg0)+1)
end


define ds
  dumpsxp $arg0 1
end

# source .pirpp.py


# multiple commands
python
from __future__ import print_function
import gdb


class Cmds(gdb.Command):
  """run multiple commands separated by ';'"""
  def __init__(self):
    gdb.Command.__init__(
      self,
      "cmds",
      gdb.COMMAND_DATA,
      gdb.COMPLETE_SYMBOL,
      True,
    )

  def invoke(self, arg, from_tty):
    for fragment in arg.split(';'):
      # from_tty is passed in from invoke.
      # These commands should be considered interactive if the command
      # that invoked them is interactive.
      # to_string is false. We just want to write the output of the commands, not capture it.
      gdb.execute(fragment, from_tty=from_tty, to_string=False)
      print()


Cmds()
end
