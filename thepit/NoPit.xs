#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

STATIC void patch_optree(pTHX_ OP *op) {
	if (op->op_type == OP_ENTERSUB) {
		int targ_type = cLISTOPx(op)->op_last->op_type;
		if (targ_type == OP_METHOD || targ_type == OP_METHOD_NAMED) {
			OP *mark = cLISTOPx(op)->op_first;
			OP *recv = OP_SIBLING(mark);
			if (recv->op_type == OP_ENTERSUB) {
				OP *cv = OP_SIBLING(cUNOPx(cUNOPx(recv)->op_first)->op_first);
				OP *padop = cUNOPx(cv)->op_first;
				if (cv->op_private & OPpENTERSUB_NOPAREN) {
					GV *gv = cGVOPx_gv(padop);
					SV *name;
					if (SvTYPE((SV*)gv) == SVt_PVGV) {
						/* Foo::Bar */
						name = cv_name(GvCV(gv), NULL, 0);
					} else if(SvROK((SV*)gv)) {
						/* Foo */
						name = cv_name((CV*)SvRV((SV*)gv), NULL, CV_NAME_NOTQUAL);
					} else {
						op_dump(padop);
						sv_dump((SV*)gv);
						croak("can't find receiver name");
					}
					op_sibling_splice(op, mark, 1, newSVOP(OP_CONST, 0, newSVsv(name)));
				}
			}
		}
	}

	if (op->op_flags & OPf_KIDS) {
		for (OP *kid = cUNOPx(op)->op_first; kid; kid = OpSIBLING(kid)) {
			patch_optree(aTHX_ kid);
		}
	}
}

STATIC void pre_end_hook(pTHX_ OP **opp) {
	int enabled = 0;
	OP *op = *opp;
	SAVETMPS;
	if (op->op_flags & OPf_KIDS) {
		for (OP *kid = cUNOPx(op)->op_first; kid; kid = OpSIBLING(kid)) {
			if (kid->op_type == OP_NEXTSTATE) {
				SV *hintsv;
				hintsv = cop_hints_fetch_pvs(cCOPx(kid), "pit/remove", 0);
				if (hintsv != NULL) {
					enabled = SvIOK(hintsv) && SvIV(hintsv) > 0;
				}
			}
			if (enabled) {
				patch_optree(aTHX_ kid);
			}
		}
	}
	FREETMPS;
}

STATIC BHK hooks;

MODULE = NoPit PACKAGE = NoPit

BOOT:
	BhkENTRY_set(&hooks, bhk_pre_end, pre_end_hook);
   	Perl_blockhook_register(aTHX_ &hooks);

