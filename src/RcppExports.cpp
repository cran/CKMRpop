// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_ancestors_and_relatives
List rcpp_ancestors_and_relatives(List L, int n);
RcppExport SEXP _CKMRpop_rcpp_ancestors_and_relatives(SEXP LSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ancestors_and_relatives(L, n));
    return rcpp_result_gen;
END_RCPP
}
// primary_ancestor_pairs
List primary_ancestor_pairs(LogicalMatrix M);
RcppExport SEXP _CKMRpop_primary_ancestor_pairs(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalMatrix >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(primary_ancestor_pairs(M));
    return rcpp_result_gen;
END_RCPP
}
// recursive_push_back
void recursive_push_back(CharacterVector& Boing, int i);
RcppExport SEXP _CKMRpop_recursive_push_back(SEXP BoingSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector& >::type Boing(BoingSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    recursive_push_back(Boing, i);
    return R_NilValue;
END_RCPP
}
// rcpp_test
CharacterVector rcpp_test(CharacterVector v1);
RcppExport SEXP _CKMRpop_rcpp_test(SEXP v1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type v1(v1SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_test(v1));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_CKMRpop_rcpp_ancestors_and_relatives", (DL_FUNC) &_CKMRpop_rcpp_ancestors_and_relatives, 2},
    {"_CKMRpop_primary_ancestor_pairs", (DL_FUNC) &_CKMRpop_primary_ancestor_pairs, 1},
    {"_CKMRpop_recursive_push_back", (DL_FUNC) &_CKMRpop_recursive_push_back, 2},
    {"_CKMRpop_rcpp_test", (DL_FUNC) &_CKMRpop_rcpp_test, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_CKMRpop(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
