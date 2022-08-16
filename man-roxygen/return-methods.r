#' @return The recovery generics `recover_*()` return [core model
#'   components][recoverers], [distribution of inertia][conference], and
#'   [supplementary elements][supplementation] but require methods for each
#'   model class to tell them what these components are.
#'
#'   The generic [as_tbl_ord()] returns its input wrapped in the 'tbl_ord'
#'   class. Its methods determine what model classes it is allowed to wrap. It
#'   then provides 'tbl_ord' methods with access to the recoverers and hence to
#'   the model components.
#'   
