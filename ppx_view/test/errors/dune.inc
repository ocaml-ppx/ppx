
(library
 (name invalid_float_lit)
 (modules invalid_float_lit)
 (preprocess (pps ppx.ppx_view)))

(rule
 (target invalid_float_lit.actual)
 (deps (:pp pp.exe) (:input invalid_float_lit.ml))
 (action
  (setenv "OCAML_ERROR_STYLE" "short"
   (setenv "OCAML_COLOR" "never"
    (with-stderr-to %{target}
     (bash "./%{pp} -no-color --impl %{input} || true"))))))

(alias
 (name runtest)
 (action (diff invalid_float_lit.expected invalid_float_lit.actual)))

(library
 (name invalid_int_lit)
 (modules invalid_int_lit)
 (preprocess (pps ppx.ppx_view)))

(rule
 (target invalid_int_lit.actual)
 (deps (:pp pp.exe) (:input invalid_int_lit.ml))
 (action
  (setenv "OCAML_ERROR_STYLE" "short"
   (setenv "OCAML_COLOR" "never"
    (with-stderr-to %{target}
     (bash "./%{pp} -no-color --impl %{input} || true"))))))

(alias
 (name runtest)
 (action (diff invalid_int_lit.expected invalid_int_lit.actual)))

(library
 (name or_pattern_vars_order)
 (modules or_pattern_vars_order)
 (preprocess (pps ppx.ppx_view)))

(rule
 (target or_pattern_vars_order.actual)
 (deps (:pp pp.exe) (:input or_pattern_vars_order.ml))
 (action
  (setenv "OCAML_ERROR_STYLE" "short"
   (setenv "OCAML_COLOR" "never"
    (with-stderr-to %{target}
     (bash "./%{pp} -no-color --impl %{input} || true"))))))

(alias
 (name runtest)
 (action (diff or_pattern_vars_order.expected or_pattern_vars_order.actual)))
