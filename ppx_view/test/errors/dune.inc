
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
