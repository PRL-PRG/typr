# A wrong signature, on purpose: strtoi returns integers, not strings. Loaded
# after the prelude (which does not declare strtoi), it makes the type
# inferred for fuzzpkg's parse_int unsound.
#| base::strtoi : (x: CHR, base: dbl1?) -> CHR
