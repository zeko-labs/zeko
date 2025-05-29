let is_compile_simple_real :
    ( Compile_simple.Proof.t * Compile_simple.Verification_key.t
    , Pickles.Side_loaded.Proof.t * Pickles.Side_loaded.Verification_key.t )
    Base.Type_equal.t
    option =
  Some Compile_simple_real.compile_simple_real
