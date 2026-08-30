(** A wall-clock limit on type-checking a single function.

    Type inference can take arbitrarily long on one function, and that function
    must not cost us the rest of the package. The same mechanism bounds both
    checkers: TypR drives the R side definition by definition, and NativeSem
    schedules its own functions but calls back through the [guard] hook of
    [Runner.run_on_pasts], so the policy lives here either way. *)

exception Elapsed of float

(* [call seconds f x] runs [f x], aborting it with [Elapsed] if it has not
   returned within [seconds].

   [Unix.setitimer] rather than [Unix.alarm], which only takes whole seconds:
   fractional timeouts are useful when scanning a package. The previous timer
   and handler are restored, so nesting is harmless. *)
let call seconds f x =
  let handler = Sys.Signal_handle (fun _ -> raise (Elapsed seconds)) in
  let old_handler = Sys.signal Sys.sigalrm handler in
  let old_timer = Unix.getitimer Unix.ITIMER_REAL in
  let reset () =
    ignore (Unix.setitimer Unix.ITIMER_REAL old_timer) ;
    Sys.set_signal Sys.sigalrm old_handler
  in
  ignore
    (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0. ; it_value = seconds }) ;
  match f x with
  | result -> reset () ; result
  | exception e -> reset () ; raise e

(* [guard seconds ~name ~unchanged f] runs [f ()] under the limit [seconds],
   reporting [name] and returning [unchanged] if it runs out. This is the shape
   NativeSem's [guard] hook expects, and what the R side uses too, so a
   function that takes too long is reported the same way whichever language it
   is written in. [None] means no limit. *)
let guard seconds ~name ~unchanged f =
  match seconds with
  | None -> f ()
  | Some seconds ->
    (try call seconds f () with
     | Elapsed seconds ->
       (* The alarm can fire while the checker is half-way through printing
          its own result. Flush first, so that what it had already produced
          stays where it belongs instead of drifting into a later entry. *)
       Format.print_flush () ;
       Format.printf "%s:@.timeout: inference/checking exceeded %.6g seconds@.@."
         name seconds ;
       unchanged)
