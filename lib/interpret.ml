let empty_map = StringMap.empty

type id = string

type bin_op = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * bin_op * exp
  | EseqExp of stm * exp

let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3))
    , CompoundStm
        ( AssignStm
            ( "b"
            , EseqExp
                ( PrintStm [IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1)]
                , OpExp (NumExp 10, Times, IdExp "a") ) )
        , PrintStm [IdExp "b"] ) )

module StringMap = Map.Make (String)

let interp =
  let m = StringMap.empty in
  interp_stm prog m

let rec interp_stm stm map =
  match stm with
  | CompoundStm (stm1, stm2) ->
      let stm' = interp_stm stm stm1 in
      interp_stm stm' stm2
  | AssignStm (id, exp) ->
      let value = interp_exp map exp in
      StringMap.add id value map
  | PrintStm exp ->
      failwith "TODO"

and interp_exp = failwith "TODO"

let rec max_args s =
  match s with
  | CompoundStm (s1, s2) ->
      max (max_args s1) (max_args s2)
  | AssignStm (_, e) ->
      max_args_exp e
  | PrintStm exps ->
      max (List.length exps) (max_args_exp_list exps)

and max_args_exp e =
  match e with
  | IdExp _ | NumExp _ ->
      0
  | OpExp (e1, _, e2) ->
      max (max_args_exp e1) (max_args_exp e2)
  | EseqExp (s, e) ->
      max (max_args s) (max_args_exp e)

and max_args_exp_list exps =
  List.fold_left (fun acc e -> max acc (max_args_exp e)) 0 exps
