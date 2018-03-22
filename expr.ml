(** Type des expressions. *)
type t =
  | Var of string
  | Int of int
  | String of string
  | App of string * t list
  | Assign of string*t
  | Skip
  | Seq of t * t
  | Ite of t * t * t
  | While of t*t

(** Utilitaire pour afficher des listes. *)
let pp_list pp_item pp_sep chan l =
  let rec aux = function
    | [] -> ()
    | [e] -> pp_item chan e
    | hd::tl -> pp_item chan hd ; pp_sep chan ; aux tl
  in aux l

(** Afficheur d'expressions. *)
let rec pp_expr chan = function
  | Var v -> Format.fprintf chan "%s" v
  | Int i -> Format.fprintf chan "%d" i
  | String s -> Format.fprintf chan "%S" s
  | App (f,args) ->
      Format.fprintf chan
        "@[%s(%a)@]"
        f
        (pp_list pp_expr (fun chan -> Format.fprintf chan ",@,")) args
  | Assign (x,e) -> Format.fprintf chan "@[%s := %a@]" x pp_expr e
  | Skip -> Format.fprintf chan "skip"
  | Seq (c,c') -> Format.fprintf chan "@[<v>%a; @,%a@]" pp_expr c pp_expr c'
  | Ite (e,c,c') -> Format.fprintf chan "@[<v>if %a then @;<0 2>@[<v 2>begin@,%a@]@;<0 2>end@,else @;<0 2>@[<v 2>begin@,%a@]@;<0 2>end@]"
                                   pp_expr e pp_expr c pp_expr c'
  | While (e,c) -> Format.fprintf chan
                      "@[<v 2>while %a do @,@[<v 2>begin@,%a@]@,end@]"
                      pp_expr e pp_expr c

