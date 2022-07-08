structure MLApp :> MLApp =
struct

_public _classtype pwb
{
  _public _static _method "main" (env : Java.String option Java.array option) =
  ignore (
  case env of
    NONE => 
    pwb.main []

  | SOME env =>
    let     
      val array = Java.toArray env
      val nargs = Array.length array
      fun gatherArgs (0, result) = pwb.main result
        | gatherArgs (i, result) =
          let 
            val i = i-1
          in
            case Array.sub(array, i) of
              NONE => gatherArgs (i, ""::result)
            | SOME jstr => gatherArgs (i, Java.toString jstr::result)
          end
    in
      gatherArgs (nargs, [])
    end
  ) 
}

end
