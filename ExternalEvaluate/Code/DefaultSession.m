
(* Perform evaluations using ExternalFunction. *)
$DefaultSession = Automatic;

(* Helper function to check for a valid saved session. *)
getExternalSession[system_String] := system
getExternalSession[assoc_Association] := 
	Replace[
		assoc["Session"], {
			_Missing|None|Inherited|Automatic :> assoc["System"],
			uuid_String :> ExternalSessionObject[uuid]
		}
	]
getExternalSession[_] := $Failed