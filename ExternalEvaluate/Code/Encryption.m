
(*

    This module is providing encryption and decryption of an arbitrary expression.

    Some evaluators, like SQL, might need to store sensitive user information.
    To do that they declare a pattern that MIGHT contain sensitive informations.

    We want to save those informations using SystemCredentials, however SystemCredentials does not allow you to store a potentially big expression.
    Every operative system is imposing some limit in the amount of data you can store, so it's not safe to store the whole list of registerd evaluators there.

    The registry is currently stored using PersistentValue that allows you to store arbitrary potentially big expressions on your machine persistently.
    Since the registry is not encrypted by PersistentValue, before saving the registry we are encrypting all parts of the expressions that are sensitive by using Encrypt.

    The key used to encrypt sensitive data is created once using GenerateSymmetricKey. 
    The byte rappresentation of the key is saved as Base64 encoded string in the SystemCredential. 
    Since we are just storing a short base64 encoded string we should not encounter any OS level problem when using SystemCredentials.

    In most machines SystemCredentials should not trigger any UI, but that might happen.

    - riccardod
    
*)

$decryptFailure := Failure["RegistryFailure", <|"MessageTemplate" :> Decrypt::failed|>]

genKey[] := 
    genKey @ Replace[
        (* 
            GenerateRandomByteArray provides os level randomization. 
            Using RandomInteger as a fallback for systems that don't have this function in the layout. 
        *)
        Cryptography`GenerateRandomByteArray[48], 
        Except[_ByteArray] :> ByteArray[RandomInteger[{0, 255}, 48]]
    ]

genKey[key_ByteArray] := {
    GenerateSymmetricKey[
        key[[1;;32]], 
        Method -> <|"Cipher" -> "AES256"|>
    ],
    key[[-16;;]]
}
genKey[s_String] := Replace[
    Quiet[BaseDecode[s]], {
        b_ByteArray /; SameQ[Length[b], 48] :> genKey[b],
        _ :> $decryptFailure
    }
]

encode[{key_, vec_}] := BaseEncode @ Join[
    key["Key"],
    vec
]

$encryptionArguments := $encryptionArguments = 
    autofail @ Replace[
        SystemCredential["ExternalEvaluateRegistry"], {
            s_String :> autofail @ genKey[s],
            _Missing :> With[
                {p = autofail @ genKey[]},
                SystemCredential["ExternalEvaluateRegistry"] = encode[p];
                p
            ]
        }
    ]
    
$sensitivePattern := $sensitivePattern = 
    Flatten[Alternatives @@ DeleteDuplicates[GetLanguageRules[All, "SensitiveInformationPattern"]]]

(* 

    this function is decrypting any leaf of the expression.
    it will typically encounter a DecryptObject in a RuleDelayed.
    In that case it MUST evaluate the content.
    to force evaluation we are using RuleCondition.

*)

decrypt[HoldPattern[e_EncryptedObject]] :=
    Replace[
        Quiet[Decrypt[First @ $encryptionArguments, e], Decrypt::failed],
        _?FailureQ :> $decryptFailure
    ]

decrypt[expr_] := 
    ReplaceAll[
        expr, 
        HoldPattern[e_EncryptedObject] :> RuleCondition[decrypt[e]]
    ]

(* 

    this function is encrypting in place any leaf of the expression.
    when encountering a sensitive pattern it MUST evaluate in place to replace the content with the encrypted one.
    to force evaluation we are using RuleCondition.

*)

encrypt[expr_] :=
    ReplaceAll[
        expr, 
        e:$sensitivePattern :> RuleCondition[Function[{key, vec}, Encrypt[key, e, Method -> <|"InitializationVector" -> vec|>]] @@ $encryptionArguments]
    ]