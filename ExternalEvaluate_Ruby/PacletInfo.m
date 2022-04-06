Paclet[
	"Name" -> "ExternalEvaluate_Ruby",
	"Version" -> "32.0.0",
	"MathematicaVersion" -> "12.3+",
	"AutoUpdating" -> False,
	"Extensions" -> {
		{
			"ExternalEvaluate", 
			"System" -> {"Ruby" -> {"Ordering" -> "0600"}}, 
			"Context" -> {"ExternalEvaluateRuby`"}
		},
		{   
			"Kernel",
			"Root" -> ".",
			"HiddenImport" -> True,
			"Context" -> {"ExternalEvaluateRuby`"}
		},
		{
			"Resource",
			"Root" -> ".",
			"Resources" -> {
		    	(*{"REPL",     "Resources/start_ruby.rb"},*)
		    	{"Icon",     "Resources/IconNormal.wxf"},
		    	{"IconCell", "Resources/IconGray.wxf"}
	    	}
		}	
	}
]
