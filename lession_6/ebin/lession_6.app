{application, lession_6, [
	{description, ""},
	{vsn, "0.1.0"},
	{modules, ['lession_6_sup', 'api_handler', 'lession_6_app', 'toppage_handler']},
	{registered, [lession_6_sup]},
	{applications, [
		kernel,
		stdlib,
		jsx,
		cowboy
	]},
	{mod, {lession_6_app, []}},
	{env, []}
]}.
