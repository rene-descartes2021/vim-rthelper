" Copyright (c) 2022 Rene.Descartes2021

" Builds JSON schema for RobustToolbox Content YAML prototypes

" Parses ctags file `b:gutentags_files.ctags` using readtags
" Uses vimscript. Output is list of dictionaries representing each
"  [DataDefinition] class/structure.
" ctags generation takes ~25s for whole SS14 project first time
"  ~9s subsequent times, and milliseconds on invidual file changes
"  Slowed down by omnisharp-roslyn concurrently parsing same files
"  Optimization would be to use regex tables, could be ~0.2s for whole
"  Now regex tables are used instead of mline regex, could be better
"  vim-gutentags regens tags subset on BufWrite event, faster than whole
" Then this schema regen takes 12.39s on my system for just [DataDef..]
"  Many readtags serial readtags queries and processed in vimscript
"  Optimization matchlist() ~8x faster than split()
"  Optimization would be to process data with pipes and perl/rust regex
"  Optimization would be to re-write in lua or rust
"  Optimization would be to write subschemas instead of whole on BufWrite
"		however yaml-language-server doesn't support subschemas at present
"  Consider optimizations in context of sharing backend between IDEs

" Parallel shell example:
"  Intermediate data are lost, necessary for output data structure
"  Consider hierarchy of [DataDefinition] and inheritance of [DataField]
"  Could possibly build flat list then process hierarchy later when
"		querying [DataField]s, or process hierarchy after than via inherits?
"  Note parallel shell technique may not readily translate to MS Windows
"readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "()") (#/(^\(|,) ?Component(,|\n|\)$)/ $inherits))' -l | cut -f 1 | xargs -P0 -I {hi} readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "(sealed)") (#/(^\(|,) ?{hi}(,|\n|\)$)/ $inherits))' -l
"
" Surprisingly this appears to work with yaml-language-server, when also the
" customTag is set up in settings.... will be a long list
	"IHTNOperator":{
		"additionalProperties":true,
		"description":"[BuildDef]C# type: HTNOperator\rC# scope: namespace.Content.Server.NPC.HTN.PrimitiveTasks",
		"allOf":[
			"{"$ref":"#/$defs/definitions/!type:SetFloatOperator"}
		"]
	"},
	"!type:SetFloatOperator": {
		"type": "object",
			"description": "testing",
			"properties": {
				"targetKey": {"type":"string", "description": "hi"},
				"amount": {"type":"number","description": "hithere"}
			"}
		"},
	"}

let s:self = expand('<sfile>')
command! -nargs=0 RobustResource call execute('source '.s:self)

" Parses data necessary for schema generation
" params:
"  [in] recache: if true will recache any cache
function! robusttoolbox#ParseData(recache) abort
	" d: The {p, i, e, G, ..., yids} definitions built by
	"  parseCS+parseYAML from the ctags file(s)
	let s:d = robusttoolbox#parseCS(a:recache)
	let y = robusttoolbox#parseYAML()
	call extend(s:d, y)
endfunction

" Generate monolithic schema
function! robusttoolbox#GenSchema() abort
	if !exists('s:d')
		call robusttoolbox#ParseData(v:true)
	endif
	if exists('*dein#get')
		let template_file = dein#get('robusttoolbox-vim').path.'/data/template.json'
	else
		let s_dir = fnamemodify(expand('<sfile>'), ':p:h')
		let template_file = s_dir.'/../data/template.json'
	endif

	let tI = reltime()
	let in = join(readfile(template_file, 'b'))
	echom '[robusttoolbox] '.reltimestr(reltime(tI)).'s Read template'
	let template = json_decode(in)
	let g:template = template

	" Qualified name (scope.name) to definition ($defs) name mapping, to avoid
	" name conflicts, lookup types already made, and not have long names in $defs
	let s:scope_name_to_defs = {}
	let s:unresolved_on_first_pass = []

	" List of dicts, e.g. [{'$ref': '#/definitions/entity'},{...}]
	let template.items.oneOf = []
	let template['$defs'] = {}
	let template['$defs']['definitions'] = {}


	" s:tDefinitions: '#/definitions/' for lookups
	" s:tDefs: The schema node to put datafield types on
	"  either '#/definitions/' or '#/$defs/definitions'
	let s:tDefinitions = template.definitions
	let s:tDefs = template['$defs'].definitions

	" 'customTags': [ '!type:SetFloatOperator mapping' ],
	let g:lsp_settings['yaml-language-server']['workspace_config']['customTags'] =
				\ map(copy(s:d.h), {_,j -> '!type:'.j.n.' mapping'})

	" TODO: This can probably be generalized to include prototypes, right? But
	" those concern [DataFields], while serializables implicitly use all fields.
	" Both BuildSerializables and BuildHierarchy should ideally get rid of all
	" unknown- types
	echom 'Hierarchy'
	call s:BuildHierarchy()

	" TODO: Rather than have s:builddef do on-the-fly types as encountered, do
	" all serializable types here???
	" But, on-the-fly may still be necessary, e.g. to link IWireAction to
	" BaseWireAction as [ImplicitDataDefinition] is on BaseWireAction not the
	" interface. Or I could just write all hierarchy to schema
	" TODO: I can't find any non-DataDefinition Serializable instances of !type:
	echom '[Serializable]s... as !type:customTags'
	call s:BuildSerializables(template)

	"echom '[ExplicitDataDefinition]s...'
	"call s:BuildExplicitDataDefinition()

	" echom '[ImplicitDataDefinition]s... (including Components)'
	" " Handle components, e.g. ['Paper', 'PlantHolder', ...]
	let unRegistered = {}
	let ScanComponentChildren = {j -> empty(map(values(j[0].c), {_,l -> ScanComponentChildren(l)})) ? s:BuildComponentDefinition(unRegistered, j) : s:BuildComponentDefinition(unRegistered, j)}
	" let ScanImplicitDDChildren = {j -> empty(has_key(j[0], 'c') ? map(copy(j[0].c), {_,l -> ScanImplicitDDChildren(l)}): 0) ? s:BuildImplicitDataDefinition(template, j) : s:BuildImplicitDataDefinition(template, j)}
	" for il in s:d.i
		" if il[0].n ==# 'Component'
			" call ScanComponentChildren(il)
		" else
			" " TODO: Add other implicit data definitions to $defs, and ddmap
			" " TODO: Handle abstract inheritance
			" call ScanImplicitDDChildren(il)
		" endif
	" endfor
	let s:tDefs['!type:Component'].properties = {}
	let s:tDefs['!type:Component'].properties.type = {
		\ 'description':'Type of component, either remove "Component" suffix or use [ComponentProtoName("name")]',
		\ 'enum':[],
		\ 'type:':'string'
	\ }
	let s:tDefs['!type:Component'].required = ['type']
	call ScanComponentChildren(s:name_to_implicit['Component'])

	" echom 'Unregistered Components...'
	" " Now handle unregistered components as long as they're not in ddmap
	" for [name, dd] in items(unRegistered)
		" if index(values(s:scope_name_to_defs), name) == -1
			" " We have a component [DataDefinition] we can add without conflict
			" let s:scope_name_to_defs[dd[0].s . '.' . dd[0].n] = name
			" call s:BuildComponentDefinitionInner(dd, name)
		" else
			" echom 'skipped registered component to avoid duplicate: '.name
		" endif
	" endfor

	" Populate Color enumeration... is in OpenTK and not an enum sadly
	call s:BuildColorDefinition(template)

	echom '[Prototype]s...'
	" Handle prototypes, e.g. ['entity', 'shader', ...]
	let s:nonDD = {}
	call s:BuildPrototypeDefinition(template)

	call s:HandleUnresolved()

	echom '[robusttoolbox] '.reltimestr(reltime(tI)).'s Schema generated'

	let out = json_encode(template)
	let out_dir = gutentags#get_project_root(getcwd()).'/Resources/Schemas'
	if !isdirectory(out_dir)
		call mkdir(out_dir, 'p')
	endif
	let out_file = out_dir.'/prototypes.json'
	call writefile([out], out_file, 'S')
	echom '[robusttoolbox] '.reltimestr(reltime(tI)).'s Schema written'
	let out = json_encode(
		\ { 'workspace_config':
			\ { 'customTags': g:lsp_settings['yaml-language-server']['workspace_config']['customTags']}})
	let out_file = out_dir.'/workspace.json'
	call writefile([out], out_file, 'S')
	echom '[robusttoolbox] '.reltimestr(reltime(tI)).'s workspace/customTags written'
endfunction

function! s:HandleUnresolved()
	call filter(s:unresolved_on_first_pass, {_,j -> !has_key(s:tDefs, '!type:' . j[0].n)})
	echom 'unresolved types: '.len(s:unresolved_on_first_pass)
	let g:unresolved = s:unresolved_on_first_pass
	for dd in s:unresolved_on_first_pass
		let schema_name = s:GetSuitableSchemaName(dd[0].s, dd[0].n)
		let s:tDefs[schema_name] = {
			\ 'type': 'object',
			\ 'description': 'undefined-' . dd[0].s . '.' . dd[0].n
			\ }
	endfor
endfunction

" Builds classes and structs with [Serializable], and also all enums
" Should be called after BuildHierarchy, as BuildHierarchy
"  should make skeleton first
function! s:BuildSerializables(template)
	" Build all enums regardless of [Serializable]
	for [scope, names] in items(s:scope_name_to_Enum)
		for [name, s] in items(names)
			let qname = scope . '.' . name
			let description = 'C# def: enum' . ' ' . qname .
				\ (has_key(s, 't') && !empty(s.t) ? (' : ' . s.t) : '')
			" Could possibly do a oneOf {string, x}, where "enum foo : x"
			if has_key(s:scope_name_to_defs, qname) && has_key(s:tDefs, s:scope_name_to_defs[qname])
				"let schema_name = s:scope_name_to_defs[qname]
				" TODO: If here then I think that this enum was built from a map_type call?
				echom 'error, should never already have parsed same enum: '.qname
				continue
			else
				let schema_name = s:GetSuitableSchemaName(s.s, s.n)
			endif
			let s:tDefs[schema_name] = {
				\ 'type': 'string',
				\ 'enum': reduce(s.e, {l,i -> add(l, i.n)}, []),
				\ 'description': '[Enum]'.description,
				\ }
		endfor
	endfor

	" Build all [Serializable]
	" See RequestPerformActionEvent ctors, will have to have oneOf
	" [properties:{}, properties:{}, properties{}] for each ctor with required
	" Will have to enumerate ctors, and then enumerate each parameter list
	" unless it is a [DataDefinition] or Inherits from an [ImplicitDataDefinition]
	for [scope, names] in items(s:scope_name_to_Serializable)
		for [name, s] in items(names)
			" Assume first partial has everything, even ctors from all partials were
			" queried and put on first partial
			let qname = scope . '.' . name
			let description = 'C# def: ' . s[0].t . ' ' . qname .
				\ (empty(s[0].i) ? '' : (' : ' . s[0].i))
			if has_key(s:scope_name_to_defs, qname)
				let schema_name = s:scope_name_to_defs[qname]
			else
				let schema_name = s:GetSuitableSchemaName(scope, name)
			endif
			if !has_key(s:tDefs, schema_name)
				let s:tDefs[schema_name] = {
					\ 'type': 'object',
					\ 'properties': {},
					\ 'description': description,
					\ }
			endif

			" properties based on ctor parameter lists
			" if len(s[0].H) == 1
				" call s:handle_plist_token(s:tDefs[name]['properties'], s[0].H[0].pl[1:-2], s[0].H[0].f, s[0].H[0].s)
			" elseif len(s[0].H) > 1
				" " Put each in oneOf
				" let s:tDefs[name]['oneOf'] = []
				" for i in range(len(s[0].H))
					" let oneOf = {'properties': {}}
					" call s:handle_plist_token(oneOf['properties'], s[0].H[i].pl[1:-2], s[0].H[i].f, s[0].H[i].s)
					" call add(s:tDefs[name]['oneOf'], oneOf)
				" endfor
			" endif
		endfor
	endfor
endfunction

" Get the types using s:map_type
" TODO: But what are the names? The parameter list names or the names on the
" containing class/struct? What if no ctor? Implicitly use names?
" TODO: Failure to parse sequential parameters with <>:
" "entry with str: "\n        Dictionary<int, List<Vector2i>>? spaceTiles,\n Dictionary<EntityUid, Dictionary<int, List<Vector2i>>> tiles,\n        Matrix3 spaceMatrix,\n        ushort spaceTileSize"
" default: ['\n        Matrix3 spaceMatrix,\n        ushort spaceTileSize', '', '', '\n        Matrix3 spaceMatrix,\n        ushort spaceTileSize', '', '', '', '', '', '']
" tiles: {'required': v:true, 'type': {'description': 'C# def: Dictionary<int, List<Vector2i>>? spaceTiles,\n        Dictionary<
" EntityUid, Dictionary<int, List<Vector2i>>>', 'additionalProperties':
" {'description': 'C# def: List<Vector2i>>? spaceTiles,\n
"        Dictionary<EntityUid, Dictionary<int, List<Vector2i>>', 'type':
"        'array', 'items': {'description': 'unknown-C# def: Vect
"        or2i>>? spaceTiles,\n        Dictionary<EntityUid, Dictionary<int,
"        List<Vector2i>', 'type': 'object'}}, 'type': 'object', 'pro
"        pertyNames': {'allOf': [{'$ref':
"        '#/$defs/definitions/ENUM_int'}]}}}"
"		DONE I think, didn't validate
" TODO: Handle tuple paramters:
"		"(TimeSpan, TimeSpan) cooldown, bool refresh, string?"
"		"bool? antagonist, (string name, EntityUid)[] allies"
"		DONE I think, didn't validate
"	TODO: Handle ref parameters:
"		"ref Matrix3 matrix"
"		DONE I think, didn't validate
" relevantComponent=null"
" [in|out] properties: the {properties: {}} dictionary to populate with str
"  parsed
" [in] str: the property list, e.g. 'int foo, float bar'
" [in] f: file str is in
" [in] scope: scope of containing type
function! s:handle_plist_token(usings, properties, str, f, scope)
	" NOTE: Appears that matchlist translates input char "\n" to string '\n' so
	" have to match for both... \%(\n\|\\n\)
	" Also jump over 'ref' keyword if there
	let Prune = {s -> matchlist(s, '\V\^\%(\[\n\t ]\|\\n\|\\t\)\*\%(ref \)\?\(\.\*\)\?')}
	let Type = {s -> matchlist(s, '\V\^\(\[a-zA-Z0-9.]\+\)\(\.\*\)\?')}
	let Tuple = {s -> matchlist(s, '\V\^\((\[^)]\*)\)\(\.\*\)\?')}
	let Brak = {s -> matchlist(s, '\V\^<\(\[^<>]\+\)\(\.\*\)\?')}
	let SBrak = {s -> matchlist(s, '\V\^[\(\[^]]\*\)]\(\.\*\)\?')}
	let Nullable = {s -> matchlist(s, '\V\^?\(\.\*\)\?')}
	let Name = {s -> matchlist(s, '\V\^\[\n\t ]\*\(\[^,=\n\t $]\+\)\[\n\t ]\*,\?\(\.\*\)\?')}
	let Defa = {s -> matchlist(s, '\V\^\(=\[\n\t ]\*\(\[^,$]\+\)\[\n\t ]\*\)\?,\?\[\n\t ]\*\(\.\*\)\?')}
	"echom '===================='
	"echom 'entry with str: "'.a:str.'"'
	let pruned = Prune(a:str)
	if !empty(pruned)
		let token = Type(pruned[1])
		if empty(token)
			"Try tuple
			let token = Tuple(pruned[1])
		endif
		if !empty(token)
			let ttype = token[1]
			let remainder = token[2]
			"echom 'token: '.string(token)
			let type_brak = Brak(remainder)
			while !empty(type_brak)
				let ttype = ttype.'<'.type_brak[1]
				let remainder = type_brak[2]
				while remainder[0] == '>'
					let ttype = ttype.'>'
					let remainder = remainder[1:]
				endwhile
				let type_brak = Brak(remainder)
			endwhile
			"echom 'remainder after brak: '.remainder
			let type_sbrak = SBrak(remainder)
			if !empty(type_sbrak)
				let ttype = ttype.'['.type_sbrak[1].']'
				let remainder = type_sbrak[2]
			endif
			"echom 'remainder after sbrak: '.remainder
			let type_nullable = Nullable(remainder)
			if !empty(type_nullable)
				let ttype = ttype.'?'
				let remainder = type_nullable[1]
			endif
			let name = Name(remainder)
			if empty(name)
				echom 'empty name for str: '.a:str.' type: '.ttype.' string remainder: '.remainder
			endif
			let remainder = name[2]
			let name = name[1]
			let ttype = s:map_type(a:usings, {'t':ttype, 'f': a:f, 's': a:scope})
			let a:properties[name] = {'type': ttype}
			let default = Defa(remainder)
			"echom 'default: '.string(default)
			if !empty(default)
				let remainder = default[3]
			endif
			if empty(default[2])
				let a:properties[name]['required'] = v:true
			else
				let a:properties[name]['description'] = 'Default: '.default[2]
			endif
			"echom name.': '.string(a:properties[name])
			" Process remainder
			if !empty(remainder)
				call s:handle_plist_token(a:usings, a:properties, remainder, a:f, a:scope)
			endif
		endif
	endif
endfunction

" First write a skeleton then later fill it with properties?
function! s:BuildHierarchy()
	for [scope, names] in items(s:scope_name_to_dd)
		for [name, dd] in items(names)
			let s = dd[0]
			if !has_key(s, 'ai') && !has_key(s, 'ae') && !has_key(s, 'ap')
				" skip non Implicit DataDefinition & Explicit DataDefinition
				continue
			endif
			call s:BuildSchemaElement(dd, has_key(s, 'ai'))
		endfor
	endfor
endfunction

" Gets suitable schema name, resolving for name conflicts
"  Maps a C# qualified type (with scope) to YAML definition (schema) name (not yaml name)
" Params:
"  [in] scope: C# scope
"  [in] name: C# name
" returns schema name (e.g. '!type:Content.Server.Foo')
"  updates s:scope_name_to_defs if not there already
function! s:GetSuitableSchemaName(scope, name)
	let schema_name = '!type:'.a:name
	let qname = a:scope . '.' . a:name
	if has_key(s:scope_name_to_defs, qname)
		"echom 'Found duplicate qname, already traversed: '.qname
		return s:scope_name_to_defs[qname]
	endif
	if has_key(s:tDefs, schema_name)
		let existing = filter(copy(s:scope_name_to_defs), {k,v -> v ==# schema_name})
		let existing_qname = keys(existing)[0]
		if existing_qname ==# qname
			echom 'Found duplicate '.qname.' in $defs, overwriting: '.schema_name
		else
			"Try scoped schema_name to avoid collision
			let schema_name = '!type:'.qname
			if has_key(s:tDefs, schema_name)
				echom 'Found duplicate in $defs collision unavoidable, overwriting: '.schema_name
			endif
		endif
	endif
	let s:scope_name_to_defs[qname] = schema_name
	return schema_name
endfunction

" Builds the !type: in schema on /$defs/definitions
" Params:
"  [in] dd: partial list
"  [in] force: Will force all children to be written
function! s:BuildSchemaElement(dd, force_children)
	let s = a:dd[0]
	" Seems that during BuildHierarchy if map_type maps a df of child of
	" implicitdatadef, that the following clause would halt subsequent
	" implicitdatadef building, so we should just overwrite what was written
	" previously
	let qname = s.s . '.' . s.n
	if has_key(s:scope_name_to_defs, qname) && has_key(s:tDefs, s:scope_name_to_defs[qname])
		return
	" elseif !a:force_children && has_key(s:scope_name_to_defs, qname)
		" return
	endif
	let description = 'C# def: ' . s.t . ' ' . qname .
		\ (empty(s.i) ? '' : (' : ' . s.i))
	let schema_name = s:GetSuitableSchemaName(s.s, s.n)
	let up_schema_name = schema_name . '__U'
	let s:tDefs[schema_name] = {
		\ 'type': 'object',
		\ 'description': description,
		\ 'allOf': [{
			\ '$ref' : '#/$defs/definitions/'.up_schema_name
			\ }]
		\ }
	let s:tDefs[up_schema_name] = {
		\ 'type': 'object',
		\ 'properties': {},
		\ }
	if has_key(s, 'h') && s.h ==# 'sealed'
		let s:tDefs[schema_name]['additionalProperties'] = v:false
	else
		" TODO: Will the oneOf work with child schema resolution? Or
		"  should I accumulate all subchildren? return concat(ret,
		"  s:tDefs[schema_name]['oneOf'])
		"let s:tDefs[schema_name]['oneOf'] = s:BuildHierarchy()
	endif
	let isAbstract = v:false
	for partial in a:dd
		if has_key(partial, 'h') && partial.h ==# 'abstract'
			let isAbstract = v:true
		endif
	endfor
	let required = []
	for partial in a:dd
		" datafields to up_schema_name
		let usings = s:getUsingsFromFileScopeParents(partial.f, partial.s, partial.p)
		for df in partial.df
			let dfn = s:ParseDataFieldName(partial, df)
			if !isAbstract && has_key(df, 'r')
				call add(required, dfn)
			endif
			" If isAbstract we're putting the abstract properties as {} on the
			"  inheriting definition
			"let s:tDefs[up_schema_name].properties[dfn] = (isAbstract ? {} : s:map_type(usings, df.df))
			let s:tDefs[up_schema_name].properties[dfn] = s:map_type(usings, df.df)
		endfor
		" Always ensure parents exist in schema regardless of
		" Implicit|Explicit DataDefinition
		if len(partial.p) > 0
			if !has_key(s:tDefs[up_schema_name], 'allOf')
				let s:tDefs[up_schema_name]['allOf'] = []
			endif
			for parent in values(partial.p)
				call s:BuildSchemaElement(parent, v:false)
				" TODO: Lookup schema_name based on parent[0].s, parent[0].n
				" But, if not in s:scope_name_to_defs yet then collision possible?
				" TODO: Same with children, see EyeComponent in schema duplication in
				"  SharedEyeComponent oneOf, one is Client and one is Server
				let parent_qname = parent[0].s . '.' . parent[0].n
				let parent_schema_name = s:scope_name_to_defs[parent_qname]
				call add(s:tDefs[up_schema_name]['allOf'],
					\ {
						\ '$ref' : '#/$defs/definitions/'.parent_schema_name . '__U',
						\ 'additionalProperties': v:true,
					\ })
			endfor
		endif
		" Only write children if we traverse from a
		" Implicit DataDefinition, otherwise just ref them
		if len(partial.c) > 0
			if !has_key(s:tDefs[schema_name], 'oneOf')
				let s:tDefs[schema_name]['oneOf'] = []
			endif
			for child in values(partial.c)
				" TODO: v:false on last param may be inapproprate, may want parents
				if (has_key(s, 'ai') || a:force_children)
					call s:BuildSchemaElement(child, v:true)
					let child_qname = child[0].s . '.' . child[0].n
					let child_schema_name = s:scope_name_to_defs[child_qname]
					call add(s:tDefs[schema_name]['oneOf'],
						\ {'$ref' : '#/$defs/definitions/'.child_schema_name})
				endif
			endfor
		endif
	endfor
	" Add requiered properties to up_schema_name
	if !empty(required)
		if !has_key(s:tDefs[up_schema_name], 'required')
			let s:tDefs[up_schema_name].required = []
		endif
		let s:tDefs[up_schema_name].required += required
	endif
	if has_key(s:tDefs[schema_name], 'oneOf') && has_key(s, 'h') &&
		\ s.h !=# 'abstract' && s.t !=# 'I'
		" If not abstract or interface then allow for itself to exist not as a child
		call add(s:tDefs[schema_name]['oneOf'], {})
	endif
	"call add(ret, {'$ref' : '#/$defs/definitions/'.schema_name})
endfunction

" Builds each prototype schema and puts in #/definitions
function! s:BuildPrototypeDefinition(template)
	let yn = ''
	for [scope, names] in items(s:scope_name_to_Prototype)
		for [name, dd] in items(names)
			" May be partial class, attribute/inherits on one not the other
			"let inherits = []
			"let InheritsToken = {s -> matchlist(s, '\V\(\[^<>\\ ,]\+\%(<\.\*>\)\?\)\%(\\n\| \|,\)\*\(\.\*\)\?')}
			for partial in dd
				if has_key(partial, 'yn')
					" Note: GameMapPrototype will have a:[] on one partial
					let yn = partial.yn
				endif
				" let m = partial.i
				" while !empty(m)
					" let m = InheritsToken(m)
					" call add(inherits, m[1])
					" let m = m[2]
				" endwhile
			endfor
			let description = 'Prototype C# type: '.dd[0].n
			let schema_name = s:GetSuitableSchemaName(scope, name)
			" NOTE: Moved from /definitions to /$defs/definitions
			let refstr = '#/$defs/definitions/'.schema_name
			call add(a:template.items.oneOf, {'$ref': refstr})
			call add(s:tDefinitions.prototype.properties.type.enum, yn)
			if !has_key(s:tDefs, schema_name)
				let s:tDefs[schema_name] = {}
			endif
			let s = s:tDefs[schema_name]
			let s['$id'] = refstr
			"	\ 'type': 'object',
			if !has_key(s, 'allOf')
				let s['allOf'] = [{'$ref': '#/definitions/prototype'}]
			else
				call add(s['allOf'], {'$ref': '#/definitions/prototype'})
			endif
			let s['additionalProperties'] = v:false
			if !has_key(s, 'properties')
				let s['properties'] = {}
			endif
			let s['properties']['id'] = {}
			let s['properties']['type'] = {
				\ 'enum': [yn],
				\ 'description': description
			\ }

			" let sNode = s:tDefs[schema_name]

			" " Now search datadefinitions for inherits
			" for parent in inherits
				" if has_key(s:nonDD, parent)
					" continue
				" endif
				" let found = v:false
				" let scopeSuffix = ''
				" let m = matchlist(parent, s:has_scope)
				" if len(m) > 0
					" let scopeSuffix = escape(m[1], '.').'$'
					" echom yn.' parent '.parent.' had scope, is now '.scopeSuffix.'.'.m[2]
					" let parent = m[2]
				" endif
				" for e in s:d.e
					" if e[0].n ==# parent && e[0].s =~# scopeSuffix
						" let found = s:resolvename(e, '!type:')
						" " additionalProperties: {} causes weird issue with entityTargetAction
						" call add(a:template.definitions[yn].allOf,
							" \ {
								" \ '$ref': '#/$defs/definitions/'.found,
								" \ 'additionalProperties': v:true
								" \ })
						" break
					" endif
				" endfor
				" if !found
					" let r = s:Scan(s:d.i, parent, scopeSuffix)
					" if type(r) == v:t_dict
						" let r.additionalProperties = v:true
						" call add(a:template.definitions[yn].allOf, r)
						" let found = split(r['$ref'], '/')[-1]
					" endif
				" endif
				" " TODO: Now try interfaces, but need to translate that datastructure to partials
				" "if !found
					" " let r = s:ScanInterfaces(s:d.in, parent, scopeSuffix)
					" " if type(r) == v:t_dict
						" " let r.additionalProperties = v:false
						" " call add(a:template.definitions[yn].allOf, r)
						" " let found = split(r['$ref'], '/')[-1]
					" " endif
				" " endif
				" if found == v:false
					" echom 'not found '.parent.' for '.yn.' : '.parent
					" let s:nonDD[parent] = v:true
				" else
					" echom 'found '.found.' for '.yn.' : '.parent
					" " Add properties in the form of {}
					" let props = a:template.definitions[yn].properties
					" for prop in keys(s:tDefs[found].properties)
						" let props[prop] = {}
					" endfor
					" " TODO: Might have to merge in required
				" endif
			" endfor

			" call s:ParseDataFields(sNode, a:dd, v:false)
		endfor
	endfor
endfunction

function! s:BuildComponentDefinition(unRegistered, dd)
	let name = a:dd[0].n
	" Check if this component has [RegisterComponent]
	let isRegisterComponent = v:false
	" Retrieve [ComponentProtoName("name")] attribs in parse
	let isComponentProtoName = v:false
	for partial in a:dd
		" TODO: UtilityAI found twice here?
		if has_key(partial, 'arc')
			let isRegisterComponent = v:true
		endif
		if has_key(partial, 'acpn')
			let s = matchlist(partial.acpn.n, '\V\.\{-}\%([\|,\| \)ComponentProtoName("\(\[^"]\+\)')
			let yaml_name = s[1]
			let isComponentProtoName = v:true
			break
		endif
	endfor
	if isComponentProtoName == v:false
		" Truncate 'Component' suffix:
		let m = match(name, 'Component$')
		if m != -1
			let yaml_name = strcharpart(name, 0, m)
		endif
		" Need to remove (Shared|Client|Server) prefix
		let prefixRemoved = v:false
		let m = match(name, '^Shared')
		if m != -1
			let yaml_name = strcharpart(name, len('Shared'))
			let prefixRemoved = v:true
		endif
		let m = match(name, '^Client')
		if m != -1 && !prefixRemoved
			let yaml_name = strcharpart(name, len('Client'))
			let prefixRemoved = v:true
		endif
		let m = match(name, '^Server')
		if m != -1 && !prefixRemoved
			let yaml_name = strcharpart(name, len('Server'))
		endif
	endif
	if a:dd[0].n ==# 'Component'
		" Handle top-level separately
		" TODO: Can generalize this into 'abstract or non-sealed' procedure
		"let sNode = s:tDefinitions['Component']
		"call s:ParseDataFields(sNode, a:dd, v:false)
		" Seems to be hit last from ScanComponentChildren recursion, nothing
		"  to do I think
	elseif !(isRegisterComponent || isComponentProtoName)
		" Doesn't have [RegisterComponent] or [ComponentProtoName]
		"  Add to list to pull from later if no name conflicts
		" Only add if sealed to avoid duplicates:
		if a:dd[0].h ==# 'sealed'
			if has_key(a:unRegistered, yaml_name)
				echom 'duplicate in unRegistred: '.yaml_name.' l isSealed: '.a:dd[0].h.' r isSealed: '.a:unRegistered[yaml_name][0].h
			endif
			let a:unRegistered[yaml_name] = a:dd
		endif
	else
		let qname = a:dd[0].s . '.' . a:dd[0].n
		"if index(values(s:scope_name_to_defs), yaml_name) == -1
		if has_key(s:scope_name_to_defs, qname)
			let schema_name = s:scope_name_to_defs[qname]
		else
			let schema_name = '!type:'.a:dd[0].n
			if has_key(s:tDefs, schema_name)
				let schema_name = '!type:'.qname
			endif
			"let s:scope_name_to_defs[qname] = schema_name
			"call s:BuildComponentDefinitionInner(a:dd, yaml_name, schema_name)
			echom 'Seems to have not been built by hierarchy: '.schema_name.'. yaml_name:'.yaml_name
		endif
		let [scope, scopeStr, description] = s:BuildDescription(a:dd, yaml_name)
		if index(s:tDefs['!type:Component'].properties.type.enum, yaml_name) == -1
			" No duplicate component name wanted... Seems both Server|Client are
			" registered now, maybe prefer Client?
			call add(s:tDefs['!type:Component'].properties.type.enum, yaml_name)
		endif
		" ComponentRegistry allows the 'components' datafield of prototype
		" to resolve
		call add(s:tDefinitions.ComponentRegistry.items.oneOf, {'$ref': '#/$defs/definitions/'.schema_name})
		" KLUDGE: Should be on up_schema_name?
		if !has_key(s:tDefs[schema_name], 'properties')
			let s:tDefs[schema_name]['properties'] = {}
		endif
		let s:tDefs[schema_name]['properties']['type'] = {
			\ 'enum': [yaml_name],
			\ 'description': description,
		\ }
	endif
	return 0
endfunction

" Returns [scope, scopeStr, description]
function s:BuildDescription(dd, name_yaml)
	" Build scope, component/DataDefinition/etc may have
	"  both Client/Server scope:
	let scope = [a:dd[0].s]
	let scopeStr = 'C# Scope: '.a:dd[0].k.'.'.a:dd[0].s
	for i in range(1, len(a:dd) - 1)
			if index(scope, a:dd[i].s) == -1
				call add(scope, a:dd[i].s)
				let scopeStr .= ','.a:dd[i].k.'.'.a:dd[i].s
			endif
	endfor
	let description = 'Name: '.a:name_yaml.nr2char(13).scopeStr
	return [scope, scopeStr, description]
endfunction

" OBSOLETE
function s:OBSOLETE_BuildComponentDefinitionInner(dd, yaml_name, schema_name)
	let name_yaml = a:yaml_name
	let name_schema = a:schema_name
	let [scope, scopeStr, description] = s:BuildDescription(a:dd, name_yaml)
	call add(s:tDefinitions.Component.properties.type.enum, name_yaml)
	call add(s:tDefinitions.ComponentRegistry.items.oneOf, {'$ref': '#/$defs/definitions/'.name_schema})
	" Don't parse parent 'Component' datafields
	" Each parent should be parsed on its own and enums set up
	"let ScanComponentParents = {j -> empty(has_key(j[0], 'p') && j[0].p[0].n !=# 'Component' ? ScanComponentParents(j[0].p) : 0 ) ? ExtendDf(j) : ExtendDf(j) }
	"call ScanComponentParents(a:dd)
	let s:tDefs[name_schema] = {
		\ '$id': '#/definitions/'.name_schema,
		"\ 'description': description,
		\ 'type': 'object',
		\ 'allOf': [{ '$ref': '#/definitions/Component' }],
		\ 'additionalProperties': v:false,
		\ 'properties': {
			\ 'type': {
				\ 'enum': [name_yaml],
				\ 'description': description
			\ }
		\ }
	\ }
	let sNode = s:tDefs[name_schema],
	call s:ParseDataFields(sNode, a:dd, v:true)
endfunction

function! s:BuildExplicitDataDefinition()
	for scope in keys(s:scope_name_to_explicit)
		for [name, e] in items(s:scope_name_to_explicit[scope])
			let name_yaml = name
			" Use E prefix in schema to prevent name conflict with prototypes/others
			" Looks like in yaml I've seen !type:PhysShapeAabb, and Exlicit Data
			" Definition, so we should do away with E prefix and just use !type.
			let qname = scope . '.' . name
			let name_schema = s:scope_name_to_defs[qname]
			let [scope2, scopeStr, description] = s:BuildDescription(e, name_yaml)
			" let sNode = {
				" \ '$id': '#/$defs/definitions/'.name_schema,
				" "\ 'description': description,
				" \ 'type': 'object',
				" \ 'additionalProperties': v:false,
				" \ 'properties': {
				" \ }
			" \ }
			" let s:tDefs[name_schema] = sNode
			" TODO: s:tDefs[name_schema] already written by BuildHierarchy
			"  just want datafields
			"  Should make test schema and yaml file again and do manual edits
			"  to figure out hierarchy design
			let sNode = s:tDefs[name_schema]
			let sNode['additionalProperties'] = v:false
			let sNode['properties'] = {}
			call s:ParseDataFields(sNode, e, v:false)
		endfor
	endfor
endfunction

function! s:BuildImplicitDataDefinition(template, dd)
endfunction

" Parses the datafields (df) for definition (dn) and stores in template
" Params:
"  [in/out] sNode: The schema node to put datafields/required on
"  [in] dd: List of partials
"  [in] recursive: ???
function! s:ParseDataFields(sNode, dd, recursive)
	echom 'ParseDataFields: '.a:dd[0].n
	let isAbstract = v:false
	if a:recursive && has_key(a:dd[0], 'h') && a:dd[0].h ==# 'abstract'
		"if a:dd[0].s.'.'.a:dd[0].n ==# 'Robust.Shared.GameObjects.Component'
			let isAbstract = v:true
			" For now only consider Component to be abstract, later we'll fix
			"  We inherit the properties of the parent using empty {}
		"endif
	endif
	let required = []
	let traversed_parents = []
	let emptyD = {}
	for partial in a:dd
		let usings = s:getUsingsFromFileScope(partial.f, partial.s, partial.p)
		for df in partial.df
			let dfn = s:ParseDataFieldName(partial, df)
			echom 'parsing '.dfn
			if !isAbstract && has_key(df, 'r')
				call add(required, dfn)
			endif
			" If isAbstract we're putting the abstract properties as {} on the
			"  inheriting definition
			let a:sNode.properties[dfn] = (isAbstract ? {} : s:map_type(usings, df.df))
		endfor
		" TODO: I think required being duplicated in both e.g.
		"  StackComponent and ClothingComponent is because the required
		"  DataField is on the Shared ancestor, and because the partial list
		"  includes (Client|Server)StackComponent : SharedStackComponent,
		"  the solution seems to be... either proper abstraction, or if
		"  we already traversed this parent then don't do it again
		if a:recursive && !empty(partial.p)
			let check = partial.p[0].s . '.' . partial.p[0].n
			if index(traversed_parents, check) != -1
				continue
			endif
			call add(traversed_parents, check)
			call s:ParseDataFields(a:sNode, partial.p, a:recursive)
		endif
	endfor
	if !empty(required)
		if !has_key(a:sNode, 'required')
			let a:sNode.required = []
		endif
		let a:sNode.required += required
	endif
endfunction

function! s:BuildColorDefinition(template)
	let colors = map(copy(s:d.colors), {i,j -> j.n})
	let a:template.definitions.Color.oneOf[0].enum = colors
endfunction

" TODO: Make integer schema types with minimum/maximum
let s:typemap_simple = {
	\	'string': 'string',
	\	'String': 'string',
	\	'bool': 'boolean',
	\	'Boolean': 'boolean',
	\	'float': 'number',
	\	'double': 'number',
	\	'byte': 'integer',
	\	'sbyte': 'integer',
	\	'ubyte': 'nonNegativeInteger',
	\	'short': 'integer',
	\	'sshort': 'integer',
	\	'ushort': 'nonNegativeInteger',
	\	'int': 'integer',
	\	'sint': 'integer',
	\	'uint': 'nonNegativeInteger',
	\	'long': 'integer',
	\	'slong': 'integer',
	\	'ulong': 'nonNegativeInteger'
	\ }

" TODO: May be able to drop NPCBlackbordSerializer and HTNTaskListSerializer
" as they're found in template.json and need no special handling?
" No, looks like if dropped then they're interpreted as array...
" Oh, maybe reloading dedefines the functions but not this script s: variable
" Ok so reloaded and looks like it doesn't work... uses base type not
" customTypeSerializer
" Added PrototypeIdValueDictionarySerializer, hope it works, maybe not
" backwards?
let s:typemap_customTypeSerializer = {
	\ '\V\^FlagSerializer<\(\.\+\)>\$': 'FlagSerializer',
	\ '\V\^ConstantSerializer<\(\.\+\)>\$': 'ConstantSerializer',
	\ '\V\^PrototypeIdSerializer<\(\.\+\)>\$': 'PrototypeIdSerializer',
	\ '\V\^PrototypeIdListSerializer<\(\.\+\)>\$': 'PrototypeIdListSerializer',
	\ '\V\^PrototypeIdArraySerializer<\(\.\+\)>\$': 'PrototypeIdArraySerializer',
	\ '\V\^PrototypeIdHashSetSerializer<\(\.\+\)>\$': 'PrototypeIdHashSetSerializer',
	\ '\V\^PrototypeIdDictionarySerializer<\(\[^,]\{-}\), \?\(\.\*\)>\$': 'PrototypeIdDictionarySerializer',
	\ '\V\^PrototypeIdValueDictionarySerializer<\(\[^,]\{-}\), \?\(\.\*\)>\$': 'PrototypeIdValueDictionarySerializer',
	\ '\V\^AbstractPrototypeIdArraySerializer<\(\.\+\)>\$': 'AbstractPrototypeIdArraySerializer',
	\ }
	"\ '\V\^NPCBlackboardSerializer\$': 'NPCBlackboardSerializer',
	"\ '\V\^HTNTaskListSerializer\$': 'HTNTaskListSerializer',

" Handles PrototypeFlags<IPrototype>, not a customTypeSerializer
let s:typemap_Prototype = {
	\ '\V\^PrototypeFlags<\(\.\+\)>\$': 'PrototypeIdHashSetSerializer',
	\ }

" TODO: Dictionary pattern won't resolve 'Dictionary<(string, string), string>' right
let s:typemap_recurse = {
	\ '\V\^\(\.\*\)\[\(\.\*\)\]\$': 'narray',
	\ '\V\^List<\(\.\*\)>\$': 'array',
	\ '\V\^Queue<\(\.\*\)>\$': 'array',
	\ '\V\^IReadOnlyCollection<\(\.\*\)>\$': 'array',
	\ '\V\^IReadOnlyList<\(\.\*\)>\$': 'array',
	\ '\V\^ImmutableList<\(\.\*\)>\$': 'array',
	\ '\V\^Dictionary<\(\.\{-}\), \?\(\.\*\)>\$': 'dict',
	\ '\V\^SortedDictionary<\(\.\{-}\), \?\(\.\*\)>\$': 'dict',
	\ '\V\^HashSet<\(\.\*\)>\$': 'set',
	\ '\V\^(\(\[^,]\+\%( \[^,]\+\)\?\)\%(, \?\)\(\[^,]\+\%( \[^,]\+\)\?\)\%(, \?\(\[^,]\+\%( \[^,]\+\)\?\)\)\?\%(, \?\(\[^,]\+\%( \[^,]\+\)\?\)\)\?\%(, \?\(\[^,]\+\%( \[^,]\+\)\?\)\)\?)\$': 'tuple',
	\ }

let s:is_nullable = '\V\^\(\.\*\)\(?\)\$'
let s:has_scope = '\V\^\(\.\+\).\(\.\+\)\$'

" This function builds schema definitions in defs as needed, maps each
"  type t to schema type
" Example:
"  Should be recursive, e.g. List<List<string?>>[]
"   call 1 { type: array, items: s:map_type(d, 'List<string?>') }
"   call 2 { type: array, items: s:map_type(d, 'string?') }
"   call 3 { type: nullable-string
"  If not (nullable-)simple type then recurse
" Params:
"  [in] usings: Preprocessed usings list from s:file_to_using and
"   s:path_to_global_using
"  [іn/out] s:tDefinitions: is '$defs' element of schema, an output
"  [in/out] s:tDefs: is '$defs' element of schema, an output
"   $defs contain [DataField]s, definitions contain [DataDefinition]s
"  [in] df.t: the type being resolved, e.g. 'Dictionary<string, int?>?[]'
"   When df only has t key it means there is no ѕcope, it is a lookup
"   only on type, so ideal to have all [DataDefinition]s defined already
"  [in] df.f: the file the type being resolved is in, for usings queries
"  [in] df.s: the scope of the containing lookup
" Returns:
"  Dictionary representing type element in schema { type: ... }
function! s:map_type(usings, df)
	let customTypeSerializerNotice = has_key(a:df, 'a') && has_key(a:df.a, 'c') ?
		\ nr2char(13).'C# customTypeSerializer: '.a:df.a.c : ''
	let description = 'C# def: '.a:df.t.(has_key(a:df, 'n') ? ' '.a:df.n.
		\ (has_key(a:df, 'd') && !empty(a:df.d) ? ' = '.a:df.d : '') : '').
		\ customTypeSerializerNotice

	" Handle s:typemap_Prototype, overlap with s:typemap_customTypeSerializer
	for [k,v] in items(s:typemap_Prototype)
		let m = matchlist(a:df.t, k)
		if !empty(m)
			return s:HandleSerializerTypemap(v, m, description, a:df.f, a:df.s, a:usings)
		endif
	endfor

	" Handle customTypeSerializer first, it overrides df's type
	if has_key(a:df, 'a') && has_key(a:df.a, 'c')
		for [k,v] in items(s:typemap_customTypeSerializer)
			let m = matchlist(a:df.a.c, k)
			if !empty(m)
				return s:HandleSerializerTypemap(v, m, description, a:df.f, a:df.s, a:usings)
			endif
		endfor
		" Assume it is within template.json, e.g. 'NPCBlackboardSerializer', 'HTNTaskListSerializer'
		" Hmmmm, I think that their base should be shim'd as it isn't a
		" [DataDefinition], this Serializer suffix won't map to that shim
		"return s:HandleSerializerTypemap(a:df.a.c, [a:df.a.c, '', ''], description)
	endif

	let nullable = v:false
	let rt = a:df.t
	let rt_scoped = rt
	let m = matchlist(a:df.t, s:is_nullable)
	if len(m) > 0
		let nullable = v:true
		let rt = m[1]
		let rt_scoped = rt
	endif

	for [k,v] in items(s:typemap_recurse)
		let m = matchlist(rt, k)
		if len(m) > 0
			if v ==# 'narray'
				if !empty(m[2])
					let rt = m[1].'['.m[2][:-2].']'
				endif
			elseif v ==# 'dict'
				" hopefully the comma split right...
				"if m[1] !~# '^string$'
					"TODO: In readtags look up validate usings to ensure correct one
				"endif
				let domain = s:map_type(a:usings, {'t':m[1], 'f':a:df.f, 's':a:df.s})
				let range = s:map_type(a:usings, {'t':m[2], 'f':a:df.f, 's':a:df.s})
				" TODO: see meat.yml and soup.yml nested dict in list problems
				"let range.description = description
				let stype = {
					\ 'type': 'object',
					\ 'description': description,
					\ 'additionalProperties': range
					\ }
				if has_key(domain, 'enum') || m[1] !~# '^string$'
					" TODO: While this maps enumRefs for domain, what about range?
					" Better to put logic in s:map_type()?
					let enumName = 'ENUM_'.m[1]
					if !has_key(s:tDefs, enumName)
						let s:tDefs[enumName] = domain
					endif
					let enumRef = {
						\ '$ref': '#/$defs/definitions/'.enumName,
						\ }
					let domain = {
						\ 'allOf': [ enumRef ],
						\ }
					let stype['propertyNames'] = domain
				endif
				return stype
			elseif v ==# 'tuple'
				" I've not seen more than two element types, note names optional
				if empty(m[3])
					"echom 'for m[0]: '.m[0].' see m[1]: '.m[1].' m[2]: '.m[2]
					let t1 = split(m[1])
					let t2 = split(m[2])
					let t1t = {'t':t1[0], 'f':a:df.f, 's':a:df.s}
					if len(t1) > 1
						let t1t.n = t1[1]
					endif
					let t2t = {'t':t2[0], 'f':a:df.f, 's':a:df.s}
					if len(t2) > 1
						let t2t.n = t2[1]
					endif
					"echom 'see t1t: '.string(t1t).' t2t: '.string(t2t)
					let domain = s:map_type(a:usings, t1t)
					let range = s:map_type(a:usings, t2t)
					let range.description = description
					" description necessary below in the case of
					"  List<string, tuple>. Note List<string, tuple> results in a
					" nested additionalProperties, in-place type
					return {
						\ 'type': 'object',
						\ 'description': description,
						\ 'propertyNames': domain,
						\ 'additionalProperties': range,
						\ }
				else
					echom 'unexpected tuple length '.m[0]
				endif
			endif
			let stype = {
				"\ 'type': (nullable?'nullable-':'').'array',
				\ 'items': s:map_type(a:usings, {'t':m[1], 'f':a:df.f, 's':a:df.s}),
				\ 'description': description
				\ }
			if nullable
				let stype['allOf'] = [{ '$ref': '#/definitions/nullable-array' }]
			else
				let stype['type'] = 'array'
			endif
			if v ==# 'set'
				let stype['uniqueItems'] = v:true
			endif
			return stype
		endif
	endfor

	" If scope is at all in the code, it is likely to be just a suffix rather
	" than fully resolved type
	let scopeSuffix = ''
	let m = matchlist(a:df.t, s:has_scope)
	if len(m) > 0
		let scopeSuffix = escape(m[1], '.').'$'
		let rt_noscope = m[2]
	else
		let rt_noscope = rt
	endif
	unlet rt

	if has_key(s:typemap_simple, rt_noscope)
		"return { 'type': (nullable?'nullable-':'').s:typemap_simple[rt] }
		let tt = s:typemap_simple[rt_noscope]
		if !empty(scopeSuffix)
			echom 'unhandled scope '.rt_scoped.' for type '.tt
		endif
		if tt ==# 'nonNegativeInteger'
			" TODO: Allow nullable, maybe nullable-nonNegativeInteger entry?
			return {
				\	"$ref": "#/definitions/nonNegativeInteger",
				\ "description": description
				\ }
		endif
		if nullable
			return {
				\ 'allOf': [{ '$ref': '#/definitions/nullable-'.s:typemap_simple[rt_noscope] }],
				\ "description": description
				\ }
		else
			return {
				\ 'type': s:typemap_simple[rt_noscope],
				\ 'description': description
				\ }
		endif
	endif

	" When something isn't easily deduced via algorithm on ctags data,
	"  it is preferrable to use a shim in the template.json file,
	"  These are those types
	if has_key(s:tDefinitions, rt_noscope)
		if !empty(scopeSuffix)
			" TODO: validate against scopeSuffix
			echom 'unhandled scope '.rt_scoped.' for type '.rt_noscope
		endif
		return {
			\ '$ref': '#/definitions/'.rt_noscope,
			\ 'description': description
			\ }
	" elseif has_key(s:tDefs, rt_noscope)
		" " TODO: This branch only happens for SoundSpecifier variants... I think?
		" " Otherwise customTypeSerializers, but those are elsewhere...
		" if !empty(scopeSuffix)
			" " TODO: validate against scopeSuffix
			" echom 'unhandled scope '.rt_scoped.' for type '.rt_noscope
		" endif
		" return {
			" \ '$ref': '#/$defs/definitions/'.rt_noscope,
			" \ 'description': description
			" \ }
	endif

	" This should replace most everything below which scans E|I|G,
	"  and should replace those peeks into #/$defs/defininions and #/definitions
	" Try with scopeSuffix on top of the usings first
	let scope_name = ['','']
	let found = s:lookupToken(a:usings, rt_scoped, scope_name)
	if found
		let qname = join(scope_name, '.')
		if has_key(s:scope_name_to_defs, qname)
			let schema_name = s:scope_name_to_defs[qname]
		else
			" TODO: Not in template.json, Call BuildSerializable before
			" BuildExplicitDataDefinition and BuildImplicitDataDefinition?
			" Robust.Shared.Maths.Angle, Robust.Shared.Map.EntityCoordinates,
			" Robust.Shared.Utility.SpriteSpecifier
			" Add to s:unresolved and return a ref hoping that it will later be
			"  resolved? If not resolved, then add an empty object stub?
			" Could instead of BuildExplicit|Implicit iterate over datafieds?
			"
			" 4 unresolveds in SS14 now:
			"  RSI and MapChunk are classes without DataDefinition attributes or datafields
			"  DoAfterId and MagnetState are record structs, only the latter has
			"  [DataRecord], neither of which are used in SS14 yaml. Actually all 4
			"  not used in SS14 yaml as far as I can tell.
			call add(s:unresolved_on_first_pass, s:scope_name_to_dd[scope_name[0]][scope_name[1]])
			let schema_name = s:GetSuitableSchemaName(scope_name[0], scope_name[1])
		endif
		let ret = { '$ref': '#/$defs/definitions/'.schema_name }
		if has_key(a:df, 'd') && !empty(a:df.d) && a:df.d !=# 'new()'
			let ret['description'] = description
		endif
		return ret
	endif

	" Search ddmap to see if we have it in definitions or defs already
	"  Can't use s:resolvename yet as we don't know if it's an i/e/enum
	" Though I suppose the below checks for explicit/implicit/enum
	"  could be refactored into s:resolvename
	" TODO: We don't have partials associated with t at this point,
	"  Thus the subsequent lookups, and how can we know the partials
	"  without scope??? maybe df has scope? I should re-add scope to dfs
	" TODO: Now with C prefix to differentiate Components from Prototypes
	"  This crude lookup is uninformed? Why is
	"  SurveillanceCameraVisualComponent not found is this lookup???
	"   What refers to SurveillanceCameraVisualComponent?
	" Turns out SurveillanceCameraVisuals and MachinePart are Enums,
	"  and lookup of component with same name+'Component' is WRONG
	"  We're looking up C# types, not yaml types,
	"   so don't append Component or look up yaml types in defs/definitions
	" ALL OF THE BELOW QUOTED IS WRONG, BAD OPTIMIZATION
	" Can cache/map C# type to yaml defs/defininitions instead to speed
	"  up schema gen
	"let qname = a:dd[0].s.'.'.a:dd[0].n
	"if index(values(s:ddmap), t) != -1
	"	if has_key(s:tDefinitions, 'C'.t)
	"		echom 'found '.t.'Component in definitions'
	"		return {
	"			\ '$ref': '#/definitions/C'.t,
	"			\ 'description': description
	"			\ }
	"	elseif has_key(s:tDefinitions, t)
	"		echom 'found '.t.' in definitions'
	"		return {
	"			\ '$ref': '#/definitions/'.t,
	"			\ 'description': description
	"			\ }
	"	elseif has_key(s:tDefs, t)
	"		"echom 'found '.t.' in $defs'
	"		" TODO: Is there disadvantage to the lookup here?!?
	"		return {
	"			\ '$ref': '#/$defs/definitions/'.t,
	"			\ 'description': description
	"			\ }
	"	else
	"		echom 'anomaly, did not find '.t.' in template'
	"	endif
	"endif

	" Search s:d.i s:d.e for [DataDefinition] of name t
	" NOTE: At present doesn't parse for usings within the prototype,
	"  No [DataDefinitions] have same name I hope?
	" for e in s:d.e
		" if e[0].n ==# rt_noscope && e[0].s =~# scopeSuffix
			" let n = s:resolvename(e, '!type:')
			" return {
				" \ '$ref': '#/$defs/definitions/'.n,
				" \ 'description': description
				" \ }
		" endif
	" endfor

	" Might be best to do a map where the v:true sets the side effect:
	"{ '$ref': '#/$defs/definitions/'.s:resolvename(k, 'I') }
	"let S = {i -> filter(copy(i), {_,k -> k[0].n ==# rt ? v:true : (has_key(k[0], 'c') ? S(k[0].c) : v:false)})}
	"let g:r = S(s:d.i)
	"if g:r != v:false
	"	return g:r
	"endif

	" This filter results in branch of s:d.i where match is leaf node (in child list or first element), could augment the v:true branch to have side effect:
	"let M = {i -> filter(copy(i), {_,j-> j[0].n ==# rt ? v:true : (has_key(j[0], 'c') ? len(g:M(j[0].c))>0 : v:false)})}
	"let g:r = M(s:d.i)
	"if !empty(g:r)
		" Is on branch in r
	"endif

	" More efficient than the filter may be to do a for loop to avoid
	"  extra processing, yet isn't parallel. Maybe map is best.
	" let r = s:Scan(s:d.i, rt_noscope, scopeSuffix)
	" if type(r) == v:t_dict
		" return r
	" endif

	" Now, we still don't know the type, try an enum:
	" TODO: Refactor enum representation in schema, all should be prefixed with
	" !type:
	"for g in s:d.G
	"	if g.n ==# rt && g.s =~# scopeSuffix
	"		return {
	"			\ 'type': 'string',
	"			\ 'enum': reduce(g.e, {l,i -> add(l, i.n)}, []),
	"			\ 'description': '[Enum]'.description,
	"			\ }
	"	endif
	"endfor
	" Iterate over all I|E|G or just iterate over all usings with a ddmap
	" lookup? How to query the usings from rt?
	" Moved to top of function...

	" Might be a Prototype ID, e.g. LatheRecipePrototype
	" for p in s:d.p
		" if p[0].n ==# rt_noscope && p[0].s =~# scopeSuffix
			" " Name is e.g. '#/$defs/definitions/ENUM_'.LatheRecipePrototype
			" return s:HandleSerializerTypemap('PrototypeIdSerializer',
				" \ [rt_noscope, rt_noscope, ''], description, a:df.f, a:df.s)
		" endif
	" endfor

	" At this point it appears to be a [Serializable] class or struct,
	"  With no [DataField] members.
	" e.g. CargoOrderData, EntityCoordinates, BoundKeyFunction,
	"  MapId, GridId, other shims
	"  Must parse constructor parameters I think
	" Or it is a record, e.g. DecalGridChunkCollection, MagnetState
	" IReadOnlyList appears to be list of entity IDs?
	" SpriteSpecifier is an [Serializable] abstract class
	" customTypeSerializer may shed light on type?
	" TODO: Almost ready to get rid of this?
	"  Would have to pre-parse types as graph or enque re-runs of unknowns on a
	"   stack somehow, all in order to know what is unknown-
	"  Or could relax that constraint and just throw in !type:, but then
	"   schema will complain on load of only first missing type.
	" TODO: Need to handle records, and interface+hierarchy, and that will get
	"  rid of most remaining unknowns. Also relax above constraint for most all.
	"  What will be left over should go into the template.json file, e.g.
	"  !type:object
	"return {
	"	\ 'type': 'object',
	"	\ 'description': 'unknown-'.description
	"	\ }
	"	Guess the scope, unresolved, maybe it'll lookup successfully?
	"	KLUDGE: A hidden problem with this appears to be with not resolving scope,
	"	 may be a problem with name collision
	"	I think this name collision may be improved by a datastructure
	"	 like ddmap, rather than scope.name->yamlname, do
	"	 name->[dd1, dd2, ...] where other dds are for name conflicts
	"	 each will have their own scope, e.g. dd1[0].s
	"	TODO: I do wonder if hierarchy should be made first, and e,p,i
	"	 point into that hierarchy, rather than stitching them together
	"	 with interfaces. Should make get_Datafields faster, by replacing s:Z3
	"	 with dictionary[scope] lookup rather than iterating over e,p,i (and i.c)
	"	 scope->[dd1,dd2,...] should be made with hierarchy
	"	 Will speed up get_Explicit/Implicit/Prototypes too by dict lookup
	"	  rather than nested map on filtertag per name+scope
	"

	" At this point:
	"  1) not in s:tDefinitions
	"  2) s:lookupToken failed to find a the token in scope_name_to_dd
	" I don't think with improved algorithm that we'll ever get here?
	" Just toss what we know of it on s:scope_name_to_defs
	"if !has_key(s:scope_name_to_defs, a:df.s . '.' . rt_noscope)
	let n = s:GetSuitableSchemaName(a:df.s, rt_noscope)
	let ret = {
		\ '$ref': '#/$defs/definitions/'.n,
		\ }
	if has_key(a:df, 'd') && !empty(a:df.d) && a:df.d !=# 'new()'
		let ret['description'] = description
	endif
	return ret
endfunction

" Defines serializer types in $defs with supporting ENUM_ if necessary
" params:
"  [in] v: type category, see values of e.g. s:typemap_customTypeSerializer
"  [in] m: matchlist of type on pattern k in caller
"  [in] description: description of schema type
"  [in] f: file which type is in, for usings lookups
"  [in] scope: scope of type
"  [in] usings: usings for file and scope
" returns: schema type derrived from v and m
function s:HandleSerializerTypemap(v, m, description, f, scope, usings)
	let defstr = a:v.
		\ (!empty(a:m[1]) ? '_'.a:m[1] : '').
		\ (!empty(a:m[2]) ? '_'.a:m[2] : '')
	let ret = {
		\ '$ref': '#/$defs/definitions/'.defstr,
		\ 'description': a:description,
		\ }
	" Need to define defstr in s:tDefs if not there
	if has_key(s:tDefinitions, defstr) || has_key(s:tDefs, defstr)
		"echom 'found customTypeSerializer(...) '.defstr.' in $defs'
	"elseif a:v ==# 'NPCBlackboardSerializer' || 'HTNTaskListSerializer'
		" These are not parametric so are in template.json
		" TaskList appears to be either a mapping or string or array
	elseif a:v ==# 'FlagSerializer' || a:v ==# 'ConstantSerializer'
		" If len(G) > 1 we merge enums together,
		"  possible if > 1 Content assembly
		let enum = []
		let enumNames = []
		let Gsets = {
			\ 'FlagSerializer': s:d.FF,
			\ 'ConstantSerializer': s:d.CF,
			\ }

		for G in Gsets[a:v][a:m[1]]
			call add(enumNames, G.n)
			call extend(enum, reduce(G.e, {l,i -> add(l, i.n)}, []))
		endfor
		let ctdesc = 'C# Union('.join(enumNames, ',').')={'.join(enum, ',').'}'
		let enumName = 'ENUM_'.a:m[1]
		let s:tDefs[enumName] = {
			\ 'enum': enum,
			\ 'type': 'string',
			\ }
		let enumRef = {
			\ '$ref': '#/$defs/definitions/'.enumName,
			\ }
		if a:v ==# 'FlagSerializer'
			let s:tDefs[defstr] = {
				\ 'type': 'array',
				\ 'uniqueItems': v:true,
				\ 'items': {
					\ 'allOf': [ enumRef ],
					\ },
				\ }
		elseif a:v ==# 'ConstantSerializer'
			let s:tDefs[defstr] = {
				\ 'allOf': [ enumRef ],
				\ }
		endif
	elseif !empty(a:m[1])
		let lookup = a:m[1]
		if a:v ==# 'PrototypeIdDictionarySerializer' || a:v ==# 'PrototypeIdValueDictionarySerializer'
			let lookup = a:m[2]
		endif
		" TODO: Oh... d.yids is yaml type but we need to convert
		"  those from C# types, e.g. EntityPrototype->entity
		" Find lookup in s:d.yids
		"let match = filter(copy(s:d.p), {_,j -> j[0].n ==# lookup})
		" if len(match) == 1
			" for partial in match[0]
				" if has_key(partial, 'yn')
					" let lookup = partial.yn
					" break
				" endif
			" endfor
		" else
			" echom 'Failed to find C# prototype specified in '.a:m[0].' defstr:'.defstr
		" endif
		let scope_name = ['', '']
		let found = s:lookupToken(a:usings, lookup, scope_name)
		if found
			let dd = s:scope_name_to_Prototype[scope_name[0]][scope_name[1]]
			for partial in dd
				if has_key(partial, 'yn')
					let lookup = partial.yn
					break
				endif
			endfor
		else
			echom 'Failed to find C# prototype specified in '.a:m[0].' defstr:'.defstr
		endif
		if has_key(s:d.yids, lookup)
			let enum = s:d.yids[lookup]
		else
			echom 'lookup failure in yids for '.lookup
			let enum = []
		endif
		let enumName = 'ENUM_'.lookup
		let s:tDefs[enumName] = {
			\ 'description': 'YAML prototype id of type: '.lookup,
			\ 'enum': enum,
			\ }
		let enumRef = {
			\ '$ref': '#/$defs/definitions/'.enumName,
			\ }
		" Description should come from parent for hover
		let stype = {}
		if a:v ==# 'PrototypeIdSerializer'
			let stype['allOf'] = [ enumRef ]
		elseif a:v ==# 'PrototypeIdListSerializer' || a:v ==# 'PrototypeIdArraySerializer'
			let stype['type'] = 'array'
			let stype['items'] = {
				\ 'allOf': [ enumRef ],
				\ }
		elseif a:v ==# 'PrototypeIdHashSetSerializer'
			let stype['type'] = 'array'
			let stype['items'] = {
				\ 'allOf': [ enumRef ],
				\ }
			let stype['uniqueItems'] = v:true
		elseif a:v ==# 'AbstractPrototypeIdArraySerializer'
			" TODO: Dunno what this 'Abstract' means yet
			let stype['oneOf'] = [
				\ {
					\ 'allOf': [ enumRef ],
				\ }, {
					\ 'type': 'array',
					\ 'items': {
						\ 'allOf': [ enumRef ],
						\ },
					\ },
				\ ]
		elseif a:v ==# 'PrototypeIdDictionarySerializer' || a:v ==# 'PrototypeIdValueDictionarySerializer'
			let stype['type'] = 'object'
			" IDK why, but the C# type is backwards from YAML
			" e.g. Dictionary<FixedType2, FooBarPrototype> maps in YAML to
			" FooBarPrototypeID: FixedType2
			let domain = {
				\ 'allOf': [ enumRef ],
				\ }
			let stype['propertyNames'] = domain
			if a:m[1] !~# '^string$'
				" Need to get the type for a:m[1]
				"  Assuming it's in C# and not yaml ids?
				let range = s:map_type(a:usings, {'t':a:m[1], 'f': a:f, 's': a:scope})
				" TODO: with domain/range swap idk if this condition valid
				"if has_key(range, 'enum') || a:m[1] !~# '^string$'
				"	let stype['additionalProperties'] = range
				"endif
				" TODO: with enum refactor the above was commented, as no enum key
				let stype['additionalProperties'] = range
			endif
		else
			echom 'anomaly in customTypeSerializer/PrototypeID handling, '.
				\ 'unhandled case: '.a:v
		endif
		let s:tDefs[defstr] = stype
	endif
	return ret
endfunction

" params:
function! s:OBSOLUTE_Scan(l, t, scopeSuffix)
	" For each partial list in list a:l
	for i in a:l
		if i[0].n ==# a:t && i[0].s =~# a:scopeSuffix
			return { '$ref': '#/$defs/definitions/'.s:resolvename(i, '!type:') }
		endif
		if has_key(i[0], 'c')
			" Bug is here, c is a list of lists: FIXED, NOPE NOT FIXED
			"for c in i[0].c
				let r = s:Scan(i[0].c, a:t, a:scopeSuffix)
				if type(r) == v:t_dict
					return r
				endif
			"endfor
		elseif len(i) > 1
			if has_key(i[1], 'c')
				echom 'anomaly in s:Scan'
			endif
		endif
	endfor
	return v:false
endfunction

" Unlike s:Scan, search doesn't have to deal with partial lists, I think it
" should though??? Still need to hook Interface hierarchy up to rest somehow
" for the purpose of resolution
" params:
function! s:OBSOLETE_ScanInterfaces(l, t, scopeSuffix)
	for i in a:l
		if i.n ==# a:t && i.s =~# a:scopeSuffix
			return { '$ref': '#/$defs/definitions/'.s:resolvename(i, '!type:') }
		endif
		" Bug is here, c is a list of lists: FIXED, NOPE NOT FIXED
		"for c in i[0].c
			let r = s:ScanInterfaces(i.c, a:t, a:scopeSuffix)
			if type(r) == v:t_dict
				return r
			endif
		"endfor
	endfor
	return v:false
endfunction

" Builds the definition in the schema and resolves for name conflicts
"  Maps a C# qualified type (with scope) to YAML definition (schema) name (not yaml name)
" Example:
"  { '$ref': '#/$defs/definitions/'.s:resolvename(i, '!type:') }
" Params:
"  [in] dd: A list of partials
"  [in] prefix: e.g. 'E', 'P', 'I' for better disambiguation
" Returns:
"  name of dd in ddmap, e.g.:
"  'Content.Client.Actions.Assignments.ActionAssignments' -> 'ActionAssignments'
function! s:OBSOLETE_resolvename(dd, prefix)
	let name = a:dd[0].n
	let qname = a:dd[0].s . '.' . name
	"let tryname = a:prefix . name
	if has_key(s:scope_name_to_defs, qname)
		return s:scope_name_to_defs[qname]
	else
		"" Just use a number to resolve name conflicts
		" let i = 0
		" while has_key(s:tDefs, tryname)
			" let tryname = a:prefix.name.string(i)
			" let i = i + 1
		" endwhile
		" let s:ddmap[qname] = tryname
		" call s:builddef(a:dd, tryname)
		" return tryname

		" Not in ddmap yet, stick on an undefined list for consideration later
		call add(s:unresolved_on_first_pass, a:dd)
		return a:prefix . name
	endif
endfunction

" Makes the schema definition in $defs
" params:
"  [in] name: yaml schema name of the DataDefinition
"   e.g. ESeedData (ExplicitDD), ISoundSpecifier (ImplicitDD)
" TODO: If is in hierarchy, needs parent properties inherited {}, e.g.
" see instantAction in mech.yml, the IInstantAction built by BuildDef needs
" id: {}, type:{enum:[instantAction]}
" But, then problem arries that IInstantAction only works in single case?!?
" Oh, silly me, I can do type:{}!!!
" NOTE: This function is obsolete, all defs are in yaml so nothing to be built
"  on-the-fly
function! s:builddef_OBSOLETE(dd, name)
	let sNode = {
		\	'type': 'object',
		\ 'description': '[BuildDef]C# type: '.a:dd[0].n.nr2char(13).'C# scope: '.a:dd[0].k.'.'.a:dd[0].s,
		\	'additionalProperties': v:false,
		\	'properties': {}
		\ }
	"for partial in a:dd
	"	for df in partial.df
	"		let dfn = s:ParseDataFieldName(partial, df)
	"		let def.properties[dfn] = s:map_type(df)
	"	endfor
	"endfor
	let s:tDefs[a:name] = sNode
	call s:ParseDataFields(sNode, a:dd, v:true)
	" TODO: abstract class enums
	"let n = a:dd
	"while has_key(n[0], 'p')
		"let n = n[0].p
		"" Put parent dfs onto child nodes for now
		"call s:ParseDataFields(sNode, n)
		""for partial in n
		""	for df in partial.df
		""		let dfn = s:ParseDataFieldName(partial, df)
		""		" Why is this printed on components? Oh... non-components
		""		" TODO: After putting implicit/explicit datadefinition in defs, uncomment:
		""		"echom 'found child df ('.dfn.') in parent ('.partial.n.')'
		""		let def.properties[dfn] = s:map_type(df)
		""	endfor
		""endfor
	"endwhile
endfunction

" Returns datafield name
function! s:ParseDataFieldName(partial, df)
	if has_key(a:df, 'nn')
		return a:df.nn
	elseif has_key(a:df, 'sym')
		echom 'failure to resolve df sym earlier'
		return a:df.sym
	else
		" I guess it empty
		return a:df.t
	endif
	" Old stuff:
	" let n = a:df.nn
	" let m = matchlist(n, '\V\^"\(\.\+\)"\$')
	" if !empty(m)
		" let s = m[1]
	" else
		" " We have no e.g. [DataField("name")], but [DataField(Node)]
		" "  It is a const member of the container, in same partial
		" for m in a:partial.m
			" if m.n ==# n
				" echom 'Found DataField member ' . s . ' with value ' . m.d
				" let s = m.d
				" break
			" endif
		" endfor
	" endif
	" return s
endfunction

function! robusttoolbox#parseCS(recache) abort
	let s:tf = b:gutentags_files.ctags
	let tI = reltime()
	echom '[robusttoolbox] Parsing usings'
	call s:get_Usings(a:recache)
	echom '[robusttoolbox] Parsing interface hierarchy'
	let h = s:get_Hierarchy()
	echom '[robusttoolbox] Parsing [Serializable]s'
	call s:get_Serializable()
	echom '[robusttoolbox] Parsing [Prototype]s'
	call s:get_Prototypes()
	echom '[robusttoolbox] Parsing explicit [DataDefinition]s'
	call s:get_ExplicitDataDefinitions()
	echom '[robusttoolbox] Parsing implicit [DataDefinition]s'
	call s:get_ImplicitDataDefinitions()
	"echom '[robusttoolbox] Parsing [Serializable] ctors'
	"call s:post_Serializable()
	echom '[robusttoolbox] ['.reltimestr(reltime(tI))[:-5].'s] Parsing [Prototype]s and implicit/explicit [DataDefinition]s completed'
	" 95.45s with mtables and attribute tags, 7.6x slower than mlines
	"call s:tests({'i':i, 'e':e, 'p':p})
	call s:get_DataFields()
	echom '[robusttoolbox] ['.reltimestr(reltime(tI))[:-5].'s] Parsing+Processing [DataField]s completed'
	call s:get_AbstractDataFields()
	call s:get_ParentDataFields()
	echom '[robusttoolbox] ['.reltimestr(reltime(tI))[:-5].'s] Parsing+Processing [AbstractDataField][ParentDataField]s completed'
	call s:get_Enums()
	echom '[robusttoolbox] ['.reltimestr(reltime(tI)).'s] Parsing enums completed'
	let colors = s:get_Colors()
	"May be faster knowing name rather than querying for all kind:G first
	" Then kind:E
	let FF = s:get_FlagsFor()
	let CF = s:get_ConstantsFor()
	echom '[robusttoolbox] ['.reltimestr(reltime(tI)).'s] Parsing [FlagsFor]&[ConstantsFor] completed'
	"return {'U': U, 'h': h, 's': s, 'i':i, 'e':e, 'p':p, 'G':G, 'colors':colors, 'FF':FF, 'CF':CF}
	return #{h: h, colors: colors, FF: FF, CF: CF}
endfunction

" Parse YAML prototype ids
function! robusttoolbox#parseYAML() abort
	let tI = reltime()
	let yids = s:get_YAMLids()
	echom '[robusttoolbox] Parsing YAML prototype ids completed in +'.reltimestr(reltime(tI)).' seconds'
	return {'yids': yids}
endfunction

function! robusttoolbox#filtertags(f) abort
	let ret = systemlist('readtags -t '.s:tf." -eE -Q '".a:f."' -l")
	if v:shell_error
		echohl ErrorMsg
		echom '[robusttoolbox] readtags error: '.v:shell_error.' filter:'''.a:f.''''
		echohl None
	endif
	return ret
endfunction

function! s:get_YAMLids()
	" Step 1: Query id tags
	let t = s:splitup_id(robusttoolbox#filtertags('(and $kind (eq? $kind "D")))'))
	" Step 2: Map into {type: [id, ...]} container
	" TODO: See if sort quicker:
	"  sort(t, {i,j -> ((i.t < j.t) == 0) ? -1 : 1})
	let d = {}
	while len(t) > 0
		let m = t[0].t
		let d[m] = []
		call extend(d[m], reduce(filter(copy(t), {_,i -> i.t ==# m}),
			\ {l,i -> add(l, i.n)}, []))
		call filter(t, {_,i -> i.t !=# m})
	endwhile
	return d
endfunction

" Gets all [ImplicitDataDefinitionForInheritors] class tags in a list of lists of dictionaries
"  Each sublist element representing a class tag
"  Each sublist a set of class tags for a class (i.e. partial)
"  Top level list is [ImplicitDataDefinitionForInheritors] list
"  Any child classes will be in 'c' element of first class tag in set
"  Children must be navigated recursively
"  [{n:name s:scope i:'inherits list' h:'sealed|abstract' c:{...}, {...}...}]
"  'h' and 'c' key won't exist on structures
"  'c' key won't exist when no children
"  'h' key empty when not abstract or sealed
function! robusttoolbox#get_ImplicitDataDefinitions()
	return s:get_ImplicitDataDefinitions()
endfunction
function! s:get_ImplicitDataDefinitions()
	" Step 1: Query attribute tags
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )ImplicitDataDefinitionForInheritors(,|\])/ $name) $kind (eq? $kind "A")))'))
	" Step 2: for each attribute tag: query the class/interface tags
	" Gets scope-name from attribute of class to match class scope
	"let roots = map(map(roots, {_,j -> robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") $kind (#/(C|I)/ $kind) (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')}), {_,j -> s:splitup_class_struct(j)})
	let s:name_to_implicit = {}
	let s:scope_name_to_implicit = {}
	for aa in a
		let scope = matchlist(aa.s, '\V\(\.\*\).\.\*\$')[1]
		" Add [ImplicitDataDefinitionForInheritors] to each or just the first?
		call add(s:scope_name_to_dd[scope][aa.t][0].a, aa)
		" Will be faster to query attributes by has_key('ap') rather than searching [a]
		" with regex on each element
		let s:scope_name_to_dd[scope][aa.t][0].ai = aa
		let name = s:scope_name_to_dd[scope][aa.t][0].n
		if !has_key(s:name_to_implicit, name)
			let s:name_to_implicit[name] = s:scope_name_to_dd[scope][aa.t]
		else
			echom 'Duplicate [ImplicitDataDefinitionForInheritors]: '.name
			call extend(s:name_to_implicit[name], s:scope_name_to_dd[scope][aa.t])
		endif
		if !has_key(s:scope_name_to_implicit, scope)
			let s:scope_name_to_implicit[scope] = {}
		endif
		let s:scope_name_to_implicit[scope][name] = s:scope_name_to_dd[scope][aa.t]
	endfor

	" Step 3: for each class get any child classes recursively
	"  'c' element only on first element of list of class tag dictionaries
	"call map(roots, {_,j -> map(j, {k,l -> k != 0 ? l : (has_key(l, 'h') && l.h ==# 'sealed' ? l : extend(l, {'c': s:get_Children(l.n, j)}))})})

	" Step 4: Query ComponentProtoName attributes
	let compProtoName = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )ComponentProtoName\(/ $name) $kind (eq? $kind "A"))'))

	" Step 5: Find component, search its children for each ComponontProtoName
	"let component = filter(copy(roots), {i,j -> j[0].n ==# 'Component'})[0]
	"let component = s:name_to_implicit['Component'][0]

	" Step 6: Assign each [ComponentProtoName] attribute to their component
	"  NOTE: Since (Content|Robust).Client are filtered out, doesn't see
	"   ClientEntitySpawner/ClientEntitySpawnerComponent
	" let CheckScope = {j,a -> map(copy(j), {_,m -> m.s.'.'.m.n ==# a.s ? add(m.a, a.n) : 0})}
	" let ScanComponentChildren = {j,a -> empty(has_key(j[0], 'c') ? map(copy(j[0].c), {k,l -> ScanComponentChildren(l, a)}): 0) ? CheckScope(j, a) : CheckScope(j, a)}
	" for a in compProtoName
		" "echom 'ComponentProtoName: '.a.t
		" call ScanComponentChildren(component, a)
	" endfor
	" Map [ComponentProtoName(name)] -> [dd], for yaml schema later
	let s:name_to_ComponentProtoName = {}
	for aa in compProtoName
		let scope = matchlist(aa.s, '\V\(\.\*\).\.\*\$')[1]
		let yn = matchlist(aa.n, '\V\.\{-}\%([\|,\| \)ComponentProtoName("\(\[^"]\+\)"')[1]
		" Add [ComponentProtoName] to each or just the first?
		call add(s:scope_name_to_dd[scope][aa.t][0].a, aa)
		" Will be faster to query attributes by has_key('acpn') rather than searching [a]
		" with regex on each element
		let s:scope_name_to_dd[scope][aa.t][0].acpn = aa
		let s:scope_name_to_dd[scope][aa.t][0].yn = yn
		let s:name_to_ComponentProtoName[yn] = s:scope_name_to_dd[scope][aa.t]
	endfor

	" Step 7: Query RegisterComponent attributes
	let registerComponent = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )RegisterComponent(,|\])/ $name) $kind (eq? $kind "A"))'))
	" Step 8: Assign each [RegisterComponent] attribute to their component
	" for a in registerComponent
		" call ScanComponentChildren(component, a)
	" endfor
	" Map name -> [dd], for yaml schema later
	let s:name_to_RegisterComponent = {}
	for aa in registerComponent
		let scope = matchlist(aa.s, '\V\(\.\*\).\.\*\$')[1]
		" Add [ComponentProtoName] to each or just the first?
		call add(s:scope_name_to_dd[scope][aa.t][0].a, aa)
		" Will be faster to query attributes by has_key('arc') rather than searching [a]
		" with regex on each element
		let s:scope_name_to_dd[scope][aa.t][0].arc = aa
		let s:name_to_RegisterComponent[s:scope_name_to_dd[scope][aa.t][0].n] = s:scope_name_to_dd[scope][aa.t]
	endfor
	" return roots
endfunction

" Gets all [DataDefinition] classes in a list, no children
function! robusttoolbox#get_ExplicitDataDefinitions()
	return s:get_ExplicitDataDefinitions()
endfunction
function! s:get_ExplicitDataDefinitions()
	" Step 1: Query attribute tags
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )DataDefinition(,|\])/ $name) $kind (eq? $kind "A"))'))

	" TODO: Looks like subset of E is failing to parse now, see:
	" echo map(copy(g:d.e), {i,j->len(j)})
	" Ok, looks like this is parsing as a property not a struct:
	"  ['AlarmThresholdSetting
	"   /home/browser/dev/space-station-14/Content.Shared/Atmos/Monitor/AtmosAlarmThreshold.cs 243;"   kind:P
	"   scope:class:Content.Shared.Atmos.Monitor.AtmosAlarmThreshold    type:struct     df:']
	" Ok, there were multiple bugs, the above, enums, and also there is only one
	" group in this: matchlist(j.s, '\V\(\.\*\).\.\*\$')[2], had to change to [1], weird how it worked much of the time
	" Step 2: for each attribute tag get the relevant class/struct tags
	"let roots = map(map(roots, {_,j -> robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") $kind (eq? $kind "'.(j.k==#'class'?'C':(j.k==#'struct'?'S':'Undefined')).'") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')}), {_,j -> s:splitup_class_struct(j)})
	let s:scope_name_to_explicit = {}
	for aa in a
		let scope = matchlist(aa.s, '\V\(\.\*\).\.\*\$')[1]
		" Add [DataDefinition] to each or just the first?
		call add(s:scope_name_to_dd[scope][aa.t][0].a, aa)
		" Will be faster to query attributes by has_key('ae') rather than searching [a]
		" with regex on each element
		let s:scope_name_to_dd[scope][aa.t][0].ae = aa
		let name = s:scope_name_to_dd[scope][aa.t][0].n
		" Filter out prototypes which also have explicit [DataDefinition],
		" since datafields are put on the prototypes in schema the explicit
		" datadefinition will have no datafields in schema
		" See [Prototype ("alertOrder")][DataDefinition] class AlertOrderPrototype,
		" about 10 of those redundancies in SS14
		if has_key(s:scope_name_to_dd[scope][aa.t][0], 'ap')
			continue
		endif
		if !has_key(s:scope_name_to_explicit, scope)
			let s:scope_name_to_explicit[scope] = {}
		endif
		let s:scope_name_to_explicit[scope][name] = s:scope_name_to_dd[scope][aa.t]
	endfor

	" I could split Step 2 above into two queries: one for each kind!:
	"return s:splitup_class(robusttoolbox#filtertags('(and $kind (eq? $kind "C") (#/\[(.+, )?DataDefinition(,|\])/ ($"at")))')) +
	"\ s:splitup_struct(robusttoolbox#filtertags('(and $kind (eq? $kind "S") (#/\[(.+, )?DataDefinition(,|\])/ ($"at")))'))

	"let roots = filter(roots, {_,j -> empty(filter(copy(a:p), {_,k -> k[0].s ==# j[0].s && k[0].n ==# j[0].n}))})
	"return roots
endfunction

" Gets all usings, by file
"  [in] recache: if true will reparse usings
"  [out] s:file_to_using: file->[usings] | usings = {n: name, f: file, t:type}
function! robusttoolbox#get_Usings(recache)
	return s:get_Usings(a:recache)
endfunction
function! s:get_Usings(recache)
	if exists('s:file_to_using') && !a:recache
		return
	endif
	let tI = reltime()
	let u = s:splitup_using(robusttoolbox#filtertags('(and $kind (eq? $kind "U"))'))
	" Tried this as possible speedup, only 0.03s faster, 16.12s->16.09s
	" let u = sort(u, {l,r->l<#r?-1:(l>#r?1:0)})
	" let indicies = [0]
	" for i in range(len(u)-1)
		" if u[i] !=# u[i+1]
			" call add(indicies, i)
		" endif
	" endfor
	" call add(indicies, len(u)-1)
	" for i in range(len(indicies)-1)
		" let ret[u[indicies[i]].f] = u[indicies[i]:indicies[i+1]]
	" endfor
	" while !empty(u)
		" let f = u[0].f
		" let ret[f] = filter(copy(u), {_,j -> j.f ==# f})
		" call filter(u, {_,j -> j.f !=# f})
	" endwhile
	let s:file_to_using = {}
	let s:path_to_global_using = {}
	for uu in u
		let suffix = matchlist(uu.n, '\V\^static \(\.\*\)\$')
		if !empty(suffix)
			let uu['nn'] = suffix[1]
		endif
		if !has_key(uu, 'g')
			if !has_key(s:file_to_using, uu.f)
				let s:file_to_using[uu.f] = []
			endif
			call add(s:file_to_using[uu.f], uu)
		else
			" It's a global using, store the path not the file for later regex
			let p = '\V\^'.fnamemodify(uu.f, ':h')
			if !has_key(s:path_to_global_using, p)
				let s:path_to_global_using[p] = []
			endif
			call add(s:path_to_global_using[p], uu)
		endif
	endfor
	echom '[robusttoolbox] ['.reltimestr(reltime(tI))[:-5].'s] parsing usings completed'
endfunction

" Appends scopes of parents to usings
function! s:getUsingsFromFileScopeParents(f, s, pl)
	let usings = s:getUsingsFromFileScope(a:f, a:s)
	if !empty(a:pl)
		let g:pl = a:pl
		let g:usings = copy(usings)
	endif
	let parent_usings = []
	for parent in values(a:pl)
		"let uu = s:lookupToken(usings, parent.n)
		call add(parent_usings, {'n': parent[0].s . '.' . parent[0].n})
	endfor
	return add(usings, parent_usings)
endfunction

" Params:
"  [in] f: file
"  [in] s: scope (e.g. a.b.c -> [a.b.c, a.b, a])
function! s:getUsingsFromFileScope(f, s)
	" Get global usings first
	" TODO: Something with these usings filters added 20s to processing time
	"  Should instead just for loop over the keys, if match, process and break
	"  from loop
	" Wierd, went from 30s->217s with above alg, and 0->3 ignores.... weird
	"  Have all components now apparently, instead of ~20, maybe that is
	"  slowdown?
	let using_global = filter(copy(s:path_to_global_using),
		\ {k,_ -> 0==match(a:f, k)})
	if len(using_global) > 0
		let using_global = values(using_global)[0]
	else
		let using_global = []
		" "echom 'no global usings found for '.hh.f
	endif
	" let global_usings = []
	" for [k,v] in items(s:path_to_global_using)
		" if match(hh.f, k)
			" let global_usings = v
			" break
		" endif
	" endfor
	" let usings = []
	" Get any aliases "using foo = System.foo"
	let using_scope = []
	let ns = a:s
	while v:true
		call add(using_scope, #{n:ns})
		let pscope = matchlist(ns, '\V\^\(\.\*\).\.\*\$')
		if empty(pscope)
			break
		endif
		let ns = pscope[1]
	endwhile
	let using_file = has_key(s:file_to_using, a:f) ? s:file_to_using[a:f] : []
	" Check scope as well as usings in file, self scope first
	return [using_scope, using_file, using_global]
endfunction

" Gets all interfaces/children in a tree
"  TODO: Search for serializable?
" Note not all interfaces implement [ImplicitDataDefinition], in those cases
" an implementor of the interface implements [ImplicitDataDefinition]
"  TODO: Handle partials? Helper functions expect partials, and if I am to
"  link this to existing partials... then this should handle partials.
function! robusttoolbox#get_Hierarchy()
	return s:get_Hierarchy()
endfunction
function! s:get_Hierarchy()
	let h = s:splitup_class_struct(robusttoolbox#filtertags('(and $kind (#/(C|S|I)/ $kind))'))
	let s:scope_name_to_dd = {}
	let s:name_to_dd = {}
	let s:scope_name_to_yamlname = {}
	call reduce(copy(h), {_,j -> s:setup_lookups(j)}, 0)
	" Traverse inherits to build tree
	"let top_level = filter(copy(h), {i,j->empty(j.i) ? 1 : 0})
	"let has_inherits = filter(copy(h), {i,j->empty(j.i) ? 0 : 1})
	" Now traverse classes building tree, Can terminate when encounter
	"  [ExplicitDataDefinition] or [ImplicitDataDefinition]? Or is it useful to
	"  continue?

	" A list of inherits to ignore, e.g. IDisposable, IEquatable, Exception
	let s:ignore_inherits = {}
	" Traverse inherits lists, build parent/child relationships between dds
	for hh in h
		let tokens = []
		let str = hh.i
		call s:handle_inherits_token(tokens, str)
		"if !empty(tokens)
			"echom string(tokens)
		"endif
		if empty(tokens)
			continue
		endif
		let usings = s:getUsingsFromFileScope(hh.f, hh.s)
		for token in tokens
			if has_key(s:ignore_inherits, token)
				" 0.25s faster time with s:ignore_inherits
				" But will miss things like IShootable
				continue
			endif
			" Now we have tokens, look them up with either
			"  has_key(scope_name_to_dd[usings[hh.f][i], token), or
			"  name_to_dd[token][i].s ==# usings[hh.f][j].s ?
			" I think 2nd one is faster to show no name exists in types
			" If token not found, e.g. IEquatable, then not in hierachy we care about
			if has_key(s:name_to_dd, token)
				let found = v:false
				if token ==# 'Component'
					"echom 'file: '.hh.f . ' usings: '.join(map(copy(usings), {_,j->j.n}), ',')
				endif
				" TODO: Merge this usings logic with s:map_type logic, handle
				"  subscope.token
				let scope_name = ['', '']
				let found = s:lookupToken(usings, token, scope_name)
				if found
					call s:get_Hierarchy_setter(hh, scope_name[0], scope_name[1])
				else
					let s:ignore_inherits[token] = ''
				endif
			endif
		endfor
	endfor
	echom 'ignored inherits: '.join(keys(s:ignore_inherits), ',')
	return h
endfunction

" Tries to lookup the token in the usings, if some combination exists
"  in s:scope_name_to_dd then it exists
" Params:
"  [in] usings: list of [scope, file, globals] using lists
"  [in] token: str to lookup, might be e.g. foo.bar.baz
"  [out] scope_name: set to resolved [scope,name] if found
" Returns:
"  true if found, else false
function! s:lookupToken(usings, token, scope_name)
	let m = matchlist(a:token, s:has_scope)
	if len(m) > 0
		let token_scope = '.' . m[1]
		let token_noscope = m[2]
	else
		let token_scope = ''
		let token_noscope = a:token
	endif
	for usingI in a:usings
		for using in usingI
			if has_key(using, 't')
				if a:token ==# using.t
					" Looks like this may be the alias, look up the alias
					let alias_scope_name = matchlist(using.n, '\V\^\(\.\*\).\(\.\*\)\$')
					let ascope = alias_scope_name[1]
					let aname = alias_scope_name[2]
					if has_key(s:scope_name_to_dd, ascope) &&
						\ has_key(s:scope_name_to_dd[ascope], aname)
						let a:scope_name[0] = ascope
						let a:scope_name[1] = aname
						return v:true
					endif
				endif
			endif
			if has_key(using, 'nn')
				let testscope = using.nn
			else
				let testscope = using.n . token_scope
			endif
			if has_key(s:scope_name_to_dd, testscope) &&
				\ has_key(s:scope_name_to_dd[testscope], token_noscope)
				let a:scope_name[0] = testscope
				let a:scope_name[1] = token_noscope
				return v:true
			endif
		endfor
	endfor
	" TODO: Alternate logic taken from s:map_type, benchmark!:
	" for usingsI in a:usings
		" let matches = filter(copy(usingsI), {_,j -> (!has_key(j, 't') || j.t !=#
		"  a:token) ?
			" \ has_key(s:scope_name_to_defs, j.n.'.'.rt_scoped) :
			" \ has_key(s:scope_name_to_defs, j.n)})
		" if len(matches) > 0
			" if len(matches) > 1
				" echom 'Resolving type '.rt_scoped.' against usings in same file yielded > 1 match'
			" endif
		" elseif !empty(scopeSuffix)
			" let matches = filter(copy(usingsI), {_,j -> has_key(s:scope_name_to_defs, j.n.'.'.rt_noscope)})
			" if len(matches) > 0
				" if len(matches) > 1
					" echom 'Resolving type '.rt_noscope.' against usings in same file yielded > 1 match'
				" endif
			" endif
		" endif
	" endfor
endfunction

" Helper function for get_Hierarchy
function! s:get_Hierarchy_setter(hh, scope, name)
	" Found it, so... set hh as a child, index by scopename for
	"  separation of child groups
	let scopename = a:hh.s . '.' . a:hh.n
	if has_key(s:scope_name_to_dd[a:scope][a:name][0].c, scopename)
		call add(s:scope_name_to_dd[a:scope][a:name][0].c[scopename], a:hh)
	else
		let s:scope_name_to_dd[a:scope][a:name][0].c[scopename] = [a:hh]
	endif
	" Now parent:
	" Maybe p should index by name to [dd]?
	" Better than list of dd lists
	let a:hh.p[a:name] = s:scope_name_to_dd[a:scope][a:name]
endfunction

" Populates a set of dictionaries to allow fast lookups into C|S|I
" Params:
"  [in] dd: Not a list of partials, but individual partial
function! s:setup_lookups(dd)
	" Do scope->name->[dd] instead of scope->[dd]
	if !has_key(s:scope_name_to_dd, a:dd.s)
		let s:scope_name_to_dd[a:dd.s] = {}
		let s:scope_name_to_yamlname[a:dd.s] = {}
	endif
	if has_key(s:scope_name_to_dd[a:dd.s], a:dd.n)
		call add(s:scope_name_to_dd[a:dd.s][a:dd.n], a:dd)
	else
		let s:scope_name_to_dd[a:dd.s][a:dd.n] = [a:dd]
		" Now ddmap, scope_name_to_yamlname for yaml element
		let s:scope_name_to_yamlname[a:dd.s][a:dd.n] = '!type:'.a:dd.n
	endif
	" Now name->[dd], useful if scope unknown or resolving usings?
	if has_key(s:name_to_dd, a:dd.n)
		call add(s:name_to_dd[a:dd.n], a:dd)
	else
		let s:name_to_dd[a:dd.n] = [a:dd]
	endif
endfunction

" Handles the class : inherits1, inherits2<T> where T : foo, bar
" params:
"  [in|out] tokens: The list of tokens so far
"  [in] str: The remainder to tokenize
function! s:handle_inherits_token(tokens, str)
	" NOTE: Appears that matchlist translates input char "\n" to string '\n' so
	" have to match for both... \%(\n\|\\n\)
	let Token = {s -> matchlist(s, '\V\^\%(\[\n\t ]\|\\n\|\\t\)\*\(\[^,<$]\+\)\(\.\*\)\?')}
	let CleanToken = {s -> matchlist(s, '\V\^\(\w\+\)\[\n\t ]\*')}
	let Brak = {s -> matchlist(s, '\V\^<\(\[^<>]\+\)\(<\.\*\)\?>')}
	if empty(a:str)
		return
	endif
	let token = Token(a:str)
	"echom '"'.a:str
	let remainder = token[2]
	let template_params = Brak(remainder)
	if !empty(template_params)
		" It's a template, use Brak to split the template prams from the remainder of inherits
		" To get token with template: token.'<'.template_params[1].template_params[2].'>'
		" But we're dropping template params for convenience...
		let remainder = template_params[3]
	endif
	" Clean up token, prune suffix:
	let token = CleanToken(token[1])
	if len(token) < 2
		echom 'error'.string(a:str)
	endif
	call add(a:tokens, token[1])
	" Process remainder, trim off leading comma
	if !empty(remainder)
		call s:handle_inherits_token(a:tokens, remainder[1:])
	endif
endfunction

" Gets all [Serializable]s. Emit to schema, then hierachy emitted will have
" these types to use!
" Added a few minutes, typescript will really help with parallel execution
function! robusttoolbox#get_Serializable()
	return s:get_Serializable()
endfunction
function! s:get_Serializable()
	" Step 1: Query attribute tags
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )Serializable(\]|,| )/ $name) $kind (eq? $kind "A") (not (eq? $scope-kind "enumeration")))'))

	" Step 2: For each attribute tag: query for the class|struct tags
	" Gets scope-name from attribute of class to match class|struct scope
	"let s = map(copy(a), {_,j -> map(robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") $kind (#/(C|S)/ $kind) (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))'), {_1,m -> s:splitup_class_struct_i(m)})})
	let s:name_to_Serializable = {}
	let s:scope_name_to_Serializable = {}
	for aa in a
		let scope = matchlist(aa.s, '\V\(\.\*\).\.\*\$')[1]
		" Add [Serializable] to each or just the first?
		call add(s:scope_name_to_dd[scope][aa.t][0].a, aa)
		" Will be faster to query attributes by has_key('as') rather than searching [a]
		" with regex on each element
		let s:scope_name_to_dd[scope][aa.t][0].as = aa
		let name = s:scope_name_to_dd[scope][aa.t][0].n
		if !has_key(s:name_to_Serializable, name)
			let s:name_to_Serializable[name] = s:scope_name_to_dd[scope][aa.t]
		else
			call extend(s:name_to_Serializable[name], s:scope_name_to_dd[scope][aa.t])
		endif
		if !has_key(s:scope_name_to_Serializable, scope)
			let s:scope_name_to_Serializable[scope] = {}
		endif
		let s:scope_name_to_Serializable[scope][name] = s:scope_name_to_dd[scope][aa.t]
	endfor
	"return s
endfunction

" Filtering after get_Implicit|Explicit, then
" get the functions and match then to the [Serializer]s, only then can we
" access the parameter-lists in BuildSerialiable for the schema
" TODO: Why are these needed again???
function! robusttoolbox#post_Serializable(s, p, e, i)
	return s:post_Serializable(a:s, a:p, a:e, a:i)
endfunction
function! s:post_Serializable(s, p, e, i)
	" Step 1: Filter out the [DataDefinition]s, they're serialized with
	" datafields not ctors
	"echom '[Serializer]len pre-explicit filter: '.len(a:s)
	" First filter out [DataDefinition]:
	"let ret = filter(a:s, {_,j -> empty(filter(copy(a:e), {_,k -> k[0].n ==# j[0].n && k[0].s ==# j[0].s}))})
	" Filter on each dd in s:name_to_explicit, different scope possible
	" TODO: Oh crap... logic error, serializable [dd] may have name conflicts in
	" addition to explicit [dd]... so, may have to iterate through each
	" serializable dd... Oh, or instead can test s:scope_name_to_dd????
	" Or make scope_name_to_explicit instead of name_to_explicit?
	"call filter(s:name_to_Serializable, {i,j -> !(has_key(s:name_to_explicit, i) && !empty(filter(copy(s:name_to_explicit[i]), {_,k -> j[0].s ==# k.s})))})

	" Step 2: Filter out the [ImplicitDataDefinition]s, they're serialized with
	" I think I need to loop over each rather than just recursing into first's
	" children
	"let ScanImplicit = {s,i -> i[0].n ==# s[0].n && i[0].s ==# s[0].s ? v:true :
	"	\ (empty(i[0].c ? v:false : reduce(i[0].c, {t,c->t||ScanImplicit(s, c)}, v:false)))}
	"echom '[Serializer]len pre-implicit filter: '.len(ret)
	"call filter(a:s, {_,s -> Scan_Implicit(s, a:i)})
	let ScanImplicit = {i -> empty(filter(ret, {_,s -> i[0].n !=# s[0].n && i[0].s !=# s[0].n})) ?
		\ (has_key(i[0], 'c') ? map(copy(i[0].c), {_,c -> ScanImplicit(c)}) : 0) :
		\ (has_key(i[0], 'c') ? map(copy(i[0].c), {_,c -> ScanImplicit(c)}) : 0)
		\ }
	call map(copy(a:i), {_,j -> ScanImplicit(j)})

	"echom '[Serializer]len pre-prototype filter: '.len(ret)
	" Step 3: Filter out the [Prototype]s, they're serialized with,
	" TODO: Well, I could generalize this into step 2 if I parsed
	" [MeansDataDefinition], maybe... but that is on attribute
	call map(copy(a:p), {_,j -> ScanImplicit(j)})
	"echom '[Serializer]len post filter: '.len(ret)

	" Step 4: Query the ctors (same name as class/struct)
	" Just stick all the partials ctors onto the first partial
	for s in ret
		let s[0]['H'] = s:splitup_function(robusttoolbox#filtertags('(and (eq? $name "'.s[0].n.'") $kind (eq? $kind "H") (eq? $scope-name "'.s[0].s . '.' . s[0].n.'"))'))
	endfor
	return ret
endfunction

" Gets all [Prototype("...")] classes in a list, no children
" All are classes, no structs. No children???
" list of list of dicts
" But where to put the prototype parameters like name?
" Also see: [DataField("name", required=true, serverOnly=true,...)]
" Store the split up attribute first in sublist? Yes, why not!
function! robusttoolbox#get_Prototypes()
	return s:get_Prototypes()
endfunction
function! s:get_Prototypes()
	let tI = reltime()
	"let p = s:splitup_class(robusttoolbox#filtertags('(and (#/\[(.+, )?Prototype\(\"[[:alpha:]]+\"\)/ ($"at")) $kind (eq? $kind "C"))'))
	" Step 1: Query attribute tags
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )Prototype\(\"/ $name) $kind (eq? $kind "A"))'))

	" Step 2: For each attribute tag: query for the class tags
	" Gets scope-name from attribute of class to match class scope
	"let p = map(copy(a), {_,j -> map(robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") $kind (eq? $kind "C") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))'), {_1,m -> s:splitup_class_i(m)})})
	let s:scope_name_to_Prototype = {}
	for aa in a
		let scope = matchlist(aa.s, '\V\(\.\*\).\.\*\$')[1]
		" Add [Prototype] to each or just the first?
		call add(s:scope_name_to_dd[scope][aa.t][0].a, aa)
		" Will be faster to query attributes by has_key('ap') rather than searching [a]
		" with regex on each element
		let s:scope_name_to_dd[scope][aa.t][0].ap = aa
		let s:scope_name_to_dd[scope][aa.t][0].yn = matchlist(aa.n, '\V\.\{-}\%([\|,\| \)Prototype("\(\[^"]\+\)"')[1]
		" Should never be a name conflict... also shouldn't I use yn?
		if !has_key(s:scope_name_to_Prototype, scope)
			let s:scope_name_to_Prototype[scope] = {}
		endif
		let s:scope_name_to_Prototype[scope][s:scope_name_to_dd[scope][aa.t][0].n] = s:scope_name_to_dd[scope][aa.t]
	endfor
	" for i in range(len(p))
		" " Assign the text of the attribute to 'a':
		" let p[i][0].a = a[i].n
		" let p[i][0].yn = matchlist(a[i].n, '\V\.\{-}\%([\|,\| \)Prototype("\(\[^"]\+\)"')[1]
	" endfor
	"call map(copy(p), {i,j -> execute('let '.j[0].p.' = '.string(a[i]))})})
	echom '[robusttoolbox] Parsing [Prototype]s completed in +'.reltimestr(reltime(tI))[:-5].' seconds'
	" 14+ seconds on first run, then ~1/3 time afterwards
	" 5.3 seconds
	" 5.285 without matchlist()[1:-5] indexing on splitup_class_i
	" Wil be faster by not modifying list, but adding new dict value
	" Actually nope, same or .02s slower. copy(p) randomly faster 0.06s.
	"return p
	"return map(map(p, {_,j -> [j] + robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") $kind (eq? $kind "C") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')}), {_,j -> type(j) == v:t_dict ? j : s:splitup_class(j)})
endfunction

" [ConstantsFor][FlagsFor] attributes are post-processed to include:
"  { ['tl':[typelist]] }
"  This attribute can apply to multiple types, so list of types
function! s:Splitup_attribute_for(tag, a)
	let tl = []
	let f = matchstrpos(a:a.n, '\V\( \|,\|[\)'.a:tag.'(typeof(')
	let start = f[2]
	let m = matchstrpos(strpart(a:a.n, start), '\V\(\[^)]\+\)\())\)\@=')
	while !empty(m[0])
		call add(tl, m[0])
		let start += m[2]+2
		let f = matchstrpos(strpart(a:a.n, start), '\V\( \|,\|[\)'.a:tag.'(typeof(')
		if empty(f[0])
			break
		endif
		let start += f[2]
		let m = matchstrpos(strpart(a:a.n, start), '\V\(\[^)]\+\)\())\)\@=')
	endwhile
	"echom 'for attribute for ('.a:tag.') parsed out tl: '.string(tl)
	call extend(a:a, {'tl': tl})
	return a:a
endfunction

" [DataField] attributes are post-processed to include:
"  { ['nn':nodename,]['sym':symbol,]['r':required,]['c': customTypeSerializer] }
"  symbol if present must be resolved later to determine nodename
"  nodename if present means there is no symbol lookup
function! s:Splitup_attribute_datafield(a)
	let rml = matchlist(a:a.n, '\V\.\{-}\%([\|,\| \)DataField(\(\[^,)]\+\)\%(\.\*,\[ \n]\+required: \?\(\w\+\)\)\?')
	if rml[2] ==# 'true'
		call extend(a:a, {'r': v:true})
	endif
	let m = matchlist(rml[1], '\V\^"\(\.\+\)"\$')
	if !empty(m)
		call extend(a:a, {'nn': m[1]})
	else
		" The name is a const string symbol, must be resolved
		call extend(a:a, {'sym': rml[1]})
	endif
	" Since required and customTypeSerializer may be in any order...
	let cml = matchlist(a:a.n, '\V\.\{-}\%([\|,\| \)DataField(\[^,)]\+\%(\.\*,\[ \n]\+customTypeSerializer:\n\? \*(\?typeof(\(\[^)]\+\))\)\?')
	if !empty(cml[1])
		"echom 'customTypeSerializer found '.cml[1].' for '.(has_key(a:a, 'nn')?a:a.nn:a:a.sym)
		call extend(a:a, {'c': cml[1]})
	endif
	return a:a
endfunction

" [ParentDataField] attributes are post-processed to include:
"  { ['c': customTypeSerializer, 'nn': 'parent'] }
function! s:Splitup_attribute_parentdatafield(a)
	let cml = matchlist(a:a.n, '\V\.\{-}\%([\|,\| \)ParentDataField\%(Attribute\)\?(typeof(\(\[^)]\+\)))')
	"echom '[ParentDataField] customTypeSerializer found '.cml[1].' for '.a:a.n
	call extend(a:a, {'c': cml[1], 'nn': 'parent'})
	return a:a
endfunction

function! s:get_ForInner(forAttrib)
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )'.a:forAttrib.'\(/ $name) $kind (eq? $kind "A"))'))

	call map(a, {_,j -> s:Splitup_attribute_for(a:forAttrib, j)})

	" Step 2: Find the enums which this attribute applies to
	let ret = {}
	for aa in a
		" G will have list of enums, with list of enumerators on e
		"let m = filter(copy(a:G), {_,j -> j.s.'.'.j.n ==# at.s})
		let scope = matchlist(aa.s, '\V\(\.\*\).\(\.\*\)\$')
		if !has_key(s:scope_name_to_Enum, scope[1]) || !has_key(s:scope_name_to_Enum[scope[1]], scope[2])
			echom 'error in get_ForInner, no type exists in G set: '.aa.s
		else
			let m = s:scope_name_to_Enum[scope[1]][scope[2]]
			for t in aa.tl
				"echom 'Found type '.t.' len instances of ['.a:forAttrib.'(typeof('.t.'))]: '.len(m)
				if !has_key(ret, t)
					let ret[t] = []
				endif
				call add(ret[t], m)
			endfor
		endif
	endfor
	return ret
endfunction

function! s:get_FlagsFor()
	return s:get_ForInner('FlagsFor')
endfunction

function! s:get_ConstantsFor()
	return s:get_ForInner('ConstantsFor')
endfunction

" Assigns each attribute to corresponding MP in s:scope_name_to_MP
" params:
" [in] a: List of attributes with df
function! s:AssignAToMP(a)
	"echom '[robusttoolbox] len attribs before dd search: '.len(a)
	"let index = 0
	"let matches = 0
	for aa in a:a
		let scope = matchlist(aa.s, '\V\(\.\*\).\(\.\*\)\$')
		for partial in s:scope_name_to_dd[scope[1]][scope[2]]
			" Checks to see if this df goes into this partial
			if partial.f ==# aa.df.f
				"let matches += 1
				"call remove(a:a, index)
				"let index -= 1
				call add(partial.df, aa)
				" Check to see if we need to lookup symbol
				if has_key(aa.df, 'sym')
					if !has_key(s:scope_name_to_MP[aa.s], aa.df.sym)
						echom 'failure in get_DataFields symbol lookup'
					endif
					let m = s:scope_name_to_MP[aa.s][aa.df.sym]
					"echom 'For name '.aa.df.sym.' (const string) found default '.m.d
					let ml = matchlist(m.d, '\V\^"\(\.\+\)"\$')
					if !empty(ml)
						let aa.nn = ml[1]
					else
						echom 'failure to parse default for df symbol '.aa.df.sym
					endif
					" What was algorithmic point of this unletting sym?
					"unlet aa.df.sym
				endif
			endif
		endfor
		"let index += 1
	endfor
	"echom '[robusttoolbox] len attribs after dd search: '.len(a)
	"echom '[robusttoolbox] matches after dd search: '.matches
endfunction

" Gets all qualified [DataField] members/properties in a list
function! s:get_DataFields()
	let tI = reltime()
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )DataField\(/ $name) $kind (eq? $kind "A"))'))

	call map(a, {_,j -> s:Splitup_attribute_datafield(j)})

	" Step 2: For each attribute tag: query for the member/property tags
	" Gets scope-name from attribute of M/P tag to determine container
	" container scope will either be in:
	"  The Prototype sublist, the attribute/scope element
	"  The DataDefinition sublist, the attribute/scope element
	" Store this attribute tag where? On 'a'!
	"  Create a member tag list and property tag list and populate
	"  as queried? No... store members and property tags directly
	"  on associated container, in new element of dictionary on first
	"  element of container sublist. New element being a list of
	"  [DataField] tags, and somehow store attribute on that tag dict.
	" SLOWWW:
	"let t = map(copy(a), {_,j -> map(robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))'), {_1,m -> s:splitup_member_property_i(m)})})
	" TODO: Alternate of above, test for better time, also no sublist:
	"let t = map(copy(a), {_,j -> s:splitup_member_property_i(robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))')[0])})
	"for i in range(len(t))
	""	let t[i] = t[i][0]
	"	let t[i].a = a[i].n
	"endfor
	" TODO: Consistency for other attributes? To handle more?
	"call map(t, {i,j -> extend(j, {'a': [a[i]]})})
	"call map(t, {i,j -> extend(j, {'a': a[i]})})

	" Step 2: Get all M|P, map into scope -> [M|P]
	let mp = map(robusttoolbox#filtertags('(and $kind (#/(M|P)/ $kind))'), {_,j -> s:splitup_member_property_i(j)})
	let s:scope_name_to_MP = {}
	for i in mp
		if !has_key(s:scope_name_to_MP, i.s)
			let s:scope_name_to_MP[i.s] = {}
		endif
		let s:scope_name_to_MP[i.s][i.n] = i
	endfor

	" Step 3: Find the M|P for each a (thus a datafield)
	"  Since a is right length, and mp too much extraneous, slow to remove
	for aa in a
		let aa.df = s:scope_name_to_MP[aa.s][aa.t]
	endfor

	echom '[robusttoolbox] Parsing [DataField]s completed in +'.reltimestr(reltime(tI))[:-5].' seconds'
	" 167.9s

	" Step 3: Store [DataField] tags on prototypes and datadefinitions
	" KLUDGE: filtered out scope info on [DataField] tag, might need later
	" TODO?: Need to add scope back to [DataField] attribs to find partials?
	" DONE: will want to only put [DataField] on right partial for
	"  intelligent updating, but would need to compare $input file,
	"  for now put all [DataField]s on first partial
	" TODO?: I now do all partials, fix change effects
	call s:AssignAToMP(a)

	" c is a list of lists, and each sublist element represents a partial
	"  and each partial may have [DataField]s
	" Could flatten partial sublists as $input isn't compared?
	" Actually... not flattening is desired for quicker updates
	"let R = {o,j -> (has_key(o, 'c') ? empty(map(copy(o.c), {_,n -> map(copy(n), {_1,m -> R(m,j)})})) : v:true) ? Z(o,j) : Z(o,j)}

	"broke: call map(copy(t), {_,j -> map(copy(a:i), {_1,m -> R(m,j)})})
	"call map(copy(t), {_,j -> map(copy(a:i), {_1,m -> map(m, {_2,n -> R(n,j)})})})
	" Alternate (no memcopy):
	"let R2 = {o,j -> map(o, {_,n -> map(n, {_1,m -> empty(has_key(m,'c') ? R2(m.c, j) : []) ? Z2(m, j) : Z2(m, j)})})}
	" Alternate (filter, see if better than 203s Parse+Process):
	" let s:R3 = {o,j -> len(filter(copy(o), {_,i -> len(filter(copy(i), {_1,p -> !empty((has_key(p,'c') && !empty(p.c)) ? s:R3(p.c, j) : v:false) || s:Z3(p,j)}))}))}
	" let g:t = deepcopy(t)
	" call filter(t, {_,j -> !s:R3(a:i, j)})
	" echom '[robusttoolbox] len attribs after implicits: '.len(aa)
	" echom '[robusttoolbox] s:matches after implicits: '.s:matches

	echom '[robusttoolbox] Processing [DataField]s completed in +'.reltimestr(reltime(tI))[:-5].' seconds'
	" 6.84s only p,e, encoding flux
	" 5.2s only p,e, encoding flux
	" 6.0s only p,e, encoding flux, with Z lambda
	" ~33s with p,e,i
	" Ok with attribute tags its 8x slower due to complexity
	" query at, foreach at query matching c/s/m/p
	" Rather than query all c/s/m/p with [match] in attribute field
	" as |a| = |t|, O(t*t) queries vs O(t) and extra processing
endfunction


function! s:get_AbstractDataFields()
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )AbstractDataField(Attribute)?(\]|,)/ $name) $kind (eq? $kind "A"))'))

	" [AbstractDataField] has Name of 'abstract' in ctor
	call map(a, {_,j -> extend(j, {'nn': 'abstract'})})

	" Step 2: For each attribute tag: query for the member/property tags
	" Gets scope-name from attribute of M/P tag to determine container
	" container scope, appears to only be in:
	"  The Prototype sublist, the attribute/scope element
	"let t = map(copy(a), {_,j -> s:splitup_member_property_i(robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))')[0])})
	"call map(t, {i,j -> extend(j, {'a': a[i]})})
	for aa in a
		let aa.df = s:scope_name_to_MP[aa.s][aa.t]
	endfor

	" Step 3: For each member/property tag, put on attribute
	" DONE: will want to only put [DataField] on right partial for
	"  intelligent updating, but would need to compare $input file,
	"  for now put all [DataField]s on first partial
	" TODO: I now do all partials, fix change effects
	"echom '[robusttoolbox] len [AbstractDataField] before prototypes: '.len(aa)
	"call filter(t, {_,j -> !len(filter(copy(a:p), {_1,e -> len(filter(copy(e), {_2,p -> s:Z3(p,j)}))}))})
	call s:AssignAToMP(a)
	"echom '[robusttoolbox] len [AbstractDataField] after prototypes: '.len(aa)
endfunction

function! s:get_ParentDataFields()
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )ParentDataField(Attribute)?\(/ $name) $kind (eq? $kind "A"))'))

	" [ParentDataField] has Name of 'parent' in ctor
	call map(a, {i,j -> s:Splitup_attribute_parentdatafield(j)})

	" Step 2: For each attribute tag: query for the member/property tags
	" Gets scope-name from attribute of M/P tag to determine container
	" container scope, appears to only be in:
	"  The Prototype sublist, the attribute/scope element
	"let t = map(copy(a), {_,j -> s:splitup_member_property_i(robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))')[0])})
	"call map(t, {i,j -> extend(j, {'a': a[i]})})
	for aa in a
		let aa.df = s:scope_name_to_MP[aa.s][aa.t]
	endfor

	" Step 3: For each member/property tag, put on attribute
	" DONE: will want to only put [DataField] on right partial for
	"  intelligent updating, but would need to compare $input file,
	"  for now put all [DataField]s on first partial
	" TODO: I now do all partials, fix change effects
	"echom '[robusttoolbox] len [ParentDataField] before prototypes: '.len(t)
	"call filter(t, {_,j -> !len(filter(copy(a:p), {_1,e -> len(filter(copy(e), {_2,p -> s:Z3(p,j)}))}))})
	call s:AssignAToMP(a)
	"echom '[robusttoolbox] len [ParentDataField] after prototypes: '.len(t)
endfunction

" KLUDGE: Assuming scope-kind on attribute will be enumeration
" Returns:
"  List of enums: {n:name, k:scope-kind, s:scope-name, [t:typename], e:[enumerators]}
"   typename: e.g. 'sbyte' in 'enum VentPipeDirection : sbyte'
"   enumerators: {n:name, v:value}
function! s:get_Enums()
	" Step 1: Query attribute tags
	" EDIT: Actually, see SpeciesSkinColor, no attribute but is used
	"let roots = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )Serializable(,|\])/ $name) $kind (eq? $kind "A") (eq? $scope-kind "enumeration"))'))

	" Step 2: for each attribute tag: query the enum tags
	" Gets scope-name from attribute of class to match class scope
	" There will never be same-named item in same scope, no partials!
	"call map(roots, {_,j -> s:splitup_enum_i(robusttoolbox#filtertags('(and (eq? $name "'.j.t.'") $kind (eq? $kind "G") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')[0])})

	" Step 1: Get enum tags
	let g = map(robusttoolbox#filtertags('(and $kind (eq? $kind "G"))'), {i,j -> s:splitup_enum_i(j)})
	let s:scope_name_to_Enum = {}
	for gg in g
		if !has_key(s:scope_name_to_Enum, gg.s)
			let s:scope_name_to_Enum[gg.s] = {}
		endif
		let s:scope_name_to_Enum[gg.s][gg.n] = gg
	endfor

	" Step 3 (now 2): for each enum get every enumerator
	"call map(copy(roots), {_,j -> extend(j.e, map(robusttoolbox#filtertags('(and $kind (eq? $kind "E") (eq? $scope-name "'.j.s.'.'.j.n.'"))'), {_1,l -> s:splitup_enumerator_i(l)}))})
	let e = map(robusttoolbox#filtertags('(and $kind (eq? $kind "E"))'), {_,l -> s:splitup_enumerator_i(l)})
	for ee in e
		let scope = matchlist(ee.s, '\V\(\.\*\).\(\.\*\)\$')
		unlet ee.s
		call add(s:scope_name_to_Enum[scope[1]][scope[2]].e, ee)
	endfor
endfunction

function! s:get_Colors()
	return s:splitup_member(robusttoolbox#filtertags('(and $kind (eq? $kind "M") (eq? $scope-name "Robust.Shared.Maths.Color") (eq? ($"type") "Color"))'))
endfunction

" For kind:D puts into {n:name, f:file, t:typename}
function! s:splitup_id(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:D\ttyperef:typename:\(\[^\t]\+\)\$')}), {_,j -> {'n': j[1], 'f': j[2], 't': j[3]}})
endfunction

" TODO: Might not need f:file on this?
" For kind:A puts into {n:name, k:scope-kind, s:scope-name, t:typename}
function! robusttoolbox#splitup_attribute(list)
	return s:splitup_attribute(a:list)
endfunction
function! s:splitup_attribute(list)
	" Is Extended Posix Regex, so can't match nested parens IIRC
	"  Appears to be no performance benefit of not capturing groups
	" For Class attributes should prune suffix of scope-name after this call
	" For Class members should leave scope-name as-is
	" TODO: See if obtaining file and querying against that gives a speedup
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\[^\t]\+\t\[^\t]\+\tkind:A\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttyperef:typename:\(\[^\t]\+\)\$')}), {_,j -> {'n': j[1], 'k': j[2], 's': j[3], 't': j[4]}})
endfunction

" For kind:U puts into {n:name, f:file, t:type, g:global}, examples:
" using n;
" using type = n;
" using static n;
" global using n;
" global using type = n;
" global using static n;
function! s:splitup_using(list)
	try
		return map(map(copy(a:list), {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:U\ttype:\(\[^\t\$]\*\)\%(\tgl:\(\.\*\)\)\?\$')}), {_,j -> empty(j[4]) ? (empty(j[3]) ? {'n': j[1], 'f': j[2]} : {'n': j[1], 'f': j[2], 't': j[3]}) : (empty(j[3]) ? {'n': j[1], 'f': j[2], 'g': v:true} : {'n': j[1], 'f': j[2], 't': j[3], 'g': v:true})})
	catch
		let ret = map(copy(a:list), {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:U\ttype:\(\[^\t\$]\*\)\%(\tgl:\(\.\*\)\)\?\$')})
		for i in range(len(ret))
			let j = ret[i]
			if empty(j)
				echom 'splitup_using failure: '.string(a:list[i])
			endif
			let ret[i] = empty(j[4]) ? (empty(j[3]) ? {'n': j[1], 'f': j[2]} : {'n': j[1], 'f': j[2], 't': j[3]}) : (empty(j[3]) ? {'n': j[1], 'f': j[2], 'g': v:true} : {'n': j[1], 'f': j[2], 't': j[3], 'g': v:true})
		endfor
		return ret
	endtry
endfunction

" For kind:C puts into {n:name, f:file, t:langtype, k:scope-kind, s:scope-name, i:inherits, h:[sealed|abstract], df:datafields, a:attributes}
" Operates on a list of list of strings
"function! s:splitup_class(list)
	"return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:C\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^\t]\*\))\%(\tpl:(\(\[^\t]\*\))\)\?\tsl:\(\.\*\)\$')}), {_,j -> {'n': j[1], 'f':j[2], 't': 'C', 'k': j[3], 's': j[4], 'i': j[5], 'pl': j[6], 'h': j[7], 'df': [], 'a': []}})
"endfunction

" Same as splitup_class but for single string
function! s:splitup_class_i(str)
	let j = matchlist(a:str, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:C\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^\t]\*\))\%(\tpl:(\(\[^\t]\*\))\)\?\tsl:\(\.\*\)\$')
	return {'n': j[1], 'f': j[2], 't': 'C', 'k': j[3], 's': j[4], 'i': j[5], 'pl': j[6], 'h': j[7], 'df': [], 'a': []}
endfunction

" For kind:S puts into {n:name, f:file, t:langtype, k:scope-kind, s:scope-name, i:inherits, df:datafields, a:attributes}
"function! s:splitup_struct(list)
	"return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:S\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^\t]\*\))\%(\tpl:(\(\[^\t]\*\))\)\?\$')}), {_,j -> {'n': j[1], 'f': j[2], 't': 'S', 'k': j[3], 's': j[4], 'i': j[5], 'pl': j[6], 'df': [], 'a': []}})
"endfunction

" For kind:C|S|I combo queries, slower than one
" Suitable for when an attribute applies to either, perhaps
" Should benchmark to see if separating first is faster
" Had to modify sl: capture to be optional to catch both struct and class
function! robusttoolbox#splitup_class_struct(list)
	return s:splitup_class_struct(a:list)
endfunction
function! s:splitup_class_struct(list)
	try
		" Most efficient without for loop... but prone to bad underlying code
		return map(map(copy(a:list), {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\(\[CSI]\)\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^\t]\*\))\%(\tpl:(\(\[^\t]\*\))\)\?\%(\tsl:\(\[^$]\*\)\)\?\$')}), {_,j -> (j[3] ==# 'C' ? {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'pl': j[7], 'h': j[8], 'df': [], 'a': [], 'p': {}, 'c': {}} : {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'pl': j[7], 'df': [], 'a': [], 'p': {}, 'c': {}})})
	catch
		"let foo = map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\(\[CSI]\)\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^)]\*\))\%(\tsl:\(\[^$]\*\)\)\?\$')})
		let foo = copy(a:list)
		for j in range(len(foo))
			" Changed inherits from ^) to ^\t termination, I think it works, also
			" changed in splitup_class_struct_i, TODO: change elsewhere
			"let k = matchlist(a:list[j], '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\(\[CSI]\)\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^)]\*\))\%(\tsl:\(\[^$]\*\)\)\?\$')
			let k = matchlist(a:list[j], '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\(\[CSI]\)\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^\t]\*\))\%(\tpl:(\(\[^\t]\*\))\)\?\%(\tsl:\(\[^$]\*\)\)\?\$')
			if len(k) < 3
				" Ok the problem is the inherits contains a couple new() new() which
				" throws off the inherits: group termination, and extended posix regex
				" can't handle nested parentheses...
				" The following is bad:ComponentTreeSystem<TTreeComp,TComp>^I/home/browser/dev/space-station-14/RobustToolbox/Robust.Shared/ComponentTrees/ComponentTreeSystem.cs^I18;"^Ikind:C^Iscope:namespace:Robust.Shared.ComponentTrees^Iinherits:(EntitySystem\nwhere TTreeComp : Component, IComponentTreeComponent<TComp>, new()\nwhere TComp : Component, IComponentTreeEntry<TComp>, neW()\N)^iSL:ABSTRACt with result dict:[]"
				echom 'The following failed to match in splitup_class_struct:'.a:list[j].' with result dict:'.string(k)
			endif
			let foo[j] = k
		endfor
		return map(foo, {_,j -> (j[3] ==# 'C' ? {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'pl': j[7], 'h': j[8], 'df': [], 'a': []} : {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'pl': j[7], 'df': [], 'a': []})})
	endtry
endfunction

" Same as splitup_class_struct but for single string
function! s:splitup_class_struct_i(str)
	let j = matchlist(a:str, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\(\[CSI]\)\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^\t]\*\))\%(\tpl:(\(\[^\t]\*\))\)\?\%(\tsl:\(\[^$]\*\)\)\?\$')
	return j[3] ==# 'C' ? {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'pl': j[7], 'h': j[8], 'df': [], 'a': [], 'p': {}, 'c': {}} : {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'pl': j[7], 'df': [], 'a': [], 'p': {}, 'c': {}}
endfunction

" For kind:M puts into {n:name, f:file, k:scope-kind, s:scope-name, t:type, d:default}
function! s:splitup_member(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\w\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:M\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttype:\(\[^\t]\+\)\tdf:\(\.\*\)\$')}), {_,j -> {'n': j[1], 'f': j[2], 'k': j[3], 's': j[4], 't': j[5], 'd': j[6]}})
endfunction

" For kind:H puts into {n:name, f:file, k:scope-kind, s:scope-name, pl:parameter-list}
function! s:splitup_function(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\w\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:H\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tpl:\(\.\*\)\$')}), {_,j -> {'n': j[1], 'f': j[2], 'k': j[3], 's': j[4], 'pl': j[5]}})
endfunction

" For kind:P puts into {n:name, k:scope-kind, s:scope-name, t:type, d:default}
"  Almost identical to splitup_member but regex compiled not to 'kind:M'
function! s:splitup_property(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\w\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:P\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttype:\(\[^\t]\+\)\tdf:\(\.\*\)\$')}), {_,j -> {'n': j[1], 'f': j[2], 'k': j[3], 's': j[4], 't': j[5], 'd': j[6]}})
endfunction

" For kind:M or kind:P puts into {n:name, f:file, k:scope-kind, s:scope-name, t:type, d:default}
"  Almost identical to splitup_member but regex compiled not to 'kind:M'
function! s:splitup_member_property_i(str)
	let j = matchlist(a:str, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\.\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttype:\(\[^\t]\+\)\tdf:\(\.\*\)\$')
	return {'n': j[1], 'f': j[2], 'k': j[3], 's': j[4], 't': j[5], 'd': j[6]}
endfunction

" For kind:G puts into {n:name, k:scope-kind, s:scope-name, t:typename}
"  typename in tag only іf enum type specified,
"  e.g. 'enum VentPumpDirection : sbyte'
function! s:splitup_enum_i(str)
	let j = matchlist(a:str, '\V\(\w\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:G\tscope:\(\[^:]\+\):\(\[^\t]\+\)\%(\ttyperef:typename:\(\[^\t]\+\)\)\?\$')
	return {'n': j[1], 'f': j[2], 'k': j[3], 's': j[4], 't': j[5], 'e': []}
endfunction

" For kind:E puts into {n:name, v:value}
"  value may be empty
" k:scope-kind and s:scope-name are on containing enum so ignore
function! s:splitup_enumerator_i(str)
	" KLUDGE: v may be optional
	let j = matchlist(a:str, '\V\(\w\+\)\t\[^\t]\+\t\[^\t]\+\tkind:E\tscope:\[^:]\+:\(\[^\t]\+\)\tdf:\(\[^$]\*\)\$')
	return {'n': j[1], 's': j[2], 'v': j[3]}
endfunction


" Params:
"  [in] tag: parent name
"  [in] pdd: parent partial list
function! robusttoolbox#get_Children(tag, pdd)
	call s:get_Children(a:tag, a:pdd)
endfunction
function! s:get_Children(tag, pdd)
	let parent = a:tag
	" Check for template parameters and replace with '<[^>]+>'
	let Token = {s -> matchlist(s, '\V\(\[^<>]\+\)\(\.\*\)\?')}
	let m = Token(parent)
	if !empty(m[2])
		" TODO: Recursive... to handle nested angle brackets? and more params
		"  Could maybe handle nesting with a terminator seen in inherits, e.g. [\n,$ ]
		"  Seems terminator used properly in subequent queries, so, could validate
		"  readtags behaves as expected
		let parent = m[1].'<[^<]+>'
		"echom 'had a template parent: '.parent
	endif

	" Query for everything not in Client, later we'll merge in Client
	" TODO: IDK if pdd may have more than one scope... e.g. Client/Server
	let children = s:splitup_class_struct(robusttoolbox#filtertags('(and $kind (#/(C|I)/ $kind) (#/(^\(|,| )('.escape(a:pdd[0].s, '.').'\.)?'.parent.'( |,|\n|\)$)/ $inherits) (not (#/^(Content|Robust)\.Client/ $scope-name)))'))

	" For each child query all class/interface tag instances (i.e. partial)
	" as only one will have the $inherits populated which we queried above
	" Setting 'p' to a:pdd may make for an infinіte loop on printing
	"  Nope! Doesn't go down that rabbit hole and prints as [...]
	call map(children, {_,j -> map(robusttoolbox#filtertags('(and (eq? $name "'.j.n.'") $kind (#/(C|I)/ $kind) (eq? $scope "'.j.k.':'.j.s.'"))'), {_,k -> extend(s:splitup_class_struct_i(k), {'p': a:pdd})})})
	" List processing is slow sadly :(

	" Now we get children in client, and later merge into server
	"  This list should include some Components, and MobStates
	let clientChildren = s:splitup_class_struct(robusttoolbox#filtertags('(and $kind (#/(C|I)/ $kind) (#/(^\(|,| )('.escape(a:pdd[0].s, '.').'\.)?'.parent.'( |,|\n|\)$)/ $inherits) (#/^(Content|Robust)\.Client/ $scope-name))'))

	" For each child query all class/interface tag instances (i.e. partial)
	"  as only one will have the $inherits populated which we have now
	call map(clientChildren, {_,j -> map(robusttoolbox#filtertags('(and (eq? $name "'.j.n.'") $kind (#/(C|I)/ $kind) (eq? $scope "'.j.k.':'.j.s.'"))'), {_,k -> extend(s:splitup_class_struct_i(k), {'p': a:pdd})})})

	" Match should be a list of a list of partials, but outer len==1
	call reduce(clientChildren, {_,c -> s:get_Children_handle_matches(parent, children, c, filter(copy(children), {_,j -> j[0].n ==# c[0].n}))}, 0)

	"return map(children, {_,j -> j[0].h !=# 'sealed' ? [extend(j[0], {'c': s:get_Children(j[0].n)})] + j[1:] : j})
	return map(children, {_,j -> map(j, {k,l -> k != 0 ? l : (has_key(l, 'h') && l.h ==# 'sealed' ? l : extend(l, {'c': s:get_Children(l.n, j)}))})})

	" Attempt to replace the above nested map with nested reduce, no improvement
	" in time. 5 seconds slower. Doing extend as a dict or list makes no time
	" difference. Same with changing the get_Children caller? Still ~3-4s slow.
	"call reduce(children, {_,j -> reduce(j, {k,l -> !empty(k) ? 1 : (has_key(l, 'h') && l.h ==# 'sealed' ? 1 : extend(l.c, s:get_Children(l.n, j)))}, 0)}, 0)
	"return children
endfunction

" Called from s:get_Children to avoid slow for loop, seems maybe 0.5s faster
" 198.5s -> 198.0s
function! s:get_Children_handle_matches(parent, children, c, match)
	if len(a:match) == 0
		" No shared/server partials to merge client partials to
		" Put them into children
		call add(a:children, a:c)
	elseif len(a:match) == 1
		" Must merge the client partials to the shared/server partials
		let dd = a:match[0]
		if dd[0].h !=# 'sealed' || a:c[0].h !=# 'sealed'
			echom 'anomaly in get_Children, Client/Server merge class unsealed, Client children will not be parsed, server:'.a:match[0][0].n. ' client:'.a:c[0].n
			" Looks like ItemComponent isn't sealed... I may have to do
			"  this algorithm differently then
			" ItemComponent has both Client/Server classes, and each have
			"  a child class 'ClothingComponent'. Oh looks like this was
			"  fixed in master... need to update
		endif
		" Just add the partials together, the [DataField]s will merge
		"  This works since these classes have no children
		call extend(dd, a:c)
	else
		echom 'anomaly in get_Children, more than one sibling match, parent: '.a:parent.' child: '.c[0].n
	endif
	return 0
endfunction

" Run sanity checks for where ctags fails to parse the C# code properly
function! s:tests(d)
	" Check [DataDefinition]s:
	let failedIndicies = filter(map(copy(a:d.e), {i,j->len(j)==0 ? i : -1}), {i,j -> j>=0 ? v:true : v:false})
	" Query roots again to print the tags that failed:
	let roots = s:splitup_attribute(robusttoolbox#filtertags('(and (#/(\[|,| )DataDefinition(,|\])/ $name) $kind (eq? $kind "A"))'))
	for i in failedIndicies
		echom 'The following [DataDefinition] attributes failed to match with their object, due to underlying failure to parse: '.string(roots[failedIndicies])
	endfor
endfunction
