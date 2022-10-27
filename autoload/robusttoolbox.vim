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

let s:self = expand('<sfile>')
command! -nargs=0 RobustResource call execute('source '.s:self)
command! -nargs=0 RobustSetup call execute('let g:tf = b:gutentags_files.ctags')

" Parses data necessary for schema generation
function! robusttoolbox#ParseData() abort
	let s:d = robusttoolbox#parseCS()
	let y = robusttoolbox#parseYAML()
	call extend(s:d, y)
endfunction

" Generate monolithic schema
function! robusttoolbox#GenSchema() abort
	if !exists('s:d')
		call robusttoolbox#ParseData()
	endif
	if exists('*dein#get')
		let template_file = dein#get('robusttoolbox-vim').path.'/data/template.json'
	else
		let s_dir = fnamemodify(expand('<sfile>'), ':p:h')
		let template_file = s_dir.'/../data/template.json'
	endif

	let t1 = reltime()
	let in = join(readfile(template_file, 'b'))
	let ts1 = reltimestr(reltime(t1))
	echom '[robusttoolbox] Read template in '.ts1.' seconds'
	let template = json_decode(in)
	let g:template = template

	" [DataDefinitions] scope -> name to avoid name conflicts
	let ddmap = {
		\ 'Robust.Shared.Audio.SoundSpecifier': 'SoundSpecifier',
		\ 'Robust.Shared.Audio.SoundPathSpecifier': 'SoundPathSpecifier',
		\ 'Robust.Shared.Audio.SoundCollectionSpecifier': 'SoundPathCollectionSpecifier',
		\ }

	" List of dicts, e.g. [{'$ref': '#/definitions/entity'},{...}]
	let template.items.oneOf = []

	echom '[ExplicitDataDefinition]s...'
	for el in s:d.e
		call s:BuildExplicitDataDefinition(s:d, ddmap, template, el)
	endfor

	echom '[ImplicitDataDefinition]s... (including Components)'
	" Handle components, e.g. ['Paper', 'PlantHolder', ...]
	let unRegistered = {}
	let ScanComponentChildren = {j -> empty(has_key(j[0], 'c') ? map(copy(j[0].c), {_,l -> ScanComponentChildren(l)}): 0) ? s:BuildComponentDefinition(s:d, unRegistered, ddmap, template, j) : s:BuildComponentDefinition(s:d, unRegistered, ddmap, template, j)}
	let ScanImplicitDDChildren = {j -> empty(has_key(j[0], 'c') ? map(copy(j[0].c), {_,l -> ScanImplicitDDChildren(l)}): 0) ? s:BuildImplicitDataDefinition(s:d, ddmap, template, j) : s:BuildImplicitDataDefinition(s:d, ddmap, template, j)}
	for il in s:d.i
		if il[0].n ==# 'Component'
			call ScanComponentChildren(il)
		else
			" TODO: Add other implicit data definitions to $defs, and ddmap
			" TODO: Handle abstract inheritance
			call ScanImplicitDDChildren(il)
		endif
	endfor

	echom 'Unregistered Components...'
	" Now handle unregistered components as long as they're not in ddmap
	for [name, dd] in items(unRegistered)
		if index(values(ddmap), name) == -1
			" We have a component [DataDefinition] we can add without conflict
			let ddmap[dd[0].s.'.'.dd[0].n] = name
			call s:BuildComponentDefinitionInner(s:d, ddmap, template, dd, name)
		else
			echom 'skipped registered component to avoid duplicate: '.name
		endif
	endfor

	" Populate Color enumeration... is in OpenTK and not an enum sadly
	call s:BuildColorDefinition(s:d, template)

	echom '[Prototype]s...'
	" Handle prototypes, e.g. ['entity', 'shader', ...]
	let s:nonDD = {}
	for pl in s:d.p
		call s:BuildPrototypeDefinition(s:d, ddmap, template, pl)
	endfor

	let out = json_encode(template)
	let out_dir = gutentags#get_project_root(getcwd()).'/Resources/Schemas'
	if !isdirectory(out_dir)
		call mkdir(out_dir, 'p')
	endif
	let out_file = out_dir.'/prototypes.json'
	call writefile([out], out_file, 'S')
endfunction

" Builds each prototype schema and puts in #/definitions
function! s:BuildPrototypeDefinition(d, ddmap, template, dd)
	" May be partial class, attribute/inherits on one not the other
	let yn = ''
	let inherits = []
	let InheritsToken = {s -> matchlist(s, '\V\(\[^<>\\ ,]\+\%(<\.\*>\)\?\)\%(\\n\| \|,\)\*\(\.\*\)\?')}
	for partial in a:dd
		if has_key(partial, 'yn')
			" Note: GameMapPrototype will have a:[] on one partial
			let yn = partial.yn
		endif
		let m = partial.i
		while !empty(m)
			let m = InheritsToken(m)
			call add(inherits, m[1])
			let m = m[2]
		endwhile
	endfor
	let description = 'Prototype C# type: '.a:dd[0].n
	let refstr = '#/definitions/'.yn
	call add(a:template.items.oneOf, {'$ref': refstr})
	call add(a:template.definitions.prototype.properties.type.enum, yn)
	let a:template.definitions[yn] = {
		\ '$id': refstr,
		"\ 'description': description,
		\ 'type': 'object',
		\ 'allOf': [{ '$ref': '#/definitions/prototype' }],
		\ 'additionalProperties': v:false,
		\ 'properties': {
			\ 'id': {},
			\ 'type': {
				\ 'enum': [yn],
				\ 'description': description
			\ }
		\ }
	\ }

	let dsh = {
		\ 'd': a:d,
		\ 'ddmap': a:ddmap,
		\ 'sNode': a:template.definitions[yn],
		\ 'sDefinitions': a:template.definitions,
		\ 'sDefs': a:template['$defs'].definitions,
		\ }

	" Now search datadefinitions for inherits
	for parent in inherits
		if has_key(s:nonDD, parent)
			continue
		endif
		let found = v:false
		let scopeSuffix = ''
		let m = matchlist(parent, s:has_scope)
		if len(m) > 0
			let scopeSuffix = escape(m[1], '.').'$'
			echom yn.' parent '.parent.' had scope, is now '.scopeSuffix.'.'.m[2]
			let parent = m[2]
		endif
		for e in dsh.d.e
			if e[0].n ==# parent && e[0].s =~# scopeSuffix
				let found = s:resolvename(dsh, e, 'E')
				" additionalProperties: {} causes weird issue with entityTargetAction
				call add(a:template.definitions[yn].allOf,
					\ {
						\ '$ref': '#/$defs/definitions/'.found,
						\ 'additionalProperties': v:false
						\ })
				break
			endif
		endfor
		if !found
			let r = s:Scan(dsh, dsh.d.i, parent, scopeSuffix)
			if type(r) == v:t_dict
				let r.additionalProperties = v:false
				call add(a:template.definitions[yn].allOf, r)
				let found = split(r['$ref'], '/')[-1]
			endif
		endif
		if found == v:false
			echom 'not found '.parent.' for '.yn.' : '.parent
			let s:nonDD[parent] = v:true
		else
			echom 'found '.found.' for '.yn.' : '.parent
			" Add properties in the form of {}
			let props = a:template.definitions[yn].properties
			for prop in keys(dsh.sDefs[found].properties)
				let props[prop] = {}
			endfor
			" TODO: Might have to merge in required
		endif
	endfor

	call s:ParseDataFields(dsh, a:dd, v:false)
endfunction

function! s:BuildComponentDefinition(d, unRegistered, ddmap, template, dd)
	let name = a:dd[0].n
	" Check if this component has [RegisterComponent]
	let isRegisterComponent = v:false
	for partial in a:dd
		for a in partial.a
			let s = matchlist(a, '\V\.\{-}\%([\|,\| \)RegisterComponent\%(,\|]\)')
			"echom 'found RegisterComponent for '.partial.n
			" TODO: UtilityAI found twice here?
			if !empty(s)
				let isRegisterComponent = v:true
				break
			endif
		endfor
		if isRegisterComponent
			break
		endif
	endfor
	let isComponentProtoName = v:false
	" Retrieve [ComponentProtoName("name")] attribs in parse
	for d in a:dd
		for a in d.a
			let s = matchlist(a, '\V\.\{-}\%([\|,\| \)ComponentProtoName("\(\[^"]\+\)')
			if len(s) >= 3
				let name = s[1]
				let isComponentProtoName = v:true
				break
			endif
		endfor
		if isComponentProtoName
			break
		endif
	endfor
	if isComponentProtoName == v:false
		" Truncate 'Component' suffix:
		let m = match(name, 'Component$')
		if m != -1
			let name = strcharpart(name, 0, m)
		endif
		" Need to remove (Shared|Client|Server) prefix
		let prefixRemoved = v:false
		let m = match(name, '^Shared')
		if m != -1
			let name = strcharpart(name, len('Shared'))
			let prefixRemoved = v:true
		endif
		let m = match(name, '^Client')
		if m != -1 && !prefixRemoved
			let name = strcharpart(name, len('Client'))
			let prefixRemoved = v:true
		endif
		let m = match(name, '^Server')
		if m != -1 && !prefixRemoved
			let name = strcharpart(name, len('Server'))
		endif
	endif
	if !has_key(a:dd[0], 'p')
		" Handle top-level separately
		" TODO: Can generalize this into 'abstract or non-sealed' procedure
		let dsh = {
			\ 'd': a:d,
			\ 'ddmap': a:ddmap,
			\ 'sNode': a:template.definitions['Component'],
			\ 'sDefinitions': a:template.definitions,
			\ 'sDefs': a:template['$defs'].definitions,
		\ }
		call s:ParseDataFields(dsh, a:dd, v:false)
	elseif !(isRegisterComponent || isComponentProtoName)
		" Doesn't have [RegisterComponent] or [ComponentProtoName]
		"  Add to list to pull from later if no name conflicts
		" Only add if sealed to avoid duplicates:
		if a:dd[0].h ==# 'sealed'
			if has_key(a:unRegistered, name)
				echom 'duplicate in unRegistred: '.name.' l isSealed: '.a:dd[0].h.' r isSealed: '.a:unRegistered[name][0].h
			endif
			let a:unRegistered[name] = a:dd
		endif
	else
		if index(values(a:ddmap), name) == -1
			let a:ddmap[a:dd[0].s.'.'.a:dd[0].n] = name
			call s:BuildComponentDefinitionInner(a:d, a:ddmap, a:template, a:dd, name)
		else
		endif
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

function s:BuildComponentDefinitionInner(d, ddmap, template, dd, name)
	let name_yaml = a:name
	" Use C prefix in schema to prevent name conflict with prototypes
	let name_schema = 'C'.name_yaml
	let [scope, scopeStr, description] = s:BuildDescription(a:dd, name_yaml)
	call add(a:template.definitions.Component.properties.type.enum, name_yaml)
	call add(a:template.definitions.ComponentRegistry.items.oneOf, {'$ref': '#/definitions/'.name_schema})
	" Don't parse parent 'Component' datafields
	" Each parent should be parsed on its own and enums set up
	"let ScanComponentParents = {j -> empty(has_key(j[0], 'p') && j[0].p[0].n !=# 'Component' ? ScanComponentParents(j[0].p) : 0 ) ? ExtendDf(j) : ExtendDf(j) }
	"call ScanComponentParents(a:dd)
	let a:template.definitions[name_schema] = {
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
	let dsh = {
		\ 'd': a:d,
		\ 'ddmap': a:ddmap,
		\ 'sNode': a:template.definitions[name_schema],
		\ 'sDefinitions': a:template.definitions,
		\ 'sDefs': a:template['$defs'].definitions,
	\ }
	call s:ParseDataFields(dsh, a:dd, v:true)
endfunction

function! s:BuildExplicitDataDefinition(d, ddmap, template, dd)
	let name_yaml = a:dd[0].n
	" Use E prefix in schema to prevent name conflict with prototypes/others
	let name_schema = 'E'.name_yaml
	let [scope, scopeStr, description] = s:BuildDescription(a:dd, name_yaml)
	" Use the first scope... there may be more though...
	let qname = scope[0].'.'.a:dd[0].n
	if has_key(a:ddmap, qname)
		if a:ddmap[qname] !=# name_schema
			echom 'anomaly in BuildExplicitDataDefinition '.qname.
				\ ' already in ddmap as '.a:ddmap[qname].' but doesn't match '.
				\ name_schema
		else
			return
		endif
	endif
	let a:ddmap[qname] = name_schema

	let a:template['$defs'].definitions[name_schema] = {
		\ '$id': '#/$defs/definitions/'.name_schema,
		"\ 'description': description,
		\ 'type': 'object',
		\ 'additionalProperties': v:false,
		\ 'properties': {
		\ }
	\ }
	let dsh = {
		\ 'd': a:d,
		\ 'ddmap': a:ddmap,
		\ 'sNode': a:template['$defs'].definitions[name_schema],
		\ 'sDefinitions': a:template.definitions,
		\ 'sDefs': a:template['$defs'].definitions,
	\ }
	call s:ParseDataFields(dsh, a:dd, v:false)
endfunction

function! s:BuildImplicitDataDefinition(d, ddmap, template, dd)
endfunction

" Parses the datafields (df) for definition (dn) and stores in template
" Params:
"  [in] dsh: Dict containing the following:
"    [in] d: Definitions parsed from ctags file(s)
"    [in/out] ddmap: Qualified name to definition name mapping
"    [in/out] sNode: The schema node to put datafields/required on
"    [in/out] sDefinitions: '#/definitions/' for lookups
"    [in/out] sDefs: The schema node to put datafield types on
"     either '#/definitions/' or '#/$defs/definitions'
"  [in] dd: List of partials
"  [in] recursive: ???
function! s:ParseDataFields(dsh, dd, recursive)
	let isAbstract = v:false
	if a:recursive && has_key(a:dd[0], 'h') && a:dd[0].h ==# 'abstract'
		if a:dd[0].s.'.'.a:dd[0].n ==# 'Robust.Shared.GameObjects.Component'
			let isAbstract = v:true
			" For now only consider Component to be abstract, later we'll fix
			"  We inherit the properties of the parent using empty {}
		endif
	endif
	let required = []
	let traversed_parents = []
	let emptyD = {}
	for partial in a:dd
		for df in partial.df
			let dfn = s:ParseDataFieldName(partial, df)
			if !isAbstract && has_key(df.a, 'r')
				call add(required, dfn)
			endif
			" If isAbstract we're putting the abstract properties as {} on the
			"  inheriting definition
			let a:dsh.sNode.properties[dfn] = (isAbstract ? {} : s:map_type(a:dsh, df))
		endfor
		" TODO: I think required being duplicated in both e.g.
		"  StackComponent and ClothingComponent is because the required
		"  DataField is on the Shared ancestor, and because the partial list
		"  includes (Client|Server)StackComponent : SharedStackComponent,
		"  the solution seems to be... either proper abstraction, or if
		"  we already traversed this parent then don't do it again
		if a:recursive && has_key(partial, 'p')
			let check = partial.p[0].s.'.'.partial.p[0].n
			if index(traversed_parents, check) != -1
				continue
			endif
			call add(traversed_parents, check)
			call s:ParseDataFields(a:dsh, partial.p, a:recursive)
		endif
	endfor
	if !empty(required)
		if !has_key(a:dsh.sNode, 'required')
			let a:dsh.sNode.required = []
		endif
		let a:dsh.sNode.required += required
	endif
endfunction

function! s:BuildColorDefinition(d, template)
	let colors = map(copy(a:d.colors), {i,j -> j.n})
	let a:template.definitions.Color.oneOf[0].enum = colors
endfunction

" TODO: Make integer schema types with minimum/maximum
let s:typemap_simple = {
	\	'string': 'string',
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

let s:typemap_customTypeSerializer = {
	\ '\V\^FlagSerializer<\(\.\+\)>\$': 'FlagSerializer',
	\ '\V\^ConstantSerializer<\(\.\+\)>\$': 'ConstantSerializer',
	\ '\V\^PrototypeIdSerializer<\(\.\+\)>\$': 'PrototypeIdSerializer',
	\ '\V\^PrototypeIdListSerializer<\(\.\+\)>\$': 'PrototypeIdListSerializer',
	\ '\V\^PrototypeIdHashSetSerializer<\(\.\+\)>\$': 'PrototypeIdHashSetSerializer',
	\ '\V\^PrototypeIdDictionarySerializer<\(\[^,]\{-}\), \?\(\.\*\)>\$': 'PrototypeIdDictionarySerializer',
	\ '\V\^AbstractPrototypeIdArraySerializer<\(\.\+\)>\$': 'AbstractPrototypeIdArraySerializer',
	\ '\V\^NPCBlackboardSerializer\$': 'NPCBlackboardSerializer',
	\ '\V\^HTNTaskListSerializer\$': 'HTNTaskListSerializer',
	\ }

" Handles PrototypeFlags<IPrototype>, not a customTypeSerializer
let s:typemap_Prototype = {
	\ '\V\^PrototypeFlags<\(\.\+\)>\$': 'PrototypeIdHashSetSerializer',
	\ }

" TODO: Dictionary pattern won't resolve 'Dictionary<(string, string), string>' right
let s:typemap_recurse = {
	\ '\V\^\(\.\*\)\[\(\.\*\)\]\$': 'narray',
	\ '\V\^List<\(\.\*\)>\$': 'array',
	\ '\V\^IReadOnlyCollection<\(\.\*\)>\$': 'array',
	\ '\V\^IReadOnlyList<\(\.\*\)>\$': 'array',
	\ '\V\^ImmutableList<\(\.\*\)>\$': 'array',
	\ '\V\^Dictionary<\(\.\{-}\), \?\(\.\*\)>\$': 'dict',
	\ '\V\^SortedDictionary<\(\.\{-}\), \?\(\.\*\)>\$': 'dict',
	\ '\V\^HashSet<\(\.\*\)>\$': 'set',
	\ '\V\^(\(\[^,]\+\%( \[^,]\+\)\?\)\%(, \?\)\(\[^,]\+\%( \[^,]\+\)\?\)\%(, \?\(\[^,]\+\%( \[^,]\+\)\?\)\)\?\%(, \?\(\[^,]\+\%( \[^,]\+\)\?\)\)\?\%(, \?\(\[^,]\+\%( \[^,]\+\)\?\)\)\?)\$': 'tuple',
	\ }

let s:typemap_replace = {
	\	'ComponentRegistry': '',
	\	'char': '',
	\	'ResourcePath': '',
	\	'SpriteSpecifier': '',
	\	'Color': '',
	\	'EntityUid': '',
	\ 'GridId': '',
	\ 'Enum': '',
	\	'Regex': '',
	\	'Angle': '',
	\	'FixedPoint2': '',
	\	'FormattedMessage': '',
	\	'Box2': '',
	\	'TimeSpan': '',
	\	'Vector2': '',
	\	'Vector2i': '',
	\	'Vector4': '',
	\	'IAlertClick': '',
	\	'IConstructionCondition': '',
	\	'IObjectiveCondition': '',
	\	'IObjectiveRequirement': '',
	\	'IGasReactionEffect': '',
	\	'IHolidayCelebrate': '',
	\	'IHolidayGreet': '',
	\	'IHolidayShouldCelebrate': '',
	\	'IParallaxTextureSource': '',
	\	'IPhysShape': '',
	\	'IThresholdBehavior': '',
	\	'IThresholdTrigger': '',
	\	'ITileReaction': '',
	\	'IGraphAction': '',
	\	'IGraphCondition': '',
	\	'IWireAction': '',
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
"  [in] dsh: a dict containing the following:
"    [in] d: The {p, i, e, G, ..., yids} definitions built by
"     parseCS+parseYAML from the ctags file(s)
"    [in/out] ddmap: dictionary, maps qualified name (scope.name)
"     to $defs name to avoid conflicts and not have long names in $defs
"    [іn/out] sDefinitions: is '$defs' element of schema, an output
"    [in/out] sDefs: is '$defs' element of schema, an output
"     $defs contain [DataField]s, definitions contain [DataDefinition]s
"  [in] df.t: the type being resolved, e.g. 'Dictionary<string, int?>?[]'
"   When df only has t key it means there is no ѕcope, it is a lookup
"   only on type, so ideal to have all [DataDefinition]s defined already
" Returns:
"  Dictionary representing type element in schema { type: ... }
function! s:map_type(dsh, df)
	let customTypeSerializerNotice = has_key(a:df, 'a') && has_key(a:df.a, 'c') ? nr2char(13).'C# customTypeSerializer: '.a:df.a.c : ''
	let description = 'C# def: '.a:df.t.(has_key(a:df, 'n') ? ' '.a:df.n.
		\ (has_key(a:df, 'd') && !empty(a:df.d) ? ' = '.a:df.d : '') : '').
		\ customTypeSerializerNotice

	" Handle s:typemap_Prototype, overlap with s:typemap_customTypeSerializer
	for [k,v] in items(s:typemap_Prototype)
		let m = matchlist(a:df.t, k)
		if !empty(m)
			return s:HandleSerializerTypemap(a:dsh, v, m, description)
		endif
	endfor

	" Handle customTypeSerializer first, it overrides df's type
	if has_key(a:df, 'a') && has_key(a:df.a, 'c')
		for [k,v] in items(s:typemap_customTypeSerializer)
			let m = matchlist(a:df.a.c, k)
			if !empty(m)
				return s:HandleSerializerTypemap(a:dsh, v, m, description)
			endif
		endfor
	endif

	let nullable = v:false
	let rt = a:df.t
	let m = matchlist(a:df.t, s:is_nullable)
	if len(m) > 0
		let nullable = v:true
		let rt = m[1]
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
				let domain = s:map_type(a:dsh, {'t':m[1]})
				let range = s:map_type(a:dsh, {'t':m[2]})
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
					if !has_key(a:dsh.sDefs, enumName)
						let a:dsh.sDefs[enumName] = domain
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
					let t1t = {'t':t1[0]}
					if len(t1) > 1
						let t1t.n = t1[1]
					endif
					let t2t = {'t':t2[0]}
					if len(t2) > 1
						let t2t.n = t2[1]
					endif
					"echom 'see t1t: '.string(t1t).' t2t: '.string(t2t)
					let domain = s:map_type(a:dsh, t1t)
					let range = s:map_type(a:dsh, t2t)
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
				\ 'items': s:map_type(a:dsh, {'t':m[1]}),
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

	let scopeSuffix = ''
	let m = matchlist(a:df.t, s:has_scope)
	if len(m) > 0
		let scopeSuffix = escape(m[1], '.').'$'
		let rt = m[2]
	endif

	if has_key(s:typemap_simple, rt)
		"return { 'type': (nullable?'nullable-':'').s:typemap_simple[rt] }
		let tt = s:typemap_simple[rt]
		if !empty(scopeSuffix)
			echom 'unhandled scope '.scopeSuffix.' for type '.tt
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
				\ 'allOf': [{ '$ref': '#/definitions/nullable-'.s:typemap_simple[rt] }],
				\ "description": description
				\ }
		else
			return {
				\ 'type': s:typemap_simple[rt],
				\ 'description': description
				\ }
		endif
	endif

	" When something isn't easily deduced via algorithm on ctags data,
	"  it is preferrable to use a shim in the template.json file,
	"  These are those types
	if has_key(s:typemap_replace, rt)
		if !empty(scopeSuffix)
			" TODO: validate against scopeSuffix
			echom 'unhandled scope '.scopeSuffix.' for type '.rt
		endif
		return {
			\ '$ref': '#/definitions/'.rt,
			\ 'description': description
			\ }
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
	"if index(values(a:dsh.ddmap), t) != -1
	"	if has_key(a:dsh.sDefinitions, 'C'.t)
	"		echom 'found '.t.'Component in definitions'
	"		return {
	"			\ '$ref': '#/definitions/C'.t,
	"			\ 'description': description
	"			\ }
	"	elseif has_key(a:dsh.sDefinitions, t)
	"		echom 'found '.t.' in definitions'
	"		return {
	"			\ '$ref': '#/definitions/'.t,
	"			\ 'description': description
	"			\ }
	"	elseif has_key(a:dsh.sDefs, t)
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

	" Search a:dsh.d.i a:dsh.d.e for [DataDefinition] of name t
	" NOTE: At present doesn't parse for usings within the prototype,
	"  No [DataDefinitions] have same name I hope?
	for e in a:dsh.d.e
		if e[0].n ==# rt && e[0].s =~# scopeSuffix
			let n = s:resolvename(a:dsh, e, 'E')
			return {
				\ '$ref': '#/$defs/definitions/'.n,
				\ 'description': description
				\ }
		endif
	endfor

	" Might be best to do a map where the v:true sets the side effect:
	"{ '$ref': '#/$defs/definitions/'.s:resolvename(a:dsh, k, 'I') }
	"let S = {i -> filter(copy(i), {_,k -> k[0].n ==# rt ? v:true : (has_key(k[0], 'c') ? S(k[0].c) : v:false)})}
	"let g:r = S(a:dsh.d.i)
	"if g:r != v:false
	"	return g:r
	"endif

	" This filter results in branch of a:dsh.d.i where match is leaf node (in child list or first element), could augment the v:true branch to have side effect:
	"let M = {i -> filter(copy(i), {_,j-> j[0].n ==# rt ? v:true : (has_key(j[0], 'c') ? len(g:M(j[0].c))>0 : v:false)})}
	"let g:r = M(a:dsh.d.i)
	"if !empty(g:r)
		" Is on branch in r
	"endif

	" More efficient than the filter may be to do a for loop to avoid
	"  extra processing, yet isn't parallel. Maybe map is best.
	let r = s:Scan(a:dsh, a:dsh.d.i, rt, scopeSuffix)
	if type(r) == v:t_dict
		return r
	endif

	" Now, we still don't know the type, try an enum:
	for g in a:dsh.d.G
		if g.n ==# rt && g.s =~# scopeSuffix
			return {
				\ 'type': 'string',
				\ 'enum': reduce(g.e, {l,i -> add(l, i.n)}, []),
				\ 'description': '[Enum]'.description,
				\ }
		endif
	endfor

	" Might be a Prototype ID, e.g. LatheRecipePrototype
	for p in a:dsh.d.p
		if p[0].n ==# rt && p[0].s =~# scopeSuffix
			" Name is e.g. '#/$defs/definitions/ENUM_'.LatheRecipePrototype
			return s:HandleSerializerTypemap(a:dsh, 'PrototypeIdSerializer',
				\ [rt, rt, ''], description)
		endif
	endfor

	" At this point it appears to be a [Serializable] class or struct,
	"  With no [DataField] members.
	" e.g. CargoOrderData, EntityCoordinates, BoundKeyFunction,
	"  MapId, GridId, other shims
	"  Must parse constructor parameters I think
	" Or it is a record, e.g. DecalGridChunkCollection, MagnetState
	" IReadOnlyList appears to be list of entity IDs?
	" SpriteSpecifier is an [Serializable] abstract class
	" customTypeSerializer may shed light on type?
	return {
		\ 'type': 'object',
		\ 'description': 'unknown-'.description
		\ }
endfunction

" Defines serializer types in $defs with supporting ENUM_ if necessary
" params:
"  [in] dsh: see caller
"  [in] v: type category, see values of e.g. s:typemap_customTypeSerializer
"  [in] m: matchlist of type on pattern k in caller
"  [in] description: description of schema type
" returns: schema type derrived from v and m
function s:HandleSerializerTypemap(dsh, v, m, description)
	let defstr = a:v.
		\ (!empty(a:m[1]) ? '_'.a:m[1] : '').
		\ (!empty(a:m[2]) ? '_'.a:m[2] : '')
	let ret = {
		\ '$ref': '#/$defs/definitions/'.defstr,
		\ 'description': a:description,
		\ }
	" Need to define defstr in a:dsh.sDefs if not there
	if has_key(a:dsh.sDefinitions, defstr) || has_key(a:dsh.sDefs, defstr)
		"echom 'found customTypeSerializer(...) '.defstr.' in $defs'
	elseif a:v ==# 'NPCBlackboardSerializer' || 'HTNTaskListSerializer'
		" These are not parametric so are in template.json
		" TaskList appears to be either a mapping or string or array
	elseif a:v ==# 'FlagSerializer' || a:v ==# 'ConstantSerializer'
		" If len(G) > 1 we merge enums together,
		"  possible if > 1 Content assembly
		let enum = []
		let enumNames = []
		let Gsets = {
			\ 'FlagSerializer': a:dsh.d.FF,
			\ 'ConstantSerializer': a:dsh.d.CF,
			\ }

		for G in Gsets[a:v][a:m[1]]
			call add(enumNames, G.n)
			call extend(enum, reduce(G.e, {l,i -> add(l, i.n)}, []))
		endfor
		let ctdesc = 'C# Union('.join(enumNames, ',').')={'.join(enum, ',').'}'
		let enumName = 'ENUM_'.a:m[1]
		let a:dsh.sDefs[enumName] = {
			\ 'enum': enum,
			\ 'type': 'string',
			\ }
		let enumRef = {
			\ '$ref': '#/$defs/definitions/'.enumName,
			\ }
		if a:v ==# 'FlagSerializer'
			let a:dsh.sDefs[defstr] = {
				\ 'type': 'array',
				\ 'uniqueItems': v:true,
				\ 'items': {
					\ 'allOf': [ enumRef ],
					\ },
				\ }
		elseif a:v ==# 'ConstantSerializer'
			let a:dsh.sDefs[defstr] = {
				\ 'allOf': [ enumRef ],
				\ }
		endif
	else
		let lookup = a:m[1]
		if a:v ==# 'PrototypeIdDictionarySerializer'
			let lookup = a:m[2]
		endif
		" TODO: Oh... d.yids is yaml type but we need to convert
		"  those from C# types, e.g. EntityPrototype->entity
		let match = filter(copy(a:dsh.d.p), {_,j -> j[0].n ==# lookup})
		" Find lookup in a:dsh.d.yids
		if len(match) == 1
			for partial in match[0]
				if has_key(partial, 'yn')
					let lookup = partial.yn
					break
				endif
			endfor
		else
			echom 'Failed to find C# prototype specified in '.m[0]
		endif
		if has_key(a:dsh.d.yids, lookup)
			let enum = a:dsh.d.yids[lookup]
		else
			echom 'lookup failure in yids for '.lookup
			let enum = []
		endif
		let enumName = 'ENUM_'.lookup
		let a:dsh.sDefs[enumName] = {
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
		elseif a:v ==# 'PrototypeIdListSerializer'
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
		elseif a:v ==# 'PrototypeIdDictionarySerializer'
			let stype['type'] = 'object'
			" IDK why, but the C# type is backwards from YAML
			" e.g. Dictionary<FixedType2, FooBarPrototype> maps in YAML to
			" FooBarPrototypeID: FixedType2
			let domain = {
				\ 'allOf': [ enumRef ],
				\ }
			let stype['propertyNames'] = domain
			" Need to get the type for a:m[1]
			"  Assuming it's in C# and not yaml ids?
			let range = s:map_type(a:dsh, {'t':a:m[1]})
			" TODO: with domain/range swap idk if this condition valid
			if has_key(range, 'enum') || a:m[1] !~# '^string$'
				let stype['additionalProperties'] = range
			endif
		else
			echom 'anomaly in customTypeSerializer/PrototypeID handling, '.
				\ 'unhandled case: '.a:v
		endif
		let a:dsh.sDefs[defstr] = stype
	endif
	return ret
endfunction

" params:
"  [in/out] dsh: See s:ParseDataFields() and s:map_type()
function! s:Scan(dsh, l, t, scopeSuffix)
	" For each partial list in list a:l
	for i in a:l
		if i[0].n ==# a:t && i[0].s =~# a:scopeSuffix
			return { '$ref': '#/$defs/definitions/'.s:resolvename(a:dsh, i, 'I') }
		endif
		if has_key(i[0], 'c')
			" Bug is here, c is a list of lists: FIXED, NOPE NOT FIXED
			"for c in i[0].c
				let r = s:Scan(a:dsh, i[0].c, a:t, a:scopeSuffix)
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

" Builds the definition in the schema and resolves for name conflicts
"  Maps a C# qualified type (with scope) to YAML definition (schema) name (not yaml name)
" Example:
"  { '$ref': '#/$defs/definitions/'.s:resolvename(a:dsh, i, 'E') }
" Params:
"  [in/out] dsh: See s:ParseDataFields() and s:map_type()
"  [in] dd: A list of partials
"  [in] prefix: e.g. 'E', 'P', 'I' for better disambiguation
" Returns:
"  name of dd in ddmap, e.g.:
"  'Content.Client.Actions.Assignments.ActionAssignments' -> 'ActionAssignments'
function! s:resolvename(dsh, dd, prefix)
	let name = a:dd[0].n
	let qname = a:dd[0].s.'.'.name
	let tryname = a:prefix.name
	if has_key(a:dsh.ddmap, qname)
		return a:dsh.ddmap[qname]
	else
		" Just use a number to resolve name conflicts
		let i = 0
		while has_key(a:dsh.sDefs, tryname)
			let tryname = a:prefix.name.string(i)
			let i = i + 1
		endwhile
		let a:dsh.ddmap[qname] = tryname
		call s:builddef(a:dsh, a:dd, tryname)
		return tryname
	endif
endfunction

" Makes the schema definition in $defs
" params:
"  [in/out] dsh: See s:ParseDataFields() and s:map_type()
"  [in] name: yaml schema name of the DataDefinition
"   e.g. ESeedData (ExplicitDD), ISoundSpecifier (ImplicitDD)
function! s:builddef(dsh, dd, name)
	let def = {
		\	'type': 'object',
		\ 'description': '[BuildDef]C# type: '.a:dd[0].n.nr2char(13).'C# scope: '.a:dd[0].k.'.'.a:dd[0].s,
		\	'additionalProperties': v:false,
		\	'properties': {}
		\ }
	"for partial in a:dd
	"	for df in partial.df
	"		let dfn = s:ParseDataFieldName(partial, df)
	"		let def.properties[dfn] = s:map_type(a:dsh, df)
	"	endfor
	"endfor
	let new_dsh = copy(a:dsh)
	let new_dsh.sNode = def
	call s:ParseDataFields(new_dsh, a:dd, v:true)
	" TODO: abstract class enums
	"let n = a:dd
	"while has_key(n[0], 'p')
		"let n = n[0].p
		"" Put parent dfs onto child nodes for now
		"call s:ParseDataFields(new_dsh, n)
		""for partial in n
		""	for df in partial.df
		""		let dfn = s:ParseDataFieldName(partial, df)
		""		" Why is this printed on components? Oh... non-components
		""		" TODO: After putting implicit/explicit datadefinition in defs, uncomment:
		""		"echom 'found child df ('.dfn.') in parent ('.partial.n.')'
		""		let def.properties[dfn] = s:map_type(a:dsh, df)
		""	endfor
		""endfor
	"endwhile
	let a:dsh.sDefs[a:name] = def
endfunction

" Returns datafield name
function! s:ParseDataFieldName(partial, df)
	if has_key(a:df.a, 'nn')
		return a:df.a.nn
	elseif has_key(a:df.a, 'sym')
		echom 'failure to resolve df sym earlier'
		return a:df.a.sym
	else
		" I guess it empty
		return a:df.a.t
	endif
	" Old stuff:
	let n = a:df.nn
	let m = matchlist(n, '\V\^"\(\.\+\)"\$')
	if !empty(m)
		let s = m[1]
	else
		" We have no e.g. [DataField("name")], but [DataField(Node)]
		"  It is a const member of the container, in same partial
		for m in a:partial.m
			if m.n ==# n
				echom 'Found DataField member '.s.' with value '.m.d
				let s = m.d
				break
			endif
		endfor
	endif
	return s
endfunction

function! robusttoolbox#parseCS() abort
	let tf = b:gutentags_files.ctags
	let t1 = reltime()
	echom '[robusttoolbox] Parsing [Prototype]s'
	let p = s:get_Prototypes(tf)
	echom '[robusttoolbox] Parsing explicit [DataDefinition]s'
	let e = s:get_ExplicitDataDefinitions(tf)
	echom '[robusttoolbox] Parsing implicit [DataDefinition]s'
	let i = s:get_ImplicitDataDefinitions(tf)
	let ts1 = reltimestr(reltime(t1))
	echom '[robusttoolbox] Parsing [Prototype]s completed in '.ts1.' seconds'
	" 95.45s with mtables and attribute tags, 7.6x slower than mlines
	let t2 = reltime()
	call s:get_DataFields(tf, p, i, e)
	let ts2 = reltimestr(reltime(t2))
	echom '[robusttoolbox] Parsing+Processing [DataField]s completed in '.ts2.' seconds'
	let t2_1 = reltime()
	call s:get_AbstractDataFields(tf, p)
	call s:get_ParentDataFields(tf, p)
	let ts2_1 = reltimestr(reltime(t2_1))
	echom '[robusttoolbox] Parsing+Processing [AbstractDataField][ParentDataField]s completed in '.ts2_1.' seconds'
	let t3 = reltime()
	let G = s:get_Enums(tf)
	let ts3 = reltimestr(reltime(t3))
	echom '[robusttoolbox] Parsing enums completed in '.ts3.' seconds'
	let colors = s:get_Colors(tf)
	"May be faster knowing name rather than querying for all kind:G first
	" Then kind:E
	let t4 = reltime()
	let FF = s:get_FlagsFor(tf, G)
	let CF = s:get_ConstantsFor(tf, G)
	let ts4 = reltimestr(reltime(t4))
	echom '[robusttoolbox] Parsing [FlagsFor]&[ConstantsFor] completed in '.ts4.' seconds'
	return {'i':i, 'e':e, 'p':p, 'G':G, 'colors':colors, 'FF':FF, 'CF':CF}
endfunction

" Parse YAML prototype ids
function! robusttoolbox#parseYAML() abort
	let tf = b:gutentags_files.ctags
	let t1 = reltime()
	let yids = s:get_YAMLids(tf)
	let ts1 = reltimestr(reltime(t1))
	echom '[robusttoolbox] Parsing YAML prototype ids completed in '.ts1.' seconds'
	return {'yids': yids}
endfunction

function! robusttoolbox#filtertags(tf, f) abort
	let ret = systemlist('readtags -t '.a:tf." -eE -Q '".a:f."' -l")
	if v:shell_error
		echohl ErrorMsg
		echom '[robusttoolbox] readtags error: '.v:shell_error.' filter:'''.a:f.''''
		echohl None
	endif
	return ret
endfunction

function! s:get_YAMLids(tf)
	" Step 1: Query id tags
	let t = s:splitup_id(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "D")))'))
	let g:t = t
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
function! s:get_ImplicitDataDefinitions(tf)
	" Step 1: Query attribute tags
	let roots = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )ImplicitDataDefinitionForInheritors(,|\])/ $name) $kind (eq? $kind "A")))'))
	" Step 2: for each attribute tag: query the class/interface tags
	" Gets scope-name from attribute of class to match class scope
	let roots = map(map(roots, {_,j -> robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") $kind (#/(C|I)/ $kind) (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')}), {_,j -> s:splitup_class_struct(j)})
	" Step 3: for each class get any child classes recursively
	"  'c' element only on first element of list of class tag dictionaries
	call map(roots, {_,j -> map(j, {k,l -> k != 0 ? l : (has_key(l, 'h') && l.h ==# 'sealed' ? l : extend(l, {'c': s:get_Children(a:tf, l.n, j)}))})})
	" Step 4: Query ComponentProtoName attributes
	let compProtoName = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )ComponentProtoName\(/ $name) $kind (eq? $kind "A"))'))
	" Step 5: Find component
	let component = filter(copy(roots), {i,j -> j[0].n ==# 'Component'})[0]
	" Step 6: Assign each [ComponentProtoName] attribute to their component
	"  NOTE: Since (Content|Robust).Client are filtered out, doesn't see
	"   ClientEntitySpawner/ClientEntitySpawnerComponent
	"execute('echom "hi"', '')
	let CheckScope = {j,a -> map(copy(j), {_,m -> m.s.'.'.m.n ==# a.s ? add(m.a, a.n) : 0})}
	let ScanComponentChildren = {j,a -> empty(has_key(j[0], 'c') ? map(copy(j[0].c), {k,l -> ScanComponentChildren(l, a)}): 0) ? CheckScope(j, a) : CheckScope(j, a)}
	for a in compProtoName
		"echom 'ComponentProtoName: '.a.t
		call ScanComponentChildren(component, a)
	endfor
	" Step 7: Query RegisterComponent attributes
	let registerComponent = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )RegisterComponent(,|\])/ $name) $kind (eq? $kind "A"))'))
	" Step 8: Assign each [RegisterComponent] attribute to their component
	for a in registerComponent
		call ScanComponentChildren(component, a)
	endfor
	return roots
endfunction

" Gets all [DataDefinition] classes in a list, no children
function! s:get_ExplicitDataDefinitions(tf)
	" Step 1: Query attribute tags
	let roots = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )DataDefinition(,|\])/ $name) $kind (eq? $kind "A"))'))

	" Step 2: for each attribute tag get the relevant class/struct tags
	return map(map(roots, {_,j -> robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") $kind (eq? $kind "'.(j.k==#'class'?'C':(j.k==#'struct'?'S':'Undefined')).'") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')}), {_,j -> s:splitup_class_struct(j)})

	" I could split Step 2 above into two queries: one for each kind!:
	"return s:splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[(.+, )?DataDefinition(,|\])/ ($"at")))')) +
	"\ s:splitup_struct(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "S") (#/\[(.+, )?DataDefinition(,|\])/ ($"at")))'))
endfunction

" Gets all [Prototype("...")] classes in a list, no children
" All are classes, no structs. No children???
" list of list of dicts
" But where to put the prototype parameters like name?
" Also see: [DataField("name", required=true, serverOnly=true,...)]
" Store the split up attribute first in sublist? Yes, why not!
function! s:get_Prototypes(tf)
	let t1 = reltime()
	"let p = s:splitup_class(robusttoolbox#filtertags(a:tf, '(and (#/\[(.+, )?Prototype\(\"[[:alpha:]]+\"\)/ ($"at")) $kind (eq? $kind "C"))'))
	" Step 1: Query attribute tags
	let a = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )Prototype\(\"/ $name) $kind (eq? $kind "A"))'))

	" Step 2: For each attribute tag: query for the class tags
	" Gets scope-name from attribute of class to match class scope
	let p = map(copy(a), {_,j -> map(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") $kind (eq? $kind "C") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))'), {_1,m -> s:splitup_class_i(m)})})
	for i in range(len(p))
		" Assign the text of the attribute to 'a':
		let p[i][0].a = a[i].n
		let p[i][0].yn = matchlist(a[i].n, '\V\.\{-}\%([\|,\| \)Prototype("\(\[^"]\+\)"')[1]
	endfor
	"call map(copy(p), {i,j -> execute('let '.j[0].p.' = '.string(a[i]))})})
	let ts = reltimestr(reltime(t1))
	echom '[robusttoolbox] Parsing [Prototype]s completed in '.ts.' seconds'
	" 14+ seconds on first run, then ~1/3 time afterwards
	" 5.3 seconds
	" 5.285 without matchlist()[1:-5] indexing on splitup_class_i
	" Wil be faster by not modifying list, but adding new dict value
	" Actually nope, same or .02s slower. copy(p) randomly faster 0.06s.
	return p
	"return map(map(p, {_,j -> [j] + robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") $kind (eq? $kind "C") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')}), {_,j -> type(j) == v:t_dict ? j : s:splitup_class(j)})
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

let s:matches = 0
" Checks to see if this df goes into this partial
"  called by get_DataFields
function! s:Z3(tf, partial, df)
	"let Z3 = {q,j -> q.s.'.'.q.n !=# j.s ? v:false : !empty(add(q.df, j))}
	if a:partial.f ==# a:df.f && a:partial.s.'.'.a:partial.n ==# a:df.s
		let s:matches += 1
		call add(a:partial.df, a:df)
		" Check to see if we need to lookup symbol
		if has_key(a:df.a, 'sym')
			let matches = robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.a:df.a.sym.'") (eq? $scope-name "'.a:df.a.s.'"))')
			if len(matches) != 1
				echom 'failure in get_DataFields symbol lookup'
				return v:true
			endif
			let m = s:splitup_member_property_i(matches[0])
			"echom 'For name '.a:df.a.sym.' (const string) found default '.m.d
			let ml = matchlist(m.d, '\V\^"\(\.\+\)"\$')
			if !empty(ml)
				let a:df.a.nn = ml[1]
			else
				echom 'failure to parse default for df symbol '.a:df.a.sym
				return v:false
			endif
			unlet a:df.a.sym
		endif
		return v:true
	endif
	return v:false
endfunction

function! s:get_ForInner(tf, G, forAttrib)
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )'.a:forAttrib.'\(/ $name) $kind (eq? $kind "A"))'))

	call map(a, {i,j -> s:Splitup_attribute_for(a:forAttrib, j)})

	" Step 2: Find the enums which this attribute applies to
	let ret = {}
	for at in a
		" G will have list of enums, with list of enumerators on e
		let m = filter(copy(a:G), {i,j -> j.s.'.'.j.n ==# at.s})
		if !len(m)
			echom 'error in get_ForInner, no type exists in G set: '.at.s
		else
			for t in at.tl
				"echom 'Found type '.t.' len instances of ['.a:forAttrib.'(typeof('.t.'))]: '.len(m)
				if !has_key(ret, t)
					let ret[t] = []
				endif
				call extend(ret[t], m)
			endfor
		endif
	endfor
	return ret
endfunction

function! s:get_FlagsFor(tf, G)
	return s:get_ForInner(a:tf, a:G, 'FlagsFor')
endfunction

function! s:get_ConstantsFor(tf, G)
	return s:get_ForInner(a:tf, a:G, 'ConstantsFor')
endfunction

" Gets all qualified [DataField] members/properties in a list
"  p are [Prototype]s, i are implicit [Datadefinition]s, and e explicit
function! s:get_DataFields(tf, p, i, e)
	"let m = s:splitup_member(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "M") (#/^\[(.+, )?DataField\(/ ($"at")))'))
	"let p = s:splitup_property(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "P") (#/^\[(.+, )?DataField\(/ ($"at")))'))
	let t1 = reltime()
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )DataField\(/ $name) $kind (eq? $kind "A"))'))

	call map(a, {i,j -> s:Splitup_attribute_datafield(j)})

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
	"let t = map(copy(a), {_,j -> map(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))'), {_1,m -> s:splitup_member_property_i(m)})})
	" TODO: Alternate of above, test for better time, also no sublist:
	let t = map(copy(a), {_,j -> s:splitup_member_property_i(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))')[0])})
	"for i in range(len(t))
	""	let t[i] = t[i][0]
	"	let t[i].a = a[i].n
	"endfor
	" TODO: Consistency for other attributes? To handle more?
	"call map(t, {i,j -> extend(j, {'a': [a[i]]})})
	call map(t, {i,j -> extend(j, {'a': a[i]})})
	let g:t = t

	let ts1 = reltimestr(reltime(t1))
	echom '[robusttoolbox] Parsing [DataField]s completed in '.ts1.' seconds'
	" 167.9s

	let t2 = reltime()
	" Step 3: Store [DataField] tags on prototypes and datadefinitions
	" KLUDGE: filtered out scope info on [DataField] tag, might need later
	" TODO: Need to add scope back to [DataField] attribs to find partials?
	" TODO: Need to add members to d.i and d.e to resolve DataField consts?

	"let Z = {q,j -> (q.s.'.'.q.n ==# j.s ? empty(add(q.df, {'t':j.t, 'k':j.k, 's':j.s, 'd':j.d, 'n':j.n, 'a':j.a})) : v:false)}
	" Alternate (no memcopy):
	"  Old pruned scope,etc off of DataField attribute
	"let Z2 = {q,j -> q.s.'.'.q.n !=# j.s ? q : (empty(add(q.df, {'t':j.t, 'k':j.k, 's':j.s, 'd':j.d, 'n':j.n, 'a':j.a})) ? q : q)}
	"let Z2 = {q,j -> q.s.'.'.q.n !=# j.s ? q : (empty(add(q.df, j)) ? q : q)}
	"For filter (see if faster) Filter is indeed faster!:
	"let Z3 = {q,j -> q.s.'.'.q.n !=# j.s ? v:false : !empty(add(q.df, j))}
	" TODO: will want to only put [DataField] on right partial for
	"  intelligent updating, but would need to compare $input file,
	"  for now put all [DataField]s on first partial
	" TODO: I now do all partials, fix change effects
	echom '[robusttoolbox] len attribs before prototypes: '.len(t)
	"call map(copy(t), {_,j -> map(copy(a:p), {_1,m -> Z2(m[0],j)})})
	"For filter (see if faster):
	call filter(t, {_,j -> !len(filter(copy(a:p), {_1,e -> len(filter(copy(e), {_2,p -> s:Z3(a:tf,p,j)}))}))})
	echom '[robusttoolbox] len attribs after prototypes: '.len(t)
	echom '[robusttoolbox] s:matches after prototypes: '.s:matches

	"call map(copy(t), {_,j -> map(copy(a:e), {_1,m -> map(copy(m), {_2,o -> Z(o,j)})})})
	call filter(t, {_,j -> !len(filter(copy(a:e), {_1,e -> len(filter(copy(e), {_2,p -> s:Z3(a:tf,p,j)}))}))})
	echom '[robusttoolbox] len attribs after explicits: '.len(t)
	echom '[robusttoolbox] s:matches after explicits: '.s:matches

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
	let R3 = {o,j -> len(filter(copy(o), {_,i -> len(filter(copy(i), {_1,p -> !empty(has_key(p,'c') ? R3(p.c, j) : v:false) || s:Z3(a:tf,p,j)}))}))}
	call filter(t, {_,j -> !R3(a:i, j)})
	echom '[robusttoolbox] len attribs after implicits: '.len(t)
	echom '[robusttoolbox] s:matches after implicits: '.s:matches

	let ts2 = reltimestr(reltime(t2))
	echom '[robusttoolbox] Processing [DataField]s completed in '.ts2.' seconds'
	" 6.84s only p,e, encoding flux
	" 5.2s only p,e, encoding flux
	" 6.0s only p,e, encoding flux, with Z lambda
	" ~33s with p,e,i
	" Ok with attribute tags its 8x slower due to complexity
	" query at, foreach at query matching c/s/m/p
	" Rather than query all c/s/m/p with [match] in attribute field
	" as |a| = |t|, O(t*t) queries vs O(t) and extra processing
endfunction


function! s:get_AbstractDataFields(tf, p)
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )AbstractDataField(Attribute)?(\]|,)/ $name) $kind (eq? $kind "A"))'))

	" [AbstractDataField] has Name of 'abstract' in ctor
	call map(a, {i,j -> extend(j, {'nn': 'abstract'})})

	" Step 2: For each attribute tag: query for the member/property tags
	" Gets scope-name from attribute of M/P tag to determine container
	" container scope, appears to only be in:
	"  The Prototype sublist, the attribute/scope element
	let t = map(copy(a), {_,j -> s:splitup_member_property_i(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))')[0])})
	call map(t, {i,j -> extend(j, {'a': a[i]})})

	" Step 3: For each member/property tag, put on attribute
	" TODO: will want to only put [DataField] on right partial for
	"  intelligent updating, but would need to compare $input file,
	"  for now put all [DataField]s on first partial
	" TODO: I now do all partials, fix change effects
	"echom '[robusttoolbox] len [AbstractDataField] before prototypes: '.len(t)
	call filter(t, {_,j -> !len(filter(copy(a:p), {_1,e -> len(filter(copy(e), {_2,p -> s:Z3(a:tf,p,j)}))}))})
	"echom '[robusttoolbox] len [AbstractDataField] after prototypes: '.len(t)
endfunction

function! s:get_ParentDataFields(tf, p)
	" Step 1: Query attribute tags:
	let a = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )ParentDataField(Attribute)?\(/ $name) $kind (eq? $kind "A"))'))

	" [ParentDataField] has Name of 'parent' in ctor
	call map(a, {i,j -> s:Splitup_attribute_parentdatafield(j)})

	" Step 2: For each attribute tag: query for the member/property tags
	" Gets scope-name from attribute of M/P tag to determine container
	" container scope, appears to only be in:
	"  The Prototype sublist, the attribute/scope element
	let t = map(copy(a), {_,j -> s:splitup_member_property_i(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") (eq? $scope-name "'.j.s.'"))')[0])})
	call map(t, {i,j -> extend(j, {'a': a[i]})})

	" Step 3: For each member/property tag, put on attribute
	" TODO: will want to only put [DataField] on right partial for
	"  intelligent updating, but would need to compare $input file,
	"  for now put all [DataField]s on first partial
	" TODO: I now do all partials, fix change effects
	"echom '[robusttoolbox] len [ParentDataField] before prototypes: '.len(t)
	call filter(t, {_,j -> !len(filter(copy(a:p), {_1,e -> len(filter(copy(e), {_2,p -> s:Z3(a:tf,p,j)}))}))})
	"echom '[robusttoolbox] len [ParentDataField] after prototypes: '.len(t)
endfunction

" KLUDGE: Assuming scope-kind on attribute will be enumeration
" Returns:
"  List of enums: {n:name, k:scope-kind, s:scope-name, [t:typename], e:[enumerators]}
"   typename: e.g. 'sbyte' in 'enum VentPipeDirection : sbyte'
"   enumerators: {n:name, v:value}
function! s:get_Enums(tf)
	" Step 1: Query attribute tags
	" EDIT: Actually, see SpeciesSkinColor, no attribute but is used
	"let roots = s:splitup_attribute(robusttoolbox#filtertags(a:tf, '(and (#/(\[|,| )Serializable(,|\])/ $name) $kind (eq? $kind "A") (eq? $scope-kind "enumeration"))'))

	" Step 2: for each attribute tag: query the enum tags
	" Gets scope-name from attribute of class to match class scope
	" There will never be same-named item in same scope, no partials!
	"call map(roots, {_,j -> s:splitup_enum_i(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.t.'") $kind (eq? $kind "G") (eq? $scope-name "'.matchlist(j.s, '\V\(\.\*\).\.\*\$')[1].'"))')[0])})

	" Step 1: Get enum tags
	let roots =  map(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "G"))'), {i,j -> s:splitup_enum_i(j)})

	" Step 3 (now 2): for each enum get every enumerator
	call map(copy(roots), {_,j -> extend(j.e, map(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "E") (eq? $scope-name "'.j.s.'.'.j.n.'"))'), {_1,l -> s:splitup_enumerator_i(l)}))})

	return roots
endfunction

function! s:get_Colors(tf)
	return s:splitup_member(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "M") (eq? $scope-name "Robust.Shared.Maths.Color") (eq? ($"type") "Color"))'))
endfunction

" For kind:D puts into {n:name, f:file, t:typename}
function! s:splitup_id(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:D\ttyperef:typename:\(\[^\t]\+\)')}), {_,j -> {'n': j[1], 'f': j[2], 't': j[3]}})
endfunction

" TODO: Might not need f:file on this?
" For kind:A puts into {n:name, k:scope-kind, s:scope-name, t:typename}
function! s:splitup_attribute(list)
	" Is Extended Posix Regex, so can't match nested parens IIRC
	"  Appears to be no performance benefit of not capturing groups
	" For Class attributes should prune suffix of scope-name after this call
	" For Class members should leave scope-name as-is
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\[^\t]\+\t\[^\t]\+\tkind:A\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttyperef:typename:\(\[^\t]\+\)')}), {_,j -> {'n': j[1], 'k': j[2], 's': j[3], 't': j[4]}})
endfunction

" For kind:C puts into {n:name, f:file, t:langtype, k:scope-kind, s:scope-name, i:inherits, h:[sealed|abstract], df:datafields, a:attributes}
" Operates on a list of list of strings
function! s:splitup_class(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:C\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^)]\*\))\tsl:\(\.\*\)\$')}), {_,j -> {'n': j[1], 'f':j[2], 't': 'C', 'k': j[3], 's': j[4], 'i': j[5], 'h': j[6], 'df': [], 'a': []}})
endfunction

" Same as splitup_class but for single string
function! s:splitup_class_i(str)
	let j = matchlist(a:str, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:C\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^)]\*\))\tsl:\(\.\*\)\$')
	return {'n': j[1], 'f': j[2], 't': 'C', 'k': j[3], 's': j[4], 'i': j[5], 'h': j[6], 'df': [], 'a': []}
endfunction

" For kind:S puts into {n:name, f:file, t:langtype, k:scope-kind, s:scope-name, i:inherits, df:datafields, a:attributes}
function! s:splitup_struct(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:S\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^)]\*\))\$')}), {_,j -> {'n': j[1], 'f': j[2], 't': 'S', 'k': j[3], 's': j[4], 'i': j[5], 'df': [], 'a': []}})
endfunction

" For kind:C|S|I combo queries, slower than one
" Suitable for when an attribute applies to either, perhaps
" Should benchmark to see if separating first is faster
" Had to modify sl: capture to be optional to catch both struct and class
function! s:splitup_class_struct(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\(\[CSI]\)\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^)]\*\))\%(\tsl:\(\[^$]\*\)\)\?\$')}), {_,j -> (j[3] ==# 'C' ? {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'h': j[7], 'df': [], 'a': []} : {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'df': [], 'a': []})})
endfunction

" Same as splitup_class_struct but for single string
function! s:splitup_class_struct_i(str)
	let j = matchlist(a:str, '\V\(\[^\t]\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\(\[CSI]\)\tscope:\(\[^:]\+\):\(\[^\t]\+\)\tinherits:(\(\[^)]\*\))\%(\tsl:\(\[^$]\*\)\)\?\$')
	return j[3] ==# 'C' ? {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'h': j[7], 'df': [], 'a': []} : {'n': j[1], 'f': j[2], 't': j[3], 'k': j[4], 's': j[5], 'i': j[6], 'df': [], 'a': []}
endfunction

" For kind:M puts into {n:name, f:file, k:scope-kind, s:scope-name, t:type, d:default}
function! s:splitup_member(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\w\+\)\t\[^\t]\+\t\[^\t]\+\tkind:M\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttype:\(\[^\t]\+\)\tdf:\(\.\*\)\$')}), {_,j -> {'n': j[1], 'k': j[2], 's': j[3], 't': j[4], 'd': j[5]}})
endfunction

" For kind:P puts into {n:name, k:scope-kind, s:scope-name, t:type, d:default}
"  Almost identical to splitup_member but regex compiled not to 'kind:M'
function! s:splitup_property(list)
	return map(map(a:list, {_,j -> matchlist(j, '\V\(\w\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:P\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttype:\(\[^\t]\+\)\tdf:\(\.\*\)\$')}), {_,j -> {'n': j[1], 'f': j[2], 'k': j[3], 's': j[4], 't': j[5], 'd': j[6]}})
endfunction

" For kind:M or kind:P puts into {n:name, f:file, k:scope-kind, s:scope-name, t:type, d:default}
"  Almost identical to splitup_member but regex compiled not to 'kind:M'
function! s:splitup_member_property_i(str)
	let j = matchlist(a:str, '\V\(\w\+\)\t\(\[^\t]\+\)\t\[^\t]\+\tkind:\.\tscope:\(\[^:]\+\):\(\[^\t]\+\)\ttype:\(\[^\t]\+\)\tdf:\(\.\*\)\$')
	return {'n': j[1], 'f': j[2], 'k': j[3], 's': j[4], 't': j[5], 'd': j[6]}
endfunction

" For kind:G puts into {n:name, k:scope-kind, s:scope-name, t:typename}
"  typename in tag only іf enum type specified,
"  e.g. 'enum VentPumpDirection : sbyte'
function! s:splitup_enum_i(str)
	let j = matchlist(a:str, '\V\(\w\+\)\t\[^\t]\+\t\[^\t]\+\tkind:G\tscope:\(\[^:]\+\):\(\[^\t]\+\)\%(\ttyperef:typename:\(\[^\t]\+\)\)\?\$')
	return {'n': j[1], 'k': j[2], 's': j[3], 't': j[4], 'e': []}
endfunction

" For kind:E puts into {n:name, v:value}
"  value may be empty
" k:scope-kind and s:scope-name are on containing enum so ignore
function! s:splitup_enumerator_i(str)
	" KLUDGE: v may be optional
	let j = matchlist(a:str, '\V\(\w\+\)\t\[^\t]\+\t\[^\t]\+\tkind:E\tscope:\[^:]\+:\[^\t]\+\tdf:\(\[^$]\*\)\$')
	return {'n': j[1], 'v': j[2]}
endfunction

" Params:
"  [in] tag: parent name
"  [in] pdd: parent partial list
function! s:get_Children(tf, tag, pdd)
	let parent = a:tag
	" Check for template parameters and replace with '<[^>]+>'
	"  TODO: Recursive... to handle nested angle brackets? and more params
	let Token = {s -> matchlist(s, '\V\(\[^<>]\+\)\(\.\*\)\?')}
	let m = Token(parent)
	if !empty(m[2])
		let parent = m[1].'<[^<]+>'
		"echom 'had a template parent: '.parent
	endif

	" Query for everything not in Client, later we'll merge in Client
	" TODO: IDK if pdd may have more than one scope... e.g. Client/Server
	let children = s:splitup_class_struct(robusttoolbox#filtertags(a:tf, '(and $kind (#/(C|I)/ $kind) (#/(^\(|,| )('.escape(a:pdd[0].s, '.').'\.)?'.parent.'( |,|\n|\)$)/ $inherits) (not (#/^(Content|Robust)\.Client/ $scope-name)))'))

	" For each child query all class/interface tag instances (i.e. partial)
	" as only one will have the $inherits populated which we queried above
	" Setting 'p' to a:pdd may make for an infinіte loop on printing
	"  Nope! Doesn't go down that rabbit hole and prints as [...]
	call map(children, {_,j -> map(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.n.'") $kind (#/(C|I)/ $kind) (eq? $scope "'.j.k.':'.j.s.'"))'), {_,k -> extend(s:splitup_class_struct_i(k), {'p': a:pdd})})})
	" List processing is slow sadly :(

	" Now we get children in client, and later merge into server
	"  This list should include some Components, and MobStates
	let clientChildren = s:splitup_class_struct(robusttoolbox#filtertags(a:tf, '(and $kind (#/(C|I)/ $kind) (#/(^\(|,| )('.escape(a:pdd[0].s, '.').'\.)?'.parent.'( |,|\n|\)$)/ $inherits) (#/^(Content|Robust)\.Client/ $scope-name))'))

" For each child query all class/interface tag instances (i.e. partial)
	"  as only one will have the $inherits populated which we have now
	call map(clientChildren, {_,j -> map(robusttoolbox#filtertags(a:tf, '(and (eq? $name "'.j.n.'") $kind (#/(C|I)/ $kind) (eq? $scope "'.j.k.':'.j.s.'"))'), {_,k -> extend(s:splitup_class_struct_i(k), {'p': a:pdd})})})
	for c in clientChildren
		" Match should be a list of a list of partials, but outer len==1
		let match = filter(copy(children), {_,j -> j[0].n ==# c[0].n})
		if len(match) > 1
			echom 'anomaly in get_Children, more than one sibling match, parent: '.parent.' child: '.c[0].n
		elseif len(match) == 0
			" No shared/server partials to merge client partials to
			" Put them into children
			call add(children, c)
		elseif len(match) == 1
			" Must merge the client partials to the shared/server partials
			let dd = match[0]
			if dd[0].h !=# 'sealed' || c[0].h !=# 'sealed'
				echom 'anomaly in get_Children, Client/Server merge class unsealed, Client children will not be parsed, server:'.match[0][0].n. ' client:'.c[0].n
				" Looks like ItemComponent isn't sealed... I may have to do
				"  this algorithm differently then
				" ItemComponent has both Client/Server classes, and each have
				"  a child class 'ClothingComponent'. Oh looks like this was
				"  fixed in master... need to update
			endif
			" Just add the partials together, the [DataField]s will merge
			"  This works since these classes have no children
			call extend(dd, c)
		endif
	endfor

	"return map(children, {_,j -> j[0].h !=# 'sealed' ? [extend(j[0], {'c': s:get_Children(a:tf, j[0].n)})] + j[1:] : j})
	return map(children, {_,j -> map(j, {k,l -> k != 0 ? l : (has_key(l, 'h') && l.h ==# 'sealed' ? l : extend(l, {'c': s:get_Children(a:tf, l.n, j)}))})})
endfunction
