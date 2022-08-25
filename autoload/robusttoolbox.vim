" Copyright (c) 2022 Rene.Descartes2021

" Builds JSON schema for SS14 YAML prototypes
" Parses ctags file `b:gutentags_files.ctags` using readtags
" Uses vimscript. Output is list of dictionaries representing each
"  [DataDefinition] class/structure.
" ctags generation takes ~2s for whole SS14 project
"  Optimization would be to use regex tables, could be ~0.2s for whole
"  vim-gutentags regens tags subset on BufWrite event, faster than whole
" Then this schema regen takes 12.39s on my system for just [DataDef..]
"  Many readtags serial readtags queries and processed in vimscript
"  Optimization would be to process data with pipes and perl/rust regex
"  Optimization would be to re-write in lua or rust
"  Optimization would be to write subschemas instead of whole on BufWrite
"   however yaml-language-server doesn't support subschemas at present
"  Consider optimizations in context of sharing backend between IDEs

" Parallel shell example:
"  Intermediate data are lost, necessary for output data structure
"  Consider hierarchy of [DataDefinition] and inheritance of [DataField]
"  Could possibly build flat list then process hierarchy later when
"   querying [DataField]s, or process hierarchy after than via inherits?
"  Note parallel shell technique may not readily translate to MS Windows
"readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "()") (#/(^\(|,) ?Component(,|\)$)/ $inherits))' -l | cut -f 1 | xargs -P0 -I {hi} readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "(sealed)") (#/(^\(|,) ?{hi}(,|\)$)/ $inherits))' -l

function! robusttoolbox#regen() abort
	let tf = b:gutentags_files.ctags
	let t1 = reltime()
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing [Prototype]s')
	let p = robusttoolbox#get_Prototypes(tf)
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing explicit [DataDefinition]s')
	let e = robusttoolbox#get_ExplicitDataDefinitions(tf)
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing implicit [DataDefinition]s')
	let i = robusttoolbox#get_ImplicitDataDefinitions(tf)
	let ts = reltimestr(reltime(t1))
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing [Prototype]s completed in '.ts.' seconds')
	return {'i':i, 'e':e, 'p':p}
endfunction

function! robusttoolbox#filtertags(tf, f)
	let ret = systemlist('readtags -t '.a:tf." -eE -Q '".a:f."' -l")
	if v:shell_error
		echohl ErrorMsg
		call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] readtags error: '.v:shell_error.' filter:'''.a:f.'''')
		echohl None
	endif
	return ret
endfunction

" Gets all [ImplicitDataDefinitionForInheritors] classes in a list
"  Top level list is [ImplicitDataDefinitionForInheritors] list
"  Any child classes will be in 'c' element as list
"  Children must be navigated recursively
"  [{t:tag a:'attribute list' i:'inherits list' s:'sealed|abstract' c:{...}, {...}...}]
"  's' and 'c' key won't exist on structures
"  'c' key won't exist when no children
"  's' key empty when not abstract or sealed
function! robusttoolbox#get_ImplicitDataDefinitions(tf)
	" Get [ImplicitDataDefinitionForInheritors] classes, all are abstract
	let roots = robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[ImplicitDataDefinitionForInheritors\]/ ($"at")))'))

	call map(roots, {i,j -> j.s !=# 'sealed' ? extend(j, {'c': robusttoolbox#get_Children(a:tf, j.t)}) : j})
	return roots
endfunction

" Gets all [DataDefinition] classes in a list, no children
function! robusttoolbox#get_ExplicitDataDefinitions(tf)
	return robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[DataDefinition\]/ ($"at")))')) +
	\ robusttoolbox#splitup_struct(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "S") (#/\[DataDefinition\]/ ($"at")))'))
endfunction

" Gets all [Prototype] classes in a list, no children
function! robusttoolbox#get_Prototypes(tf)
	let p = robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[Prototype\(\"[[:alpha:]]+\"\)/ ($"at")))'))
	return p
endfunction

" Gets all qualified [DataField] members/properties in a list
function! robusttoolbox#get_DataFields(tf)
	let m = robusttoolbox#splitup_member(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "M") (#/\[DataField\(\"/ ($"at")))'))
	let p = robusttoolbox#splitup_member(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "P") (#/\[DataField\(\"/ ($"at")))'))
	" TODO: filter results of the query to only include those with qualified tags, at present none appear to be qualified. Qualification appears to be necessary for scoping of which [DataField] applieѕ to which [DataDefinіtion]
	return m
endfunction

" For kind:C puts into {t:tag, a:attributes, i:inherits, s:[sealed|abstract]}
function! robusttoolbox#splitup_class(list)
	" Is Extended Posix Regex, so can't match nested parens IIRC
	"  Appears to be no performance benefit of not capturing groups
	return map(map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t[^\t]\+\t[^\t]\+\tkind:C\tat:\(([^\t]*)\)\t\inherits:(\([^)]*\))\tsl:(\([^)]*\))')[1:-6]}), {i,j -> {'t': j[0], 'a': j[1], 'i': j[2], 's': j[3]}})
endfunction

" For kind:S puts into {t:tag, a:attributes, i:inherits}
function! robusttoolbox#splitup_struct(list)
	return map(map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t[^\t]\+\t[^\t]\+\tkind:S\tat:\(([^\t]*)\)\t\inherits:(\([^)]*\))')[1:-7]}), {i,j -> {'t': j[0], 'a': j[1], 'i': j[2]}})
endfunction

" For kind:M puts into {t:tag, a:attributes, y:type, d:default}
function! robusttoolbox#splitup_member(list)
	return map(map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t[^\t]\+\t[^\t]\+\tkind:M\tat:\([^\t]*\)\ttype:\([^\t]\+\)\tdf:\(.*\)$')[1:-6]}), {i,j -> {'t': j[0], 'a': j[1], 'y': j[2], 'd': j[3]}})
	"return map(map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t[^\t]\+\t[^\t]\+\tkind:M\tat:\([^\t]\+\)\ttype:\([^\t]\+\)\tdf:\(.*\)$')[1:-6]}), {i,j -> len(j)})
endfunction

function! robusttoolbox#get_Children(tf, tag)
	let children = robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/(^\(|,)'.a:tag.'(,|\)$)/ $inherits))'))
	return map(children, {i,j -> j.s !=# 'sealed' ? extend(j, {'c': robusttoolbox#get_Children(a:tf, j.t)}) : j})
endfunction
